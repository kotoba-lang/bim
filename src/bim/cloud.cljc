(ns bim.cloud
  "Transport-neutral cloud-itonami BIM synchronization contracts: resumable
  chunk uploads, integrity checks, and idempotent conversion jobs.")

(def schema-version 1)

(defn checksum
  "Portable Adler-32-style checksum for transport corruption detection."
  [text]
  (let [[a b] (reduce (fn [[a b] character]
                        (let [code #?(:clj (int character)
                                     :cljs (.charCodeAt character 0))
                              next-a (mod (+ a code) 65521)]
                          [next-a (mod (+ b next-a) 65521)]))
                      [1 0] text)]
    (str b "-" a)))

(defn create-upload
  "Split serialized BIM/IFC content into independently verifiable chunks."
  [{:keys [id project-id revision media-type content chunk-size]
    :or {media-type "application/ifc" chunk-size 1048576}}]
  (when-not (pos? chunk-size)
    (throw (ex-info "upload chunk size must be positive" {:chunk-size chunk-size})))
  (let [chunks (->> (range 0 (count content) chunk-size)
                    (map-indexed (fn [index start]
                                   (let [data (subs content start (min (count content)
                                                                       (+ start chunk-size)))]
                                     {:chunk/index index :chunk/data data
                                      :chunk/size (count data)
                                      :chunk/checksum (checksum data)})))
                    vec)]
    {:upload/schema-version schema-version :upload/id id :upload/project-id project-id
     :upload/revision revision :upload/media-type media-type
     :upload/size (count content) :upload/checksum (checksum content)
     :upload/chunk-size chunk-size :upload/chunk-count (count chunks)
     :upload/chunks chunks}))

(defn receiver [upload]
  (assoc (select-keys upload [:upload/schema-version :upload/id :upload/project-id
                              :upload/revision :upload/media-type :upload/size
                              :upload/checksum :upload/chunk-size :upload/chunk-count])
         :upload/received {} :upload/status :receiving))

(defn receive-chunk
  "Accept a chunk idempotently; a conflicting retry is rejected."
  [state chunk]
  (let [index (:chunk/index chunk) count (:upload/chunk-count state)
        expected (:chunk/checksum chunk) actual (checksum (:chunk/data chunk))
        existing (get-in state [:upload/received index])]
    (when-not (< -1 index count)
      (throw (ex-info "upload chunk index is out of range" {:index index :count count})))
    (when-not (= expected actual)
      (throw (ex-info "upload chunk checksum mismatch"
                      {:index index :expected expected :actual actual})))
    (when (and existing (not= existing chunk))
      (throw (ex-info "upload chunk retry conflicts with received data" {:index index})))
    (assoc-in state [:upload/received index] chunk)))

(defn missing-chunks [state]
  (vec (remove #(contains? (:upload/received state) %)
               (range (:upload/chunk-count state)))))

(defn complete-upload [state]
  (when-let [missing (seq (missing-chunks state))]
    (throw (ex-info "upload is incomplete" {:missing-chunks (vec missing)})))
  (let [content (apply str (map #(get-in state [:upload/received % :chunk/data])
                                (range (:upload/chunk-count state))))
        actual (checksum content)]
    (when-not (and (= (:upload/size state) (count content))
                   (= (:upload/checksum state) actual))
      (throw (ex-info "completed upload integrity mismatch"
                      {:expected-size (:upload/size state) :actual-size (count content)
                       :expected-checksum (:upload/checksum state) :actual-checksum actual})))
    (assoc state :upload/status :complete :upload/content content)))

(defn job-queue [] {:jobs/by-idempotency-key {} :jobs/by-id {}})

(defn enqueue-job
  "Enqueue once for an idempotency key, returning the existing job on retry."
  [queue {:keys [id idempotency-key kind input] :as request}]
  (when-not (and id idempotency-key kind)
    (throw (ex-info "cloud job requires id, idempotency key, and kind" {:request request})))
  (if-let [existing-id (get-in queue [:jobs/by-idempotency-key idempotency-key])]
    {:jobs/status :deduplicated :jobs/queue queue
     :jobs/job (get-in queue [:jobs/by-id existing-id])}
    (let [job {:job/id id :job/idempotency-key idempotency-key :job/kind kind
               :job/input input :job/status :queued :job/attempt 0}
          next-queue (-> queue
                         (assoc-in [:jobs/by-idempotency-key idempotency-key] id)
                         (assoc-in [:jobs/by-id id] job))]
      {:jobs/status :enqueued :jobs/queue next-queue :jobs/job job})))

(def allowed-job-transitions
  {:queued #{:running :cancelled} :running #{:succeeded :failed}
   :failed #{:queued} :succeeded #{} :cancelled #{}})

(defn transition-job [queue job-id status result]
  (let [job (get-in queue [:jobs/by-id job-id]) current (:job/status job)]
    (when-not job (throw (ex-info "cloud job not found" {:job-id job-id})))
    (when-not (contains? (get allowed-job-transitions current #{}) status)
      (throw (ex-info "invalid cloud job transition"
                      {:job-id job-id :from current :to status})))
    (update-in queue [:jobs/by-id job-id]
               #(cond-> (assoc % :job/status status :job/result result)
                  (= status :running) (update :job/attempt inc)))))
