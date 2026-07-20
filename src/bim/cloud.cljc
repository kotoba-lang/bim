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

(defn sync-store
  "Create a transport-neutral store for bidirectional project deltas. The
  returned value is serializable and can be persisted atomically by a cloud
  adapter."
  []
  {:sync/schema-version schema-version :sync/projects {}})

(defn register-sync-project
  "Register a project head and the snapshot required to bootstrap new peers."
  [store project-id revision snapshot]
  (when-not (and project-id (integer? revision) (<= 0 revision))
    (throw (ex-info "sync project requires an id and non-negative revision"
                    {:project-id project-id :revision revision})))
  (if-let [project (get-in store [:sync/projects project-id])]
    (if (and (= revision (:sync/head-revision project))
             (= snapshot (:sync/head-snapshot project)))
      store
      (throw (ex-info "sync project is already registered with different state"
                      {:project-id project-id
                       :head-revision (:sync/head-revision project)})))
    (assoc-in store [:sync/projects project-id]
              {:sync/project-id project-id :sync/head-revision revision
               :sync/head-snapshot snapshot :sync/compacted-through revision
               :sync/deltas [] :sync/publications {} :sync/peer-acks {}})))

(defn publish-delta
  "Append one project delta with optimistic head checking and idempotent retry.
  `apply-delta` must deterministically produce the next project snapshot."
  [store {:keys [project-id peer-id idempotency-key base-revision target-revision delta]
          :as publication}
   apply-delta]
  (let [project (get-in store [:sync/projects project-id])
        existing (get-in project [:sync/publications idempotency-key])
        request (select-keys publication [:project-id :peer-id :idempotency-key
                                          :base-revision :target-revision :delta])]
    (when-not project
      (throw (ex-info "sync project not found" {:project-id project-id})))
    (when-not (and peer-id idempotency-key (integer? base-revision)
                   (integer? target-revision) (= target-revision (inc base-revision)))
      (throw (ex-info "sync publication requires peer, key, and consecutive revisions"
                      {:publication publication})))
    (cond
      existing
      (if (= request (:sync/request existing))
        {:sync/status :deduplicated :sync/store store :sync/delta (:sync/delta existing)}
        (throw (ex-info "sync idempotency key conflicts with prior publication"
                        {:project-id project-id :idempotency-key idempotency-key})))

      (not= base-revision (:sync/head-revision project))
      {:sync/status :conflict :sync/store store
       :sync/conflict {:sync/base-revision base-revision
                       :sync/head-revision (:sync/head-revision project)}}

      :else
      (let [snapshot (apply-delta (:sync/head-snapshot project) delta)
            entry {:sync/project-id project-id :sync/peer-id peer-id
                   :sync/idempotency-key idempotency-key
                   :sync/base-revision base-revision :sync/revision target-revision
                   :sync/delta delta}
            record {:sync/request request :sync/delta entry}
            next-store (-> store
                           (assoc-in [:sync/projects project-id :sync/head-revision]
                                     target-revision)
                           (assoc-in [:sync/projects project-id :sync/head-snapshot] snapshot)
                           (update-in [:sync/projects project-id :sync/deltas] conj entry)
                           (assoc-in [:sync/projects project-id :sync/publications
                                      idempotency-key] record))]
        {:sync/status :applied :sync/store next-store :sync/delta entry
         :sync/head-snapshot snapshot}))))

(defn pull-deltas
  "Return ordered deltas after a peer revision. A peer behind the compaction
  floor must bootstrap from the returned head snapshot instead."
  [store project-id after-revision]
  (let [project (get-in store [:sync/projects project-id])]
    (when-not project
      (throw (ex-info "sync project not found" {:project-id project-id})))
    (when-not (and (integer? after-revision) (<= 0 after-revision)
                   (<= after-revision (:sync/head-revision project)))
      (throw (ex-info "sync cursor is outside the project revision range"
                      {:project-id project-id :after-revision after-revision
                       :head-revision (:sync/head-revision project)})))
    (if (< after-revision (:sync/compacted-through project))
      {:sync/status :snapshot-required
       :sync/project-id project-id :sync/revision (:sync/head-revision project)
       :sync/snapshot (:sync/head-snapshot project)}
      {:sync/status :delta
       :sync/project-id project-id :sync/from-revision after-revision
       :sync/to-revision (:sync/head-revision project)
       :sync/deltas (vec (filter #(> (:sync/revision %) after-revision)
                                 (:sync/deltas project)))})))

(defn acknowledge-revision
  "Advance a peer's durable acknowledgement monotonically."
  [store project-id peer-id revision]
  (let [project (get-in store [:sync/projects project-id])
        previous (get-in project [:sync/peer-acks peer-id]
                         (:sync/compacted-through project))]
    (when-not project
      (throw (ex-info "sync project not found" {:project-id project-id})))
    (when-not (and peer-id (integer? revision) (<= previous revision)
                   (<= revision (:sync/head-revision project)))
      (throw (ex-info "peer acknowledgement must advance within the project head"
                      {:project-id project-id :peer-id peer-id :previous previous
                       :revision revision :head-revision (:sync/head-revision project)})))
    (assoc-in store [:sync/projects project-id :sync/peer-acks peer-id] revision)))

(defn compact-deltas
  "Discard deltas acknowledged by every registered peer while retaining the
  current snapshot for stale or newly joining peers."
  [store project-id]
  (let [project (get-in store [:sync/projects project-id])
        acknowledgements (vals (:sync/peer-acks project))]
    (when-not project
      (throw (ex-info "sync project not found" {:project-id project-id})))
    (if (seq acknowledgements)
      (let [floor (reduce min acknowledgements)]
        (-> store
            (assoc-in [:sync/projects project-id :sync/compacted-through] floor)
            (update-in [:sync/projects project-id :sync/deltas]
                       #(vec (remove (fn [entry] (<= (:sync/revision entry) floor)) %)))))
      store)))
