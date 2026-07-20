(ns bim.spatial
  "Uniform-grid spatial index and deterministic LOD/stream planning for large
  BIM models. Mesh bounds are computed once when the index is built."
  (:require [bim :as bim]))

(def schema-version 1)
(defn- floor [value] (#?(:clj Math/floor :cljs js/Math.floor) value))
(defn- sqrt [value] (#?(:clj Math/sqrt :cljs js/Math.sqrt) value))

(defn element-bounds [element]
  (when-let [positions (seq (:positions (bim/element-mesh element)))]
    {:min (apply mapv min positions) :max (apply mapv max positions)}))

(defn- cell-coordinate [cell-size point]
  (mapv #(long (floor (/ % cell-size))) point))

(defn- cells-for-bounds [cell-size {:keys [min max]}]
  (let [lower (cell-coordinate cell-size min) upper (cell-coordinate cell-size max)]
    (for [x (range (first lower) (inc (first upper)))
          y (range (second lower) (inc (second upper)))
          z (range (nth lower 2) (inc (nth upper 2)))] [x y z])))

(defn build-index
  ([elements] (build-index elements {}))
  ([elements {:keys [cell-size] :or {cell-size 10.0}}]
   (when-not (pos? cell-size)
     (throw (ex-info "spatial index cell size must be positive" {:cell-size cell-size})))
   (let [entries (into {}
                       (keep (fn [element]
                               (when-let [bounds (element-bounds element)]
                                 [(:id element) {:element element :bounds bounds}])))
                       elements)
         cells (reduce-kv
                (fn [result id {:keys [bounds]}]
                  (reduce #(update %1 %2 (fnil conj #{}) id)
                          result (cells-for-bounds cell-size bounds)))
                {} entries)]
     {:spatial/schema-version schema-version :spatial/cell-size cell-size
      :spatial/entries entries :spatial/cells cells})))

(defn- intersects? [left right]
  (every? true? (map (fn [left-min left-max right-min right-max]
                       (and (<= left-min right-max) (<= right-min left-max)))
                     (:min left) (:max left) (:min right) (:max right))))

(defn query
  "Return exactly intersecting indexed elements in stable id order."
  [index bounds]
  (let [ids (into #{} (mapcat #(get-in index [:spatial/cells %] #{}))
                  (cells-for-bounds (:spatial/cell-size index) bounds))]
    (->> ids
         (keep #(get-in index [:spatial/entries %]))
         (filter #(intersects? bounds (:bounds %)))
         (sort-by (comp str :id :element))
         (mapv :element))))

(defn nearest
  "Find the nearest indexed element AABB to a point within a search radius."
  [index point radius]
  (let [bounds {:min (mapv #(- % radius) point) :max (mapv #(+ % radius) point)}
        distance (fn [element]
                   (let [{lower :min upper :max} (get-in index [:spatial/entries (:id element) :bounds])]
                     (sqrt (reduce + (map (fn [value minimum maximum]
                                           (let [delta (cond (< value minimum) (- minimum value)
                                                             (> value maximum) (- value maximum)
                                                             :else 0.0)]
                                             (* delta delta)))
                                         point lower upper)))))]
    (when-let [candidate (first (sort-by (juxt distance (comp str :id))
                                         (query index bounds)))]
      (assoc candidate :spatial/distance (distance candidate)))))

(defn choose-lod
  "Choose :full, :coarse, or :bounds from projected AABB size in pixels."
  [bounds camera-position focal-length-px {:keys [full-threshold coarse-threshold]
                                           :or {full-threshold 80.0 coarse-threshold 12.0}}]
  (let [center (mapv #(/ (+ %1 %2) 2.0) (:min bounds) (:max bounds))
        diagonal (sqrt (reduce + (map (fn [a b] (let [delta (- b a)] (* delta delta)))
                                      (:min bounds) (:max bounds))))
        distance (max 1.0e-6
                      (sqrt (reduce + (map (fn [a b] (let [delta (- b a)] (* delta delta)))
                                           camera-position center))))
        projected (* focal-length-px (/ diagonal distance))]
    {:lod/level (cond (>= projected full-threshold) :full
                      (>= projected coarse-threshold) :coarse
                      :else :bounds)
     :lod/projected-size-px projected :lod/distance distance}))

(defn stream-plan
  "Partition visible elements into bounded batches with their requested LOD."
  [index view-bounds camera-position focal-length-px max-batch-size]
  (when-not (pos? max-batch-size)
    (throw (ex-info "stream batch size must be positive" {:max-batch-size max-batch-size})))
  (->> (query index view-bounds)
       (mapv (fn [element]
               (let [bounds (get-in index [:spatial/entries (:id element) :bounds])]
                 {:element/id (:id element) :element element
                  :bounds bounds :lod (choose-lod bounds camera-position focal-length-px {})})))
       (partition-all max-batch-size)
       (map-indexed (fn [index entries]
                      {:stream/batch index :stream/entries (vec entries)}))
       vec))
