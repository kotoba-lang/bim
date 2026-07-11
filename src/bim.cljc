(ns bim
  "KAMI BIM — Building Information Modeling kernel. Restored from the
  legacy kami-engine/kami-bim Rust crate (deleted in kotoba-lang/
  kami-engine PR #82 'Remove Rust workspace from kami-engine') as part of
  the clj-wgsl migration (ADR-2607010930, com-junkawasaki/root).

  An IFC-like model for building authoring: spatial hierarchy (Project ->
  Site -> Building -> Storey -> Space), element taxonomy (Wall/Slab/
  Column/Beam/Door/Window/Roof/Stair/Railing/Furniture/MepSegment/
  Opening), PropertySet (Pset_*) and Qto_* quantities (IFC convention),
  material/layer/classification, and a link to kotoba-lang/brep for
  element geometry (BREP body or Axis+Profile sweep).

  Zero-dep portable CLJC — pure data + pure functions, no IO/GPU. f64
  precision throughout ([x y z] 3-vectors / affine transforms as opaque
  data, matching kotoba-lang/brep's conventions). A `BimId` is either a
  string (IFC GUID) or an integer (local id) — Clojure doesn't need the
  original's `#[serde(untagged)]` enum wrapper since both cases are
  already distinguishable scalar types.

  Depends on kotoba-lang/brep for element BREP geometry.")

;; ── spatial hierarchy ──

(def space-categories
  #{:office :residential :circulation :service :mechanical-room :outdoor-covered :external :other})

(defn unit-system
  ([] (unit-system {}))
  ([{:keys [length angle time] :or {length :metre angle :radian time :second}}]
   {:length length :angle angle :time time}))

(defn project
  "A fresh, empty BIM project."
  [name]
  {:id 0 :name name :description "" :units (unit-system)
   :world-origin [0.0 0.0 0.0] :true-north-rad 0.0 :sites [] :psets {}})

(defn geo-ref [latitude-deg longitude-deg elevation-m]
  {:latitude-deg latitude-deg :longitude-deg longitude-deg :elevation-m elevation-m})

(defn site [{:keys [id name geo placement buildings]}]
  {:id id :name name :geo geo :placement placement :buildings (vec buildings)})

(defn building [{:keys [id name placement reference-elevation storeys]}]
  {:id id :name name :placement placement :reference-elevation reference-elevation :storeys (vec storeys)})

(defn storey [{:keys [id name elevation height placement spaces elements]}]
  {:id id :name name :elevation elevation :height height :placement placement
   :spaces (vec spaces) :elements (vec elements)})

(defn space [{:keys [id name long-name label category boundary height quantities psets]}]
  {:id id :name name :long-name long-name :label label :category category
   :boundary boundary :height height :quantities quantities :psets (or psets {})})

;; ── elements ──

(def element-kinds
  #{:wall :slab :roof :column :beam :door :window :stair :railing :curtain
    :furniture :mep-segment :opening :other})

(defn- sqrt [x] #?(:clj (Math/sqrt x) :cljs (js/Math.sqrt x)))

(defn element [{:keys [id kind name global-id placement geometry material-layers
                        classification quantities psets openings connected-to]}]
  {:id id :kind kind :name name :global-id global-id :placement placement :geometry geometry
   :material-layers (vec material-layers) :classification classification
   :quantities (or quantities {}) :psets (or psets {}) :openings (vec openings)
   :connected-to (vec connected-to)})

;; ElementGeometry variants
(defn brep-geometry [brep-solid] {:kind :brep :solid brep-solid})
(defn axis-sweep-geometry [axis profile] {:kind :axis-sweep :axis axis :profile profile})
(defn mesh-ref-geometry [blob-key triangle-count] {:kind :mesh-ref :blob-key blob-key :triangle-count triangle-count})
(defn no-geometry [] {:kind :none})

;; Profile variants
(defn rectangle-profile [thickness height] {:kind :rectangle :thickness thickness :height height})
(defn circle-profile [diameter] {:kind :circle :diameter diameter})
(defn i-shape-profile [height flange-width flange-thickness web-thickness]
  {:kind :i-shape :height height :flange-width flange-width
   :flange-thickness flange-thickness :web-thickness web-thickness})
(defn polygon-profile [points] {:kind :polygon :points points})

(defn opening [{:keys [id placement profile depth filled-by]}]
  {:id id :placement placement :profile profile :depth depth :filled-by filled-by})

;; ── material ──

(def material-categories
  #{:concrete :steel :timber :masonry :gypsum :insulation :glass :finish :other})

(defn material-layer [material thickness is-ventilated category]
  {:material material :thickness thickness :is-ventilated is-ventilated :category category})

;; ── classification / properties / quantities ──

(defn classification-ref [source code description] {:source source :code code :description description})

(defn property-set
  ([name] (property-set name {}))
  ([name props] {:name name :props props}))

;; PropertyValue variants
(defn bool-value [v] {:kind :bool :value v})
(defn int-value [v] {:kind :int :value v})
(defn real-value [v] {:kind :real :value v})
(defn text-value [v] {:kind :text :value v})
(defn measured-value [value unit] {:kind :measured :value value :unit unit})
(defn enum-value [value allowed] {:kind :enum :value value :allowed allowed})

(defn quantities
  ([] (quantities {}))
  ([{:keys [gross-area-m2 net-area-m2 gross-volume-m3 net-volume-m3 weight-kg length-m]}]
   {:gross-area-m2 gross-area-m2 :net-area-m2 net-area-m2 :gross-volume-m3 gross-volume-m3
    :net-volume-m3 net-volume-m3 :weight-kg weight-kg :length-m length-m}))

;; ── scene projection (consumed by a kotoba-lang/pipelines-style scene adapter) ──

(defn storey-scene [{:keys [storey-id storey-name elevation items bounds-min bounds-max]}]
  {:storey-id storey-id :storey-name storey-name :elevation elevation
   :items (vec items) :bounds-min bounds-min :bounds-max bounds-max})

(defn scene-item [{:keys [element-id kind world-transform geom base-color highlight]}]
  {:element-id element-id :kind kind :world-transform world-transform :geom geom
   :base-color base-color :highlight highlight})

;; SceneGeom variants
(defn triangles-geom [positions indices normals] {:kind :triangles :positions positions :indices indices :normals normals})
(defn axis-geom [points] {:kind :axis :points points})
(defn mesh-ref-geom [blob-key] {:kind :mesh-ref :blob-key blob-key})

(def highlights #{:none :selected :reviewed :has-issue})

;; ── model helpers ──

(defn for-each-element
  "Call `f` on every element in `proj`, walking site -> building -> storey."
  [proj f]
  (doseq [s (:sites proj) b (:buildings s) st (:storeys b) e (:elements st)]
    (f e)))

(defn find-storey
  "Find a storey by `id` (linear scan across the full hierarchy)."
  [proj id]
  (some (fn [s]
          (some (fn [b]
                  (some (fn [st] (when (= (:id st) id) st)) (:storeys b)))
                (:buildings s)))
        (:sites proj)))

(defn- update-storey* [proj storey-id f]
  (update proj :sites
          (fn [sites]
            (mapv (fn [s]
                    (update s :buildings
                            (fn [buildings]
                              (mapv (fn [b]
                                      (update b :storeys
                                              (fn [storeys]
                                                (mapv #(if (= storey-id (:id %)) (f %) %) storeys))))
                                    buildings))))
                  sites))))

(defn add-element [proj storey-id elem]
  (when-not (find-storey proj storey-id)
    (throw (ex-info "storey not found" {:storey-id storey-id})))
  (update-storey* proj storey-id #(update % :elements conj elem)))

(defn update-element [proj storey-id element-id f & args]
  (update-storey* proj storey-id
                   #(update % :elements
                            (fn [elements]
                              (mapv (fn [e] (if (= element-id (:id e))
                                              (apply f e args) e)) elements)))))

(defn delete-element [proj storey-id element-id]
  (update-storey* proj storey-id
                   #(update % :elements
                            (fn [elements] (vec (remove (fn [e] (= element-id (:id e))) elements))))))

(defn wall
  [{:keys [id name start end thickness height material]
    :or {name "Wall" thickness 0.2 height 3.0 material "Concrete"}}]
  (element {:id id :kind :wall :name name :global-id (str id) :placement :identity
            :geometry (axis-sweep-geometry [start end] (rectangle-profile thickness height))
            :material-layers [(material-layer material thickness false :concrete)]
            :classification (classification-ref "Uniclass" "Ss_25_10" "Wall systems")
            :quantities (quantities {:length-m (sqrt (+ (let [d (- (first end) (first start))] (* d d))
                                                        (let [d (- (second end) (second start))] (* d d))))})
            :psets {"Pset_WallCommon" (property-set "Pset_WallCommon" {:IsExternal (bool-value true)})}
            :openings [] :connected-to []}))

(defn wall-mesh
  "Convert a horizontal axis-sweep rectangle wall into an indexed box mesh."
  [{:keys [geometry]}]
  (let [[[x0 y0 z0] [x1 y1 _]] (:axis geometry)
        {:keys [thickness height]} (:profile geometry)
        dx (- x1 x0) dy (- y1 y0) len (sqrt (+ (* dx dx) (* dy dy)))
        px (* (/ (- dy) len) (/ thickness 2)) py (* (/ dx len) (/ thickness 2))
        positions [[(+ x0 px) (+ y0 py) z0] [(- x0 px) (- y0 py) z0]
                   [(+ x1 px) (+ y1 py) z0] [(- x1 px) (- y1 py) z0]
                   [(+ x0 px) (+ y0 py) (+ z0 height)] [(- x0 px) (- y0 py) (+ z0 height)]
                   [(+ x1 px) (+ y1 py) (+ z0 height)] [(- x1 px) (- y1 py) (+ z0 height)]]
        indices [0 2 1 1 2 3, 4 5 6 5 7 6, 0 4 2 2 4 6,
                 1 3 5 3 7 5, 0 1 4 1 5 4, 2 6 3 3 6 7]]
    {:positions positions :indices indices :normals (vec (repeat 8 [0 0 1]))}))

(defn merge-meshes [meshes]
  (loop [remaining meshes positions [] normals [] indices []]
    (if-let [m (first remaining)]
      (let [offset (count positions)]
        (recur (next remaining) (into positions (:positions m)) (into normals (:normals m))
               (into indices (map #(+ offset %) (:indices m)))))
      {:positions (vec positions) :normals (vec normals) :indices (vec indices)})))
