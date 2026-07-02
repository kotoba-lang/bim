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
