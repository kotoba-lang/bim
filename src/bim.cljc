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

  Depends on kotoba-lang/brep for element BREP geometry."
  (:require [brep.mesh-csg :as mesh-csg]
            [brep.polygon :as polygon]
            [brep.spline :as spline]))

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
                        classification quantities psets openings connected-to
                        appearance presentation-layers]}]
  {:id id :kind kind :name name :global-id global-id :placement placement :geometry geometry
   :material-layers (vec material-layers) :classification classification
   :quantities (or quantities {}) :psets (or psets {}) :openings (vec openings)
   :connected-to (vec connected-to) :appearance appearance
   :presentation-layers (vec presentation-layers)})

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

(defn rectangular-opening
  [{:keys [id offset sill width height depth filled-by]
    :or {sill 0.0 depth 0.3}}]
  (when (or (neg? offset) (neg? sill) (not (pos? width)) (not (pos? height)))
    (throw (ex-info "opening dimensions and placement are invalid"
                    {:offset offset :sill sill :width width :height height})))
  (opening {:id id :placement {:offset offset :sill sill}
            :profile {:kind :rectangle :width width :height height}
            :depth depth :filled-by filled-by}))

(declare no-geometry classification-ref quantities property-set enum-value measured-value)
(defn door
  [{:keys [id name host-id opening-id width height operation]
    :or {name "Door" width 0.9 height 2.1 operation :single-swing-left}}]
  (element {:id id :kind :door :name name :global-id (str id) :placement :hosted
            :geometry (no-geometry) :classification (classification-ref "Uniclass" "Pr_30_59_24" "Doors")
            :quantities (quantities {})
            :psets {"Pset_DoorCommon" (property-set "Pset_DoorCommon"
                                                      {:OperationType (enum-value operation #{:single-swing-left :single-swing-right :double-swing})
                                                       :OverallWidth (measured-value width :metre)
                                                       :OverallHeight (measured-value height :metre)})}
            :openings [] :connected-to [host-id opening-id]}))

(defn window
  [{:keys [id name host-id opening-id width height operation]
    :or {name "Window" width 1.2 height 1.2 operation :fixed}}]
  (element {:id id :kind :window :name name :global-id (str id) :placement :hosted
            :geometry (no-geometry) :classification (classification-ref "Uniclass" "Pr_30_59_98" "Windows")
            :quantities (quantities {})
            :psets {"Pset_WindowCommon" (property-set "Pset_WindowCommon"
                                                        {:OperationType (enum-value operation #{:fixed :casement :sliding})
                                                         :OverallWidth (measured-value width :metre)
                                                         :OverallHeight (measured-value height :metre)})}
            :openings [] :connected-to [host-id opening-id]}))

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
(defn enum-values [values allowed] {:kind :enum-list :values (vec values) :allowed (vec allowed)})
(defn bounded-value [{:keys [lower upper set-point unit value-type]}]
  {:kind :bounded :lower lower :upper upper :set-point set-point
   :unit unit :value-type value-type})
(defn list-value [values unit value-type]
  {:kind :list :values (vec values) :unit unit :value-type value-type})

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

(defn find-building [proj id]
  (some (fn [s] (some #(when (= id (:id %)) %) (:buildings s))) (:sites proj)))

(defn- update-building* [proj building-id f]
  (update proj :sites
          #(mapv (fn [site]
                   (update site :buildings
                           (fn [buildings] (mapv (fn [b] (if (= building-id (:id b)) (f b) b)) buildings)))) %)))

(defn add-storey [proj building-id new-storey]
  (when-not (find-building proj building-id) (throw (ex-info "building not found" {:building-id building-id})))
  (when (find-storey proj (:id new-storey)) (throw (ex-info "storey id already exists" {:storey-id (:id new-storey)})))
  (update-building* proj building-id #(update % :storeys conj new-storey)))

(defn delete-storey [proj building-id storey-id]
  (let [target (find-storey proj storey-id)]
    (when-not target (throw (ex-info "storey not found" {:storey-id storey-id})))
    (when (or (seq (:elements target)) (seq (:spaces target)))
      (throw (ex-info "cannot delete non-empty storey" {:storey-id storey-id})))
    (update-building* proj building-id #(update % :storeys (fn [storeys] (vec (remove (fn [s] (= storey-id (:id s))) storeys)))))))

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

(defn- map-point [point point-fn]
  (when point (vec (point-fn point))))

(defn- map-placement [placement point-fn vector-fn]
  (if (or (nil? placement) (= :identity placement) (map? placement))
    (let [placement (if (map? placement) placement {})]
      (cond-> (assoc placement :location
                     (map-point (or (:location placement) [0.0 0.0 0.0]) point-fn))
        (:axis placement) (update :axis #(map-point % vector-fn))
        (:ref-direction placement) (update :ref-direction #(map-point % vector-fn))))
    placement))

(declare map-geometry)

(defn- map-curve [curve point-fn vector-fn]
  (case (:kind curve)
    :polyline (update curve :points #(mapv (fn [p] (map-point p point-fn)) %))
    :indexed-polycurve (update curve :points #(mapv (fn [p] (map-point p point-fn)) %))
    :composite-curve (update curve :segments
                             #(mapv (fn [segment]
                                      (update segment :parent-curve map-curve point-fn vector-fn)) %))
    :line (-> curve (update :origin map-point point-fn)
              (update :direction map-point vector-fn))
    (:circle :ellipse) (update curve :position map-placement point-fn vector-fn)
    :b-spline-curve (update curve :control-points
                            #(mapv (fn [p] (map-point p point-fn)) %))
    curve))

(defn- map-bound [bound point-fn vector-fn]
  (cond-> bound
    (seq (:points bound))
    (update :points #(mapv (fn [p] (map-point p point-fn)) %))
    (seq (:edges bound))
    (update :edges
            #(mapv (fn [edge]
                     (-> edge
                         (update :start map-point point-fn)
                         (update :end map-point point-fn)
                         (update :curve map-curve point-fn vector-fn))) %))))

(defn- map-surface [surface point-fn vector-fn]
  (cond-> surface
    (:position surface) (update :position map-placement point-fn vector-fn)
    (= :b-spline-surface (:kind surface))
    (update :control-points
            #(mapv (fn [row] (mapv (fn [p] (map-point p point-fn)) row)) %))))

(defn- map-face [face point-fn vector-fn]
  (cond-> (update face :bounds #(mapv (fn [bound] (map-bound bound point-fn vector-fn)) %))
    (:surface face)
    (update :surface map-surface point-fn vector-fn)))

(defn- map-geometry [geometry point-fn vector-fn]
  (case (:kind geometry)
    :axis-sweep (update geometry :axis #(mapv (fn [p] (map-point p point-fn)) %))
    :slab-extrusion (update geometry :boundary #(mapv (fn [p] (map-point p point-fn)) %))
    (:extruded-area-solid :revolved-area-solid
     :fixed-reference-swept-area-solid :surface-curve-swept-area-solid)
    (cond-> (update geometry :position map-placement point-fn vector-fn)
      (:direction geometry) (update :direction map-point vector-fn)
      (:axis geometry) (update :axis map-placement point-fn vector-fn)
      (:directrix geometry) (update :directrix map-curve point-fn vector-fn)
      (:fixed-reference geometry) (update :fixed-reference map-point vector-fn)
      (:reference-surface geometry) (update :reference-surface map-surface point-fn vector-fn))
    :swept-disk-solid (update geometry :directrix
                              #(mapv (fn [p] (map-point p point-fn)) %))
    :mapped-item
    (-> geometry
        (update-in [:transform :origin]
                   #(map-point (or % [0.0 0.0 0.0]) point-fn))
        (cond-> (get-in geometry [:transform :axis1])
          (update-in [:transform :axis1] map-point vector-fn))
        (cond-> (get-in geometry [:transform :axis2])
          (update-in [:transform :axis2] map-point vector-fn))
        (cond-> (get-in geometry [:transform :axis3])
          (update-in [:transform :axis3] map-point vector-fn)))
    (:faceted-brep :advanced-brep)
    (update geometry :faces #(mapv (fn [face] (map-face face point-fn vector-fn)) %))
    (:triangulated-face-set :polygonal-face-set)
    (update geometry :coordinates #(mapv (fn [p] (map-point p point-fn)) %))
    :collection (update geometry :items #(mapv (fn [item] (map-geometry item point-fn vector-fn)) %))
    :boolean-result (-> geometry
                        (update :first-operand map-geometry point-fn vector-fn)
                        (update :second-operand map-geometry point-fn vector-fn))
    :half-space-solid
    (cond-> geometry
      (get-in geometry [:base-surface :position])
      (update :base-surface map-surface point-fn vector-fn)
      (:position geometry) (update :position map-placement point-fn vector-fn)
      (get-in geometry [:boundary :points])
      (update-in [:boundary :points]
                 #(mapv (fn [p] (map-point p point-fn)) %)))
    geometry))

(defn translate-geometry
  "Translate renderable BIM/IFC geometry by a world-space `[dx dy dz]` delta."
  [geometry delta]
  (map-geometry geometry #(mapv + % (take (count %) delta)) identity))

(defn- map-element [element point-fn vector-fn]
  (cond-> (-> element
              (update :placement map-placement point-fn vector-fn)
              (update :geometry map-geometry point-fn vector-fn))
    (seq (:mep/connectors element))
    (update :mep/connectors
            #(mapv (fn [connector]
                     (cond-> connector
                       (:connector/point connector)
                       (update :connector/point map-point point-fn)
                       (:connector/direction connector)
                       (update :connector/direction map-point vector-fn))) %))
    (seq (:ports element))
    (update :ports
            #(mapv (fn [port]
                     (update port :placement map-placement point-fn vector-fn)) %))))

(defn translate-element
  "Move an element without changing its identity, type, quantities, or links."
  [element delta]
  (when-not (and (= 3 (count delta))
                 (every? #(and (number? %)
                               #?(:clj (Double/isFinite (double %))
                                  :cljs (js/Number.isFinite %))) delta))
    (throw (ex-info "element translation must be a numeric 3D delta" {:delta delta})))
  (map-element element #(mapv + % (take (count %) delta)) identity))

(defn- finite-number? [value]
  (and (number? value)
       #?(:clj (Double/isFinite (double value)) :cljs (js/Number.isFinite value))))

(defn- valid-point3? [point]
  (and (= 3 (count point)) (every? finite-number? point)))

(defn- rotate-z-point [point pivot angle]
  (let [[x y & [z]] point [px py pz] pivot
        cosine (#?(:clj Math/cos :cljs js/Math.cos) angle)
        sine (#?(:clj Math/sin :cljs js/Math.sin) angle)
        dx (- x px) dy (- y py)
        result [(+ px (- (* dx cosine) (* dy sine)))
                (+ py (* dx sine) (* dy cosine))
                (+ pz (- (or z pz) pz))]]
    (subvec result 0 (count point))))

(defn rotate-element-z
  "Rotate an element around a world-space Z axis through `pivot`, in radians."
  [element pivot angle]
  (when-not (and (valid-point3? pivot) (finite-number? angle))
    (throw (ex-info "element rotation requires a 3D pivot and finite angle"
                    {:pivot pivot :angle angle})))
  (map-element element
               #(rotate-z-point % pivot angle)
               #(rotate-z-point % [0.0 0.0 0.0] angle)))

(defn- dot3 [left right] (reduce + (map * left right)))

(defn- reflect-vector [vector unit-normal]
  (mapv - vector (mapv #(* 2.0 (dot3 vector unit-normal) %) unit-normal)))

(defn- reflect-point [point origin unit-normal]
  (let [dimension (count point)
        point3 (vec (concat point (repeat (- 3 dimension) 0.0)))
        delta (mapv - point3 origin)
        reflected (mapv + origin (reflect-vector delta unit-normal))]
    (subvec reflected 0 dimension)))

(defn mirror-element
  "Mirror an element across a world-space plane defined by origin and normal."
  [element origin normal]
  (when-not (and (valid-point3? origin) (valid-point3? normal)
                 (pos? (dot3 normal normal)))
    (throw (ex-info "element mirror requires a 3D plane with non-zero normal"
                    {:origin origin :normal normal})))
  (let [magnitude (sqrt (dot3 normal normal))
        unit-normal (mapv #(/ % magnitude) normal)]
    (map-element element
                 #(reflect-point % origin unit-normal)
                 #(reflect-vector % unit-normal))))

(defn duplicate-element
  "Create an independent translated copy. Root/opening identities are renewed
  and external topology links are cleared so the copy cannot alias its source."
  ([element new-id delta]
   (duplicate-element element new-id (str new-id) delta))
  ([element new-id new-global-id delta]
   (when (or (nil? new-id) (not (string? new-global-id)) (empty? new-global-id))
     (throw (ex-info "element copy requires fresh local and global identities"
                     {:id new-id :global-id new-global-id})))
   (let [copy (translate-element element delta)]
     (-> copy
         (assoc :id new-id :global-id new-global-id :connected-to [])
         (update :openings
                 (fn [openings]
                   (mapv (fn [index opening]
                           (assoc opening :id (str new-id "-opening-" (inc index))
                                          :filled-by nil))
                         (range) openings)))
         (update :mep/connectors
                 (fn [connectors]
                   (mapv #(assoc % :connector/connected-to nil) connectors)))))))

(defn linear-array
  "Create translated copies for ordered `{:id :global-id}` identities.
  `step` is the world-space displacement between adjacent instances."
  [element identities step]
  (when-not (valid-point3? step)
    (throw (ex-info "linear array requires a finite 3D step" {:step step})))
  (mapv (fn [index {:keys [id global-id]}]
          (duplicate-element element id (or global-id (str id))
                             (mapv #(* (inc index) %) step)))
        (range) identities))

(defn radial-array
  "Create rotated copies for ordered identities around a world-space Z axis."
  [element identities pivot angle-step]
  (when-not (and (valid-point3? pivot) (finite-number? angle-step))
    (throw (ex-info "radial array requires a 3D pivot and finite angle step"
                    {:pivot pivot :angle-step angle-step})))
  (mapv (fn [index {:keys [id global-id]}]
          (-> (duplicate-element element id (or global-id (str id)) [0.0 0.0 0.0])
              (rotate-element-z pivot (* (inc index) angle-step))))
        (range) identities))

(declare polygon-area)

(defn room-space
  [{:keys [id name label category boundary height]
    :or {name "Room" category :other height 3.0}}]
  (when (< (count boundary) 3)
    (throw (ex-info "room boundary needs at least three points" {:id id})))
  (let [area (polygon-area boundary)]
    (space {:id id :name name :long-name name :label label :category category
            :boundary (vec boundary) :height height
            :quantities (quantities {:gross-area-m2 area :net-area-m2 area
                                     :gross-volume-m3 (* area height)
                                     :net-volume-m3 (* area height)})})))

(defn add-space [proj storey-id new-space]
  (when-not (find-storey proj storey-id)
    (throw (ex-info "storey not found" {:storey-id storey-id})))
  (update-storey* proj storey-id #(update % :spaces conj new-space)))

(defn update-space [proj storey-id space-id f & args]
  (update-storey* proj storey-id
                   #(update % :spaces
                            (fn [spaces]
                              (mapv (fn [s] (if (= space-id (:id s))
                                              (apply f s args) s)) spaces)))))

(defn delete-space [proj storey-id space-id]
  (update-storey* proj storey-id
                   #(update % :spaces
                            (fn [spaces] (vec (remove (fn [s] (= space-id (:id s))) spaces))))))

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

(defn set-wall-layers
  "Apply an ordered compound build-up, deriving total thickness and volumes."
  [wall-element layers]
  (when-not (= :wall (:kind wall-element))
    (throw (ex-info "compound layers require a wall" {:element-id (:id wall-element)})))
  (when-not (seq layers)
    (throw (ex-info "compound wall requires at least one material layer" {})))
  (doseq [layer layers]
    (when-not (and (some? (:material layer)) (finite-number? (:thickness layer))
                   (pos? (:thickness layer))
                   (contains? material-categories (:category layer)))
      (throw (ex-info "invalid compound wall layer" {:layer layer}))))
  (let [total-thickness (reduce + (map :thickness layers))
        length (get-in wall-element [:quantities :length-m])
        height (get-in wall-element [:geometry :profile :height])
        opening-area (reduce + 0.0 (map #(let [{:keys [width height]} (:profile %)]
                                                (* width height))
                                            (:openings wall-element)))
        gross-area (* length height) net-area (- gross-area opening-area)
        layers (mapv #(assoc % :gross-volume-m3 (* gross-area (:thickness %))
                               :net-volume-m3 (* net-area (:thickness %))) layers)
        gross-volume (* gross-area total-thickness)
        net-volume (* net-area total-thickness)]
    (-> wall-element
        (assoc :material-layers layers)
        (assoc-in [:geometry :profile :thickness] total-thickness)
        (assoc-in [:quantities :gross-area-m2] gross-area)
        (assoc-in [:quantities :net-area-m2] net-area)
        (assoc-in [:quantities :gross-volume-m3] gross-volume)
        (assoc-in [:quantities :net-volume-m3] net-volume))))

(defn compound-wall
  "Construct a wall whose thickness and material quantities derive from layers."
  [{:keys [layers] :as options}]
  (set-wall-layers (wall (dissoc options :layers)) layers))

(defn- polygon-area [points]
  (#?(:clj Math/abs :cljs js/Math.abs)
   (/ (reduce + (map (fn [[[x1 y1 _] [x2 y2 _]]] (- (* x1 y2) (* x2 y1)))
                     (partition 2 1 (conj (vec points) (first points))))) 2)))

(defn slab
  [{:keys [id name boundary thickness material]
    :or {name "Slab" thickness 0.25 material "Concrete"}}]
  (when (< (count boundary) 3) (throw (ex-info "slab boundary needs at least three points" {})))
  (when-not (pos? thickness) (throw (ex-info "slab thickness must be positive" {:thickness thickness})))
  (let [area (polygon-area boundary)]
    (element {:id id :kind :slab :name name :global-id (str id) :placement :identity
              :geometry {:kind :slab-extrusion :boundary (vec boundary) :thickness thickness}
              :material-layers [(material-layer material thickness false :concrete)]
              :classification (classification-ref "Uniclass" "Ss_20_30" "Floor systems")
              :quantities (quantities {:gross-area-m2 area :net-area-m2 area
                                       :gross-volume-m3 (* area thickness) :net-volume-m3 (* area thickness)})
              :psets {"Pset_SlabCommon" (property-set "Pset_SlabCommon" {:IsExternal (bool-value false)})}
              :openings [] :connected-to []})))

(defn slab-mesh [{:keys [geometry]}]
  (let [boundary (:boundary geometry) thickness (:thickness geometry) n (count boundary)
        top (mapv (fn [[x y z]] [x y (+ z thickness)]) boundary)
        positions (into (vec boundary) top)
        bottom-tris (mapcat (fn [i] [0 (inc i) i]) (range 1 (dec n)))
        top-tris (mapcat (fn [i] [n (+ n i) (+ n (inc i))]) (range 1 (dec n)))
        sides (mapcat (fn [i] (let [j (mod (inc i) n)] [i j (+ n i), j (+ n j) (+ n i)])) (range n))
        indices (vec (concat bottom-tris top-tris sides))]
    {:positions positions :indices indices :normals (vec (repeat (* 2 n) [0 0 1]))}))

(defn- point3 [point]
  (let [[x y & [z]] (or point [0.0 0.0 0.0])]
    [(or x 0.0) (or y 0.0) (or z 0.0)]))

(declare v3-normalize v3-cross)
(defn- prism-mesh [boundary extrusion]
  (let [n (count boundary) top (mapv #(mapv + % extrusion) boundary)
        positions (into (vec boundary) top)
        bottom-tris (mapcat (fn [i] [0 (inc i) i]) (range 1 (dec n)))
        top-tris (mapcat (fn [i] [n (+ n i) (+ n (inc i))]) (range 1 (dec n)))
        sides (mapcat (fn [i]
                        (let [j (mod (inc i) n)] [i j (+ n i), j (+ n j) (+ n i)]))
                      (range n))]
    {:positions positions :indices (vec (concat bottom-tris top-tris sides))
     :normals (vec (repeat (* 2 n) (v3-normalize extrusion)))}))

(defn- profile-points [profile]
  (let [raw-points (case (:kind profile)
                     :rectangle (let [hx (/ (:x-dim profile) 2.0) hy (/ (:y-dim profile) 2.0)]
                                  [[(- hx) (- hy)] [hx (- hy)] [hx hy] [(- hx) hy]])
                     :arbitrary-closed (:points profile)
                     :circle (mapv (fn [i]
                                     (let [angle (* 2.0 #?(:clj Math/PI :cljs js/Math.PI) (/ i 24.0))]
                                       [(* (:radius profile) (#?(:clj Math/cos :cljs js/Math.cos) angle))
                                        (* (:radius profile) (#?(:clj Math/sin :cljs js/Math.sin) angle))]))
                                   (range 24))
                     :i-shape (let [w (/ (:overall-width profile) 2.0)
                                    d (/ (:overall-depth profile) 2.0)
                                    web (/ (:web-thickness profile) 2.0)
                                    flange (:flange-thickness profile)]
                                [[(- w) (- d)] [w (- d)] [w (+ (- d) flange)]
                                 [web (+ (- d) flange)] [web (- d flange)] [w (- d flange)]
                                 [w d] [(- w) d] [(- w) (- d flange)]
                                 [(- web) (- d flange)] [(- web) (+ (- d) flange)]
                                 [(- w) (+ (- d) flange)]])
                     nil)]
    (if (= (first raw-points) (last raw-points)) (vec (butlast raw-points)) raw-points)))

(defn- positioned-profile-boundary [profile position]
  (let [points (profile-points profile)
        origin (point3 (:location position))
        z-axis (v3-normalize (point3 (or (:axis position) [0.0 0.0 1.0])))
        x-axis (v3-normalize (point3 (or (:ref-direction position) [1.0 0.0 0.0])))
        y-axis (v3-normalize (v3-cross z-axis x-axis))
        [px py pz] (get-in profile [:position :location] [0.0 0.0 0.0])
        boundary (mapv (fn [point]
                         (let [[x y z] (point3 point)]
                           (mapv + origin
                                 (mapv #(* (+ px x) %) x-axis)
                                 (mapv #(* (+ py y) %) y-axis)
                                 (mapv #(* (+ pz z) %) z-axis)))) points)]
    boundary))

(defn- extruded-area-mesh [geometry]
  (let [boundary (positioned-profile-boundary (:profile geometry) (:position geometry))
        position (:position geometry)
        z-axis (v3-normalize (point3 (or (:axis position) [0.0 0.0 1.0])))
        x-axis (v3-normalize (point3 (or (:ref-direction position) [1.0 0.0 0.0])))
        y-axis (v3-normalize (v3-cross z-axis x-axis))
        [dx dy dz] (point3 (or (:direction geometry) [0.0 0.0 1.0]))
        unit-direction (v3-normalize
                        (mapv + (mapv #(* dx %) x-axis)
                              (mapv #(* dy %) y-axis) (mapv #(* dz %) z-axis)))
        extrusion (mapv #(* (:depth geometry) %) unit-direction)]
    (when (>= (count boundary) 3)
      (prism-mesh boundary extrusion))))

(defn- transform-point [[x y z] {:keys [axis1 axis2 axis3 origin scale scale2 scale3]} mapping-origin]
  (let [[mx my mz] (point3 (:location mapping-origin))
        [x y z] [(- x mx) (- y my) (- z mz)]
        [a1x a1y a1z] (point3 axis1) [a2x a2y a2z] (point3 axis2)
        [a3x a3y a3z] (point3 axis3) [ox oy oz] (point3 origin)
        sx (or scale 1.0) sy (* sx (or scale2 1.0)) sz (* sx (or scale3 1.0))]
    [(+ ox (* x sx a1x) (* y sy a2x) (* z sz a3x))
     (+ oy (* x sx a1y) (* y sy a2y) (* z sz a3y))
     (+ oz (* x sx a1z) (* y sy a2z) (* z sz a3z))]))

(declare geometry-mesh merge-meshes)
(defn- mapped-item-mesh [{:keys [source transform mapping-origin]}]
  (when-let [mesh (geometry-mesh source)]
    (update mesh :positions #(mapv (fn [point] (transform-point point transform mapping-origin)) %))))

(defn- face-normal [[[ax ay az] [bx by bz] [cx cy cz]]]
  (let [ux (- bx ax) uy (- by ay) uz (- bz az)
        vx (- cx ax) vy (- cy ay) vz (- cz az)
        [nx ny nz] [(- (* uy vz) (* uz vy))
                    (- (* uz vx) (* ux vz))
                    (- (* ux vy) (* uy vx))]
        length (sqrt (+ (* nx nx) (* ny ny) (* nz nz)))]
    (if (pos? length) [(/ nx length) (/ ny length) (/ nz length)] [0.0 0.0 0.0])))

(defn- planar-projector [normal]
  (let [[nx ny nz] (mapv #(#?(:clj Math/abs :cljs js/Math.abs) %) normal)]
    (cond
      (and (>= nx ny) (>= nx nz)) (fn [[_ y z]] [y z])
      (>= ny nz) (fn [[x _ z]] [x z])
      :else (fn [[x y _]] [x y]))))

(defn- planar-rings-mesh [rings]
  (when (and (seq rings) (>= (count (first rings)) 3))
    (let [desired-normal (face-normal (take 3 (first rings)))
          result (polygon/triangulate-rings rings (planar-projector desired-normal))
          vertices (:vertices result) indices (:indices result)
          actual-normal (when (>= (count indices) 3)
                          (face-normal (map #(nth vertices %) (take 3 indices))))
          indices (if (and actual-normal (neg? (reduce + (map * desired-normal actual-normal))))
                    (vec (mapcat (fn [[a b c]] [a c b]) (partition 3 indices))) indices)]
      (when (seq indices)
        {:positions vertices :indices indices
         :normals (vec (repeat (count vertices) desired-normal))}))))

(defn- faceted-brep-mesh [geometry]
  (let [face-meshes
        (keep (fn [face]
                (let [ordered (sort-by #(if (= :outer (:kind %)) 0 1) (:bounds face))
                      rings (mapv (fn [bound]
                                    (cond-> (vec (:points bound))
                                      (false? (:orientation bound)) reverse))
                                  ordered)]
                  (planar-rings-mesh rings)))
              (:faces geometry))]
    (when (seq face-meshes) (merge-meshes face-meshes))))

(declare cylindrical-face-mesh spherical-face-mesh toroidal-face-mesh b-spline-face-mesh)
(defn- advanced-brep-mesh [geometry]
  (let [face-meshes
        (keep (fn [face]
                (case (get-in face [:surface :kind])
                  :plane
                  (let [ordered (sort-by #(if (= :outer (:kind %)) 0 1) (:bounds face))
                        rings (mapv (fn [bound]
                                      (cond-> (vec (:points bound))
                                        (false? (:orientation bound)) reverse
                                        (false? (:same-sense face)) reverse))
                                    ordered)]
                    (planar-rings-mesh rings))
                  :cylinder (cylindrical-face-mesh face)
                  :sphere (spherical-face-mesh face)
                  :torus (toroidal-face-mesh face)
                  :b-spline-surface (b-spline-face-mesh face)
                  nil))
              (:faces geometry))]
    (when (seq face-meshes) (merge-meshes face-meshes))))

(defn- indexed-face-mesh [coordinates indices]
  (let [points (mapv #(nth coordinates (dec %)) indices)]
    (when (>= (count points) 3)
      {:positions points
       :indices (vec (mapcat (fn [i] [0 i (inc i)]) (range 1 (dec (count points)))))
       :normals (vec (repeat (count points) (face-normal (take 3 points))))})))

(defn- tessellated-mesh [geometry]
  (let [meshes (case (:kind geometry)
                 :triangulated-face-set
                 (keep #(indexed-face-mesh (:coordinates geometry) %) (:coord-indices geometry))
                 :polygonal-face-set
                 (keep (fn [face]
                         (let [rings (mapv (fn [indices]
                                             (mapv #(nth (:coordinates geometry) (dec %)) indices))
                                           (cons (:outer face) (:inners face)))]
                           (planar-rings-mesh rings)))
                       (:faces geometry)))]
    (when (seq meshes) (merge-meshes meshes))))

(defn- v3-sub [a b] (mapv - a b))
(defn- v3-cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])
(defn- v3-normalize [v]
  (let [length (sqrt (reduce + (map #(* % %) v)))]
    (if (pos? length) (mapv #(/ % length) v) [0.0 0.0 0.0])))
(defn- v3-dot [a b] (reduce + (map * a b)))

(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))

(defn- surface-frame [surface]
  (let [position (:position surface)
        z-axis (v3-normalize (point3 (or (:axis position) [0 0 1])))
        initial-x (v3-normalize (point3 (or (:ref-direction position) [1 0 0])))
        y-axis (v3-normalize (v3-cross z-axis initial-x))
        x-axis (v3-normalize (v3-cross y-axis z-axis))]
    {:origin (point3 (:location position)) :x x-axis :y y-axis :z z-axis}))

(defn- local->world [{:keys [origin x y z]} [lx ly lz]]
  (mapv + origin (mapv #(* lx %) x) (mapv #(* ly %) y) (mapv #(* lz %) z)))

(defn- local-vector->world [{:keys [x y z]} [lx ly lz]]
  (v3-normalize (mapv + (mapv #(* lx %) x) (mapv #(* ly %) y) (mapv #(* lz %) z))))

(defn- point->local [{:keys [origin x y z]} point]
  (let [delta (v3-sub (point3 point) origin)]
    [(v3-dot delta x) (v3-dot delta y) (v3-dot delta z)]))

(defn- angular-range [values default-range]
  (if (seq values) [(reduce min values) (reduce max values)] default-range))

(defn- parametric-surface-mesh
  [face u-range v-range u-segments v-segments u-periodic? v-periodic? sample]
  (let [[u0 u1] u-range [v0 v1] v-range
        u-count (if u-periodic? u-segments (inc u-segments))
        v-count (if v-periodic? v-segments (inc v-segments))
        vertices (vec (for [vi (range v-count) ui (range u-count)]
                        (sample (+ u0 (* (- u1 u0) (/ ui u-segments)))
                                (+ v0 (* (- v1 v0) (/ vi v-segments))))))
        u-side-count (if u-periodic? u-count (dec u-count))
        v-side-count (if v-periodic? v-count (dec v-count))
        raw-indices
        (vec (mapcat (fn [vi]
                       (mapcat (fn [ui]
                                 (let [next-u (if u-periodic? (mod (inc ui) u-count) (inc ui))
                                       next-v (if v-periodic? (mod (inc vi) v-count) (inc vi))
                                       a (+ (* vi u-count) ui)
                                       b (+ (* vi u-count) next-u)
                                       c (+ (* next-v u-count) ui)
                                       d (+ (* next-v u-count) next-u)]
                                   [a b c b d c]))
                               (range u-side-count)))
                     (range v-side-count)))
        reversed? (false? (:same-sense face))]
    {:positions (mapv :position vertices)
     :normals (mapv (fn [{:keys [normal]}]
                      (if reversed? (mapv - normal) normal)) vertices)
     :indices (if reversed?
                (vec (mapcat (fn [[a b c]] [a c b]) (partition 3 raw-indices)))
                raw-indices)}))

(defn- unwrap-parameter-ring [ring periodic-dimensions]
  (reduce (fn [result point]
            (if-let [previous (peek result)]
              (conj result
                    (mapv (fn [dimension value]
                            (if (contains? periodic-dimensions dimension)
                              (let [delta (- value (nth previous dimension))]
                                (cond (> delta pi) (- value (* 2.0 pi))
                                      (< delta (- pi)) (+ value (* 2.0 pi))
                                      :else value))
                              value))
                          (range (count point)) point))
              [point])) [] ring))

(defn- trimmed-parametric-mesh
  ([face point->uv sample] (trimmed-parametric-mesh face point->uv sample #{}))
  ([face point->uv sample periodic-dimensions]
   (when (seq (:bounds face))
    (let [midpoint (fn [a b] (mapv #(/ % 2.0) (mapv + a b)))
          subdivide (fn [triangles]
                      (vec (mapcat (fn [[a b c]]
                                     (let [ab (midpoint a b) bc (midpoint b c) ca (midpoint c a)]
                                       [[a ab ca] [ab b bc] [ca bc c] [ab bc ca]]))
                                   triangles)))
          ordered (sort-by #(if (= :outer (:kind %)) 0 1) (:bounds face))
          raw-rings (mapv (fn [bound]
                            (let [uvs (-> (mapv #(point->uv (point3 %)) (:points bound))
                                          (unwrap-parameter-ring periodic-dimensions))]
                              (vec (cond-> uvs (false? (:orientation bound)) reverse))))
                          ordered)
          outer-center (when-let [outer (first raw-rings)]
                         (mapv #(/ % (count outer)) (apply mapv + outer)))
          rings (mapv (fn [ring]
                        (let [center (mapv #(/ % (count ring)) (apply mapv + ring))
                              shifts (mapv (fn [dimension outer-value value]
                                             (if (contains? periodic-dimensions dimension)
                                               (* 2.0 pi
                                                  (#?(:clj Math/round :cljs js/Math.round)
                                                   (/ (- outer-value value) (* 2.0 pi))))
                                               0.0))
                                           (range (count center)) outer-center center)]
                          (mapv #(mapv + % shifts) ring)))
                      raw-rings)
          triangulated (polygon/triangulate-rings rings identity)
          triangles (mapv (fn [[a b c]]
                            [(nth (:vertices triangulated) a)
                             (nth (:vertices triangulated) b)
                             (nth (:vertices triangulated) c)])
                          (partition 3 (:indices triangulated)))
          refined (nth (iterate subdivide triangles) 2)
          parameters (vec (mapcat identity refined))
          vertices (mapv #(apply sample %) parameters)
          reversed? (false? (:same-sense face))]
      (when (seq vertices)
        {:positions (mapv :position vertices)
         :normals (mapv (fn [{:keys [normal]}]
                          (if reversed? (mapv - normal) normal)) vertices)
         :indices (if reversed?
                    (vec (mapcat (fn [[a b c]] [a c b])
                                 (partition 3 (range (count vertices)))))
                    (vec (range (count vertices))))})))))

(defn- spherical-face-mesh [face]
  (let [surface (:surface face) frame (surface-frame surface)
        radius (:radius surface)
        local-points (map #(point->local frame %) (mapcat :points (:bounds face)))
        us (map (fn [[x y _]] (#?(:clj Math/atan2 :cljs js/Math.atan2) y x)) local-points)
        vs (map (fn [[_ _ z]]
                  (#?(:clj Math/asin :cljs js/Math.asin)
                   (max -1.0 (min 1.0 (/ z radius))))) local-points)
        u-range (or (:u-range surface) (angular-range us [0.0 (* 2.0 pi)]))
        v-range (or (:v-range surface) (angular-range vs [(/ (- pi) 2.0) (/ pi 2.0)]))
        u-periodic? (and (empty? local-points) (nil? (:u-range surface)))
        point->uv (fn [point]
                    (let [[x y z] (point->local frame point)]
                      [(#?(:clj Math/atan2 :cljs js/Math.atan2) y x)
                       (#?(:clj Math/asin :cljs js/Math.asin)
                        (max -1.0 (min 1.0 (/ z radius))))]))
        sample (fn [u v]
                 (let [cv (#?(:clj Math/cos :cljs js/Math.cos) v)
                       normal [(* cv (#?(:clj Math/cos :cljs js/Math.cos) u))
                               (* cv (#?(:clj Math/sin :cljs js/Math.sin) u))
                               (#?(:clj Math/sin :cljs js/Math.sin) v)]]
                   {:position (local->world frame (mapv #(* radius %) normal))
                    :normal (local-vector->world frame normal)}))]
    (or (trimmed-parametric-mesh face point->uv sample #{0})
        (parametric-surface-mesh face u-range v-range 24 12 u-periodic? false sample))))

(defn- toroidal-face-mesh [face]
  (let [surface (:surface face) frame (surface-frame surface)
        major-radius (:major-radius surface) minor-radius (:minor-radius surface)
        local-points (map #(point->local frame %) (mapcat :points (:bounds face)))
        us (map (fn [[x y _]] (#?(:clj Math/atan2 :cljs js/Math.atan2) y x)) local-points)
        vs (map (fn [[x y z]]
                  (#?(:clj Math/atan2 :cljs js/Math.atan2)
                   z (- (sqrt (+ (* x x) (* y y))) major-radius))) local-points)
        u-range (or (:u-range surface) (angular-range us [0.0 (* 2.0 pi)]))
        v-range (or (:v-range surface) (angular-range vs [0.0 (* 2.0 pi)]))
        u-periodic? (and (empty? local-points) (nil? (:u-range surface)))
        v-periodic? (and (empty? local-points) (nil? (:v-range surface)))
        point->uv (fn [point]
                    (let [[x y z] (point->local frame point)]
                      [(#?(:clj Math/atan2 :cljs js/Math.atan2) y x)
                       (#?(:clj Math/atan2 :cljs js/Math.atan2)
                        z (- (sqrt (+ (* x x) (* y y))) major-radius))]))
        sample (fn [u v]
                 (let [cu (#?(:clj Math/cos :cljs js/Math.cos) u)
                       su (#?(:clj Math/sin :cljs js/Math.sin) u)
                       cv (#?(:clj Math/cos :cljs js/Math.cos) v)
                       sv (#?(:clj Math/sin :cljs js/Math.sin) v)
                       radial (+ major-radius (* minor-radius cv))
                       normal [(* cv cu) (* cv su) sv]]
                   {:position (local->world frame [(* radial cu) (* radial su) (* minor-radius sv)])
                    :normal (local-vector->world frame normal)}))]
    (or (trimmed-parametric-mesh face point->uv sample #{0 1})
        (parametric-surface-mesh face u-range v-range 24 12 u-periodic? v-periodic? sample))))

(defn- b-spline-face-mesh [face]
  (let [surface (:surface face)
        u-count (count (:control-points surface))
        v-count (count (first (:control-points surface)))
        u-knots (spline/expand-knots (:u-knots surface) (:u-multiplicities surface)
                                    (:u-degree surface) u-count)
        v-knots (spline/expand-knots (:v-knots surface) (:v-multiplicities surface)
                                    (:v-degree surface) v-count)
        u-range (spline/parameter-range (:u-degree surface) u-knots u-count)
        v-range (spline/parameter-range (:v-degree surface) v-knots v-count)
        u-epsilon (* 1.0e-5 (- (second u-range) (first u-range)))
        v-epsilon (* 1.0e-5 (- (second v-range) (first v-range)))
        clamp (fn [value [lower upper]] (max lower (min upper value)))
        spline-surface (assoc surface :u-knots u-knots :v-knots v-knots)
        point (fn [u v] (spline/surface-point spline-surface u v))
        normal (fn [u v]
                 (let [u0 (clamp (- u u-epsilon) u-range) u1 (clamp (+ u u-epsilon) u-range)
                       v0 (clamp (- v v-epsilon) v-range) v1 (clamp (+ v v-epsilon) v-range)
                       du (v3-sub (point u1 v) (point u0 v))
                       dv (v3-sub (point u v1) (point u v0))]
                   (v3-normalize (v3-cross du dv))))
        sample (fn [u v]
                 {:position (point u v) :normal (normal u v)})
        trimmed-mesh
        (trimmed-parametric-mesh
         face
         #(-> (spline/closest-surface-parameters spline-surface %) :parameters)
         sample)]
    (when (and (pos? u-count) (pos? v-count))
      (or trimmed-mesh
          (parametric-surface-mesh face u-range v-range 16 16
                                   (true? (:u-closed surface)) (true? (:v-closed surface)) sample)))))

(defn- cylindrical-face-mesh [face]
  (let [surface (:surface face) position (:position surface)
        origin (point3 (:location position))
        z-axis (v3-normalize (point3 (or (:axis position) [0 0 1])))
        x-axis (v3-normalize (point3 (or (:ref-direction position) [1 0 0])))
        y-axis (v3-normalize (v3-cross z-axis x-axis))
        radius (:radius surface)
        points (vec (mapcat :points (:bounds face)))
        local (mapv (fn [point]
                      (let [delta (v3-sub (point3 point) origin)]
                        [(v3-dot delta x-axis) (v3-dot delta y-axis) (v3-dot delta z-axis)]))
                    points)
        zs (map #(nth % 2) local)
        full-circle? (some (fn [edge]
                             (and (= :circle (get-in edge [:curve :kind]))
                                  (= (:start edge) (:end edge))))
                           (mapcat :edges (:bounds face)))
        point->uv (fn [point]
                    (let [[x y z] (point->local {:origin origin :x x-axis :y y-axis :z z-axis}
                                                point)]
                      [(#?(:clj Math/atan2 :cljs js/Math.atan2) y x) z]))
        trim-sample (fn [u v]
                      (let [radial (mapv +
                                         (mapv #(* (#?(:clj Math/cos :cljs js/Math.cos) u) %) x-axis)
                                         (mapv #(* (#?(:clj Math/sin :cljs js/Math.sin) u) %) y-axis))]
                        {:position (mapv + origin (mapv #(* radius %) radial)
                                         (mapv #(* v %) z-axis))
                         :normal radial}))]
    (or (when-not full-circle?
          (trimmed-parametric-mesh face point->uv trim-sample #{0}))
        (when (seq zs)
      (let [z0 (reduce min zs) z1 (reduce max zs)
            angles (map (fn [[x y _]] (#?(:clj Math/atan2 :cljs js/Math.atan2) y x)) local)
            start (if full-circle? 0.0 (reduce min angles))
            end (if full-circle? (* 2.0 #?(:clj Math/PI :cljs js/Math.PI)) (reduce max angles))
            segments (if full-circle? 24
                         (max 3 (long (#?(:clj Math/ceil :cljs js/Math.ceil)
                                       (/ (- end start)
                                          (/ #?(:clj Math/PI :cljs js/Math.PI) 12.0))))))
            ring-count (if full-circle? segments (inc segments))
            sample (fn [z i]
                     (let [theta (+ start (* (- end start) (/ i segments)))
                           radial (mapv +
                                        (mapv #(* (#?(:clj Math/cos :cljs js/Math.cos) theta) %) x-axis)
                                        (mapv #(* (#?(:clj Math/sin :cljs js/Math.sin) theta) %) y-axis))]
                       {:position (mapv + origin (mapv #(* radius %) radial)
                                        (mapv #(* z %) z-axis))
                        :normal (if (false? (:same-sense face)) (mapv #(- %) radial) radial)}))
            vertices (vec (concat (map #(sample z0 %) (range ring-count))
                                  (map #(sample z1 %) (range ring-count))))
            next-index (fn [i] (if full-circle? (mod (inc i) ring-count) (inc i)))
            side-count (if full-circle? ring-count (dec ring-count))
            raw-indices (vec (mapcat (fn [i]
                                       (let [j (next-index i)
                                             a i b j c (+ ring-count i) d (+ ring-count j)]
                                         [a b c b d c]))
                                     (range side-count)))
            indices (if (false? (:same-sense face))
                      (vec (mapcat (fn [[a b c]] [a c b]) (partition 3 raw-indices)))
                      raw-indices)]
        {:positions (mapv :position vertices) :normals (mapv :normal vertices)
         :indices indices})))))

(defn- clipped-extrusion [extrusion half-space]
  (let [z-axis (v3-normalize (point3 (or (get-in extrusion [:position :axis]) [0 0 1])))
        x-axis (v3-normalize (point3 (or (get-in extrusion [:position :ref-direction]) [1 0 0])))
        y-axis (v3-normalize (v3-cross z-axis x-axis))
        [dx dy dz] (point3 (or (:direction extrusion) [0 0 1]))
        direction (v3-normalize (mapv + (mapv #(* dx %) x-axis)
                                        (mapv #(* dy %) y-axis)
                                        (mapv #(* dz %) z-axis)))
        normal (v3-normalize (point3 (get-in half-space [:base-surface :position :axis])))
        base (point3 (get-in extrusion [:position :location]))
        plane (point3 (get-in half-space [:base-surface :position :location]))
        denominator (v3-dot normal direction)
        depth (:depth extrusion)]
    (when (> (#?(:clj Math/abs :cljs js/Math.abs) denominator) 0.999)
      (let [intersection (/ (v3-dot normal (v3-sub plane base)) denominator)
            agreement? (true? (:agreement-flag half-space))
            keep-after? (if agreement? (neg? denominator) (pos? denominator))
            start (if keep-after? (max 0.0 intersection) 0.0)
            end (if keep-after? depth (min depth intersection))]
        (when (> end start)
          (-> extrusion
              (assoc :depth (- end start))
              (assoc-in [:position :location]
                        (mapv + base (mapv #(* start %) direction)))))))))

(defn- boolean-result-mesh [geometry]
  (let [first-operand (:first-operand geometry) second-operand (:second-operand geometry)
        first-mesh (geometry-mesh first-operand) second-mesh (geometry-mesh second-operand)]
    (case (:operator geometry)
      :union (if (and first-mesh second-mesh)
               (mesh-csg/mesh-boolean :union first-mesh second-mesh)
               (when-let [meshes (seq (keep identity [first-mesh second-mesh]))]
                 (merge-meshes meshes)))
      :difference
      (if (and first-mesh second-mesh)
        (mesh-csg/mesh-boolean :difference first-mesh second-mesh)
        (when (and (= :extruded-area-solid (:kind first-operand))
                   (= :half-space-solid (:kind second-operand)))
          (some-> (clipped-extrusion first-operand second-operand) extruded-area-mesh)))
      :intersection
      (if (and first-mesh second-mesh)
        (mesh-csg/mesh-boolean :intersection first-mesh second-mesh)
        (when (and (= :extruded-area-solid (:kind first-operand))
                   (= :half-space-solid (:kind second-operand)))
          ;; Intersection is the complementary extrusion interval.
          (some-> (clipped-extrusion first-operand
                                     (update second-operand :agreement-flag not))
                  extruded-area-mesh)))
      nil)))

(defn- v3-scale [factor vector] (mapv #(* factor %) vector))
(defn- v3-add [& vectors] (reduce #(mapv + %1 %2) [0.0 0.0 0.0] vectors))

(defn- loft-rings-mesh [rings cap-ends?]
  (let [ring-count (count rings) ring-size (count (first rings))
        positions (vec (mapcat identity rings))
        sides (mapcat (fn [ring]
                        (mapcat (fn [j]
                                  (let [next-j (mod (inc j) ring-size)
                                        a (+ (* ring ring-size) j)
                                        b (+ (* ring ring-size) next-j)
                                        c (+ (* (inc ring) ring-size) j)
                                        d (+ (* (inc ring) ring-size) next-j)]
                                    [a b c b d c]))
                                (range ring-size)))
                      (range (dec ring-count)))
        caps (when cap-ends?
               (concat
                (mapcat (fn [i] [0 (inc i) i]) (range 1 (dec ring-size)))
                (let [offset (* (dec ring-count) ring-size)]
                  (mapcat (fn [i] [offset (+ offset i) (+ offset (inc i))])
                          (range 1 (dec ring-size))))))
        indices (vec (concat sides caps))
        normals
        (reduce (fn [acc [a b c]]
                  (let [normal (face-normal [(nth positions a) (nth positions b)
                                             (nth positions c)])]
                    (-> acc (update a v3-add normal) (update b v3-add normal)
                        (update c v3-add normal))))
                (vec (repeat (count positions) [0.0 0.0 0.0]))
                (partition 3 indices))]
    (when (and (> ring-count 1) (> ring-size 2))
      {:positions positions :indices indices :normals (mapv v3-normalize normals)})))

(defn- rotate-about-axis [point origin axis angle]
  (let [relative (v3-sub point origin)
        cosine (#?(:clj Math/cos :cljs js/Math.cos) angle)
        sine (#?(:clj Math/sin :cljs js/Math.sin) angle)]
    (v3-add origin
            (v3-scale cosine relative)
            (v3-scale sine (v3-cross axis relative))
            (v3-scale (* (- 1.0 cosine) (v3-dot axis relative)) axis))))

(defn- revolved-area-mesh [geometry]
  (let [boundary (positioned-profile-boundary (:profile geometry) (:position geometry))
        origin (point3 (get-in geometry [:axis :location]))
        axis (v3-normalize (point3 (or (get-in geometry [:axis :axis]) [0 0 1])))
        angle (:angle geometry)
        steps (max 3 (long (#?(:clj Math/ceil :cljs js/Math.ceil)
                            (/ (#?(:clj Math/abs :cljs js/Math.abs) angle) (/ pi 12.0)))))
        rings (mapv (fn [step]
                      (let [theta (* angle (/ step steps))]
                        (mapv #(rotate-about-axis % origin axis theta) boundary)))
                    (range (inc steps)))
        full? (< (#?(:clj Math/abs :cljs js/Math.abs)
                  (- (#?(:clj Math/abs :cljs js/Math.abs) angle) (* 2.0 pi))) 1.0e-6)]
    (loft-rings-mesh rings (not full?))))

(defn- positive-angle [angle]
  (let [tau (* 2.0 pi)] (if (neg? angle) (+ angle tau) angle)))

(defn- sampled-arc [p1 p2 p3]
  (let [a (v3-sub p2 p1) b (v3-sub p3 p1) normal (v3-cross a b)
        normal2 (v3-dot normal normal)]
    (if (< normal2 1.0e-12)
      [p1 p2 p3]
      (let [center (v3-add p1
                           (v3-scale (/ 1.0 (* 2.0 normal2))
                                     (v3-add (v3-scale (v3-dot a a) (v3-cross b normal))
                                             (v3-scale (v3-dot b b) (v3-cross normal a)))))
            u (v3-normalize (v3-sub p1 center))
            n (v3-normalize normal) v (v3-normalize (v3-cross n u))
            angle-of (fn [point]
                       (let [delta (v3-sub point center)]
                         (positive-angle (#?(:clj Math/atan2 :cljs js/Math.atan2)
                                          (v3-dot delta v) (v3-dot delta u)))))
            middle-angle (angle-of p2) end-angle (angle-of p3)
            end-angle (if (<= middle-angle end-angle) end-angle (- end-angle (* 2.0 pi)))
            steps (max 4 (long (#?(:clj Math/ceil :cljs js/Math.ceil)
                                 (/ (#?(:clj Math/abs :cljs js/Math.abs) end-angle)
                                    (/ pi 12.0)))))
            radius (sqrt (v3-dot (v3-sub p1 center) (v3-sub p1 center)))]
        (mapv (fn [step]
                (let [angle (* end-angle (/ step steps))]
                  (v3-add center
                          (v3-scale (* radius (#?(:clj Math/cos :cljs js/Math.cos) angle)) u)
                          (v3-scale (* radius (#?(:clj Math/sin :cljs js/Math.sin) angle)) v))))
              (range (inc steps)))))))

(defn- curve-path [curve]
  (case (:kind curve)
    :polyline (mapv point3 (:points curve))
    :indexed-polycurve
    (let [points (mapv point3 (:points curve))]
      (if (seq (:segments curve))
        (vec
         (mapcat (fn [{:keys [kind indices]}]
                   (let [selected (mapv #(nth points (dec %)) indices)]
                     (if (= :arc kind)
                       (sampled-arc (nth selected 0) (nth selected 1) (nth selected 2))
                       selected)))
                 (:segments curve)))
        points))
    :composite-curve
    (vec (mapcat (fn [segment]
                   (cond-> (curve-path (:parent-curve segment))
                     (false? (:same-sense segment)) reverse))
                 (:segments curve)))
    []))

(defn- dedupe-adjacent [points]
  (reduce (fn [result point]
            (if (= point (peek result)) result (conj result point))) [] points))

(defn- swept-area-mesh [geometry]
  (let [path (vec (dedupe-adjacent (curve-path (:directrix geometry))))
        profile (profile-points (:profile geometry))
        reference (point3 (or (:fixed-reference geometry)
                              (get-in geometry [:reference-surface :position :axis])
                              [0 0 1]))
        last-index (dec (count path))
        rings
        (mapv (fn [i]
                (let [point (nth path i)
                      tangent (v3-normalize
                               (cond (= i 0) (v3-sub (nth path 1) point)
                                     (= i last-index) (v3-sub point (nth path (dec i)))
                                     :else (v3-sub (nth path (inc i)) (nth path (dec i)))))
                      fallback (if (> (#?(:clj Math/abs :cljs js/Math.abs)
                                         (v3-dot tangent [0 0 1])) 0.9)
                                 [0 1 0] [0 0 1])
                      u0 (v3-cross reference tangent)
                      u (v3-normalize (if (< (v3-dot u0 u0) 1.0e-12)
                                        (v3-cross fallback tangent) u0))
                      v (v3-normalize (v3-cross tangent u))]
                  (mapv (fn [[x y & _]]
                          (v3-add point (v3-scale x u) (v3-scale y v)))
                        profile)))
              (range (count path)))]
    (when (and (> (count path) 1) (> (count profile) 2))
      (loft-rings-mesh rings true))))

(defn- swept-disk-mesh [geometry]
  (let [path (mapv point3 (:directrix geometry)) ring-size 12 radius (:radius geometry)
        last-index (dec (count path))
        frames (mapv (fn [i]
                       (let [tangent (v3-normalize
                                      (cond (= i 0) (v3-sub (nth path 1) (nth path 0))
                                            (= i last-index) (v3-sub (nth path i) (nth path (dec i)))
                                            :else (v3-sub (nth path (inc i)) (nth path (dec i)))))
                             reference (if (> (#?(:clj Math/abs :cljs js/Math.abs) (nth tangent 2)) 0.9)
                                         [0.0 1.0 0.0] [0.0 0.0 1.0])
                             u (v3-normalize (v3-cross tangent reference))
                             v (v3-normalize (v3-cross tangent u))]
                         [u v]))
                     (range (count path)))
        rings (mapv (fn [point [u v]]
                      (mapv (fn [j]
                              (let [angle (* 2.0 #?(:clj Math/PI :cljs js/Math.PI)
                                             (/ j ring-size))
                                    c (#?(:clj Math/cos :cljs js/Math.cos) angle)
                                    s (#?(:clj Math/sin :cljs js/Math.sin) angle)
                                    normal (mapv + (mapv #(* c %) u) (mapv #(* s %) v))]
                                {:position (mapv + point (mapv #(* radius %) normal))
                                 :normal normal}))
                            (range ring-size)))
                    path frames)
        vertices (vec (mapcat identity rings))
        sides (mapcat (fn [ring]
                        (mapcat (fn [j]
                                  (let [next-j (mod (inc j) ring-size)
                                        a (+ (* ring ring-size) j)
                                        b (+ (* ring ring-size) next-j)
                                        c (+ (* (inc ring) ring-size) j)
                                        d (+ (* (inc ring) ring-size) next-j)]
                                    [a b c b d c]))
                                (range ring-size)))
                      (range last-index))]
    (when (>= (count path) 2)
      {:positions (mapv :position vertices) :normals (mapv :normal vertices)
       :indices (vec sides)})))

(defn- geometry-mesh [geometry]
  (case (:kind geometry)
    :extruded-area-solid (extruded-area-mesh geometry)
    :revolved-area-solid (revolved-area-mesh geometry)
    (:fixed-reference-swept-area-solid :surface-curve-swept-area-solid)
    (swept-area-mesh geometry)
    :mapped-item (mapped-item-mesh geometry)
    :faceted-brep (faceted-brep-mesh geometry)
    :advanced-brep (advanced-brep-mesh geometry)
    (:triangulated-face-set :polygonal-face-set) (tessellated-mesh geometry)
    :swept-disk-solid (swept-disk-mesh geometry)
    :collection (let [meshes (keep geometry-mesh (:items geometry))]
                  (when (seq meshes) (merge-meshes meshes)))
    :boolean-result (boolean-result-mesh geometry)
    nil))

(declare wall-with-openings-mesh)
(defn element-mesh [element]
  (case (:kind element)
    :wall (wall-with-openings-mesh element)
    :slab (slab-mesh element)
    (geometry-mesh (:geometry element))))

(defn add-opening-to-wall [wall opening]
  (let [length (get-in wall [:quantities :length-m])
        wall-height (get-in wall [:geometry :profile :height])
        {:keys [offset sill]} (:placement opening)
        {:keys [width height]} (:profile opening)
        overlaps? (fn [other]
                    (let [{o :offset s :sill} (:placement other)
                          {w :width h :height} (:profile other)]
                      (and (< offset (+ o w)) (< o (+ offset width))
                           (< sill (+ s h)) (< s (+ sill height)))))]
    (when (or (> (+ offset width) length) (> (+ sill height) wall-height))
      (throw (ex-info "opening exceeds wall bounds" {:wall-length length :wall-height wall-height :opening opening})))
    (when (some overlaps? (:openings wall))
      (throw (ex-info "opening overlaps an existing opening" {:opening-id (:id opening)})))
    (let [updated (update wall :openings conj opening)]
      (if (> (count (:material-layers updated)) 1)
        (set-wall-layers updated (:material-layers updated)) updated))))

(defn remove-opening-from-wall [wall opening-id]
  (let [updated (update wall :openings #(vec (remove (fn [opening] (= opening-id (:id opening))) %)))]
    (if (> (count (:material-layers updated)) 1)
      (set-wall-layers updated (:material-layers updated)) updated)))

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

(declare merge-meshes)
(defn- single-layer-wall-with-openings-mesh
  "Generate wall geometry with non-overlapping rectangular hosted openings."
  [wall-element]
  (if (empty? (:openings wall-element))
    (wall-mesh wall-element)
    (let [[[x0 y0 z0] [x1 y1 _]] (get-in wall-element [:geometry :axis])
          length (get-in wall-element [:quantities :length-m])
          thickness (get-in wall-element [:geometry :profile :thickness])
          wall-height (get-in wall-element [:geometry :profile :height])
          point-at (fn [offset z] [(+ x0 (* (/ offset length) (- x1 x0)))
                                   (+ y0 (* (/ offset length) (- y1 y0))) (+ z0 z)])
          box (fn [from to bottom height]
                (wall-mesh (wall {:id :opening-segment :start (point-at from bottom)
                                  :end (point-at to bottom) :thickness thickness :height height})))
          openings (sort-by #(get-in % [:placement :offset]) (:openings wall-element))
          vertical (mapcat (fn [o]
                             (let [{:keys [offset sill]} (:placement o)
                                   {:keys [width height]} (:profile o)]
                               (cond-> []
                                 (pos? sill) (conj (box offset (+ offset width) 0 sill))
                                 (< (+ sill height) wall-height)
                                 (conj (box offset (+ offset width) (+ sill height)
                                            (- wall-height sill height)))))) openings)
          gaps (map vector
                    (cons 0 (map #(+ (get-in % [:placement :offset]) (get-in % [:profile :width])) openings))
                    (concat (map #(get-in % [:placement :offset]) openings) [length]))
          full-height (map (fn [[a b]] (box a b 0 wall-height)) (filter (fn [[a b]] (< a b)) gaps))]
      (merge-meshes (concat full-height vertical)))))

(defn wall-layer-meshes
  "Generate one opening-aware mesh per ordered compound wall layer."
  [wall-element]
  (let [layers (:material-layers wall-element)
        total (reduce + (map :thickness layers))
        [[x0 y0 z0] [x1 y1 z1]] (get-in wall-element [:geometry :axis])
        dx (- x1 x0) dy (- y1 y0) length (sqrt (+ (* dx dx) (* dy dy)))
        normal [(/ (- dy) length) (/ dx length) 0.0]]
    (loop [remaining layers cursor (/ (- total) 2.0) meshes []]
      (if-let [layer (first remaining)]
        (let [thickness (:thickness layer)
              center (+ cursor (/ thickness 2.0))
              shift (mapv #(* center %) normal)
              layer-wall (-> wall-element
                             (assoc :material-layers [layer])
                             (assoc-in [:geometry :axis]
                                       [(mapv + [x0 y0 z0] shift)
                                        (mapv + [x1 y1 z1] shift)])
                             (assoc-in [:geometry :profile :thickness] thickness))]
          (recur (next remaining) (+ cursor thickness)
                 (conj meshes (assoc (single-layer-wall-with-openings-mesh layer-wall)
                                     :material-layer layer))))
        meshes))))

(defn wall-with-openings-mesh
  "Generate a merged visual mesh while retaining per-layer mesh access."
  [wall-element]
  (if (> (count (:material-layers wall-element)) 1)
    (merge-meshes (wall-layer-meshes wall-element))
    (single-layer-wall-with-openings-mesh wall-element)))

(defn- distance3 [a b]
  (sqrt (reduce + (map (fn [x y] (let [delta (- x y)] (* delta delta))) a b))))

(defn set-wall-axis
  "Replace a wall axis, update length quantities, and preserve hosted opening
  world positions. Rejects edits that would cut through an opening."
  [wall-element [start end :as axis]]
  (when-not (and (= :wall (:kind wall-element)) (= 2 (count axis))
                 (valid-point3? start) (valid-point3? end))
    (throw (ex-info "wall axis edit requires a wall and two 3D points" {:axis axis})))
  (let [length (distance3 start end)]
    (when (< length 1.0e-9)
      (throw (ex-info "wall axis endpoints must not coincide" {:axis axis})))
    (let [[old-start old-end] (get-in wall-element [:geometry :axis])
          old-length (distance3 old-start old-end)
          old-direction (mapv #(/ % old-length) (mapv - old-end old-start))
          direction (mapv #(/ % length) (mapv - end start))
          openings
          (mapv (fn [opening]
                  (let [old-offset (get-in opening [:placement :offset])
                        world-point (mapv + old-start (mapv #(* old-offset %) old-direction))
                        offset (reduce + (map * (mapv - world-point start) direction))
                        width (get-in opening [:profile :width])]
                    (when (or (< offset -1.0e-8) (> (+ offset width) (+ length 1.0e-8)))
                      (throw (ex-info "wall axis edit would cut a hosted opening"
                                      {:wall-id (:id wall-element) :opening-id (:id opening)
                                       :offset offset :width width :wall-length length})))
                    (assoc-in opening [:placement :offset] (max 0.0 offset))))
                (:openings wall-element))
          updated (-> wall-element
                      (assoc-in [:geometry :axis] [(vec start) (vec end)])
                      (assoc-in [:quantities :length-m] length)
                      (assoc :openings openings))]
      (if (> (count (:material-layers updated)) 1)
        (set-wall-layers updated (:material-layers updated)) updated))))

(defn offset-wall
  "Offset a wall in its local plan perpendicular by a signed distance."
  [wall-element distance]
  (when-not (finite-number? distance)
    (throw (ex-info "wall offset must be numeric" {:distance distance})))
  (let [[[x0 y0 z0] [x1 y1 z1]] (get-in wall-element [:geometry :axis])
        dx (- x1 x0) dy (- y1 y0) plan-length (sqrt (+ (* dx dx) (* dy dy)))]
    (when (< plan-length 1.0e-9)
      (throw (ex-info "wall offset requires a non-vertical plan axis" {:wall-id (:id wall-element)})))
    (let [shift [(* distance (/ (- dy) plan-length))
                 (* distance (/ dx plan-length)) 0.0]]
      (set-wall-axis wall-element
                     [(mapv + [x0 y0 z0] shift) (mapv + [x1 y1 z1] shift)]))))

(defn- line-intersection-xy [[a b] [c d]]
  (let [[x1 y1] a [x2 y2] b [x3 y3] c [x4 y4] d
        denominator (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (when (> (#?(:clj Math/abs :cljs js/Math.abs) denominator) 1.0e-12)
      [(/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
               (* (- x1 x2) (- (* x3 y4) (* y3 x4)))) denominator)
       (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
               (* (- y1 y2) (- (* x3 y4) (* y3 x4)))) denominator)])))

(defn- replace-nearest-endpoint [[start end] point]
  (if (<= (distance3 start point) (distance3 end point)) [point end] [start point]))

(defn trim-extend-walls
  "Trim or extend two non-parallel plan walls to their infinite-line intersection."
  [left right]
  (when-not (and (= :wall (:kind left)) (= :wall (:kind right)))
    (throw (ex-info "trim/extend requires two walls" {})))
  (let [left-axis (get-in left [:geometry :axis]) right-axis (get-in right [:geometry :axis])]
    (when (> (#?(:clj Math/abs :cljs js/Math.abs)
              (- (nth (first left-axis) 2) (nth (first right-axis) 2))) 1.0e-6)
      (throw (ex-info "trim/extend walls must share an elevation"
                      {:left-id (:id left) :right-id (:id right)})))
    (when-let [[x y] (line-intersection-xy left-axis right-axis)]
      (let [left-point [x y (nth (first left-axis) 2)]
            right-point [x y (nth (first right-axis) 2)]]
        [(set-wall-axis left (replace-nearest-endpoint left-axis left-point))
         (set-wall-axis right (replace-nearest-endpoint right-axis right-point))]))))

(defn join-walls
  "Join two walls at their plan intersection and retain the join relationship."
  [left right]
  (when-let [[joined-left joined-right] (trim-extend-walls left right)]
    [(update joined-left :wall/joins (fnil conj #{}) (:id right))
     (update joined-right :wall/joins (fnil conj #{}) (:id left))]))

(defn merge-meshes [meshes]
  (loop [remaining meshes positions [] normals [] indices []]
    (if-let [m (first remaining)]
      (let [offset (count positions)]
        (recur (next remaining) (into positions (:positions m)) (into normals (:normals m))
               (into indices (map #(+ offset %) (:indices m)))))
      {:positions (vec positions) :normals (vec normals) :indices (vec indices)})))

(defn- mesh-bounds [element]
  (when-let [positions (:positions (element-mesh element))]
    (when (seq positions)
      {:min (mapv #(reduce min (map % positions)) [first second #(nth % 2)])
       :max (mapv #(reduce max (map % positions)) [first second #(nth % 2)])})))

(defn align-element
  "Translate an element so one bounds anchor aligns to a reference anchor on an axis."
  [reference moving {:keys [axis reference-anchor moving-anchor]
                     :or {axis :x reference-anchor :center moving-anchor :center}}]
  (let [axis-index ({:x 0 :y 1 :z 2} axis)
        anchor-value (fn [bounds anchor]
                       (case anchor
                         :min (nth (:min bounds) axis-index)
                         :max (nth (:max bounds) axis-index)
                         :center (/ (+ (nth (:min bounds) axis-index)
                                       (nth (:max bounds) axis-index)) 2.0)
                         (throw (ex-info "unsupported alignment anchor" {:anchor anchor}))))
        reference-bounds (mesh-bounds reference) moving-bounds (mesh-bounds moving)]
    (when-not (and axis-index reference-bounds moving-bounds)
      (throw (ex-info "alignment requires an axis and renderable elements" {:axis axis})))
    (let [distance (- (anchor-value reference-bounds reference-anchor)
                      (anchor-value moving-bounds moving-anchor))
          delta (assoc [0.0 0.0 0.0] axis-index distance)]
      (translate-element moving delta))))

(defn detect-clashes
  "Broad-phase element clash detection using generated mesh bounds."
  ([project] (detect-clashes project {}))
  ([project {:keys [tolerance] :or {tolerance 0.0}}]
   (vec
    (mapcat
     (fn [storey]
       (keep (fn [[a b]]
               (when-let [ba (mesh-bounds a)]
                 (when-let [bb (mesh-bounds b)]
                   (let [overlap (mapv (fn [lo-a hi-a lo-b hi-b]
                                         (- (min hi-a hi-b) (max lo-a lo-b)))
                                       (:min ba) (:max ba) (:min bb) (:max bb))]
                     (when (every? #(< tolerance %) overlap)
                       {:clash/storey (:id storey) :clash/a (:id a) :clash/b (:id b)
                        :clash/kinds [(:kind a) (:kind b)] :clash/overlap overlap
                        :clash/volume (reduce * overlap)})))))
             (for [i (range (count (:elements storey)))
                   j (range (inc i) (count (:elements storey)))]
               [(nth (:elements storey) i) (nth (:elements storey) j)])))
     (mapcat :storeys (mapcat :buildings (:sites project)))))))
