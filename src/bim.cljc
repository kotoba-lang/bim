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
  (:require [brep.polygon :as polygon]))

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

(defn- extruded-area-mesh [geometry]
  (let [profile (:profile geometry)
        raw-points (case (:kind profile)
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
                     nil)
        points (if (= (first raw-points) (last raw-points)) (vec (butlast raw-points)) raw-points)
        origin (point3 (get-in geometry [:position :location]))
        z-axis (v3-normalize (point3 (or (get-in geometry [:position :axis]) [0.0 0.0 1.0])))
        x-axis (v3-normalize (point3 (or (get-in geometry [:position :ref-direction]) [1.0 0.0 0.0])))
        y-axis (v3-normalize (v3-cross z-axis x-axis))
        [px py pz] (get-in profile [:position :location] [0.0 0.0 0.0])
        boundary (mapv (fn [point] (let [[x y z] (point3 point)]
                                     (mapv + origin
                                           (mapv #(* (+ px x) %) x-axis)
                                           (mapv #(* (+ py y) %) y-axis)
                                           (mapv #(* (+ pz z) %) z-axis)))) points)
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
        sample (fn [u v]
                 (let [cv (#?(:clj Math/cos :cljs js/Math.cos) v)
                       normal [(* cv (#?(:clj Math/cos :cljs js/Math.cos) u))
                               (* cv (#?(:clj Math/sin :cljs js/Math.sin) u))
                               (#?(:clj Math/sin :cljs js/Math.sin) v)]]
                   {:position (local->world frame (mapv #(* radius %) normal))
                    :normal (local-vector->world frame normal)}))]
    (parametric-surface-mesh face u-range v-range 24 12 u-periodic? false sample)))

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
        sample (fn [u v]
                 (let [cu (#?(:clj Math/cos :cljs js/Math.cos) u)
                       su (#?(:clj Math/sin :cljs js/Math.sin) u)
                       cv (#?(:clj Math/cos :cljs js/Math.cos) v)
                       sv (#?(:clj Math/sin :cljs js/Math.sin) v)
                       radial (+ major-radius (* minor-radius cv))
                       normal [(* cv cu) (* cv su) sv]]
                   {:position (local->world frame [(* radial cu) (* radial su) (* minor-radius sv)])
                    :normal (local-vector->world frame normal)}))]
    (parametric-surface-mesh face u-range v-range 24 12 u-periodic? v-periodic? sample)))

(defn- expanded-knots [knots multiplicities degree control-count]
  (if (and (seq knots) (= (count knots) (count multiplicities)))
    (vec (mapcat (fn [k multiplicity] (repeat multiplicity k)) knots multiplicities))
    (let [interior-count (- control-count degree 1)
          denominator (inc interior-count)]
      (vec (concat (repeat (inc degree) 0.0)
                   (map #(/ % denominator) (range 1 (inc interior-count)))
                   (repeat (inc degree) 1.0))))))

(defn- b-spline-basis [degree knots control-count parameter]
  (let [last-parameter (nth knots control-count)
        initial (mapv (fn [i]
                        (if (or (and (<= (nth knots i) parameter)
                                     (< parameter (nth knots (inc i))))
                                (and (= parameter last-parameter)
                                     (= i (dec control-count))))
                          1.0 0.0))
                      (range control-count))]
    (loop [order 1 basis initial]
      (if (> order degree)
        basis
        (recur
         (inc order)
         (mapv (fn [i]
                 (let [left-denominator (- (nth knots (+ i order)) (nth knots i))
                       right-denominator (- (nth knots (+ i order 1)) (nth knots (inc i)))
                       left (if (zero? left-denominator) 0.0
                                (* (/ (- parameter (nth knots i)) left-denominator)
                                   (nth basis i)))
                       right (if (or (zero? right-denominator) (= i (dec control-count))) 0.0
                                 (* (/ (- (nth knots (+ i order 1)) parameter)
                                       right-denominator)
                                    (nth basis (inc i))))]
                   (+ left right)))
               (range control-count)))))))

(defn- b-spline-surface-point [surface u-knots v-knots u v]
  (let [control-points (:control-points surface)
        u-count (count control-points) v-count (count (first control-points))
        u-basis (b-spline-basis (:u-degree surface) u-knots u-count u)
        v-basis (b-spline-basis (:v-degree surface) v-knots v-count v)
        weights (or (:weights surface) (vec (repeat u-count (vec (repeat v-count 1.0)))))
        terms (for [i (range u-count) j (range v-count)]
                (let [coefficient (* (nth u-basis i) (nth v-basis j)
                                     (get-in weights [i j]))]
                  [coefficient (get-in control-points [i j])]))
        denominator (reduce + (map first terms))]
    (if (zero? denominator)
      [0.0 0.0 0.0]
      (mapv #(/ % denominator)
            (reduce (fn [sum [coefficient point]]
                      (mapv + sum (mapv #(* coefficient %) (point3 point))))
                    [0.0 0.0 0.0] terms)))))

(defn- b-spline-face-mesh [face]
  (let [surface (:surface face)
        u-count (count (:control-points surface))
        v-count (count (first (:control-points surface)))
        u-knots (expanded-knots (:u-knots surface) (:u-multiplicities surface)
                                (:u-degree surface) u-count)
        v-knots (expanded-knots (:v-knots surface) (:v-multiplicities surface)
                                (:v-degree surface) v-count)
        u-range [(nth u-knots (:u-degree surface)) (nth u-knots u-count)]
        v-range [(nth v-knots (:v-degree surface)) (nth v-knots v-count)]
        u-epsilon (* 1.0e-5 (- (second u-range) (first u-range)))
        v-epsilon (* 1.0e-5 (- (second v-range) (first v-range)))
        clamp (fn [value [lower upper]] (max lower (min upper value)))
        point (fn [u v] (b-spline-surface-point surface u-knots v-knots u v))
        sample (fn [u v]
                 (let [position (point u v)
                       u0 (clamp (- u u-epsilon) u-range) u1 (clamp (+ u u-epsilon) u-range)
                       v0 (clamp (- v v-epsilon) v-range) v1 (clamp (+ v v-epsilon) v-range)
                       du (v3-sub (point u1 v) (point u0 v))
                       dv (v3-sub (point u v1) (point u v0))]
                   {:position position :normal (v3-normalize (v3-cross du dv))}))]
    (when (and (pos? u-count) (pos? v-count))
      (parametric-surface-mesh face u-range v-range 16 16
                               (true? (:u-closed surface)) (true? (:v-closed surface)) sample))))

(defn- cylindrical-face-mesh [face]
  (let [surface (:surface face) position (:position surface)
        origin (point3 (:location position))
        z-axis (v3-normalize (point3 (or (:axis position) [0 0 1])))
        x-axis (v3-normalize (point3 (or (:ref-direction position) [1 0 0])))
        y-axis (v3-normalize (v3-cross z-axis x-axis))
        points (vec (mapcat :points (:bounds face)))
        local (mapv (fn [point]
                      (let [delta (v3-sub (point3 point) origin)]
                        [(v3-dot delta x-axis) (v3-dot delta y-axis) (v3-dot delta z-axis)]))
                    points)
        zs (map #(nth % 2) local)
        full-circle? (some (fn [edge]
                             (and (= :circle (get-in edge [:curve :kind]))
                                  (= (:start edge) (:end edge))))
                           (mapcat :edges (:bounds face)))]
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
            radius (:radius surface)
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
         :indices indices}))))

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
  (let [first-operand (:first-operand geometry) second-operand (:second-operand geometry)]
    (case (:operator geometry)
      :union (let [meshes (keep geometry-mesh [first-operand second-operand])]
               (when (seq meshes) (merge-meshes meshes)))
      :difference
      (when (and (= :extruded-area-solid (:kind first-operand))
                 (= :half-space-solid (:kind second-operand)))
        (some-> (clipped-extrusion first-operand second-operand) extruded-area-mesh))
      :intersection
      (when (and (= :extruded-area-solid (:kind first-operand))
                 (= :half-space-solid (:kind second-operand)))
        ;; Intersection is the complementary extrusion interval.
        (some-> (clipped-extrusion first-operand
                                   (update second-operand :agreement-flag not))
                extruded-area-mesh))
      nil)))

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
    (update wall :openings conj opening)))

(defn remove-opening-from-wall [wall opening-id]
  (update wall :openings #(vec (remove (fn [opening] (= opening-id (:id opening))) %))))

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
(defn wall-with-openings-mesh
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
