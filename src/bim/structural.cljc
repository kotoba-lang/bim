(ns bim.structural
  "Structural shell, reinforcement, and steel connection design contracts."
  (:require [bim :as bim]))

(def schema-version 1)
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))
(defn- sqrt [value] (#?(:clj Math/sqrt :cljs js/Math.sqrt) value))
(defn- math-abs [value] (#?(:clj Math/abs :cljs js/Math.abs) value))

(defn- cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])
(defn- subtract [a b] (mapv - a b))
(defn- magnitude [v] (sqrt (reduce + (map #(* % %) v))))

(defn shell-element
  [{:keys [id nodes thickness-m elastic-modulus-pa poisson-ratio material]}]
  (when (or (< (count nodes) 3) (not (pos? thickness-m))
            (not (< -1.0 poisson-ratio 0.5)))
    (throw (ex-info "invalid structural shell element"
                    {:id id :node-count (count nodes) :thickness-m thickness-m
                     :poisson-ratio poisson-ratio})))
  {:structural.shell/id id :structural.shell/nodes (mapv vec nodes)
   :structural.shell/thickness-m thickness-m
   :structural.shell/elastic-modulus-pa elastic-modulus-pa
   :structural.shell/poisson-ratio poisson-ratio
   :structural.shell/material material})

(defn shell-area
  "Area of a planar 3D polygon using fan triangulation."
  [shell]
  (let [[origin & remaining] (:structural.shell/nodes shell)]
    (reduce + (map (fn [[a b]]
                     (/ (magnitude (cross (subtract a origin) (subtract b origin))) 2.0))
                   (partition 2 1 remaining)))))

(defn distribute-shell-load
  "Convert uniform shell pressure into equivalent equal nodal loads."
  [shell pressure-pa direction load-case]
  (let [nodes (:structural.shell/nodes shell) total (* pressure-pa (shell-area shell))
        per-node (/ total (count nodes))]
    {:structural.load/case load-case :structural.load/total-n total
     :structural.load/nodal
     (mapv (fn [index point]
             {:node/index index :node/point point
              :force-n (mapv #(* per-node %) direction)})
           (range) nodes)}))

(defn rectangular-rebar-mat
  "Generate orthogonal reinforcing bars for a rectangular slab/footing face."
  [{:keys [id origin width-m length-m elevation-m cover-m spacing-m diameter-m
           layer grade]}]
  (when (or (not (pos? width-m)) (not (pos? length-m)) (not (pos? spacing-m))
            (not (pos? diameter-m)) (neg? cover-m)
            (>= (* 2 cover-m) (min width-m length-m)))
    (throw (ex-info "invalid rectangular reinforcement layout" {:id id})))
  (let [[ox oy oz] (or origin [0.0 0.0 0.0]) z (+ oz (or elevation-m 0.0))
        x0 (+ ox cover-m) x1 (+ ox width-m (- cover-m))
        y0 (+ oy cover-m) y1 (+ oy length-m (- cover-m))
        x-count (inc (long (#?(:clj Math/floor :cljs js/Math.floor) (/ (- x1 x0) spacing-m))))
        y-count (inc (long (#?(:clj Math/floor :cljs js/Math.floor) (/ (- y1 y0) spacing-m))))
        interpolate (fn [from to index count]
                      (if (= count 1) from (+ from (* (/ index (dec count)) (- to from)))))
        x-bars (mapv (fn [index]
                       {:rebar/id (str id "-x-" index) :rebar/direction :y
                        :rebar/axis [[(interpolate x0 x1 index x-count) y0 z]
                                     [(interpolate x0 x1 index x-count) y1 z]]})
                     (range x-count))
        y-bars (mapv (fn [index]
                       {:rebar/id (str id "-y-" index) :rebar/direction :x
                        :rebar/axis [[x0 (interpolate y0 y1 index y-count) z]
                                     [x1 (interpolate y0 y1 index y-count) z]]})
                     (range y-count))]
    {:rebar-set/id id :rebar-set/layer (or layer :bottom) :rebar-set/grade grade
     :rebar-set/diameter-m diameter-m :rebar-set/spacing-m spacing-m
     :rebar-set/bars (vec (concat x-bars y-bars))
     :rebar-set/steel-volume-m3
     (* pi (/ (* diameter-m diameter-m) 4.0)
        (+ (* x-count (- y1 y0)) (* y-count (- x1 x0))))}))

(defn rebar-elements [rebar-set]
  (mapv (fn [bar]
          (bim/element {:id (:rebar/id bar) :kind :member :name "Rebar"
                        :global-id (:rebar/id bar)
                        :geometry {:kind :swept-disk-solid :directrix (:rebar/axis bar)
                                   :radius (/ (:rebar-set/diameter-m rebar-set) 2.0)}}))
        (:rebar-set/bars rebar-set)))

(defn check-bolted-shear-connection
  "Check bolt shear and connected-plate bearing resistance. SI units."
  [{:keys [bolt-count bolt-diameter-m bolt-ultimate-pa shear-planes
           plate-thickness-m plate-ultimate-pa bearing-factor design-shear-n gamma-m2]}]
  (let [gamma (or gamma-m2 1.25) bolt-area (* pi (/ (* bolt-diameter-m bolt-diameter-m) 4.0))
        shear (/ (* bolt-count (or shear-planes 1) 0.6 bolt-ultimate-pa bolt-area) gamma)
        bearing (/ (* bolt-count (or bearing-factor 2.5) bolt-diameter-m
                      plate-thickness-m plate-ultimate-pa) gamma)
        resistance (min shear bearing) utilization (/ design-shear-n resistance)]
    {:connection/bolt-shear-resistance-n shear
     :connection/plate-bearing-resistance-n bearing
     :connection/design-resistance-n resistance
     :connection/utilization utilization :connection/passes? (<= utilization 1.0)
     :connection/governing-mode (if (< shear bearing) :bolt-shear :plate-bearing)}))

(defn- transpose [matrix] (apply mapv vector matrix))
(defn- matrix-vector [matrix vector]
  (mapv #(reduce + (map * % vector)) matrix))
(defn- matrix-multiply [left right]
  (let [columns (transpose right)]
    (mapv (fn [row] (mapv #(reduce + (map * row %)) columns)) left)))

(defn- solve-system [matrix values]
  (let [n (count values)]
    (loop [column 0 matrix (mapv vec matrix) values (vec values)]
      (if (= column n)
        (loop [row (dec n) solution (vec (repeat n 0.0))]
          (if (neg? row)
            solution
            (let [known (reduce + (map (fn [column]
                                         (* (get-in matrix [row column])
                                            (nth solution column)))
                                       (range (inc row) n)))]
              (recur (dec row)
                     (assoc solution row
                            (/ (- (nth values row) known)
                               (get-in matrix [row row])))))))
        (let [pivot-row (apply max-key
                               #(math-abs (double (get-in matrix [% column])))
                               (range column n))
              matrix (assoc matrix column (nth matrix pivot-row)
                            pivot-row (nth matrix column))
              values (assoc values column (nth values pivot-row)
                            pivot-row (nth values column))
              pivot (get-in matrix [column column])]
          (when (< (math-abs (double pivot)) 1.0e-12)
            (throw (ex-info "frame stiffness matrix is singular" {:dof column})))
          (let [[matrix values]
                (reduce (fn [[m v] row]
                          (let [factor (/ (get-in m [row column]) pivot)]
                            [(assoc m row
                                    (mapv - (nth m row)
                                          (mapv #(* factor %) (nth m column))))
                             (assoc v row (- (nth v row) (* factor (nth v column))))]))
                        [matrix values] (range (inc column) n))]
            (recur (inc column) matrix values)))))))

(defn- frame-local-stiffness [area elastic-modulus inertia length]
  (let [axial (/ (* area elastic-modulus) length)
        bending (/ (* elastic-modulus inertia) (* length length length))
        a (* 12.0 bending) b (* 6.0 length bending)
        c (* 4.0 length length bending) d (* 2.0 length length bending)]
    [[axial 0.0 0.0 (- axial) 0.0 0.0]
     [0.0 a b 0.0 (- a) b]
     [0.0 b c 0.0 (- b) d]
     [(- axial) 0.0 0.0 axial 0.0 0.0]
     [0.0 (- a) (- b) 0.0 a (- b)]
     [0.0 b d 0.0 (- b) c]]))

(defn- frame-transform [cosine sine]
  [[cosine sine 0.0 0.0 0.0 0.0]
   [(- sine) cosine 0.0 0.0 0.0 0.0]
   [0.0 0.0 1.0 0.0 0.0 0.0]
   [0.0 0.0 0.0 cosine sine 0.0]
   [0.0 0.0 0.0 (- sine) cosine 0.0]
   [0.0 0.0 0.0 0.0 0.0 1.0]])

(defn- condense-frame-releases [stiffness load releases]
  (let [released (vec (keep-indexed #(when %2 %1) releases))
        retained (vec (remove (set released) (range 6)))]
    (if (empty? released)
      {:stiffness stiffness :load load}
      (let [krr (mapv (fn [row] (mapv #(get-in stiffness [row %]) released)) released)
            released-load (mapv #(nth load %) released)
            solve-released (fn [values] (solve-system krr values))
            inverse-columns
            (mapv (fn [column]
                    (solve-released
                     (mapv #(if (= % column) 1.0 0.0) (range (count released)))))
                  (range (count released)))
            inverse (transpose inverse-columns)
            correction-load (matrix-vector inverse released-load)
            condensed
            (reduce (fn [result [i j]]
                      (let [kir (mapv #(get-in stiffness [i %]) released)
                            krj (mapv #(get-in stiffness [% j]) released)
                            correction (reduce + (map * kir (matrix-vector inverse krj)))]
                        (assoc-in result [i j] (- (get-in stiffness [i j]) correction))))
                    (vec (repeat 6 (vec (repeat 6 0.0))))
                    (for [i retained j retained] [i j]))
            condensed-load
            (reduce (fn [result i]
                      (let [kir (mapv #(get-in stiffness [i %]) released)]
                        (assoc result i (- (nth load i)
                                           (reduce + (map * kir correction-load))))))
                    (vec (repeat 6 0.0)) retained)]
        {:stiffness condensed :load condensed-load}))))

(defn analyze-2d-frame
  "Linear elastic Euler-Bernoulli 2D frame analysis with axial and bending
  stiffness, three DOFs per node, uniform local member loads, end-moment
  releases, nodal displacements, reactions, and local member end forces."
  [{:keys [nodes members load-case]}]
  (let [node-index (into {} (map-indexed (fn [index node] [(:id node) index]) nodes))
        node-by-id (into {} (map (juxt :id identity) nodes))
        member-ids (map :id members)
        member-id-set (set member-ids)
        dof-count (* 3 (count nodes))]
    (when (or (empty? nodes) (not= (count nodes) (count node-index))
              (not= (count members) (count (distinct member-ids))))
      (throw (ex-info "frame model requires unique nodes and members" {})))
    (doseq [{:keys [id point restraints]} nodes]
      (when-not (and id (= 2 (count point)) (every? number? point)
                     (= 3 (count restraints)) (every? boolean? restraints))
        (throw (ex-info "invalid 2D frame node"
                        {:node id :point point :restraints restraints}))))
    (doseq [{:keys [id start-node end-node area-m2 elastic-modulus-pa inertia-m4]} members]
      (when-not (and (node-by-id start-node) (node-by-id end-node)
                     (not= start-node end-node)
                     (pos? (or area-m2 0.0)) (pos? (or elastic-modulus-pa 0.0))
                     (pos? (or inertia-m4 0.0)))
        (throw (ex-info "invalid 2D frame member" {:member id}))))
    (doseq [{:keys [node] :as load} (:nodal-loads load-case)]
      (when-not (contains? node-by-id node)
        (throw (ex-info "frame nodal load references an unknown node" {:load load}))))
    (doseq [{:keys [member] :as load} (:member-loads load-case)]
      (when-not (contains? member-id-set member)
        (throw (ex-info "frame member load references an unknown member" {:load load}))))
    (let [member-load-by-id (into {} (map (juxt :member identity)
                                           (:member-loads load-case)))
          member-data
          (mapv
           (fn [{:keys [id start-node end-node area-m2 elastic-modulus-pa inertia-m4
                        release-start-moment? release-end-moment?] :as member}]
             (let [[x1 y1] (:point (node-by-id start-node))
                   [x2 y2] (:point (node-by-id end-node))
                   dx (- x2 x1) dy (- y2 y1) length (sqrt (+ (* dx dx) (* dy dy)))
                   cosine (/ dx length) sine (/ dy length)
                   transform (frame-transform cosine sine)
                   local-stiffness (frame-local-stiffness area-m2 elastic-modulus-pa
                                                           inertia-m4 length)
                   {:keys [qx qy]} (member-load-by-id id)
                   local-load [(* (or qx 0.0) length 0.5)
                               (* (or qy 0.0) length 0.5)
                               (* (or qy 0.0) length length (/ 1.0 12.0))
                               (* (or qx 0.0) length 0.5)
                               (* (or qy 0.0) length 0.5)
                               (* -1.0 (or qy 0.0) length length (/ 1.0 12.0))]
                   release-mask [false false release-start-moment? false false
                                 release-end-moment?]
                   condensed (condense-frame-releases local-stiffness local-load release-mask)
                   global-stiffness (matrix-multiply
                                     (transpose transform)
                                     (matrix-multiply (:stiffness condensed) transform))
                   global-load (matrix-vector (transpose transform) (:load condensed))
                   start (* 3 (node-index start-node)) end (* 3 (node-index end-node))
                   dofs [start (inc start) (+ start 2) end (inc end) (+ end 2)]]
               {:member member :length length :transform transform
                :local-stiffness (:stiffness condensed) :local-load (:load condensed)
                :global-stiffness global-stiffness :global-load global-load :dofs dofs}))
           members)
          stiffness
          (reduce (fn [matrix {:keys [global-stiffness dofs]}]
                    (reduce (fn [result [i j]]
                              (update-in result [(nth dofs i) (nth dofs j)] +
                                         (get-in global-stiffness [i j])))
                            matrix (for [i (range 6) j (range 6)] [i j])))
                  (vec (repeat dof-count (vec (repeat dof-count 0.0)))) member-data)
          member-loads
          (reduce (fn [loads {:keys [global-load dofs]}]
                    (reduce (fn [result index]
                              (update result (nth dofs index) + (nth global-load index)))
                            loads (range 6)))
                  (vec (repeat dof-count 0.0)) member-data)
          loads
          (reduce (fn [result {:keys [node fx fy mz]}]
                    (let [offset (* 3 (node-index node))]
                      (-> result (update offset + (or fx 0.0))
                          (update (inc offset) + (or fy 0.0))
                          (update (+ offset 2) + (or mz 0.0)))))
                  member-loads (:nodal-loads load-case))
          fixed (into #{} (mapcat (fn [[index node]]
                                    (keep-indexed #(when %2 (+ (* 3 index) %1))
                                                  (:restraints node)))
                                  (map-indexed vector nodes)))
          inactive (into #{} (filter (fn [dof]
                                       (and (every? #(< (math-abs (double %)) 1.0e-12)
                                                    (nth stiffness dof))
                                            (< (math-abs (double (nth loads dof))) 1.0e-12)))
                                     (range dof-count)))
          free (vec (remove (into fixed inactive) (range dof-count)))
          reduced (mapv (fn [row] (mapv #(get-in stiffness [row %]) free)) free)
          solution (solve-system reduced (mapv #(nth loads %) free))
          displacements (reduce (fn [result [dof value]] (assoc result dof value))
                                (vec (repeat dof-count 0.0)) (map vector free solution))
          reactions (mapv - (matrix-vector stiffness displacements) loads)
          node-results
          (into {} (map-indexed
                    (fn [index node]
                      [(:id node) {:ux (nth displacements (* 3 index))
                                   :uy (nth displacements (inc (* 3 index)))
                                   :rz (nth displacements (+ (* 3 index) 2))
                                   :rx (nth reactions (* 3 index))
                                   :ry (nth reactions (inc (* 3 index)))
                                   :rmz (nth reactions (+ (* 3 index) 2))}]) nodes))
          member-results
          (into {}
                (map (fn [{:keys [member transform local-stiffness local-load dofs]}]
                       (let [global-displacements (mapv #(nth displacements %) dofs)
                             local-displacements (matrix-vector transform global-displacements)
                             forces (mapv - (matrix-vector local-stiffness local-displacements)
                                          local-load)]
                         [(:id member)
                          {:local-displacements local-displacements
                           :local-end-forces {:n1 (nth forces 0) :v1 (nth forces 1)
                                              :m1 (nth forces 2) :n2 (nth forces 3)
                                              :v2 (nth forces 4) :m2 (nth forces 5)}}]))
                     member-data))]
      {:structural.frame/nodes node-results :structural.frame/members member-results
       :structural.frame/displacements displacements
       :structural.frame/reactions reactions})))

(defn analyze-plane-stress-mesh
  "Analyze a 2D mesh of constant-strain triangular membrane elements. Each
  node has ux/uy DOFs; results include reactions and element εx, εy, γxy,
  σx, σy, and τxy in global coordinates."
  [{:keys [nodes elements loads]}]
  (let [node-index (into {} (map-indexed (fn [index node] [(:id node) index]) nodes))
        node-by-id (into {} (map (juxt :id identity) nodes))
        element-ids (map :id elements)
        dof-count (* 2 (count nodes))]
    (when (or (empty? nodes) (empty? elements)
              (not= (count nodes) (count node-index))
              (not= (count elements) (count (distinct element-ids))))
      (throw (ex-info "plane-stress mesh requires unique nodes and elements" {})))
    (doseq [{:keys [id point restraints]} nodes]
      (when-not (and id (= 2 (count point)) (every? number? point)
                     (= 2 (count restraints)) (every? boolean? restraints))
        (throw (ex-info "invalid plane-stress node"
                        {:node id :point point :restraints restraints}))))
    (let [element-data
          (mapv
           (fn [{:keys [id nodes thickness-m elastic-modulus-pa poisson-ratio] :as element}]
             (let [[n1 n2 n3] (map node-by-id nodes)]
               (when-not (and (= 3 (count nodes)) n1 n2 n3
                              (pos? (or thickness-m 0.0))
                              (pos? (or elastic-modulus-pa 0.0))
                              (number? poisson-ratio)
                              (< -1.0 poisson-ratio 0.5))
                 (throw (ex-info "invalid plane-stress triangle" {:element id})))
               (let [[[x1 y1] [x2 y2] [x3 y3]] (map :point [n1 n2 n3])
                     signed-double-area (+ (* x1 (- y2 y3)) (* x2 (- y3 y1))
                                           (* x3 (- y1 y2)))
                     area (/ (math-abs signed-double-area) 2.0)]
                 (when (< area 1.0e-12)
                   (throw (ex-info "plane-stress triangle has zero area" {:element id})))
                 (let [orientation (if (neg? signed-double-area) -1.0 1.0)
                       denominator (* orientation signed-double-area)
                       b (mapv #(* orientation %) [(- y2 y3) (- y3 y1) (- y1 y2)])
                       c (mapv #(* orientation %) [(- x3 x2) (- x1 x3) (- x2 x1)])
                       strain-matrix
                       (mapv #(mapv (fn [value] (/ value denominator)) %)
                             [[(b 0) 0.0 (b 1) 0.0 (b 2) 0.0]
                              [0.0 (c 0) 0.0 (c 1) 0.0 (c 2)]
                              [(c 0) (b 0) (c 1) (b 1) (c 2) (b 2)]])
                       factor (/ elastic-modulus-pa (- 1.0 (* poisson-ratio poisson-ratio)))
                       constitutive
                       [[factor (* factor poisson-ratio) 0.0]
                        [(* factor poisson-ratio) factor 0.0]
                        [0.0 0.0 (* factor (/ (- 1.0 poisson-ratio) 2.0))]]
                       stiffness (mapv #(mapv (fn [value] (* thickness-m area value)) %)
                                       (matrix-multiply
                                        (transpose strain-matrix)
                                        (matrix-multiply constitutive strain-matrix)))
                       dofs (vec (mapcat (fn [node-id]
                                           (let [offset (* 2 (node-index node-id))]
                                             [offset (inc offset)])) nodes))]
                   {:element element :area area :dofs dofs
                    :strain-matrix strain-matrix :constitutive constitutive
                    :stiffness stiffness}))))
           elements)
          stiffness
          (reduce (fn [matrix {:keys [stiffness dofs]}]
                    (reduce (fn [result [i j]]
                              (update-in result [(nth dofs i) (nth dofs j)] +
                                         (get-in stiffness [i j])))
                            matrix (for [i (range 6) j (range 6)] [i j])))
                  (vec (repeat dof-count (vec (repeat dof-count 0.0)))) element-data)
          load-vector
          (reduce (fn [result {:keys [node fx fy] :as load}]
                    (when-not (contains? node-index node)
                      (throw (ex-info "plane-stress load references an unknown node"
                                      {:load load})))
                    (let [offset (* 2 (node-index node))]
                      (-> result (update offset + (or fx 0.0))
                          (update (inc offset) + (or fy 0.0)))))
                  (vec (repeat dof-count 0.0)) loads)
          fixed (into #{} (mapcat (fn [[index node]]
                                    (keep-indexed #(when %2 (+ (* 2 index) %1))
                                                  (:restraints node)))
                                  (map-indexed vector nodes)))
          free (vec (remove fixed (range dof-count)))
          reduced (mapv (fn [row] (mapv #(get-in stiffness [row %]) free)) free)
          solution (solve-system reduced (mapv #(nth load-vector %) free))
          displacements (reduce (fn [result [dof value]] (assoc result dof value))
                                (vec (repeat dof-count 0.0)) (map vector free solution))
          reactions (mapv - (matrix-vector stiffness displacements) load-vector)
          node-results
          (into {} (map-indexed
                    (fn [index node]
                      [(:id node) {:ux (nth displacements (* 2 index))
                                   :uy (nth displacements (inc (* 2 index)))
                                   :rx (nth reactions (* 2 index))
                                   :ry (nth reactions (inc (* 2 index)))}]) nodes))
          element-results
          (into {}
                (map (fn [{:keys [element area dofs strain-matrix constitutive]}]
                       (let [local-displacements (mapv #(nth displacements %) dofs)
                             strain (matrix-vector strain-matrix local-displacements)
                             stress (matrix-vector constitutive strain)]
                         [(:id element)
                          {:area-m2 area
                           :strain {:epsilon-x (nth strain 0) :epsilon-y (nth strain 1)
                                    :gamma-xy (nth strain 2)}
                           :stress-pa {:sigma-x (nth stress 0) :sigma-y (nth stress 1)
                                       :tau-xy (nth stress 2)}}]))
                     element-data))]
      {:structural.plane-stress/nodes node-results
       :structural.plane-stress/elements element-results
       :structural.plane-stress/displacements displacements
       :structural.plane-stress/reactions reactions})))

(defn- numeric-leaves
  ([value] (numeric-leaves [] value))
  ([path value]
   (cond
     (number? value) [[path value]]
     (map? value) (mapcat (fn [[key child]] (numeric-leaves (conj path key) child)) value)
     (vector? value) (mapcat (fn [index child]
                               (numeric-leaves (conj path index) child))
                             (range) value)
     :else [])))

(defn combine-results
  "Apply a linear load combination to compatible frame or plane-stress result
  trees. Numeric leaves are combined; semantic ids and result structure come
  from the first referenced case."
  [combination-id results-by-case factors]
  (when-not (and combination-id (seq factors)
                 (every? #(contains? results-by-case %) (keys factors))
                 (every? number? (vals factors)))
    (throw (ex-info "structural combination requires known cases and numeric factors"
                    {:combination-id combination-id :factors factors})))
  (let [cases (mapv results-by-case (keys factors))
        paths (into #{} (mapcat #(map first (numeric-leaves %))) cases)
        combined
        (reduce (fn [result path]
                  (assoc-in result path
                            (reduce-kv (fn [sum case-id factor]
                                         (+ sum (* factor
                                                   (double (or (get-in (get results-by-case
                                                                            case-id) path)
                                                               0.0)))))
                                       0.0 factors)))
                (first cases) paths)]
    {:structural.combination/id combination-id
     :structural.combination/factors factors
     :structural.combination/result combined}))

(defn result-envelope
  "Return min/max values and governing case ids for every numeric result path."
  [results-by-case]
  (when-not (seq results-by-case)
    (throw (ex-info "structural result envelope requires cases" {})))
  (let [paths (into #{} (mapcat #(map first (numeric-leaves %)) (vals results-by-case)))]
    {:structural.envelope/cases (vec (keys results-by-case))
     :structural.envelope/by-path
     (into {}
           (map (fn [path]
                  (let [values (keep (fn [[case-id result]]
                                       (when-let [value (get-in result path)]
                                         (when (number? value) [case-id value])))
                                     results-by-case)
                        minimum (apply min-key second values)
                        maximum (apply max-key second values)]
                    [path {:min (second minimum) :min-case (first minimum)
                           :max (second maximum) :max-case (first maximum)}])))
           paths)}))
