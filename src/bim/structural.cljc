(ns bim.structural
  "Structural shell, reinforcement, and steel connection design contracts."
  (:require [bim :as bim]))

(def schema-version 1)
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))
(defn- sqrt [value] (#?(:clj Math/sqrt :cljs js/Math.sqrt) value))

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
