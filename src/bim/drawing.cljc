(ns bim.drawing
  "BIM-to-SVG projection. XML/SVG rendering is owned by kotoba-lang/svg."
  (:require [bim :as bim]
            [clojure.string :as string]
            [svg.core :as svg]
            [svg.shapes :as shapes]))

(defn- bounds-2d [points]
  (let [points (seq points) xs (map first points) ys (map second points)]
    (if points
      {:min [(reduce min xs) (reduce min ys)] :max [(reduce max xs) (reduce max ys)]}
      {:min [0.0 0.0] :max [10.0 10.0]})))

(defn- dimension-group [[x1 y1] [x2 y2] offset label]
  (let [dx (- x2 x1) dy (- y2 y1)
        length (#?(:clj Math/sqrt :cljs js/Math.sqrt) (+ (* dx dx) (* dy dy)))
        nx (if (pos? length) (/ (- dy) length) 0.0)
        ny (if (pos? length) (/ dx length) 0.0)
        ax (+ x1 (* offset nx)) ay (+ y1 (* offset ny))
        bx (+ x2 (* offset nx)) by (+ y2 (* offset ny))
        mx (/ (+ ax bx) 2.0) my (/ (+ ay by) 2.0)]
    [:g {:class "dimension"}
     (shapes/line x1 y1 ax ay {:stroke "#475569" :stroke-width 1})
     (shapes/line x2 y2 bx by {:stroke "#475569" :stroke-width 1})
     (shapes/line ax ay bx by {:stroke "#0f172a" :stroke-width 1
                               :marker-start "url(#arrow)" :marker-end "url(#arrow)"})
     (shapes/text mx (- my 4) label {:text-anchor "middle" :font-family "sans-serif"
                                     :font-size 11 :fill "#0f172a"})]))

(defn- format-2 [value]
  #?(:clj (format "%.2f" (double value)) :cljs (.toFixed value 2)))

(defn floor-plan
  ([storey] (floor-plan storey {}))
  ([storey {:keys [scale margin] :or {scale 60 margin 40}}]
   (let [walls (filter #(= :wall (:kind %)) (:elements storey))
         slabs (filter #(= :slab (:kind %)) (:elements storey))
         points (concat (mapcat #(get-in % [:geometry :axis]) walls)
                        (mapcat #(get-in % [:geometry :boundary]) slabs))
         xs (map first points) ys (map second points)
         min-x (if (seq xs) (reduce min xs) 0) max-x (if (seq xs) (reduce max xs) 10)
         min-y (if (seq ys) (reduce min ys) 0) max-y (if (seq ys) (reduce max ys) 10)
         width (+ (* scale (- max-x min-x)) (* 2 margin))
         height (+ (* scale (- max-y min-y)) (* 2 margin))
         sx #(+ margin (* scale (- % min-x)))
         sy #(- height margin (* scale (- % min-y)))]
     (apply svg/svg {:viewBox (str "0 0 " width " " height)}
            (concat
             [(shapes/rect 0 0 "100%" "100%" {:fill "white"})
              (shapes/text margin 24 (str (:name storey) " · Floor Plan · 1:100")
                           {:font-family "sans-serif" :font-size 16})]
             (for [slab slabs]
               [:polygon {:points (string/join " " (map (fn [[x y _]] (str (sx x) "," (sy y)))
                                                         (get-in slab [:geometry :boundary])))
                          :fill "#eef2f7" :stroke "#64748b"}])
             (for [wall walls
                   :let [[[x1 y1 _] [x2 y2 _]] (get-in wall [:geometry :axis])]]
               (shapes/line (sx x1) (sy y1) (sx x2) (sy y2)
                            {:stroke "#111827"
                             :stroke-width (max 2 (* scale (get-in wall [:geometry :profile :thickness])))})))))))

(defn floor-plan-svg
  ([storey] (floor-plan-svg storey {}))
  ([storey options] (svg/render (floor-plan storey options))))

(defn documented-floor-plan
  "Annotated plan view with wall openings, element tags, overall dimensions,
  and vector metadata suitable for placement on a drawing sheet."
  ([storey] (documented-floor-plan storey {}))
  ([storey {:keys [scale margin] :or {scale 60 margin 60} :as options}]
   (let [walls (filter #(= :wall (:kind %)) (:elements storey))
         points (mapcat #(get-in % [:geometry :axis]) walls)
         {:keys [min max]} (bounds-2d points) [min-x min-y] min [max-x max-y] max
         height (+ (* scale (- max-y min-y)) (* 2 margin))
         sx #(+ margin (* scale (- % min-x))) sy #(- height margin (* scale (- % min-y)))
         base (floor-plan storey (assoc options :scale scale :margin margin))
         defs [:defs [:marker {:id "arrow" :markerWidth 8 :markerHeight 8 :refX 4 :refY 4
                               :orient "auto-start-reverse"}
                       [:path {:d "M 0 0 L 8 4 L 0 8 z" :fill "#0f172a"}]]]
         openings
         (for [wall walls opening (:openings wall)
               :let [[[x1 y1 _] [x2 y2 _]] (get-in wall [:geometry :axis])
                     length (get-in wall [:quantities :length-m])
                     start (/ (get-in opening [:placement :offset]) length)
                     finish (/ (+ (get-in opening [:placement :offset])
                                  (get-in opening [:profile :width])) length)
                     ox1 (+ x1 (* start (- x2 x1))) oy1 (+ y1 (* start (- y2 y1)))
                     ox2 (+ x1 (* finish (- x2 x1))) oy2 (+ y1 (* finish (- y2 y1)))]]
           [:g {:class "opening" :data-opening-id (:id opening)}
            (shapes/line (sx ox1) (sy oy1) (sx ox2) (sy oy2)
                         {:stroke "white" :stroke-width
                          (+ 2 (* scale (get-in wall [:geometry :profile :thickness])))})
            (shapes/line (sx ox1) (sy oy1) (sx ox2) (sy oy2)
                         {:stroke "#64748b" :stroke-width 1})])
         tags (for [wall walls
                    :let [[[x1 y1 _] [x2 y2 _]] (get-in wall [:geometry :axis])]]
                (shapes/text (sx (/ (+ x1 x2) 2.0)) (- (sy (/ (+ y1 y2) 2.0)) 8)
                             (str "W" (:id wall))
                             {:class "element-tag" :text-anchor "middle"
                              :font-family "sans-serif" :font-size 10 :fill "#334155"}))]
     (into base
           (concat [defs]
                   openings tags
                   [(dimension-group [(sx min-x) (sy min-y)] [(sx max-x) (sy min-y)] 28
                                     (str (format-2 (- max-x min-x)) " m"))
                    (dimension-group [(sx min-x) (sy min-y)] [(sx min-x) (sy max-y)] -28
                                     (str (format-2 (- max-y min-y)) " m"))])))))

(defn documented-floor-plan-svg
  ([storey] (documented-floor-plan-svg storey {}))
  ([storey options] (svg/render (documented-floor-plan storey options))))

(defn- all-building-elements [building]
  (mapcat (fn [storey]
            (map #(assoc % :drawing/storey storey) (:elements storey)))
          (:storeys building)))

(defn- mesh-projection-bounds [element horizontal-index]
  (when-let [mesh (bim/element-mesh element)]
    (let [positions (:positions mesh)
          horizontal (map #(nth % horizontal-index) positions)
          vertical (map #(nth % 2) positions)
          depth-index (if (= horizontal-index 0) 1 0)
          depths (map #(nth % depth-index) positions)]
      {:horizontal [(reduce min horizontal) (reduce max horizontal)]
       :vertical [(reduce min vertical) (reduce max vertical)]
       :depth [(reduce min depths) (reduce max depths)]})))

(defn orthographic-view
  "Generate a section or elevation from mesh bounds. Sections distinguish cut
  objects from projected objects; elevations emit front-view silhouettes."
  [building {:keys [kind axis cut-position depth scale margin title]
             :or {kind :section axis :x cut-position 0.0 depth 1000.0 scale 50 margin 50}}]
  (let [horizontal-index (if (= axis :x) 0 1)
        entries (keep (fn [element]
                        (when-let [bounds (mesh-projection-bounds element horizontal-index)]
                          {:element element :bounds bounds}))
                      (all-building-elements building))
        visible (filter (fn [{:keys [bounds]}]
                          (let [[near far] (:depth bounds)]
                            (if (= kind :section)
                              (and (<= near (+ cut-position depth)) (>= far cut-position))
                              (<= (#?(:clj Math/abs :cljs js/Math.abs) (- near cut-position)) depth))))
                        entries)
        points (mapcat (fn [{:keys [bounds]}]
                         (let [[x0 x1] (:horizontal bounds) [z0 z1] (:vertical bounds)]
                           [[x0 z0] [x1 z1]])) visible)
        {:keys [min] maximum :max} (bounds-2d points)
        [min-x min-z] min [max-x max-z] maximum
        width (+ (* scale (- max-x min-x)) (* 2 margin))
        height (+ (* scale (- max-z min-z)) (* 2 margin))
        sx #(+ margin (* scale (- % min-x))) sz #(- height margin (* scale (- % min-z)))]
    (apply svg/svg {:viewBox (str "0 0 " width " " height)
                    :data-view-kind (name kind)}
           (concat
            [(shapes/rect 0 0 "100%" "100%" {:fill "white"})
             (shapes/text margin 24 (or title (str (string/capitalize (name kind)) " " (name axis)))
                          {:font-family "sans-serif" :font-size 16})]
            (for [{:keys [element bounds]} visible
                  :let [[x0 x1] (:horizontal bounds) [z0 z1] (:vertical bounds)
                        [near far] (:depth bounds)
                        cut? (and (= kind :section) (<= near cut-position) (<= cut-position far))]]
              [:g {:class (if cut? "cut-element" "projected-element")
                   :data-element-id (:id element)}
               (shapes/rect (sx x0) (sz z1) (max 1 (- (sx x1) (sx x0)))
                            (max 1 (- (sz z0) (sz z1)))
                            {:fill (if cut? "#dbeafe" "none")
                             :stroke (if cut? "#0f172a" "#64748b")
                             :stroke-width (if cut? 2 1)
                             :stroke-dasharray (when-not cut? "4 3")})
               (shapes/text (/ (+ (sx x0) (sx x1)) 2.0) (- (sz z1) 4)
                            (str (string/upper-case (subs (name (:kind element)) 0 1)) (:id element))
                            {:class "element-tag" :text-anchor "middle" :font-size 9})])))))

(defn orthographic-view-svg [building options]
  (svg/render (orthographic-view building options)))

(def sheet-sizes-mm {:a0 [1189 841] :a1 [841 594] :a2 [594 420] :a3 [420 297]})

(defn drawing-sheet
  "Compose renderable SVG views into a sheet with viewport frames and title block."
  [{:keys [number name size revision viewports]
    :or {number "A-001" name "Drawing Sheet" size :a1 revision "P01"}}]
  (let [[width height] (get sheet-sizes-mm size (get sheet-sizes-mm :a1))]
    (apply svg/svg {:viewBox (str "0 0 " width " " height) :data-sheet-number number}
           (concat
            [(shapes/rect 0 0 width height {:fill "white" :stroke "#111827"})]
            (for [{:keys [view x y width height title scale]
                   :or {x 20 y 20 width 360 height 240 scale 100}} viewports]
              (let [[tag attrs & children] view]
                [:g {:class "viewport" :data-view-title title}
                 (into [tag (assoc attrs :x x :y y :width width :height height)] children)
                 (shapes/rect x y width height {:fill "none" :stroke "#94a3b8"})
                 (shapes/text x (+ y height 14) (str title " · 1:" scale)
                              {:font-family "sans-serif" :font-size 10})]))
            [[:g {:class "title-block"}
              (shapes/rect (- width 250) (- height 55) 250 55
                           {:fill "white" :stroke "#111827"})
              (shapes/text (- width 240) (- height 34) name
                           {:font-family "sans-serif" :font-size 14})
              (shapes/text (- width 240) (- height 14)
                           (str number " · " revision " · " (string/upper-case (clojure.core/name size)))
                           {:font-family "sans-serif" :font-size 10})]]))))

(defn drawing-sheet-svg [sheet] (svg/render (drawing-sheet sheet)))
