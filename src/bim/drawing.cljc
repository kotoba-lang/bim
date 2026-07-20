(ns bim.drawing
  "BIM-to-SVG projection. XML/SVG rendering is owned by kotoba-lang/svg."
  (:require [clojure.string :as string]
            [svg.core :as svg]
            [svg.shapes :as shapes]))

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
