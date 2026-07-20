(ns bim.interchange
  "BIM drawing export through shared kotoba-lang DXF and ISO PDF libraries."
  (:require [dxf.core :as dxf]
            [pdf.core :as pdf]))

(defn- storey-extents [storey]
  (let [points (concat (mapcat #(get-in % [:geometry :axis])
                               (filter (comp #{:wall} :kind) (:elements storey)))
                       (mapcat #(get-in % [:geometry :boundary])
                               (filter (comp #{:slab} :kind) (:elements storey))))
        xs (map first points) ys (map second points)]
    {:min [(if (seq xs) (reduce min xs) 0.0) (if (seq ys) (reduce min ys) 0.0)]
     :max [(if (seq xs) (reduce max xs) 10.0) (if (seq ys) (reduce max ys) 10.0)]}))

(defn floor-plan-dxf
  "Export architectural plan geometry and tags as a valid ASCII DXF document."
  [storey]
  (let [entities
        (concat
         (for [element (:elements storey) :when (= :wall (:kind element))
               :let [[from to] (get-in element [:geometry :axis])]]
           [:line {:layer "A-WALL" :from from :to to}])
         (for [element (:elements storey) :when (= :slab (:kind element))]
           [:lwpolyline {:layer "A-SLAB"
                         :points (mapv #(subvec (vec %) 0 2)
                                       (get-in element [:geometry :boundary]))}])
         (for [element (:elements storey)
               :let [position (or (get-in element [:placement :location])
                                  (first (get-in element [:geometry :axis])) [0 0 0])]]
           [:text {:layer "A-ANNO" :at position :height 0.2
                   :text (str (:id element) " " (:name element))}]))]
    (apply dxf/drawing entities)))

(defn- floor-plan-pdf-content [storey {:keys [scale margin page-height]
                                        :or {scale 40.0 margin 30.0 page-height 595.0}}]
  (let [{lower :min} (storey-extents storey) [min-x min-y] lower
        point (fn [[x y & _]] [(+ margin (* scale (- x min-x)))
                               (- page-height margin (* scale (- y min-y)))])]
    (apply str
           (concat
            [(pdf/text-command {:x margin :y (- page-height 20)
                                :size 14 :text (str (:name storey) " Plan")})]
            (for [element (:elements storey) :when (= :wall (:kind element))
                  :let [[from to] (get-in element [:geometry :axis])]]
              (pdf/line-command {:from (point from) :to (point to) :width 2.0}))
            (for [element (:elements storey)
                  :let [position (or (first (get-in element [:geometry :axis]))
                                     (get-in element [:placement :location]) [0 0 0])
                        [x y] (point position)]]
              (pdf/text-command {:x (+ x 3) :y (+ y 3) :size 8
                                 :text (str (:id element))}))))))

(defn drawing-set-pdf
  "Export one vector PDF page per storey, suitable for archival or issue."
  ([storeys] (drawing-set-pdf storeys {}))
  ([storeys {:keys [width height] :or {width 842.0 height 595.0} :as options}]
   (pdf/write-document
    (mapv (fn [storey]
            {:width width :height height
             :content (floor-plan-pdf-content storey
                                              (assoc options :page-height height))})
          storeys))))
