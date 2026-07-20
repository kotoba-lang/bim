(ns bim.interchange
  "BIM drawing export through shared kotoba-lang DXF and ISO PDF libraries."
  (:require [dxf.core :as dxf]
            [bim.drawing :as drawing]
            [pdf.core :as pdf]))

(defn- storey-extents [storey]
  (let [points (concat (mapcat #(get-in % [:geometry :axis])
                               (filter (comp #{:wall} :kind) (:elements storey)))
                       (mapcat #(get-in % [:geometry :boundary])
                               (filter (comp #{:slab} :kind) (:elements storey))))
        xs (map first points) ys (map second points)]
    {:min [(if (seq xs) (reduce min xs) 0.0) (if (seq ys) (reduce min ys) 0.0)]
     :max [(if (seq xs) (reduce max xs) 10.0) (if (seq ys) (reduce max ys) 10.0)]}))

(defn- annotation-dxf-entities [annotation]
  (let [layer (case (:kind annotation) :dimension "A-DIMS" :revision-cloud "A-REVS" "A-ANNO")]
    (case (:kind annotation)
      :dimension [[:line {:layer layer :from (:from annotation) :to (:to annotation)}]
                  [:text {:layer layer :at (mapv #(/ (+ %1 %2) 2.0)
                                                 (:from annotation) (:to annotation))
                          :height 0.2 :text (or (:label annotation)
                                                (str (:value annotation) " m"))}]]
      (:tag :text :level) [[:text {:layer layer :at (:point annotation) :height 0.2
                                   :text (or (:text annotation) (:label annotation))}]]
      :leader (concat
               (map (fn [[from to]] [:line {:layer layer :from from :to to}])
                    (partition 2 1 (:points annotation)))
               [[:text {:layer layer :at (last (:points annotation)) :height 0.2
                        :text (:text annotation)}]])
      :revision-cloud [[:lwpolyline {:layer layer
                                     :points (conj (vec (:points annotation))
                                                   (first (:points annotation)))}]]
      [])))

(defn floor-plan-dxf
  "Export architectural plan geometry and tags as a valid ASCII DXF document."
  ([storey] (floor-plan-dxf storey {}))
  ([storey {:keys [annotations]}]
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
                   :text (str (:id element) " " (:name element))}])
         (mapcat annotation-dxf-entities annotations))]
     (apply dxf/drawing entities))))

(defn- annotation-pdf-commands [annotation point]
  (let [line (fn [[from to]] (pdf/line-command {:from (point from) :to (point to) :width 0.8}))
        text (fn [at value] (let [[x y] (point at)]
                              (pdf/text-command {:x (+ x 3) :y (+ y 3) :size 8 :text value})))]
    (case (:kind annotation)
      :dimension [(line [(:from annotation) (:to annotation)])
                  (text (mapv #(/ (+ %1 %2) 2.0) (:from annotation) (:to annotation))
                        (or (:label annotation) (str (:value annotation) " m")))]
      (:tag :text :level) [(text (:point annotation)
                                      (or (:text annotation) (:label annotation)))]
      :leader (concat (map line (partition 2 1 (:points annotation)))
                      [(text (last (:points annotation)) (:text annotation))])
      :revision-cloud (map line (partition 2 1 (conj (vec (:points annotation))
                                                     (first (:points annotation)))))
      [])))

(defn- floor-plan-pdf-content [storey {:keys [scale margin page-height annotations]
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
                                 :text (str (:id element))}))
            (mapcat #(annotation-pdf-commands % point) annotations)))))

(defn drawing-set-pdf
  "Export one vector PDF page per storey, suitable for archival or issue."
  ([storeys] (drawing-set-pdf storeys {}))
  ([storeys {:keys [width height annotations-by-storey print-setting] :as options}]
   (let [mm->points #(* % (/ 72.0 25.4))
         paper (get drawing/sheet-sizes-mm
                    (:print-setting/paper-size print-setting) [297 210])
         portrait? (= :portrait (:print-setting/orientation print-setting))
         [paper-width paper-height]
         (if portrait? [(min (first paper) (second paper))
                        (max (first paper) (second paper))]
             [(max (first paper) (second paper))
              (min (first paper) (second paper))])
         width (or width (mm->points paper-width))
         height (or height (mm->points paper-height))
         denominator (:print-setting/scale print-setting)
         scale (if (number? denominator) (/ (mm->points 1000.0) denominator)
                   (or (:scale options) 40.0))
         margin-mm (first (:print-setting/margins-mm print-setting))
         margin (if (number? margin-mm) (mm->points margin-mm)
                    (or (:margin options) 30.0))]
     (pdf/write-document
      (mapv (fn [storey]
              {:width width :height height
               :content (floor-plan-pdf-content
                         storey (assoc options :page-height height :scale scale :margin margin
                                       :annotations
                                       (get annotations-by-storey (:id storey))))})
            storeys)))))
