(ns bim.interchange-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [bim :as bim]
            [bim.interchange :as interchange]
            [pdf.core :as pdf]))

(def storey
  (bim/storey
   {:id 1 :name "Ground" :elevation 0 :height 3 :placement :identity :spaces []
    :elements [(bim/wall {:id 10 :name "Exterior Wall"
                          :start [0 0 0] :end [6 0 0] :height 3.0})
               (bim/slab {:id 11 :name "Floor"
                          :boundary [[0 0 0] [6 0 0] [6 4 0] [0 4 0]]
                          :thickness 0.2})]}))

(deftest exports-shared-dxf-and-iso-pdf-formats
  (let [dxf (interchange/floor-plan-dxf storey)
        pdf-bytes (interchange/drawing-set-pdf [storey])
        pdf-text (apply str (map char pdf-bytes))
        parsed (pdf/parse pdf-bytes)
        page (first (pdf/pages parsed))]
    (is (string/includes? dxf "0\nLINE\n8\nA-WALL"))
    (is (string/includes? dxf "0\nLWPOLYLINE\n8\nA-SLAB"))
    (is (string/ends-with? dxf "0\nENDSEC\n0\nEOF"))
    (is (string/starts-with? pdf-text "%PDF-1.4"))
    (is (string/includes? pdf-text "startxref"))
    (is (= 1 (count (pdf/pages parsed))))
    (is (= ["Ground Plan" "10" "11"]
           (pdf/page-text (:objects parsed) page)))))

(deftest exports-persistent-annotations-to-dxf-and-pdf
  (let [annotations [{:kind :dimension :from [0 0] :to [6 0] :value 6.0 :label "6000"
                      :annotation/id :dimension-1}
                     {:kind :leader :points [[1 0] [1 1] [2 1]] :text "Rated wall"
                      :annotation/id :leader-1}
                     {:kind :revision-cloud :points [[0 0] [2 0] [2 2] [0 2]]
                      :revision "C01" :annotation/id :revision-1}]
        dxf (interchange/floor-plan-dxf storey {:annotations annotations})
        bytes (interchange/drawing-set-pdf
               [storey] {:annotations-by-storey {1 annotations}
                         :print-setting {:print-setting/paper-size :a3
                                         :print-setting/orientation :portrait
                                         :print-setting/scale 100
                                         :print-setting/margins-mm [5 5 5 5]}})
        parsed (pdf/parse bytes)
        page (first (pdf/pages parsed))
        text (pdf/page-text (:objects parsed) page)]
    (is (string/includes? dxf "8\nA-DIMS"))
    (is (string/includes? dxf "8\nA-REVS"))
    (is (string/includes? dxf "Rated wall"))
    (is (some #{"6000"} text))
    (is (some #{"Rated wall"} text))
    (is (< 840 (get-in page [:MediaBox 2]) 843))
    (is (< 1190 (get-in page [:MediaBox 3]) 1192))))
