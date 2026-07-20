(ns bim-test
  "Restoration-fidelity tests — one per original kami-bim Rust test
  (kami-engine/kami-bim/src/lib.rs `mod tests`, deleted PR #82)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [bim]
            [bim.integration :as integration]
            [bim.ifc :as ifc]
            [bim.drawing :as drawing]))

(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? (find-ns 'bim)))))

;; mirrors `project_roundtrip` — the original round-trips through
;; serde_json to prove the model holds together; in CLJC the EDN map IS
;; the data, so this verifies the same nested-hierarchy construction
;; preserves values (no serialize/deserialize step needed).
(deftest project-roundtrip
  (let [p (-> (bim/project "Test")
              (update :sites conj
                      (bim/site {:id 1 :name "Site 1" :geo nil :placement :identity
                                 :buildings [(bim/building {:id 2 :name "Building A" :placement :identity
                                                             :reference-elevation 0.0
                                                             :storeys [(bim/storey {:id 3 :name "L1" :elevation 0.0
                                                                                     :height 3.5 :placement :identity
                                                                                     :spaces [] :elements []})]})]})))]
    (is (= "Test" (:name p)))
    (is (= 1 (count (:sites p))))
    (is (= 3.5 (:height (first (:storeys (first (:buildings (first (:sites p)))))))))))

;; mirrors `find_storey_by_id`
(deftest find-storey-by-id
  (let [storey-id 42
        p (-> (bim/project "Test")
              (update :sites conj
                      (bim/site {:id 1 :name "Site" :geo nil :placement :identity
                                 :buildings [(bim/building {:id 2 :name "B" :placement :identity
                                                             :reference-elevation 0.0
                                                             :storeys [(bim/storey {:id storey-id :name "GF" :elevation 0.0
                                                                                     :height 3.0 :placement :identity
                                                                                     :spaces [] :elements []})]})]})))]
    (is (some? (bim/find-storey p storey-id)))
    (is (nil? (bim/find-storey p 999)))))

(deftest editable-wall-model-and-mesh
  (let [base (-> (bim/project "Editor")
                 (update :sites conj
                         (bim/site {:id 1 :name "Site" :geo nil :placement :identity
                                    :buildings [(bim/building {:id 2 :name "B" :placement :identity
                                                                :reference-elevation 0 :storeys
                                                                [(bim/storey {:id 3 :name "GF" :elevation 0 :height 3
                                                                              :placement :identity :spaces [] :elements []})]})]})))
        w (bim/wall {:id 10 :start [0 0 0] :end [8 0 0] :thickness 0.25 :height 3.2})
        with-wall (bim/add-element base 3 w)
        mesh (bim/wall-mesh w)]
    (is (= 1 (count (:elements (bim/find-storey with-wall 3)))))
    (is (= 8.0 (get-in w [:quantities :length-m])))
    (is (= 8 (count (:positions mesh))))
    (is (= 36 (count (:indices mesh))))
    (is (empty? (:elements (bim/find-storey (bim/delete-element with-wall 3 10) 3))))))

(deftest hosted-door-and-window-openings
  (let [door-opening (bim/rectangular-opening {:id 20 :offset 1 :width 0.9 :height 2.1 :filled-by 30})
        window-opening (bim/rectangular-opening {:id 21 :offset 4 :sill 0.9 :width 1.2 :height 1.2 :filled-by 31})
        hosted-wall (-> (bim/wall {:id 10 :start [0 0 0] :end [8 0 0] :thickness 0.25 :height 3})
                        (bim/add-opening-to-wall door-opening)
                        (bim/add-opening-to-wall window-opening))
        d (bim/door {:id 30 :host-id 10 :opening-id 20})
        w (bim/window {:id 31 :host-id 10 :opening-id 21})
        mesh (bim/wall-with-openings-mesh hosted-wall)]
    (is (= 2 (count (:openings hosted-wall))))
    (is (= [10 20] (:connected-to d)))
    (is (= [10 21] (:connected-to w)))
    (is (= :single-swing-left (get-in d [:psets "Pset_DoorCommon" :props :OperationType :value])))
    (is (= :fixed (get-in w [:psets "Pset_WindowCommon" :props :OperationType :value])))
    (is (= 48 (count (:positions mesh))))
    (is (= 216 (count (:indices mesh))))
    (is (= 1 (count (:openings (bim/remove-opening-from-wall hosted-wall 20)))))))

(deftest opening-integrity-guards
  (let [wall (bim/wall {:id 10 :start [0 0 0] :end [5 0 0] :height 3})
        opening (bim/rectangular-opening {:id 20 :offset 1 :sill 1 :width 1 :height 1})]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (bim/add-opening-to-wall wall (bim/rectangular-opening {:id 21 :offset 4.5 :width 1 :height 2}))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (-> wall
                     (bim/add-opening-to-wall opening)
                     (bim/add-opening-to-wall (bim/rectangular-opening {:id 22 :offset 1.5 :sill 1.5 :width 1 :height 1})))))))

(deftest floor-slab-quantities-and-mesh
  (let [slab (bim/slab {:id 40 :name "Ground Slab" :boundary [[0 0 0] [8 0 0] [8 6 0] [0 6 0]]
                        :thickness 0.25})
        mesh (bim/slab-mesh slab)]
    (is (== 48.0 (get-in slab [:quantities :gross-area-m2])))
    (is (== 12.0 (get-in slab [:quantities :gross-volume-m3])))
    (is (= 8 (count (:positions mesh))))
    (is (= 36 (count (:indices mesh))))
    (is (= mesh (bim/element-mesh slab)))))

(deftest tessellated-ifc-mesh-projection
  (let [element (bim/element
                 {:id 60 :kind :other :name "Tessellated"
                  :geometry {:kind :triangulated-face-set :closed true
                             :coordinates [[0 0 0] [2 0 0] [0 2 0] [0 0 2]]
                             :coord-indices [[1 2 3] [1 4 2] [2 4 3] [3 4 1]]}})
        mesh (bim/element-mesh element)]
    (is (= 12 (count (:positions mesh))))
    (is (= 12 (count (:indices mesh))))
    (is (= [0.0 0.0 1.0] (first (:normals mesh))))))

(deftest polygonal-face-inner-bound-projection
  (let [element (bim/element
                 {:id 64 :kind :other :name "Face with opening"
                  :geometry {:kind :polygonal-face-set :closed false
                             :coordinates [[0 0 0] [6 0 0] [6 6 0] [0 6 0]
                                           [2 2 0] [2 4 0] [4 4 0] [4 2 0]]
                             :faces [{:outer [1 2 3 4] :inners [[5 6 7 8]]}]}})
        mesh (bim/element-mesh element)
        area (reduce
              + (map (fn [[ia ib ic]]
                       (let [[[ax ay] [bx by] [cx cy]]
                             (map (fn [i] (take 2 (nth (:positions mesh) i))) [ia ib ic])]
                         (/ (#?(:clj Math/abs :cljs js/Math.abs)
                             (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))) 2.0)))
                     (partition 3 (:indices mesh))))]
    (is (= 24 (count (:indices mesh))))
    (is (= 32.0 area))))

(deftest planar-advanced-brep-projection
  (let [element (bim/element
                 {:id 65 :kind :other :name "Advanced planar face"
                  :geometry {:kind :advanced-brep
                             :faces [{:same-sense true
                                      :surface {:kind :plane
                                                :position {:location [0 0 0]
                                                           :axis [0 0 1]}}
                                      :bounds [{:kind :outer :orientation true
                                                :points [[0 0 0] [4 0 0] [4 3 0] [0 3 0]]}
                                               {:kind :inner :orientation true
                                                :points [[1 1 0] [1 2 0] [3 2 0] [3 1 0]]}]}
                                     {:same-sense true
                                      :surface {:kind :cylinder :radius 2.0
                                                :position {:location [0 0 0] :axis [0 0 1]}}
                                      :bounds []}]}})
        mesh (bim/element-mesh element)]
    (is (= 24 (count (:indices mesh))))
    (is (= 10.0
           (reduce +
                   (map (fn [[ia ib ic]]
                          (let [[[ax ay] [bx by] [cx cy]]
                                (map (fn [i] (take 2 (nth (:positions mesh) i))) [ia ib ic])]
                            (/ (#?(:clj Math/abs :cljs js/Math.abs)
                                (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))) 2.0)))
                        (partition 3 (:indices mesh))))))))

(deftest cylindrical-advanced-face-projection
  (let [ring (fn [z]
               (mapv (fn [i]
                       (let [theta (* 2.0 #?(:clj Math/PI :cljs js/Math.PI) (/ i 24.0))]
                         [(* 2.0 (#?(:clj Math/cos :cljs js/Math.cos) theta))
                          (* 2.0 (#?(:clj Math/sin :cljs js/Math.sin) theta)) z]))
                     (range 24)))
        start [2.0 0.0 0.0]
        element (bim/element
                 {:id 66 :kind :other :name "Advanced cylinder"
                  :geometry {:kind :advanced-brep
                             :faces [{:same-sense true
                                      :surface {:kind :cylinder :radius 2.0
                                                :position {:location [0 0 0]
                                                           :axis [0 0 1]
                                                           :ref-direction [1 0 0]}}
                                      :bounds [{:kind :outer :orientation true
                                                :points (vec (concat (ring 0.0) (ring 3.0)))
                                                :edges [{:kind :edge-curve :start start :end start
                                                         :curve {:kind :circle :radius 2.0}}]}]}]}})
        mesh (bim/element-mesh element)]
    (is (= 48 (count (:positions mesh))))
    (is (= 144 (count (:indices mesh))))
    (is (= 0.0 (reduce min (map #(nth % 2) (:positions mesh)))))
    (is (= 3.0 (reduce max (map #(nth % 2) (:positions mesh)))))
    (is (every? #(< (#?(:clj Math/abs :cljs js/Math.abs)
                       (- 2.0 (#?(:clj Math/sqrt :cljs js/Math.sqrt)
                               (+ (* (first %) (first %)) (* (second %) (second %))))))
                    1.0e-9)
                (:positions mesh)))))

(deftest circular-and-swept-disk-mesh-projection
  (let [column (bim/element {:id 61 :kind :column :name "Round"
                             :geometry {:kind :extruded-area-solid
                                        :profile {:kind :circle :radius 0.25}
                                        :depth 3.0}})
        pipe (bim/element {:id 62 :kind :mep-segment :name "Pipe"
                           :geometry {:kind :swept-disk-solid :radius 0.1
                                      :directrix [[0 0 0] [4 0 0] [4 3 0]]}})
        column-mesh (bim/element-mesh column)
        pipe-mesh (bim/element-mesh pipe)]
    (is (= 48 (count (:positions column-mesh))))
    (is (= 276 (count (:indices column-mesh))))
    (is (= 36 (count (:positions pipe-mesh))))
    (is (= 144 (count (:indices pipe-mesh))))
    (is (every? #(< (#?(:clj Math/abs :cljs js/Math.abs)
                       (- 1.0 (reduce + (map (fn [v] (* v v)) %)))) 1.0e-9)
                (:normals pipe-mesh)))))

(deftest advanced-swept-area-mesh-projection
  (let [profile {:kind :rectangle :x-dim 0.4 :y-dim 0.2}
        revolved (bim/element-mesh
                  {:geometry {:kind :revolved-area-solid :profile profile
                              :position {:location [0 0 0]}
                              :axis {:location [2 0 0] :axis [0 1 0]}
                              :angle #?(:clj Math/PI :cljs js/Math.PI)}})
        fixed (bim/element-mesh
               {:geometry {:kind :fixed-reference-swept-area-solid :profile profile
                           :directrix {:kind :indexed-polycurve
                                       :points [[0 0 0] [2 0 0] [3 1 0] [4 0 0]]
                                       :segments [{:kind :line :indices [1 2]}
                                                  {:kind :arc :indices [2 3 4]}]}
                           :fixed-reference [0 0 1]}})
        surface (bim/element-mesh
                 {:geometry {:kind :surface-curve-swept-area-solid :profile profile
                             :directrix {:kind :composite-curve
                                         :segments
                                         [{:same-sense true
                                           :parent-curve {:kind :polyline
                                                          :points [[0 0 0] [2 0 0]]}}
                                          {:same-sense true
                                           :parent-curve {:kind :polyline
                                                          :points [[2 0 0] [3 1 0]]}}]}
                             :reference-surface {:kind :plane
                                                 :position {:axis [0 0 1]}}}})]
    (is (= 52 (count (:positions revolved))))
    (is (= 300 (count (:indices revolved))))
    (is (> (count (:positions fixed)) 8))
    (is (> (count (:indices fixed)) 24))
    (is (= 12 (count (:positions surface))))
    (is (= 60 (count (:indices surface))))
    (is (every? number? (mapcat identity (:positions fixed))))))

(deftest rotated-extrusion-placement-projection
  (let [element (bim/element
                 {:id 63 :kind :other :name "Rotated extrusion"
                  :geometry {:kind :extruded-area-solid
                             :profile {:kind :rectangle :x-dim 2.0 :y-dim 1.0}
                             :position {:location [10.0 20.0 30.0]
                                        :axis [1.0 0.0 0.0]
                                        :ref-direction [0.0 1.0 0.0]}
                             :direction [0.0 0.0 1.0] :depth 4.0}})
        positions (:positions (bim/element-mesh element))]
    (is (= 10.0 (reduce min (map first positions))))
    (is (= 14.0 (reduce max (map first positions))))
    (is (= 19.0 (reduce min (map second positions))))
    (is (= 21.0 (reduce max (map second positions))))
    (is (= 29.5 (reduce min (map #(nth % 2) positions))))
    (is (= 30.5 (reduce max (map #(nth % 2) positions))))))

(deftest storey-lifecycle-integrity
  (let [ground (bim/storey {:id 3 :name "Ground" :elevation 0 :height 3.2 :placement :identity :spaces [] :elements []})
        project (-> (bim/project "Tower")
                    (update :sites conj (bim/site {:id 1 :name "Site" :placement :identity :buildings
                                                   [(bim/building {:id 2 :name "Tower" :placement :identity
                                                                   :reference-elevation 0 :storeys [ground]})]})))
        level-1 (bim/storey {:id 4 :name "Level 01" :elevation 3.2 :height 3.2 :placement :identity :spaces [] :elements []})
        added (bim/add-storey project 2 level-1)]
    (is (= 2 (count (:storeys (bim/find-building added 2)))))
    (is (= 1 (count (:storeys (bim/find-building (bim/delete-storey added 2 4) 2)))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (bim/add-storey added 2 level-1)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (bim/delete-storey (bim/add-element added 4 (bim/slab {:id 40 :boundary [[0 0 3.2] [1 0 3.2] [1 1 3.2] [0 1 3.2]]})) 2 4)))))

(defn integrated-project []
  (let [ground (bim/storey {:id 3 :name "Ground" :elevation 0 :height 3.2
                            :placement :identity :spaces [] :elements []})]
    (-> (bim/project "Integrated Tower")
        (assoc :id "tower-01")
        (update :sites conj
                (bim/site {:id 1 :name "Site" :placement :identity
                           :buildings [(bim/building {:id 2 :name "Tower" :placement :identity
                                                      :reference-elevation 0 :storeys [ground]})]}))
        (bim/add-element 3 (bim/wall {:id 10 :start [0 0 0] :end [8 0 0]})))))

(deftest parametric-family-instantiation
  (let [family (integration/family-definition
                {:id "door-single" :name "Single Door" :category :door
                 :parameters {:width {:type :length :default 0.9}
                              :height {:type :length :default 2.1}}
                 :formulas {:area [:* [:param :width] [:param :height]]}
                 :template {:kind :door :name "Single Door"
                            :geometry {:width [:param :width] :height [:param :height]}
                            :quantities {:gross-area-m2 [:param :area]}}})
        door (integration/instantiate-family family 20 {:width 1.2})]
    (is (= 1.2 (get-in door [:geometry :width])))
    (is (= 2.1 (get-in door [:geometry :height])))
    (is (== 2.52 (get-in door [:quantities :gross-area-m2])))
    (is (= "door-single" (:family/id door)))))

(deftest family-types-constraints-and-nested-instances
  (let [handle (integration/family-definition
                {:id "handle" :name "Handle" :category :furniture
                 :parameters {:length {:type :length :scope :instance :default 0.2 :min 0.1}}
                 :template {:kind :furniture :name "Handle"
                            :geometry {:kind :handle :length [:param :length]}}})
        cabinet (integration/family-definition
                 {:id "cabinet" :name "Cabinet" :category :furniture
                  :parameters {:width {:type :length :scope :type :default 0.8 :min 0.4}
                               :height {:type :length :scope :instance :default 2.0 :max 3.0}}
                  :formulas {:double-area [:* [:param :area] 2.0]
                             :area [:* [:param :width] [:param :height]]}
                  :constraints [{:kind :equal :left [:param :double-area]
                                 :right [:* [:param :width] [:param :height] 2.0]}]
                  :types {:wide {:id "cabinet-wide" :global-id "cabinet-wide-guid"
                                 :name "Wide" :parameters {:width 1.2}
                                 :predefined-type :notdefined}}
                  :template {:kind :furniture :name "Cabinet"
                             :geometry {:kind :cabinet :width [:param :width]
                                        :height [:param :height]}
                             :components [{:family/ref "handle" :id "handle-1"
                                           :overrides {:length [:param :width]}}]}})
        catalog (integration/family-catalog [handle cabinet])
        instance (integration/instantiate-family-type catalog "cabinet" :wide 70 {:height 2.4})]
    (is (= 1.2 (get-in instance [:geometry :width])))
    (is (= 2.4 (get-in instance [:geometry :height])))
    (is (== 5.76 (get-in instance [:family/parameters :double-area])))
    (is (= "cabinet-wide" (get-in instance [:type-object :id])))
    (is (= 1.2 (get-in instance [:components 0 :geometry :length])))
    (is (= "handle" (get-in instance [:components 0 :family/id])))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"instance cannot override type parameter"
                          (integration/instantiate-family-type catalog "cabinet" :wide 71
                                                               {:width 1.5})))))

(deftest family-conditional-scientific-and-visibility-formulas
  (let [family (integration/family-definition
                {:id "parametric-panel" :name "Parametric Panel" :category :furniture
                 :parameters {:width {:type :length :default 1.2 :min 0.1}
                              :height {:type :length :default 0.9 :min 0.1}
                              :enabled {:type :boolean :default true}}
                 :formulas
                 {:area [:* [:param :width] [:param :height]]
                  :diagonal [:sqrt [:+ [:pow [:param :width] 2]
                                      [:pow [:param :height] 2]]]
                  :panel-count [:ceil [:/ [:param :width] 0.6]]
                  :large? [:>= [:param :area] 1.0]
                  :visible? [:and [:param :enabled] [:param :large?]]
                  :safe-width [:if [:> [:param :width] 0]
                               [:param :width] [:/ 1 0]]}
                 :template {:kind :furniture :name "Parametric Panel"
                            :visible [:param :visible?]
                            :geometry {:kind :panel :width [:param :safe-width]
                                       :height [:param :height]
                                       :panel-count [:param :panel-count]}}})
        instance (integration/instantiate-family family 72 {})]
    (is (= 1.08 (get-in instance [:family/parameters :area])))
    (is (= 1.5 (get-in instance [:family/parameters :diagonal])))
    (is (= 2.0 (get-in instance [:family/parameters :panel-count])))
    (is (true? (:visible instance)))
    (is (= 1.2 (get-in instance [:geometry :width])))
    (is (= 2.0 (get-in instance [:geometry :panel-count])))))

(deftest shared-parameters-and-revit-type-catalog-round-trip
  (let [family (integration/family-definition
                {:id "catalog-door" :name "Catalog Door" :category :door
                 :shared-parameters
                 {:fire-rating {:guid "12345678-1234-4abc-8def-1234567890ab"
                                :name "Fire Rating" :type :text :scope :type
                                :default "30 min"}}
                 :parameters
                 {:width {:type :length :scope :type :default 0.9
                          :catalog-unit "MILLIMETERS"}
                  :enabled {:type :boolean :scope :type :default true}}
                 :template {:kind :door :name "Catalog Door"
                            :geometry {:width [:param :width]}
                            :fire-rating [:param :fire-rating]}})
        csv (str ",Width##LENGTH##MILLIMETERS,Fire Rating##TEXT##,Enabled##YESNO##\n"
                 "\"Wide, Exterior\",1200,60 min,1\n"
                 "Narrow,800,30 min,0\n")
        imported (integration/import-family-type-catalog family csv)
        exported (integration/export-family-type-catalog imported)
        reimported (integration/import-family-type-catalog family exported)
        catalog (integration/family-catalog [imported])
        instance (integration/instantiate-family-type
                  catalog "catalog-door" :wide-exterior 73 {})]
    (is (= 1.2 (get-in imported [:family/types :wide-exterior :parameters :width])))
    (is (= "60 min"
           (get-in imported [:family/types :wide-exterior :parameters :fire-rating])))
    (is (false? (get-in imported [:family/types :narrow :parameters :enabled])))
    (is (string/includes? exported "width##LENGTH##MILLIMETERS"))
    (is (= (:family/types imported) (:family/types reimported)))
    (is (= 1.2 (get-in instance [:geometry :width])))
    (is (= "60 min" (:fire-rating instance)))
    (is (= "12345678-1234-4abc-8def-1234567890ab"
           (get-in imported [:family/shared-parameters :fire-rating :guid])))))

(deftest parameter-driven-linear-and-radial-family-arrays
  (let [family (integration/family-definition
                {:id "array-family" :name "Array Family" :category :furniture
                 :parameters {:count {:type :integer :default 3 :min 1 :max 20}
                              :spacing {:type :length :default 0.6}}
                 :template
                 {:kind :furniture :name "Array Family"
                  :linear-components
                  {:family/array {:kind :linear :count [:param :count]
                                  :spacing [:param :spacing] :direction [1 0 0]
                                  :item {:id "post" :kind :furniture
                                         :placement {:location [0 0 0]}}}}
                  :radial-components
                  {:family/array {:kind :radial :count 4
                                  :angle [:/ #?(:clj Math/PI :cljs js/Math.PI) 2]
                                  :center [0 0 0]
                                  :item {:id "chair" :kind :furniture
                                         :placement {:location [2 0 0]
                                                     :ref-direction [1 0 0]}}}}}})
        instance (integration/instantiate-family family 74 {:count 4 :spacing 0.75})
        linear (:linear-components instance)
        radial (:radial-components instance)]
    (is (= 4 (count linear)))
    (is (= [[0.0 0.0 0.0] [0.75 0.0 0.0] [1.5 0.0 0.0] [2.25 0.0 0.0]]
           (mapv #(get-in % [:placement :location]) linear)))
    (is (= ["post-0" "post-1" "post-2" "post-3"] (mapv :id linear)))
    (is (= [[2.0 0.0 0.0] [0.0 2.0 0.0] [-2.0 0.0 0.0] [0.0 -2.0 0.0]]
           (mapv (fn [item]
                   (mapv #(let [rounded (#?(:clj Math/round :cljs js/Math.round) (* % 1.0e9))]
                            (/ rounded 1.0e9))
                         (get-in item [:placement :location]))) radial)))
    (is (= [0 1 2 3] (mapv :family/array-index radial)))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"below minimum"
                          (integration/instantiate-family family 75 {:count 0})))))

(deftest reference-plane-constraints-drive-family-geometry
  (let [family (integration/family-definition
                {:id "window-double" :name "Double Window" :category :window
                 :parameters {:width {:type :length :default 1.2 :min 0.4}
                              :height {:type :length :default 1.5 :min 0.4}
                              :mullion-offset {:type :length :default 0.0}}
                 :reference-planes
                 {:left {:axis :x :offset 0.0 :locked true}
                  :right {:axis :x}
                  :mullion {:axis :x :offset [:/ [:+ [:reference :left]
                                                     [:reference :right]] 2.0]}
                  :sill {:axis :z :offset [:param :mullion-offset]}
                  :head {:axis :z}
                  :origin-copy {:axis :x}}
                 :constraints
                 [{:kind :distance :from :left :to :right :value [:param :width]}
                  {:kind :distance :from :left :to :mullion
                   :value [:/ [:param :width] 2.0]}
                  {:kind :distance :from :sill :to :head :value [:param :height]}
                  {:kind :coincident :left :left :right :origin-copy}]
                 :template
                 {:kind :window :name "Double Window"
                  :geometry {:kind :extruded-area-solid
                             :profile {:kind :rectangle
                                       :x-dim [:- [:reference :right] [:reference :left]]
                                       :y-dim 0.12}
                             :position {:location [[:reference :left] 0.0 [:reference :sill]]}
                             :direction [0.0 0.0 1.0]
                             :depth [:- [:reference :head] [:reference :sill]]}
                  :mullion {:x [:reference :mullion]}}})
        instance (integration/instantiate-family family 80 {:width 1.8 :height 2.0
                                                            :mullion-offset 0.3})]
    (is (= 1.8 (get-in instance [:geometry :profile :x-dim])))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 2.0 (get-in instance [:geometry :depth])))
           1.0e-9))
    (is (= [0.0 0.0 0.3] (get-in instance [:geometry :position :location])))
    (is (= 0.9 (get-in instance [:mullion :x])))
    (is (= :constraint
           (get-in instance [:family/reference-planes :right :solved-by])))
    (is (= 0.0
           (get-in instance [:family/reference-planes :origin-copy :offset])))
    (is (= 2.3 (get-in instance [:family/reference-planes :head :offset])))))

(deftest hosted-family-sketch-and-void-cut
  (let [family (integration/family-definition
                {:id "hosted-window" :name "Hosted Window" :category :window
                 :host {:required? true :kinds #{:wall} :faces #{:interior :exterior}}
                 :parameters {:width {:type :length :default 1.2}
                              :height {:type :length :default 1.5}
                              :depth {:type :length :default 0.3}}
                 :reference-planes {:left {:axis :x :offset 0.0}
                                    :right {:axis :x :offset [:param :width]}
                                    :sill {:axis :y :offset 0.0}
                                    :head {:axis :y :offset [:param :height]}}
                 :sketches
                 {:opening {:name "Window Opening"
                            :points {:bottom-left [[:reference :left] [:reference :sill]]
                                     :origin-copy [[:reference :left] [:reference :sill]]
                                     :bottom-right [[:reference :right] [:reference :sill]]
                                     :top-right [[:reference :right] [:reference :head]]
                                     :top-left [[:reference :left] [:reference :head]]}
                            :loop [:bottom-left :bottom-right :top-right :top-left]
                            :constraints [{:kind :horizontal :from :bottom-left :to :bottom-right}
                                          {:kind :vertical :from :bottom-right :to :top-right}
                                          {:kind :coincident :from :bottom-left :to :origin-copy}
                                          {:kind :distance :from :bottom-left :to :bottom-right
                                           :value [:param :width]}
                                          {:kind :equal-length
                                           :first [:bottom-left :bottom-right]
                                           :second [:top-left :top-right]}
                                          {:kind :parallel
                                           :first [:bottom-left :bottom-right]
                                           :second [:top-left :top-right]}
                                          {:kind :perpendicular
                                           :first [:bottom-left :bottom-right]
                                           :second [:bottom-right :top-right]}]}}
                 :template
                 {:kind :window :name "Hosted Window"
                  :geometry {:kind :extruded-area-solid
                             :profile [:sketch-profile :opening]
                             :direction [0 0 1] :depth [:param :depth]}
                  :voids [{:id "opening-cut"
                           :geometry {:kind :extruded-area-solid
                                      :profile [:sketch-profile :opening]
                                      :direction [0 0 1] :depth [:param :depth]}}]}})
        host (bim/wall {:id 100 :name "Host Wall" :start [0 0 0] :end [5 0 0]
                        :thickness 0.3 :height 3.0})
        instance (integration/place-hosted-family
                  family 101 {:width 1.8 :height 1.4} host
                  {:face :exterior :placement {:location [2.0 0.0 0.8]}})
        cut-host (integration/apply-family-voids host instance)]
    (is (= [[0.0 0.0] [1.8 0.0] [1.8 1.4] [0.0 1.4] [0.0 0.0]]
           (get-in instance [:geometry :profile :points])))
    (is (= 100 (:host/id instance)))
    (is (= :exterior (:host/face instance)))
    (is (= 1 (count (:openings cut-host))))
    (is (= {:offset 2.0 :sill 0.8}
           (get-in cut-host [:openings 0 :placement])))
    (is (= {:width 1.8 :height 1.4}
           (select-keys (get-in cut-host [:openings 0 :profile]) [:width :height])))
    (is (= 101 (get-in cut-host [:openings 0 :filled-by])))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"host kind is not allowed"
                          (integration/place-hosted-family
                           family 102 {} (bim/element {:id 200 :kind :slab})
                           {:face :exterior})))))

(deftest adaptive-family-points-drive-freeform-path
  (let [family (integration/family-definition
                {:id "adaptive-rail" :name "Adaptive Rail" :category :railing
                 :adaptive {:min-points 2 :max-points 8 :closed? false}
                 :parameters {:radius {:type :length :default 0.04 :min 0.01}}
                 :template {:kind :proxy :name "Adaptive Rail"
                            :placement {:location [:adaptive-point 0]}
                            :geometry {:kind :swept-disk-solid
                                       :directrix [:adaptive-path]
                                       :radius [:param :radius]}}})
        points [[0 0 0] [2 0 1] [4 2 1]]
        instance (integration/instantiate-adaptive-family family 300 {:radius 0.05} points)
        mesh (bim/element-mesh instance)]
    (is (= points (get-in instance [:geometry :directrix])))
    (is (= [0 0 0] (get-in instance [:placement :location])))
    (is (= 0.05 (get-in instance [:geometry :radius])))
    (is (seq (:positions mesh)))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"coincident consecutive"
                          (integration/instantiate-adaptive-family
                           family 301 {} [[0 0 0] [0 0 0]])))))

(deftest rejects-family-formula-and-nesting-cycles
  (let [formula-cycle (integration/family-definition
                       {:id "cycle" :parameters {:a {:type :number :default 1}
                                                  :b {:type :number :default 2}}
                        :formulas {:a [:+ [:param :b] 1] :b [:+ [:param :a] 1]}
                        :template {}})
        nested-cycle (integration/family-definition
                      {:id "nested" :parameters {}
                       :template {:components [{:family/ref "nested" :id "self"}]}})
        catalog (integration/family-catalog [nested-cycle])]
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"dependency cycle"
                          (integration/resolve-family-parameters formula-cycle {})))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"formula-driven parameter cannot be overridden"
                          (integration/resolve-family-parameters formula-cycle {:a 10})))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"nested family cycle"
                          (integration/instantiate-family-type catalog "nested" nil 1 {})))))

(deftest drawings-ifc-collaboration-and-itonami-contract
  (let [project (integrated-project)
        drawings (integration/generate-drawing-set project)
        ifc (integration/export-ifc project)
        event (integration/change-event {:id "e1" :actor "architect-a" :clock 1
                                         :operation :assoc :path [:description]
                                         :value "Issued for coordination"})
        changed (integration/merge-events project [event])
        payload (integration/cloud-itonami-payload changed 7)]
    (is (= :floor-plan (get-in drawings [:drawing/views 0 :view/kind])))
    (is (= #{:floor-plan :section :elevation}
           (set (map :view/kind (:drawing/views drawings)))))
    (is (= 1 (get-in drawings [:drawing/schedules 0 :schedule/total-count])))
    (is (= "schedule-elements"
           (last (get-in drawings [:drawing/sheets 0 :sheet/views]))))
    (is (= "P01" (get-in drawings [:drawing/sheets 0 :sheet/revisions 0 :revision])))
    (is (= project (integration/import-ifc ifc)))
    (is (= :wall (get-in ifc [:ifc/elements 0 :kind])))
    (is (= "Issued for coordination" (:description changed)))
    (is (= :design/revision-published (:itonami/event payload)))
    (is (= "Ss_25_10" (get-in payload [:design/line-items 0 :classification/code])))
    (is (= 7 (:design/revision payload)))))

(deftest collaboration-review-issue-and-cloud-itonami-delta
  (let [project (integrated-project)
        initial (integration/collaboration-workspace project)
        checkpoint (integration/collaboration-checkpoint initial "cloud-itonami")
        branched (integration/create-design-branch initial :architecture "root")
        event-main (integration/change-event {:id "main-name" :actor "lead" :clock 1
                                               :operation :assoc :path [:description]
                                               :value "Coordinated"})
        event-arch (integration/change-event {:id "arch-name" :actor "architect" :clock 2
                                               :operation :assoc :path [:name]
                                               :value "Integrated Tower A"})
        main (integration/commit-design-revision
              branched {:id "main-1" :branch :main :base-revision "root"
                        :actor "lead" :clock 1 :message "Coordinate" :events [event-main]})
        architecture (integration/commit-design-revision
                      main {:id "arch-1" :branch :architecture :base-revision "root"
                            :actor "architect" :clock 2 :message "Option A" :events [event-arch]})
        approved (integration/review-design-revision
                  architecture {:revision "arch-1" :reviewer "bim-manager"
                                :decision :approved :clock 3})
        merged (integration/merge-design-branches
                approved {:id "merge-1" :target :main :source :architecture
                          :actor "bim-manager" :clock 4 :required-approvals 1})
        workspace (-> (:merge/workspace merged)
                      (integration/create-coordination-issue
                       {:id "issue-1" :title "Confirm wall type" :author "bim-manager"
                        :assignee "architect" :element-ids [10] :clock 5})
                      (integration/transition-coordination-issue
                       "issue-1" :in-review "architect" 6 "Updated"))
        payload (integration/cloud-itonami-sync-payload workspace checkpoint)]
    (is (= :merged (:merge/status merged)))
    (is (= :design/collaboration-synchronized (:itonami/event payload)))
    (is (= "merge-1" (:design/head-revision payload)))
    (is (= "Integrated Tower A" (:design/project-name payload)))
    (is (= :in-review (get-in payload [:coordination/issues "issue-1" :issue/status])))
    (is (= :approved (get-in payload [:coordination/reviews "arch-1" "bim-manager"
                                      :review/decision])))
    (is (seq (:design/line-items payload)))))

(deftest structural-and-mep-federation
  (let [project (integrated-project)
        beam (integration/structural-member
              (bim/element {:id 30 :kind :beam :name "B1"})
              {:role :beam :analytical-axis [[0 0 3] [8 0 3]]
               :section {:kind :i-shape :designation "H-300x150"}
               :material "S355" :loads [{:case :dead :kn-m 12.0}]})
        system (integration/mep-system
                {:id "supply-air" :name "Supply Air" :kind :hvac :medium :air
                 :design-flow 2.4
                 :segments [{:id 50 :connected-to [51]}]})
        federation (integration/federated-design
                    {:architectural project :structural [beam] :mep [system]})]
    (is (= :structural (:discipline beam)))
    (is (= :hvac (get-in federation [:federation/mep 0 :mep/kind])))
    (is (= :mep/dangling-connection
           (get-in federation [:federation/issues 0 :issue/type])))))

(deftest solves-linear-structural-truss-analysis
  (let [model (integration/structural-model
               {:nodes [(integration/structural-node
                         {:id :n1 :point [0.0 0.0] :restraints [true true]})
                        (integration/structural-node
                         {:id :n2 :point [2.0 0.0] :restraints [false true]})]
                :members [(integration/structural-analysis-member
                           {:id :m1 :start-node :n1 :end-node :n2 :area-m2 0.01
                            :elastic-modulus-pa 2.0e11 :yield-strength-pa 2.5e8
                            :material "S355"})]
                :load-cases [(integration/structural-load-case
                              {:id :service :name "Service" :kind :service
                               :nodal-loads [{:node :n2 :fx 1000.0 :fy 0.0}]})
                             (integration/structural-load-case
                              {:id :live :name "Live" :kind :variable
                               :nodal-loads [{:node :n2 :fx 500.0 :fy 0.0}]})]
                :combinations [(integration/structural-load-combination
                                {:id :uls :name "ULS" :factors {:service 1.2 :live 1.5}})]})
        result (integration/analyze-2d-truss model :service)
        combination (integration/analyze-structural-combination model :uls)]
    (is (empty? (integration/validate-structural-model model)))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 1.0e-6 (get-in result [:structural.analysis/displacements :n2 0]))) 1.0e-12))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 1000.0 (get-in result [:structural.analysis/member-axial-forces :m1]))) 1.0e-6))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 1950.0 (get-in combination [:structural.analysis/member-axial-forces :m1])))
           1.0e-6))
    (is (true? (get-in combination [:structural.analysis/member-checks :m1 :passes?])))
    (is (< (get-in combination [:structural.analysis/member-checks :m1 :utilization]) 0.001))))

(deftest solves-3d-truss-distributed-load-self-weight-and-reactions
  (let [model (integration/structural-model
               {:nodes [(integration/structural-node
                         {:id :a :point [0.0 0.0 0.0] :restraints [true true true]})
                        (integration/structural-node
                         {:id :b :point [2.0 0.0 0.0] :restraints [false true true]})]
                :members [(integration/structural-analysis-member
                           {:id :bar :start-node :a :end-node :b :area-m2 0.01
                            :elastic-modulus-pa 2.0e11 :yield-strength-pa 2.5e8
                            :resistance-factor 0.9 :density-kg-m3 7850.0})]
                :load-cases [(integration/structural-load-case
                              {:id :dead :name "Dead" :kind :permanent
                               :nodal-loads [{:node :b :fx 1000.0 :fy 0.0 :fz 0.0}]
                               :member-loads [{:member :bar :wx 100.0 :wy 0.0 :wz 0.0}]
                               :gravity [0.0 -9.80665 0.0]})]
                :combinations [(integration/structural-load-combination
                                {:id :uls-3d :name "ULS 3D" :factors {:dead 1.5}})]})
        result (integration/analyze-3d-truss model :dead)
        combination (integration/analyze-structural-combination model :uls-3d)]
    (is (= 3 (:structural.analysis/dimension result)))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 1.1e-6 (get-in result [:structural.analysis/displacements :b 0])))
           1.0e-12))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 1100.0 (get-in result [:structural.analysis/member-axial-forces :bar])))
           1.0e-6))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (+ 1200.0 (get-in result [:structural.analysis/reactions :a 0])))
           1.0e-6))
    (is (> (get-in result [:structural.analysis/reactions :a 1]) 700.0))
    (is (= 1650.0 (get-in combination [:structural.analysis/member-checks :bar :force-n])))
    (is (true? (get-in combination [:structural.analysis/member-checks :bar :passes?])))))

(deftest routes-and-validates-mep-systems
  (let [route (integration/route-mep [0.0 0.0 0.0] [3.0 0.0 0.0]
                                     [{:min [1.0 -0.5 -0.5] :max [2.0 0.5 0.5]}
                                      {:min [2.9 -0.1 -0.1] :max [3.1 0.1 0.1]}]
                                     {:step 0.5 :clearance 0.0})
        connector-a (integration/mep-connector
                     {:id :a :point [1 0 0] :direction [1 0 0] :domain :piping
                      :shape :round :size 0.1 :flow-direction :out :connected-to :b})
        connector-b (integration/mep-connector
                     {:id :b :point [1 0 0] :direction [-1 0 0] :domain :piping
                      :shape :round :size 0.1 :flow-direction :in :connected-to :a})
        segment-a (integration/mep-segment
                   {:id 201 :kind :pipe :start [0 0 0] :end [1 0 0] :diameter 0.1
                    :system-id :heating :connectors [connector-a]})
        segment-b (integration/mep-segment
                   {:id 202 :kind :pipe :start [1 0 0] :end [2 0 0] :diameter 0.1
                    :system-id :heating :connectors [connector-b]})
        system (integration/mep-system {:id :heating :name "Heating" :kind :hydronic
                                        :medium :water :design-flow 0.005
                                        :segments [segment-a segment-b]})
        loss (integration/pressure-loss {:length-m 20.0 :diameter-m 0.1
                                         :roughness-m 1.5e-6 :flow-m3-s 0.005
                                         :density-kg-m3 998.0 :viscosity-pa-s 0.001})
        sizing (integration/size-round-mep-segment 0.005 2.0 [0.04 0.05 0.063 0.075])
        analysis (integration/analyze-mep-system
                  system {:roughness-m 1.5e-6 :density-kg-m3 998.0
                          :viscosity-pa-s 0.001})]
    (is (> (count route) 2))
    (is (some #(not= 0.0 (second %)) route))
    (is (empty? (integration/validate-mep-system system)))
    (is (= :swept-disk-solid (get-in segment-a [:geometry :kind])))
    (is (pos? (:mep/pressure-loss-pa loss)))
    (is (> (:mep/reynolds loss) 2300.0))
    (is (= 0.063 (:mep/diameter-m sizing)))
    (is (<= (:mep/velocity-m-s sizing) 2.0))
    (is (= [:b] (get-in analysis [:mep.analysis/connector-graph :a])))
    (is (= 2 (count (:mep.analysis/segments analysis))))
    (is (pos? (:mep.analysis/total-pressure-loss-pa analysis)))))

(deftest ifc-system-and-connector-graph-round-trip
  (let [connector-a (integration/mep-connector
                     {:id "port-a" :point [1 0 0] :direction [1 0 0] :domain :piping
                      :shape :round :size 0.1 :flow-direction :out :connected-to "port-b"})
        connector-b (integration/mep-connector
                     {:id "port-b" :point [1 0 0] :direction [-1 0 0] :domain :piping
                      :shape :round :size 0.1 :flow-direction :in :connected-to "port-a"})
        segment-a (integration/mep-segment
                   {:id 201 :kind :pipe :start [0 0 0] :end [1 0 0] :diameter 0.1
                    :system-id :heating :connectors [connector-a]})
        segment-b (integration/mep-segment
                   {:id 202 :kind :pipe :start [1 0 0] :end [2 0 0] :diameter 0.1
                    :system-id :heating :connectors [connector-b]})
        project (-> (integrated-project)
                    (bim/add-element 3 segment-a)
                    (bim/add-element 3 segment-b)
                    (assoc :ifc/groups
                           [{:id "heating" :global-id "heating-system"
                             :kind :distribution-system :name "Heating"
                             :predefined-type :heating
                             :member-global-ids ["201" "202"]}]))
        text (ifc/write-standard-spf project)
        imported (integration/import-ifc-spf text)
        elements (get-in imported [:sites 0 :buildings 0 :storeys 0 :elements])
        by-id (into {} (map (juxt :global-id identity) elements))]
    (is (string/includes? text "IFCDISTRIBUTIONSYSTEM"))
    (is (string/includes? text "IFCDISTRIBUTIONPORT"))
    (is (string/includes? text "IFCRELCONNECTSPORTS"))
    (is (= "port-b" (get-in by-id ["201" :mep/connectors 0
                                    :connector/connected-to])))
    (is (= :out (get-in by-id ["201" :mep/connectors 0
                               :connector/flow-direction])))
    (is (= :piping (get-in by-id ["201" :mep/connectors 0 :connector/domain])))
    (is (= "Heating" (get-in imported [:ifc/groups 0 :name])))
    (is (= 1 (count (:ifc/connections imported))))))

(deftest ifc-presentation-style-round-trip
  (let [appearance {:name "Concrete Blue" :color-name "Blue"
                    :surface-color [0.15 0.3 0.75] :transparency 0.1
                    :side :both :reflectance-method :matt}
        layers [{:name "A-WALL" :description "Architectural walls"
                 :identifier "A-WALL"}]
        project (-> (integrated-project)
                    (update-in [:sites 0 :buildings 0 :storeys 0 :elements 0]
                               assoc :appearance appearance :presentation-layers layers))
        text (ifc/write-standard-spf project)
        imported (integration/import-ifc-spf text)
        wall (get-in imported [:sites 0 :buildings 0 :storeys 0 :elements 0])
        edited (assoc-in imported
                         [:sites 0 :buildings 0 :storeys 0 :elements 0
                          :appearance :surface-color]
                         [0.8 0.2 0.1])
        reimported (integration/import-ifc-spf (ifc/write-standard-spf edited))]
    (is (string/includes? text "IFCSURFACESTYLERENDERING"))
    (is (string/includes? text "IFCPRESENTATIONLAYERASSIGNMENT"))
    (is (= appearance (:appearance wall)))
    (is (= layers (:presentation-layers wall)))
    (is (= [0.8 0.2 0.1]
           (get-in reimported [:sites 0 :buildings 0 :storeys 0 :elements 0
                               :appearance :surface-color])))))

(deftest ifc-spf-and-svg-deliverables
  (let [opening (bim/rectangular-opening {:id 20 :offset 2.0 :width 0.9 :height 2.1
                                          :filled-by 30})
        project (-> (integrated-project)
                    (assoc :ifc/georeference
                           {:projected-crs {:name "EPSG:6677" :geodetic-datum "JGD2011"}
                            :world-origin [100.0 200.0 0.0] :true-north [0.0 1.0]
                            :eastings 500000.0 :northings 3950000.0
                            :orthogonal-height 42.5 :x-axis-abscissa 1.0
                            :x-axis-ordinate 0.0 :scale 1.0})
                    (assoc-in [:sites 0 :geo] (bim/geo-ref 35.6666667 139.75 42.5))
                    (bim/update-element 3 10 bim/add-opening-to-wall opening)
                    (bim/add-element 3 (bim/door {:id 30 :host-id 10 :opening-id 20})))
        spf (ifc/write-spf project)
        standard-spf (ifc/write-standard-spf project)
        standard-document (ifc/read-document standard-spf)
        imported-project (integration/import-external-ifc standard-document)
        svg (drawing/floor-plan-svg (bim/find-storey project 3))]
    (is (.startsWith spf "ISO-10303-21;"))
    (is (re-find #"IFCWALL" spf))
    (is (= project (ifc/read-spf spf)))
    (is (= :external-spf (:ifc/source standard-document)))
    (is (re-find #"IFCEXTRUDEDAREASOLID" standard-spf))
    (is (= :arbitrary-closed
           (get-in standard-document [:ifc/elements 0 :geometry :profile :kind])))
    (is (= "Site" (get-in standard-document [:ifc/project :children 0 :name])))
    (is (= "Tower" (get-in standard-document [:ifc/project :children 0 :children 0 :name])))
    (is (= "Ground"
           (get-in standard-document [:ifc/project :children 0 :children 0 :children 0 :name])))
    (is (true? (get-in standard-document [:ifc/elements 0 :property-sets
                                          "Pset_WallCommon" :properties "IsExternal" :value])))
    (is (= :opening (get-in standard-document [:ifc/elements 0 :openings 0 :kind])))
    (is (= "30" (get-in standard-document
                         [:ifc/elements 0 :openings 0 :filled-by-global-id])))
    (is (= "EPSG:6677" (get-in standard-document
                                [:ifc/georeference :projected-crs :name])))
    (is (= [100.0 200.0 0.0] (:world-origin imported-project)))
    (is (= 0.0 (:true-north-rad imported-project)))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 35.6666667 (get-in imported-project [:sites 0 :geo :latitude-deg])))
           1.0e-6))
    (is (= 139.75 (get-in imported-project [:sites 0 :geo :longitude-deg])))
    (is (= 42.5 (get-in imported-project [:sites 0 :geo :elevation-m])))
    (is (re-find #"<svg" svg))
    (is (re-find #"<line" svg))))

(deftest bim-external-ifc-provenance-preserves-space-and-vendor-entities-on-edit
  (let [project (update-in
                 (bim/add-element
                  (integrated-project) 3
                  (bim/element {:id 12 :kind :door :name "Temporary Door"
                                :placement :identity :geometry nil}))
                 [:sites 0 :buildings 0 :storeys 0 :spaces]
                 conj (bim/space {:id 40 :name "Office 101" :long-name "Open Office"
                                  :category :office :boundary [] :height 3.2
                                  :quantities {} :psets {}}))
        standard (ifc/write-standard-spf project)
        external (string/replace
                  standard "\nENDSEC;\nEND-ISO-10303-21;"
                  "\n#99999=IFCANNOTATION('vendor-extension',$,'Keep Vendor Data',$,$,$,$);\nENDSEC;\nEND-ISO-10303-21;")
        imported (integration/import-ifc-spf external)
        imported-storey (get-in imported [:sites 0 :buildings 0 :storeys 0])
        imported-wall (first (filter #(= "10" (:global-id %))
                                     (:elements imported-storey)))
        imported-door (first (filter #(= "12" (:global-id %))
                                     (:elements imported-storey)))
        edited (-> imported
                   (bim/update-element (:id imported-storey) (:id imported-wall)
                                       assoc :name "Edited in Kotoba" :kind :column)
                   (bim/delete-element (:id imported-storey) (:id imported-door))
                   (bim/add-element
                    (:id imported-storey)
                    (bim/element {:id 50 :global-id "added-beam-guid" :kind :beam
                                  :name "Added Beam" :placement {:location [1 2 3]}
                                  :geometry {:kind :extruded-area-solid
                                             :profile {:kind :rectangle
                                                       :x-dim 0.3 :y-dim 0.5}
                                             :direction [1 0 0] :depth 4.0}})))
        output (ifc/write-standard-spf edited)
        reimported (ifc/read-document output)
        by-global (into {} (map (juxt :global-id identity) (:ifc/elements reimported)))
        wall (get by-global "10")]
    (is (= "IFC4X3_ADD2" (:ifc/schema imported)))
    (is (= "Open Office"
           (get-in imported [:sites 0 :buildings 0 :storeys 0 :spaces 0 :long-name])))
    (is (= "SPACE_40"
           (get-in imported [:sites 0 :buildings 0 :storeys 0 :spaces 0 :global-id])))
    (is (map? (:ifc/source-document imported)))
    (is (string/includes? output "IFCANNOTATION"))
    (is (string/includes? output "'Keep Vendor Data'"))
    (is (= "Edited in Kotoba" (:name wall)))
    (is (= :column (:kind wall)))
    (is (nil? (get by-global "12")))
    (is (= :beam (get-in by-global ["added-beam-guid" :kind])))
    (is (= 4.0 (get-in by-global ["added-beam-guid" :geometry :depth])))))

(deftest imports-external-ifc-hierarchy-and-wall-geometry
  (let [document {:ifc/schema "IFC4X3_ADD2"
                  :ifc/project {:id 1 :global-id "project-guid" :name "Tower"
                                :type :ifcproject
                                :children [{:id 2 :name "Site" :type :ifcsite
                                            :children [{:id 3 :name "Building" :type :ifcbuilding
                                                        :children [{:id 4 :name "Ground" :type :ifcbuildingstorey
                                                                    :placement {:location [0 0 0]}
                                                                    :children []}]}]}]}
                  :ifc/elements [{:id 100 :global-id "wall-guid" :name "External Wall"
                                  :kind :wall :container-id 4
                                  :placement {:location [10 20 0]}
                                  :property-sets {"Pset_WallCommon"
                                                  {:name "Pset_WallCommon"
                                                   :properties {"IsExternal" {:value true :value-type :ifcboolean}
                                                                "FireRating" {:value "2 HR" :value-type :ifclabel}
                                                                "Status" {:kind :enumerated
                                                                          :values ["Existing"]
                                                                          :value-type :ifclabel
                                                                          :enumeration
                                                                          {:name "PEnum_ElementStatus"
                                                                           :values ["New" "Existing"]}}
                                                                "Temperature" {:kind :bounded
                                                                               :lower 18.0 :upper 26.0
                                                                               :set-point 22.0
                                                                               :value-type :ifcreal}
                                                                "Zones" {:kind :list
                                                                         :values ["North" "Perimeter"]
                                                                         :value-type :ifclabel}}}}
                                  :quantity-sets
                                  {"Qto_WallBaseQuantities"
                                   {:name "Qto_WallBaseQuantities"
                                    :quantities {"Length" {:kind :length :value 8.0}
                                                 "GrossSideArea" {:kind :area :value 25.6}}}}
                                  :material
                                  {:kind :layer-set-usage :direction :axis2
                                   :direction-sense :positive :offset -0.1
                                   :layer-set {:name "Exterior 200mm"
                                               :layers
                                               [{:material {:name "Gypsum" :category "Gypsum"}
                                                 :thickness 0.015}
                                                {:material {:name "Concrete" :category "Concrete"}
                                                 :thickness 0.17}]}}
                                  :classifications
                                  [{:identification "Ss_25_10_20" :name "Wall systems"
                                    :source {:name "Uniclass 2015"}}]
                                  :openings [{:id 110 :kind :opening :filled-by 120
                                              :placement {:location [12 20 0]}
                                              :geometry {:kind :extruded-area-solid
                                                         :profile {:kind :rectangle :x-dim 0.9 :y-dim 0.25}
                                                         :depth 2.1}}]
                                  :geometry {:kind :extruded-area-solid
                                             :profile {:kind :rectangle :x-dim 8.0 :y-dim 0.25}
                                             :direction [0 0 1] :depth 3.2}}
                                 {:id 130 :global-id "mapped-guid" :name "Mapped Furniture"
                                  :kind :proxy :container-id 4 :placement {:location [10 20 0]}
                                  :geometry {:kind :mapped-item
                                             :mapping-origin {:location [10 20 0]}
                                             :transform {:axis1 [1 0 0] :axis2 [0 1 0] :axis3 [0 0 1]
                                                         :origin [5 6 0] :scale 2.0 :scale2 1.0 :scale3 1.0}
                                             :source {:kind :extruded-area-solid
                                                      :position {:location [10 20 0]}
                                                      :profile {:kind :arbitrary-closed
                                                                :points [[0 0] [2 0] [2 1] [0 1] [0 0]]}
                                                      :depth 1.5}}}
                                 {:id 140 :global-id "clipped-guid" :name "Clipped Assembly"
                                  :kind :proxy :container-id 4
                                  :geometry {:kind :collection
                                             :items [{:kind :boolean-result :operator :difference
                                                      :first-operand {:kind :extruded-area-solid
                                                                      :profile {:kind :rectangle :x-dim 4 :y-dim 2}
                                                                      :position {:location [0 0 0]}
                                                                      :direction [0 0 1] :depth 3}
                                                      :second-operand {:kind :half-space-solid
                                                                       :agreement-flag false
                                                                       :base-surface {:kind :plane
                                                                                      :position {:location [0 0 1.5]
                                                                                                 :axis [0 0 1]}}}}
                                                     {:kind :extruded-area-solid
                                                      :profile {:kind :rectangle :x-dim 1 :y-dim 1}
                                                      :depth 0.5}]}}
                                 {:id 150 :global-id "brep-guid" :name "Faceted Tetrahedron"
                                  :kind :proxy :container-id 4
                                  :geometry {:kind :faceted-brep
                                             :faces [{:bounds [{:kind :outer :orientation true
                                                               :points [[0 0 0] [0 2 0] [2 0 0]]}]}
                                                     {:bounds [{:kind :outer :orientation true
                                                               :points [[0 0 0] [2 0 0] [0 0 2]]}]}
                                                     {:bounds [{:kind :outer :orientation true
                                                               :points [[2 0 0] [0 2 0] [0 0 2]]}]}
                                                     {:bounds [{:kind :outer :orientation true
                                                               :points [[0 2 0] [0 0 0] [0 0 2]]}]}]}}]
                  :ifc/units {:lengthunit {:kind :si :type :lengthunit :name :metre :scale 1.0}}}
        project (integration/import-external-ifc document)
        storey (bim/find-storey project 4)
        wall (first (:elements storey))
        mapped (first (filter #(= 130 (:id %)) (:elements storey)))
        mapped-mesh (bim/element-mesh mapped)
        clipped (first (filter #(= 140 (:id %)) (:elements storey)))
        clipped-mesh (bim/element-mesh clipped)
        brep (first (filter #(= 150 (:id %)) (:elements storey)))
        brep-mesh (bim/element-mesh brep)
        reexported (ifc/read-document (ifc/write-standard-spf project))
        reexported-wall (first (filter #(= "wall-guid" (:global-id %))
                                       (:ifc/elements reexported)))
        edited-project
        (bim/update-element
         project 4 100
         (fn [element]
           (-> element
               (assoc-in [:quantities :Length] 9.0)
               (assoc-in [:material-layers 1 :thickness] 0.2)
               (assoc-in [:classification :code] "Ss_25_10_30"))))
        edited-wall
        (first (:ifc/elements
                (ifc/read-document (ifc/write-standard-spf edited-project))))]
    (is (= "Tower" (:name project)))
    (is (= "Ground" (:name storey)))
    (is (= "wall-guid" (:global-id wall)))
    (is (= [[10 20 0] [18.0 20.0 0.0]] (get-in wall [:geometry :axis])))
    (is (= 0.25 (get-in wall [:geometry :profile :thickness])))
    (is (= 3.2 (get-in wall [:geometry :profile :height])))
    (is (= :metre (get-in project [:units :length])))
    (is (true? (get-in wall [:psets "Pset_WallCommon" :props :IsExternal :value])))
    (is (= "2 HR" (get-in wall [:psets "Pset_WallCommon" :props :FireRating :value])))
    (is (= ["Existing"]
           (get-in wall [:psets "Pset_WallCommon" :props :Status :values])))
    (is (= 22.0
           (get-in wall [:psets "Pset_WallCommon" :props :Temperature :set-point])))
    (is (= ["North" "Perimeter"]
           (get-in wall [:psets "Pset_WallCommon" :props :Zones :values])))
    (is (= 8.0 (get-in wall [:quantities :Length])))
    (is (= "Concrete" (get-in wall [:material-layers 1 :material])))
    (is (= "Ss_25_10_20" (get-in wall [:classification :code])))
    (is (= {:offset 2.0 :sill 0.0} (get-in wall [:openings 0 :placement])))
    (is (= 120 (get-in wall [:openings 0 :filled-by])))
    (is (= :other (:kind mapped)))
    (is (= :proxy (:ifc/kind mapped)))
    (is (= :mapped-item (get-in mapped [:geometry :kind])))
    (is (= 8 (count (:positions mapped-mesh))))
    (is (= [5.0 6.0 0.0] (first (:positions mapped-mesh))))
    (is (= [9.0 8.0 3.0] (nth (:positions mapped-mesh) 6)))
    (is (= :collection (get-in clipped [:geometry :kind])))
    (is (= 16 (count (:positions clipped-mesh))))
    (is (= 1.5 (reduce min (map #(nth % 2) (take 8 (:positions clipped-mesh))))))
    (is (= 3.0 (reduce max (map #(nth % 2) (take 8 (:positions clipped-mesh))))))
    (is (= :faceted-brep (get-in brep [:geometry :kind])))
    (is (= 12 (count (:positions brep-mesh))))
    (is (= 12 (count (:indices brep-mesh))))
    (is (= [0.0 0.0 -1.0] (first (:normals brep-mesh))))
    (is (= 25.6 (get-in reexported-wall [:quantity-sets "Qto_WallBaseQuantities"
                                         :quantities "GrossSideArea" :value])))
    (is (= "Concrete" (get-in reexported-wall
                                [:material :layer-set :layers 1 :material :name])))
    (is (= "Ss_25_10_20"
           (get-in reexported-wall [:classifications 0 :identification])))
    (is (= 9.0 (get-in edited-wall [:quantity-sets "Qto_WallBaseQuantities"
                                    :quantities "Length" :value])))
    (is (= 0.2 (get-in edited-wall [:material :layer-set :layers 1 :thickness])))
    (is (= "Ss_25_10_30"
           (get-in edited-wall [:classifications 0 :identification])))))

(deftest imports-rotated-ifc-products-in-their-composed-coordinate-system
  (let [document {:ifc/schema "IFC4X3_ADD2"
                  :ifc/project {:id 1 :name "Rotated Tower" :type :ifcproject
                                :children [{:id 2 :name "Site" :type :ifcsite
                                            :children [{:id 3 :name "Building" :type :ifcbuilding
                                                        :children [{:id 4 :name "Ground"
                                                                    :type :ifcbuildingstorey
                                                                    :children []}]}]}]}
                  :ifc/elements
                  [{:id 100 :global-id "rotated-wall" :name "Rotated Wall"
                    :kind :wall :container-id 4
                    :placement {:location [10.0 2.0 0.0]
                                :axis [0.0 0.0 1.0]
                                :ref-direction [0.0 1.0 0.0]}
                    :geometry {:kind :extruded-area-solid
                               :profile {:kind :rectangle :x-dim 8.0 :y-dim 0.25}
                               :depth 3.2}
                    :openings [{:id 110 :kind :opening
                                :placement {:location [10.0 4.0 0.8]}
                                :geometry {:kind :extruded-area-solid
                                           :profile {:kind :rectangle :x-dim 0.9 :y-dim 0.25}
                                           :depth 2.1}}]}
                   {:id 200 :global-id "rotated-slab" :name "Rotated Slab"
                    :kind :slab :container-id 4
                    :placement {:location [20.0 5.0 0.0]
                                :axis [0.0 0.0 1.0]
                                :ref-direction [0.0 1.0 0.0]}
                    :geometry {:kind :extruded-area-solid
                               :profile {:kind :rectangle :x-dim 6.0 :y-dim 4.0}
                               :depth 0.25}}]}
        project (integration/import-external-ifc document)
        elements (:elements (bim/find-storey project 4))
        wall (first (filter #(= 100 (:id %)) elements))
        slab (first (filter #(= 200 (:id %)) elements))]
    (is (= [[10.0 2.0 0.0] [10.0 10.0 0.0]] (get-in wall [:geometry :axis])))
    (is (= {:offset 2.0 :sill 0.8} (get-in wall [:openings 0 :placement])))
    (is (= [[20.0 5.0 0.0] [20.0 11.0 0.0]
            [16.0 11.0 0.0] [16.0 5.0 0.0]]
           (get-in slab [:geometry :boundary])))))

(deftest meshes-spherical-and-toroidal-advanced-faces
  (let [sphere (bim/element-mesh
                {:geometry {:kind :advanced-brep
                            :faces [{:same-sense true :bounds []
                                     :surface {:kind :sphere :radius 2.0
                                               :position {:location [10 20 30]}}}]}})
        torus (bim/element-mesh
               {:geometry {:kind :advanced-brep
                           :faces [{:same-sense false :bounds []
                                    :surface {:kind :torus :major-radius 4.0 :minor-radius 1.0
                                              :position {:location [1 2 3]}}}]}})
        patch (bim/element-mesh
               {:geometry {:kind :advanced-brep
                           :faces [{:same-sense true
                                    :bounds [{:kind :outer
                                              :points [[2 0 0] [0 2 0] [0 0 2]]}]
                                    :surface {:kind :sphere :radius 2.0
                                              :position {:location [0 0 0]}}}]}})]
    (is (= 312 (count (:positions sphere))))
    (is (= 1728 (count (:indices sphere))))
    (is (every? #(< (#?(:clj Math/abs :cljs js/Math.abs)
                       (- 1.0 (#?(:clj Math/sqrt :cljs js/Math.sqrt)
                               (reduce + (map (fn [x] (* x x)) %)))))
                    1.0e-9)
                (:normals sphere)))
    (is (= [10.0 20.0 28.0] (first (:positions sphere))))
    (is (= 288 (count (:positions torus))))
    (is (= 1728 (count (:indices torus))))
    (is (= [6.0 2.0 3.0] (first (:positions torus))))
    (is (= [-1.0 -0.0 -0.0] (first (:normals torus))))
    (is (= 48 (count (:positions patch))))
    (is (= 48 (count (:indices patch))))))

(deftest trims-analytic-curved-faces-with-holes
  (let [radius 2.0
        sphere-point (fn [u v]
                       [(* radius (#?(:clj Math/cos :cljs js/Math.cos) v)
                           (#?(:clj Math/cos :cljs js/Math.cos) u))
                        (* radius (#?(:clj Math/cos :cljs js/Math.cos) v)
                           (#?(:clj Math/sin :cljs js/Math.sin) u))
                        (* radius (#?(:clj Math/sin :cljs js/Math.sin) v))])
        ring (fn [uvs] (mapv #(apply sphere-point %) uvs))
        mesh (bim/element-mesh
              {:geometry {:kind :advanced-brep
                          :faces [{:same-sense true
                                   :surface {:kind :sphere :radius radius
                                             :position {:location [0 0 0]}}
                                   :bounds [{:kind :outer :orientation true
                                             :points (ring [[0.0 0.0] [1.5 0.0]
                                                           [1.5 1.0] [0.0 1.0]])}
                                            {:kind :inner :orientation true
                                             :points (ring [[0.5 0.25] [0.5 0.75]
                                                           [1.0 0.75] [1.0 0.25]])}]}]}})
        uv (fn [[x y z]]
             [(#?(:clj Math/atan2 :cljs js/Math.atan2) y x)
              (#?(:clj Math/asin :cljs js/Math.asin) (/ z radius))])
        parameter-area
        (reduce + (map (fn [[a b c]]
                         (let [[ax ay] (uv (nth (:positions mesh) a))
                               [bx by] (uv (nth (:positions mesh) b))
                               [cx cy] (uv (nth (:positions mesh) c))]
                           (/ (#?(:clj Math/abs :cljs js/Math.abs)
                               (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))) 2.0)))
                       (partition 3 (:indices mesh))))]
    (is (pos? (count (:positions mesh))))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs) (- parameter-area 1.25)) 1.0e-6))))

(deftest meshes-rational-b-spline-advanced-face
  (let [mesh (bim/element-mesh
              {:geometry
               {:kind :advanced-brep
                :faces [{:same-sense true :bounds []
                         :surface {:kind :b-spline-surface
                                   :u-degree 1 :v-degree 1
                                   :control-points [[[0 0 0] [0 2 0]]
                                                    [[2 0 0] [2 2 1]]]
                                   :u-multiplicities [2 2] :v-multiplicities [2 2]
                                   :u-knots [0.0 1.0] :v-knots [0.0 1.0]
                                   :weights [[1.0 1.0] [1.0 0.75]]}}]}})]
    (is (= 289 (count (:positions mesh))))
    (is (= 1536 (count (:indices mesh))))
    (is (= [0.0 0.0 0.0] (first (:positions mesh))))
    (is (= [2.0 2.0 1.0] (last (:positions mesh))))
    (is (every? #(not= [0.0 0.0 0.0] %) (:normals mesh)))))

(deftest trims-nurbs-face-with-an-inner-hole
  (let [surface {:kind :b-spline-surface :u-degree 1 :v-degree 1
                 :control-points [[[0 0 0] [0 2 0]] [[2 0 0] [2 2 1]]]
                 :u-multiplicities [2 2] :v-multiplicities [2 2]
                 :u-knots [0.0 1.0] :v-knots [0.0 1.0]}
        surface-point (fn [u v] [(* 2.0 u) (* 2.0 v) (* u v)])
        ring (fn [uvs] (mapv #(apply surface-point %) uvs))
        mesh (bim/element-mesh
              {:geometry {:kind :advanced-brep
                          :faces [{:same-sense true :surface surface
                                   :bounds [{:kind :outer :orientation true
                                             :points (ring [[0 0] [1 0] [1 1] [0 1]])}
                                            {:kind :inner :orientation true
                                             :points (ring [[0.25 0.25] [0.25 0.75]
                                                           [0.75 0.75] [0.75 0.25]])}]}]}})
        projected-area
        (reduce + (map (fn [[a b c]]
                         (let [[ax ay] (nth (:positions mesh) a)
                               [bx by] (nth (:positions mesh) b)
                               [cx cy] (nth (:positions mesh) c)]
                           (/ (#?(:clj Math/abs :cljs js/Math.abs)
                               (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))
                              2.0)))
                       (partition 3 (:indices mesh))))]
    (is (pos? (count (:positions mesh))))
    (is (= (count (:positions mesh)) (count (:indices mesh))))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs) (- projected-area 3.0)) 1.0e-3))))

(deftest evaluates-general-closed-mesh-booleans
  (let [solid (fn [location]
                {:kind :extruded-area-solid
                 :profile {:kind :rectangle :x-dim 2.0 :y-dim 2.0}
                 :position {:location location} :direction [0 0 1] :depth 2.0})
        volume (fn [{:keys [positions indices]}]
                 (#?(:clj Math/abs :cljs js/Math.abs)
                  (/ (reduce + (map (fn [[ia ib ic]]
                                      (let [[ax ay az] (nth positions ia)
                                            [bx by bz] (nth positions ib)
                                            [cx cy cz] (nth positions ic)]
                                        (+ (* ax (- (* by cz) (* bz cy)))
                                           (* ay (- (* bz cx) (* bx cz)))
                                           (* az (- (* bx cy) (* by cx))))))
                                    (partition 3 indices)))
                     6.0)))
        operand-a (solid [0 0 0]) operand-b (solid [1 1 1])]
    (doseq [[operator expected] [[:union 15.0] [:difference 7.0] [:intersection 1.0]]]
      (let [mesh (bim/element-mesh
                  {:geometry {:kind :boolean-result :operator operator
                              :first-operand operand-a :second-operand operand-b}})]
        (is (seq (:indices mesh)))
        (is (< (#?(:clj Math/abs :cljs js/Math.abs) (- expected (volume mesh))) 1.0e-6))))))

(deftest generates-annotated-plans-sections-elevations-and-sheets
  (let [opening (bim/rectangular-opening {:id 900 :offset 2.0 :sill 0.0
                                          :width 1.0 :height 2.1})
        wall-a (bim/add-opening-to-wall
                (bim/wall {:id 101 :start [0 0 0] :end [6 0 0] :height 3.0}) opening)
        wall-b (bim/wall {:id 102 :start [0 2 0] :end [6 2 0] :height 3.0})
        slab (bim/slab {:id 103 :boundary [[0 0 0] [6 0 0] [6 4 0] [0 4 0]]
                        :thickness 0.2})
        storey (bim/storey {:id 10 :name "Ground" :elevation 0 :height 3
                            :placement :identity :spaces [] :elements [wall-a wall-b slab]})
        building (bim/building {:id 20 :name "Drawing Building" :placement :identity
                                :reference-elevation 0 :storeys [storey]})
        plan (drawing/documented-floor-plan storey)
        plan-svg (drawing/documented-floor-plan-svg storey)
        section (drawing/orthographic-view building {:kind :section :axis :x
                                                       :cut-position 0.0 :depth 3.0})
        section-svg (drawing/orthographic-view-svg building {:kind :section :axis :x
                                                              :cut-position 0.0 :depth 3.0})
        elevation (drawing/orthographic-view-svg building {:kind :elevation :axis :x
                                                            :cut-position 0.0})
        schedule (integration/element-schedule
                  {:id "wall-schedule" :name "Wall Schedule"
                   :elements [wall-a wall-b slab] :group-by [:kind :name]})
        schedule-svg (drawing/schedule-table-svg schedule)
        sheet (drawing/drawing-sheet-svg
               {:number "A-101" :name "General Arrangement" :size :a1 :revision "C01"
                :viewports [{:view plan :x 20 :y 20 :width 380 :height 260
                             :title "Ground Plan" :scale 100}
                            {:view section :x 430 :y 20 :width 380 :height 260
                             :title "Section A" :scale 50}]})]
    (is (re-find #"class=\"dimension\"" plan-svg))
    (is (re-find #"class=\"opening\"" plan-svg))
    (is (re-find #"class=\"element-tag\"" plan-svg))
    (is (re-find #"class=\"cut-element\"" section-svg))
    (is (re-find #"class=\"projected-element\"" section-svg))
    (is (re-find #"data-view-kind=\"elevation\"" elevation))
    (is (= 3 (:schedule/total-count schedule)))
    (is (= 2 (count (:schedule/rows schedule))))
    (is (re-find #"data-schedule-id=\"wall-schedule\"" schedule-svg))
    (is (re-find #"class=\"schedule-header\"" schedule-svg))
    (is (re-find #"Wall Schedule" schedule-svg))
    (is (= 2 (count (re-seq #"class=\"viewport\"" sheet))))
    (is (re-find #"class=\"title-block\"" sheet))
    (is (re-find #"data-sheet-number=\"A-101\"" sheet))))

(deftest view-templates-control-graphics-hidden-lines-and-annotations
  (let [wall-a (bim/wall {:id 201 :start [0 0 0] :end [6 0 0] :height 3.0})
        wall-b (bim/wall {:id 202 :start [0 2 0] :end [6 2 0] :height 3.0})
        slab (bim/slab {:id 203 :boundary [[0 0 0] [6 0 0] [6 4 0] [0 4 0]]
                        :thickness 0.2})
        storey (bim/storey {:id 30 :name "Template Plan" :elevation 0 :height 3
                            :placement :identity :spaces [] :elements [wall-a wall-b slab]})
        building (bim/building {:id 31 :name "Template Building" :placement :identity
                                :reference-elevation 0 :storeys [storey]})
        template (integration/view-template
                  {:id :architectural-plan :name "Architectural Plan" :scale 50
                   :hidden-line? true :show-tags? false
                   :category-visibility {:slab false}
                   :category-overrides {:wall {:stroke "#ff0000"}}
                   :annotation-style {:fill "#7c3aed"}})
        view (integration/drawing-view
              {:id :ground :kind :floor-plan :name "Ground" :template-id :architectural-plan
               :annotations [{:kind :text :point [1 1] :text "Grid note"}
                             {:kind :level :point [0 1.5] :width 80 :label "Level 0"}
                             {:kind :dimension :from [0 0] :to [6 0]
                              :offset 22 :label "6000"}]})
        options (integration/apply-view-template view template)
        plan-svg (drawing/documented-floor-plan-svg storey options)
        hidden (drawing/orthographic-view-svg
                building {:kind :elevation :axis :x :cut-position 0 :hidden-line? true})
        wire (drawing/orthographic-view-svg
              building {:kind :elevation :axis :x :cut-position 0 :hidden-line? false})]
    (is (= 50 (:scale options)))
    (is (not (re-find #"<polygon" plan-svg)))
    (is (not (re-find #"class=\"element-tag\"" plan-svg)))
    (is (re-find #"class=\"text-annotation\"" plan-svg))
    (is (re-find #"class=\"level-annotation\"" plan-svg))
    (is (re-find #"6000" plan-svg))
    (is (re-find #"#ff0000" plan-svg))
    (is (< (count (re-seq #"data-element-id=" hidden))
           (count (re-seq #"data-element-id=" wire))))))
