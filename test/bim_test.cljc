(ns bim-test
  "Restoration-fidelity tests — one per original kami-bim Rust test
  (kami-engine/kami-bim/src/lib.rs `mod tests`, deleted PR #82)."
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest rejects-family-formula-and-nesting-cycles
  (let [formula-cycle (integration/family-definition
                       {:id "cycle" :parameters {}
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
    (is (= "P01" (get-in drawings [:drawing/sheets 0 :sheet/revisions 0 :revision])))
    (is (= project (integration/import-ifc ifc)))
    (is (= :wall (get-in ifc [:ifc/elements 0 :kind])))
    (is (= "Issued for coordination" (:description changed)))
    (is (= :design/revision-published (:itonami/event payload)))
    (is (= "Ss_25_10" (get-in payload [:design/line-items 0 :classification/code])))
    (is (= 7 (:design/revision payload)))))

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
                                                                "FireRating" {:value "2 HR" :value-type :ifclabel}}}}
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
        brep-mesh (bim/element-mesh brep)]
    (is (= "Tower" (:name project)))
    (is (= "Ground" (:name storey)))
    (is (= "wall-guid" (:global-id wall)))
    (is (= [[10 20 0] [18.0 20 0]] (get-in wall [:geometry :axis])))
    (is (= 0.25 (get-in wall [:geometry :profile :thickness])))
    (is (= 3.2 (get-in wall [:geometry :profile :height])))
    (is (= :metre (get-in project [:units :length])))
    (is (true? (get-in wall [:psets "Pset_WallCommon" :props :IsExternal :value])))
    (is (= "2 HR" (get-in wall [:psets "Pset_WallCommon" :props :FireRating :value])))
    (is (= {:offset 2 :sill 0} (get-in wall [:openings 0 :placement])))
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
    (is (= [0.0 0.0 -1.0] (first (:normals brep-mesh))))))

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
    (is (= 2 (count (re-seq #"class=\"viewport\"" sheet))))
    (is (re-find #"class=\"title-block\"" sheet))
    (is (re-find #"data-sheet-number=\"A-101\"" sheet))))
