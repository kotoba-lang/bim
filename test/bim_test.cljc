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
  (let [project (integrated-project)
        spf (ifc/write-spf project)
        svg (drawing/floor-plan-svg (bim/find-storey project 3))]
    (is (.startsWith spf "ISO-10303-21;"))
    (is (re-find #"IFCWALL" spf))
    (is (= project (ifc/read-spf spf)))
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
                                                                      :depth 3}
                                                      :second-operand {:kind :half-space-solid}}
                                                     {:kind :extruded-area-solid
                                                      :profile {:kind :rectangle :x-dim 1 :y-dim 1}
                                                      :depth 0.5}]}}]
                  :ifc/units {:lengthunit {:kind :si :type :lengthunit :name :metre :scale 1.0}}}
        project (integration/import-external-ifc document)
        storey (bim/find-storey project 4)
        wall (first (:elements storey))
        mapped (first (filter #(= 130 (:id %)) (:elements storey)))
        mapped-mesh (bim/element-mesh mapped)
        clipped (first (filter #(= 140 (:id %)) (:elements storey)))
        clipped-mesh (bim/element-mesh clipped)]
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
    (is (= 8 (count (:positions clipped-mesh))))))
