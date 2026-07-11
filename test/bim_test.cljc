(ns bim-test
  "Restoration-fidelity tests — one per original kami-bim Rust test
  (kami-engine/kami-bim/src/lib.rs `mod tests`, deleted PR #82)."
  (:require [clojure.test :refer [deftest is testing]]
            [bim]))

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

(deftest semantic-room-space-lifecycle
  (let [ground (bim/storey {:id 3 :name "Ground" :elevation 0 :height 3 :placement :identity :spaces [] :elements []})
        project (-> (bim/project "House")
                    (update :sites conj (bim/site {:id 1 :name "Site" :placement :identity :buildings
                                                   [(bim/building {:id 2 :name "House" :placement :identity :reference-elevation 0 :storeys [ground]})]})))
        room (bim/room-space {:id 50 :name "Living" :category :residential :boundary [[0 0 0] [4 0 0] [4 3 0] [0 3 0]] :height 3})
        added (bim/add-space project 3 room) renamed (bim/update-space added 3 50 assoc :name "Living Room")]
    (is (== 12 (get-in room [:quantities :net-area-m2])))
    (is (== 36 (get-in room [:quantities :net-volume-m3])))
    (is (= "Living Room" (:name (first (:spaces (bim/find-storey renamed 3))))))
    (is (empty? (:spaces (bim/find-storey (bim/delete-space renamed 3 50) 3))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (bim/add-space added 3 room)))))
