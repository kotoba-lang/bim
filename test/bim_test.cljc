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
