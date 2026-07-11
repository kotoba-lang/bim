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
