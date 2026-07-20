(ns bim.editor-test
  (:require [clojure.test :refer [deftest is]]
            [bim.editor :as editor]))

(deftest transactions-are-atomic-and-undoable
  (let [initial (editor/editor-state {:name "Tower" :elements [{:id 1 :name "Wall"}]})
        changed (editor/transact
                 initial {:id "tx-1" :label "Rename and add"
                          :operations [{:op :set :path [:name] :value "Tower A"}
                                       {:op :set :path [:description] :value "Issued"}
                                       {:op :insert :path [:elements] :index 1
                                        :value {:id 2 :name "Door"}}]})
        undone (editor/undo changed)
        redone (editor/redo undone)]
    (is (= "Tower A" (get-in changed [:editor/document :name])))
    (is (= 2 (count (get-in changed [:editor/document :elements]))))
    (is (= (:editor/document initial) (:editor/document undone)))
    (is (= (:editor/document changed) (:editor/document redone)))
    (is (empty? (:editor/redo redone)))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"out of bounds"
                          (editor/transact initial {:operations [{:op :remove
                                                                  :path [:elements]
                                                                  :index 9}]})))
    (is (= (:editor/document initial) (:editor/document initial)))))

(deftest selection-and-snapping-are-deterministic
  (let [state (-> (editor/editor-state {})
                  (editor/select [1 2])
                  (editor/select [3] :add)
                  (editor/select [2 3] :toggle))
        snapped (editor/snap-point [1.04 0.02]
                                   [{:snap/kind :midpoint :snap/point [1.0 0.0]}
                                    {:snap/kind :endpoint :snap/point [1.0 0.0]}]
                                   {:grid 0.5 :tolerance 0.1})]
    (is (= #{1} (:editor/selection state)))
    (is (= :endpoint (:snap/kind snapped)))
    (is (= [1.0 0.0] (:snap/point snapped)))
    (is (nil? (editor/snap-point [5 5] [] {:tolerance 0.1})))))
