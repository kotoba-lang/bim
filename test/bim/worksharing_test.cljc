(ns bim.worksharing-test
  (:require [clojure.test :refer [deftest is]]
            [bim.worksharing :as worksharing]))

(deftest leases-permissions-and-semantic-conflicts
  (let [initial (worksharing/workspace
                 {:elements {10 {:name "Wall" :mark "W1"}}}
                 {"alice" :author "bob" :author "reviewer" :reviewer})
        checked-out (worksharing/checkout initial "alice" [10] 100 60)
        applied (worksharing/submit
                 checked-out "alice" 0 110
                 {:id "tx-a" :transaction/element-ids [10]
                  :operations [{:op :set :path [:elements 10 :name] :value "Wall A"}]})
        state-a (:worksharing/workspace applied)
        disjoint (worksharing/submit
                  state-a "alice" 0 111
                  {:id "tx-b" :transaction/element-ids [10]
                   :operations [{:op :set :path [:elements 10 :mark] :value "W2"}]})
        state-b (:worksharing/workspace disjoint)
        conflict (worksharing/submit
                  state-b "alice" 0 112
                  {:id "tx-c" :transaction/element-ids [10]
                   :operations [{:op :set :path [:elements 10 :name] :value "Wall C"}]})]
    (is (= :applied (:worksharing/status applied)))
    (is (= :applied (:worksharing/status disjoint)))
    (is (= "Wall A" (get-in state-b [:worksharing/editor :editor/document
                                      :elements 10 :name])))
    (is (= "W2" (get-in state-b [:worksharing/editor :editor/document
                                  :elements 10 :mark])))
    (is (= :conflict (:worksharing/status conflict)))
    (is (= [:elements 10 :name]
           (get-in conflict [:worksharing/conflicts 0 :incoming-path])))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"not authorized"
                          (worksharing/checkout initial "reviewer" [10] 100 60)))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"checked out"
                          (worksharing/checkout checked-out "bob" [10] 110 60)))
    (is (empty? (:worksharing/leases
                 (worksharing/relinquish checked-out "alice" [10]))))))
