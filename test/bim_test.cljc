(ns bim-test
  (:require [clojure.test :refer [deftest is testing]]
            [bim]))
(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? bim))))
