(ns bim.structural-test
  (:require [clojure.test :refer [deftest is]]
            [bim.structural :as structural]))

(deftest shell-area-and-load-distribution
  (let [shell (structural/shell-element
               {:id :s1 :nodes [[0 0 0] [4 0 0] [4 3 0] [0 3 0]]
                :thickness-m 0.2 :elastic-modulus-pa 3.0e10
                :poisson-ratio 0.2 :material "C30"})
        load (structural/distribute-shell-load shell 5000.0 [0 0 -1] :dead)]
    (is (= 12.0 (structural/shell-area shell)))
    (is (= 60000.0 (:structural.load/total-n load)))
    (is (= [0.0 0.0 -15000.0]
           (get-in load [:structural.load/nodal 0 :force-n])))
    (is (= 4 (count (:structural.load/nodal load))))))

(deftest reinforcement-and-bolted-connection-design
  (let [mat (structural/rectangular-rebar-mat
             {:id "R1" :origin [0 0 0] :width-m 4.0 :length-m 3.0
              :cover-m 0.05 :spacing-m 0.2 :diameter-m 0.016
              :layer :bottom :grade "B500"})
        elements (structural/rebar-elements mat)
        connection (structural/check-bolted-shear-connection
                    {:bolt-count 4 :bolt-diameter-m 0.02 :bolt-ultimate-pa 8.0e8
                     :shear-planes 1 :plate-thickness-m 0.012
                     :plate-ultimate-pa 4.3e8 :design-shear-n 250000.0})]
    (is (= 35 (count (:rebar-set/bars mat))))
    (is (= 35 (count elements)))
    (is (= :swept-disk-solid (get-in elements [0 :geometry :kind])))
    (is (pos? (:rebar-set/steel-volume-m3 mat)))
    (is (true? (:connection/passes? connection)))
    (is (< (:connection/utilization connection) 1.0))))
