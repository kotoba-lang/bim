(ns bim.mep-test
  (:require [clojure.test :refer [deftest is]]
            [bim.mep :as mep]))

(deftest route-assembly-generates-connected-elbows
  (let [assembly (mep/route-assembly
                  {:id "CHW" :system-id :chilled-water :domain :piping
                   :shape :round :size 0.1
                   :points [[0 0 0] [2 0 0] [2 3 0] [4 3 0]]})]
    (is (= 3 (count (:mep.assembly/segments assembly))))
    (is (= 2 (count (:mep.assembly/fittings assembly))))
    (is (= 90.0 (get-in assembly [:mep.assembly/fittings 0 :mep/angle-deg])))
    (is (= "CHW-f1-in"
           (get-in assembly [:mep.assembly/segments 0 :mep/connectors 1
                             :connector/connected-to])))
    (is (= 2 (count (:mep.assembly/open-connectors assembly))))))

(deftest electrical-panel-balances-and-checks-circuits
  (let [circuits [(mep/electrical-circuit
                   {:id :c1 :name "Lighting" :apparent-power-va 2300.0 :voltage-v 230.0
                    :length-m 30.0 :conductor-area-mm2 4.0})
                  (mep/electrical-circuit
                   {:id :c2 :name "Sockets" :apparent-power-va 1800.0 :voltage-v 230.0
                    :length-m 20.0 :conductor-area-mm2 4.0})
                  (mep/electrical-circuit
                   {:id :c3 :name "Equipment" :apparent-power-va 1500.0 :voltage-v 230.0
                    :length-m 15.0 :conductor-area-mm2 4.0})]
        result (mep/analyze-panel {:id :db1 :phases [:l1 :l2 :l3] :circuits circuits
                                   :main-rating-a 63.0 :demand-factor 0.8
                                   :max-imbalance-percent 50.0
                                   :max-voltage-drop-percent 3.0})]
    (is (= #{:l1 :l2 :l3} (set (vals (:panel/assignments result)))))
    (is (every? pos? (vals (:panel/phase-currents-a result))))
    (is (empty? (:panel/issues result)))
    (is (< (get-in result [:panel/voltage-drops :c1
                           :electrical/voltage-drop-percent]) 3.0))))
