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

(deftest rectangular-route-assembly-preserves-duct-cross-section
  (let [assembly (mep/rectangular-route-assembly
                  {:id "SA" :system-id :supply-air :domain :hvac
                   :width 0.6 :height 0.3
                   :points [[0 0 3] [3 0 3] [3 2 3]]})]
    (is (= 2 (count (:mep.assembly/segments assembly))))
    (is (= :rectangular
           (get-in assembly [:mep.assembly/segments 0 :mep/shape])))
    (is (= [0.6 0.3]
           (get-in assembly [:mep.assembly/fittings 0 :mep/connectors 0
                             :connector/size])))
    (is (= :elbow (get-in assembly [:mep.assembly/fittings 0 :mep/fitting-kind])))))

(deftest network-assembly-generates-elbows-tees-and-reducers
  (let [assembly
        (mep/network-assembly
         {:id "supply" :system-id :supply-air :domain :hvac :default-size 0.1
          :nodes {:source {:point [0 0 0]}
                  :inline {:point [1 0 0]}
                  :bend {:point [2 0 0]}
                  :tee {:point [2 2 0]}
                  :terminal-a {:point [4 2 0]}
                  :transition {:point [2 4 0]}
                  :terminal-b {:point [2 6 0]}}
          :edges [{:id "e1" :from :source :to :inline}
                  {:id "e1b" :from :inline :to :bend}
                  {:id "e2" :from :bend :to :tee}
                  {:id "e3" :from :tee :to :terminal-a}
                  {:id "e4" :from :tee :to :transition :size 0.1}
                  {:id "e5" :from :transition :to :terminal-b :size 0.15}]})
        fittings (into {} (map (juxt :mep/node-id identity)
                               (:mep.assembly/fittings assembly)))
        connectors (mapcat :mep/connectors
                           (concat (:mep.assembly/segments assembly)
                                   (:mep.assembly/fittings assembly)))
        by-id (into {} (map (juxt :connector/id identity) connectors))]
    (is (= 6 (count (:mep.assembly/segments assembly))))
    (is (nil? (get fittings :inline)))
    (is (= :elbow (get-in fittings [:bend :mep/fitting-kind])))
    (is (= :tee (get-in fittings [:tee :mep/fitting-kind])))
    (is (= :reducer (get-in fittings [:transition :mep/fitting-kind])))
    (is (= 3 (count (:mep.assembly/open-connectors assembly))))
    (is (every? (fn [connector]
                  (if-let [target-id (:connector/connected-to connector)]
                    (= (:connector/id connector)
                       (:connector/connected-to (get by-id target-id)))
                    true))
                connectors))
    (is (= 0.075 (get-in (last (:mep.assembly/segments assembly))
                          [:geometry :radius])))))

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

(deftest feeder-fault-duty-protection-and-coordination
  (let [analysis
        (mep/analyze-electrical-feeder
         {:id :main-feeder :phases 3 :apparent-power-va 40000.0 :voltage-v 400.0
          :power-factor 0.9 :length-m 50.0 :resistance-ohm-m 0.0005
          :reactance-ohm-m 0.00008 :source-resistance-ohm 0.01
          :source-reactance-ohm 0.02 :cable-ampacity-a 100.0
          :max-voltage-drop-percent 3.0})
        catalog [{:id :mccb-50 :rating-a 50.0 :breaking-capacity-a 10000.0
                  :instantaneous-trip-multiple 10.0}
                 {:id :mccb-63 :rating-a 63.0 :breaking-capacity-a 6000.0
                  :instantaneous-trip-multiple 10.0}
                 {:id :mccb-80 :rating-a 80.0 :breaking-capacity-a 10000.0
                  :instantaneous-trip-multiple 10.0}]
        protection (mep/select-protective-device analysis catalog)
        downstream (:electrical.protection/device protection)
        coordinated (mep/check-protection-coordination
                     downstream
                     {:id :upstream :rating-a 125.0
                      :instantaneous-trip-multiple 100.0}
                     (:electrical.feeder/prospective-fault-current-a analysis))
        uncoordinated (mep/check-protection-coordination
                       downstream
                       {:id :upstream-fast :rating-a 125.0
                        :instantaneous-trip-multiple 5.0}
                       (:electrical.feeder/prospective-fault-current-a analysis))]
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- (/ 40000.0 (* 400.0 (#?(:clj Math/sqrt :cljs js/Math.sqrt) 3.0)))
               (:electrical.feeder/design-current-a analysis)))
           1.0e-9))
    (is (< (:electrical.feeder/voltage-drop-percent analysis) 1.0))
    (is (> (:electrical.feeder/prospective-fault-current-a analysis) 5000.0))
    (is (empty? (:electrical.feeder/issues analysis)))
    (is (= :mccb-63 (get-in protection [:electrical.protection/device :id])))
    (is (pos? (:electrical.protection/breaking-margin-a protection)))
    (is (true? (:electrical.coordination/coordinated? coordinated)))
    (is (false? (:electrical.coordination/coordinated? uncoordinated)))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"no protective device"
         (mep/select-protective-device analysis
                                       [{:id :undersized :rating-a 50.0
                                         :breaking-capacity-a 10000.0}])))))
