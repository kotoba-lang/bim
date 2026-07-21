(ns bim.structural-test
  (:require [clojure.test :refer [deftest is]]
            [bim.structural :as structural]))

(deftest combines-and-envelopes-structural-results
  (let [dead {:nodes {:n1 {:ux 1.0 :reaction -10.0}}
              :members {:m1 {:moment [2.0 -3.0]}}}
        live {:nodes {:n1 {:ux -2.0 :reaction -4.0}}
              :members {:m1 {:moment [5.0 1.0]}}}
        results {:dead dead :live live}
        combination (structural/combine-results :uls results {:dead 1.35 :live 1.5})
        envelope (structural/result-envelope results)]
    (is (= -1.65 (get-in combination
                          [:structural.combination/result :nodes :n1 :ux])))
    (is (= -19.5 (get-in combination
                          [:structural.combination/result :nodes :n1 :reaction])))
    (is (= {:min -2.0 :min-case :live :max 1.0 :max-case :dead}
           (get-in envelope [:structural.envelope/by-path [:nodes :n1 :ux]])))
    (is (= :live
           (get-in envelope
                   [:structural.envelope/by-path [:members :m1 :moment 0] :max-case])))))

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

(deftest two-dimensional-frame-analysis-includes-bending-and-releases
  (let [cantilever
        (structural/analyze-2d-frame
         {:nodes [{:id :a :point [0.0 0.0] :restraints [true true true]}
                  {:id :b :point [3.0 0.0] :restraints [false false false]}]
          :members [{:id :ab :start-node :a :end-node :b :area-m2 0.01
                     :elastic-modulus-pa 2.0e11 :inertia-m4 8.0e-6}]
          :load-case {:nodal-loads [{:node :b :fy -1000.0}]}})
        released
        (structural/analyze-2d-frame
         {:nodes [{:id :a :point [0.0 0.0] :restraints [true true false]}
                  {:id :b :point [6.0 0.0] :restraints [false true false]}]
          :members [{:id :ab :start-node :a :end-node :b :area-m2 0.01
                     :elastic-modulus-pa 2.0e11 :inertia-m4 8.0e-6
                     :release-start-moment? true :release-end-moment? true}]
          :load-case {:member-loads [{:member :ab :qy -10000.0}]}})]
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- -0.005625 (get-in cantilever [:structural.frame/nodes :b :uy])))
           1.0e-12))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 3000.0 (get-in cantilever [:structural.frame/nodes :a :rmz])))
           1.0e-9))
    (is (= -1000.0
           (get-in cantilever [:structural.frame/members :ab :local-end-forces :v2])))
    (is (= 30000.0 (get-in released [:structural.frame/nodes :a :ry])))
    (is (= 30000.0 (get-in released [:structural.frame/nodes :b :ry])))
    (is (zero? (get-in released
                       [:structural.frame/members :ab :local-end-forces :m1])))
    (is (zero? (get-in released
                       [:structural.frame/members :ab :local-end-forces :m2])))))

(deftest constant-strain-triangle-plane-stress-analysis
  (let [model {:nodes [{:id :a :point [0.0 0.0] :restraints [true true]}
                       {:id :b :point [1.0 0.0] :restraints [false true]}
                       {:id :c :point [0.0 1.0] :restraints [false false]}]
               :elements [{:id :t1 :nodes [:a :b :c] :thickness-m 0.1
                           :elastic-modulus-pa 2.0e11 :poisson-ratio 0.3}]
               :loads [{:node :c :fx 1000.0}]}
        result (structural/analyze-plane-stress-mesh model)
        clockwise (structural/analyze-plane-stress-mesh
                   (assoc-in model [:elements 0 :nodes] [:a :c :b]))]
    (is (= 0.5 (get-in result [:structural.plane-stress/elements :t1 :area-m2])))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs)
            (- 2.6e-7 (get-in result [:structural.plane-stress/nodes :c :ux])))
           1.0e-15))
    (is (= 20000.0
           (get-in result [:structural.plane-stress/elements :t1 :stress-pa :tau-xy])))
    (is (= -1000.0 (get-in result [:structural.plane-stress/nodes :a :rx])))
    (is (= (get-in result [:structural.plane-stress/nodes :c :ux])
           (get-in clockwise [:structural.plane-stress/nodes :c :ux])))
    (is (= (get-in result [:structural.plane-stress/elements :t1 :stress-pa])
           (get-in clockwise [:structural.plane-stress/elements :t1 :stress-pa])))
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"zero area"
         (structural/analyze-plane-stress-mesh
          (-> model
              (assoc-in [:nodes 2 :point] [2.0 0.0])))))))
