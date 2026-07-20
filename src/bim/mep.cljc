(ns bim.mep
  "MEP fittings/assemblies and electrical circuit design.")

(def schema-version 1)
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))
(defn- sqrt [value] (#?(:clj Math/sqrt :cljs js/Math.sqrt) value))
(defn- acos [value] (#?(:clj Math/acos :cljs js/Math.acos) value))
(defn- magnitude [vector] (sqrt (reduce + (map #(* % %) vector))))
(defn- normalize [vector] (let [length (magnitude vector)] (mapv #(/ % length) vector)))
(defn- subtract [a b] (mapv - a b))
(defn- dot [a b] (reduce + (map * a b)))

(defn route-assembly
  "Turn an orthogonal/segmented route into connected segments and elbow
  fittings. End connectors remain available for equipment or branch hookup."
  [{:keys [id system-id domain shape size points bend-radius]}]
  (when (or (< (count points) 2) (some #(not= 3 (count %)) points))
    (throw (ex-info "MEP route requires at least two 3D points" {:id id})))
  (let [points (mapv vec points)
        segment-count (dec (count points))
        fittings
        (mapv (fn [index]
                (let [point (nth points index)
                      incoming (normalize (subtract (nth points (dec index)) point))
                      outgoing (normalize (subtract (nth points (inc index)) point))
                      angle (* (/ 180.0 pi) (acos (max -1.0 (min 1.0 (dot incoming outgoing)))))]
                  {:id (str id "-elbow-" index) :kind :flow-fitting
                   :mep/fitting-kind :elbow :mep/system-id system-id
                   :mep/angle-deg angle :mep/bend-radius (or bend-radius (* 1.5 size))
                   :mep/connectors
                   [{:connector/id (str id "-f" index "-in") :connector/point point
                     :connector/domain domain :connector/shape shape :connector/size size
                     :connector/flow-direction :in
                     :connector/connected-to (str id "-s" (dec index) "-end")}
                    {:connector/id (str id "-f" index "-out") :connector/point point
                     :connector/domain domain :connector/shape shape :connector/size size
                     :connector/flow-direction :out
                     :connector/connected-to (str id "-s" index "-start")}]}))
              (range 1 segment-count))
        segments
        (mapv (fn [index]
                (let [start (nth points index) end (nth points (inc index))]
                  {:id (str id "-segment-" index) :kind :mep-segment
                   :mep/system-id system-id :mep/domain domain :mep/shape shape
                   :mep/size size :geometry {:kind :swept-disk-solid
                                             :directrix [start end] :radius (/ size 2.0)}
                   :mep/connectors
                   [{:connector/id (str id "-s" index "-start") :connector/point start
                     :connector/domain domain :connector/shape shape :connector/size size
                     :connector/flow-direction :in
                     :connector/connected-to (when (pos? index)
                                               (str id "-f" index "-out"))}
                    {:connector/id (str id "-s" index "-end") :connector/point end
                     :connector/domain domain :connector/shape shape :connector/size size
                     :connector/flow-direction :out
                     :connector/connected-to (when (< index (dec segment-count))
                                               (str id "-f" (inc index) "-in"))}]}))
              (range segment-count))]
    {:mep.assembly/id id :mep.assembly/system-id system-id
     :mep.assembly/segments segments :mep.assembly/fittings fittings
     :mep.assembly/open-connectors
     [(get-in segments [0 :mep/connectors 0])
      (get-in segments [(dec segment-count) :mep/connectors 1])]}))

(defn electrical-circuit
  [{:keys [id name apparent-power-va voltage-v power-factor poles phase
           length-m conductor-area-mm2]}]
  {:circuit/id id :circuit/name name :circuit/apparent-power-va apparent-power-va
   :circuit/voltage-v voltage-v :circuit/power-factor (or power-factor 1.0)
   :circuit/poles (or poles 1) :circuit/phase phase :circuit/length-m length-m
   :circuit/conductor-area-mm2 conductor-area-mm2})

(defn balance-panel
  "Assign unphased single-pole circuits greedily to the least-loaded phase."
  [phases circuits]
  (let [initial-loads (zipmap phases (repeat 0.0))]
    (reduce
     (fn [{:keys [loads assignments]} circuit]
       (if-let [phase (:circuit/phase circuit)]
         {:loads (update loads phase + (:circuit/apparent-power-va circuit))
          :assignments (assoc assignments (:circuit/id circuit) phase)}
         (let [phase (first (sort-by (juxt loads name) phases))]
           {:loads (update loads phase + (:circuit/apparent-power-va circuit))
            :assignments (assoc assignments (:circuit/id circuit) phase)})))
     {:loads initial-loads :assignments {}}
     (sort-by (juxt (comp - :circuit/apparent-power-va) (comp str :circuit/id)) circuits))))

(defn voltage-drop
  "Copper/aluminium resistive voltage drop for single- or three-phase circuit."
  [{:keys [apparent-power-va voltage-v power-factor poles length-m
           conductor-area-mm2 resistivity-ohm-m]}]
  (let [three-phase? (= poles 3)
        current (/ apparent-power-va (* voltage-v (or power-factor 1.0)
                                        (if three-phase? (sqrt 3.0) 1.0)))
        resistance (/ (* (or resistivity-ohm-m 1.724e-8) length-m)
                      (* conductor-area-mm2 1.0e-6))
        drop (* (if three-phase? (sqrt 3.0) 2.0) current resistance)
        percent (* 100.0 (/ drop voltage-v))]
    {:electrical/current-a current :electrical/voltage-drop-v drop
     :electrical/voltage-drop-percent percent}))

(defn analyze-panel
  [{:keys [id phases circuits main-rating-a demand-factor max-imbalance-percent
           max-voltage-drop-percent]}]
  (let [balance (balance-panel phases circuits)
        demanded-loads (into {} (map (fn [[phase load]] [phase (* load (or demand-factor 1.0))])
                                     (:loads balance)))
        phase-currents (into {} (map (fn [[phase load]] [phase (/ load 230.0)]) demanded-loads))
        maximum (reduce max 0.0 (vals phase-currents)) minimum (reduce min (vals phase-currents))
        average (/ (reduce + (vals phase-currents)) (count phases))
        imbalance (if (zero? average) 0.0 (* 100.0 (/ (- maximum minimum) average)))
        drops (into {} (map (fn [circuit]
                              [(:circuit/id circuit)
                               (voltage-drop {:apparent-power-va (:circuit/apparent-power-va circuit)
                                              :voltage-v (:circuit/voltage-v circuit)
                                              :power-factor (:circuit/power-factor circuit)
                                              :poles (:circuit/poles circuit)
                                              :length-m (:circuit/length-m circuit)
                                              :conductor-area-mm2 (:circuit/conductor-area-mm2 circuit)})])
                            circuits))
        issues (vec (concat
                     (when (> maximum main-rating-a)
                       [{:issue/type :electrical/panel-overload :current-a maximum}])
                     (when (> imbalance (or max-imbalance-percent 20.0))
                       [{:issue/type :electrical/phase-imbalance :percent imbalance}])
                     (for [[circuit-id drop] drops
                           :when (> (:electrical/voltage-drop-percent drop)
                                    (or max-voltage-drop-percent 3.0))]
                       {:issue/type :electrical/voltage-drop :circuit-id circuit-id
                        :percent (:electrical/voltage-drop-percent drop)})))]
    {:panel/id id :panel/assignments (:assignments balance)
     :panel/phase-loads-va demanded-loads :panel/phase-currents-a phase-currents
     :panel/imbalance-percent imbalance :panel/voltage-drops drops :panel/issues issues}))
