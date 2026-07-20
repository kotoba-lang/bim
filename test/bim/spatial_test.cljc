(ns bim.spatial-test
  (:require [clojure.test :refer [deftest is]]
            [bim :as bim]
            [bim.spatial :as spatial]))

(deftest spatial-index-culls-picks-and-plans-lod-streams
  (let [walls (mapv (fn [id]
                      (let [x (* 5.0 id)]
                        (bim/wall {:id id :start [x 0 0] :end [(+ x 4.0) 0 0]
                                   :height 3.0 :thickness 0.2})))
                    (range 20))
        index (spatial/build-index walls {:cell-size 10.0})
        visible (spatial/query index {:min [9.0 -1.0 -1.0] :max [21.0 1.0 4.0]})
        picked (spatial/nearest index [15.5 0.0 1.0] 2.0)
        batches (spatial/stream-plan index {:min [0 -1 -1] :max [100 1 4]}
                                            [0 -20 10] 900 6)]
    (is (= [1 2 3 4] (mapv :id visible)))
    (is (= 3 (:id picked)))
    (is (<= (:spatial/distance picked) 0.2))
    (is (= 4 (count batches)))
    (is (every? #(<= (count (:stream/entries %)) 6) batches))
    (is (every? #{:full :coarse :bounds}
                (map #(get-in % [:lod :lod/level]) (mapcat :stream/entries batches))))))

(deftest lod-degrades-with-distance
  (let [bounds {:min [0 0 0] :max [2 2 2]}]
    (is (= :full (:lod/level (spatial/choose-lod bounds [1 1 5] 1000 {}))))
    (is (= :bounds (:lod/level (spatial/choose-lod bounds [1 1 1000] 1000 {}))))))
