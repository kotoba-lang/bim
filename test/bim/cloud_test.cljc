(ns bim.cloud-test
  (:require [clojure.test :refer [deftest is]]
            [bim.cloud :as cloud]))

(deftest resumable-upload-is-idempotent-and-integrity-checked
  (let [content "ISO-10303-21;\nDATA;\n#1=IFCPROJECT('p',$,'Tower',$,$,$,$,$,$);\nENDSEC;"
        upload (cloud/create-upload {:id "up-1" :project-id "tower" :revision 9
                                     :content content :chunk-size 17})
        chunks (:upload/chunks upload)
        partial (-> (cloud/receiver upload)
                    (cloud/receive-chunk (nth chunks 2))
                    (cloud/receive-chunk (nth chunks 0))
                    (cloud/receive-chunk (nth chunks 2)))
        resumed (reduce cloud/receive-chunk partial
                        (map chunks (cloud/missing-chunks partial)))
        completed (cloud/complete-upload resumed)]
    (is (> (:upload/chunk-count upload) 3))
    (is (seq (cloud/missing-chunks partial)))
    (is (= :complete (:upload/status completed)))
    (is (= content (:upload/content completed)))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                          #"checksum mismatch"
                          (cloud/receive-chunk
                           (cloud/receiver upload)
                           (assoc (first chunks) :chunk/data "corrupt"))))))

(deftest conversion-jobs-are-idempotent-and-retryable
  (let [request {:id "job-1" :idempotency-key "tower/rev-9/ifc"
                 :kind :ifc-conversion :input {:upload-id "up-1"}}
        enqueued (cloud/enqueue-job (cloud/job-queue) request)
        duplicate (cloud/enqueue-job (:jobs/queue enqueued)
                                     (assoc request :id "job-retry"))
        running (cloud/transition-job (:jobs/queue duplicate) "job-1" :running nil)
        failed (cloud/transition-job running "job-1" :failed {:error :timeout})
        retried (cloud/transition-job failed "job-1" :queued nil)]
    (is (= :enqueued (:jobs/status enqueued)))
    (is (= :deduplicated (:jobs/status duplicate)))
    (is (= "job-1" (get-in duplicate [:jobs/job :job/id])))
    (is (= 1 (get-in running [:jobs/by-id "job-1" :job/attempt])))
    (is (= :queued (get-in retried [:jobs/by-id "job-1" :job/status])))))

(deftest bidirectional-sync-is-ordered-idempotent-and-compactable
  (let [apply-delta (fn [snapshot delta] (merge snapshot delta))
        initial (cloud/register-sync-project (cloud/sync-store) "tower" 0
                                             {:name "Tower" :status :concept})
        publication-1 {:project-id "tower" :peer-id "desktop-a"
                       :idempotency-key "desktop-a/1"
                       :base-revision 0 :target-revision 1
                       :delta {:status :design}}
        applied-1 (cloud/publish-delta initial publication-1 apply-delta)
        duplicate (cloud/publish-delta (:sync/store applied-1) publication-1 apply-delta)
        conflict (cloud/publish-delta
                  (:sync/store duplicate)
                  {:project-id "tower" :peer-id "desktop-b"
                   :idempotency-key "desktop-b/stale" :base-revision 0
                   :target-revision 1 :delta {:status :stale}}
                  apply-delta)
        publication-2 {:project-id "tower" :peer-id "cloud-itonami"
                       :idempotency-key "cloud/2"
                       :base-revision 1 :target-revision 2
                       :delta {:estimate-status :priced}}
        applied-2 (cloud/publish-delta (:sync/store conflict) publication-2 apply-delta)
        before-compaction (cloud/pull-deltas (:sync/store applied-2) "tower" 0)
        acknowledged (-> (:sync/store applied-2)
                         (cloud/acknowledge-revision "tower" "desktop-a" 2)
                         (cloud/acknowledge-revision "tower" "cloud-itonami" 1))
        compacted (cloud/compact-deltas acknowledged "tower")]
    (is (= :applied (:sync/status applied-1)))
    (is (= :deduplicated (:sync/status duplicate)))
    (is (= :conflict (:sync/status conflict)))
    (is (= 1 (get-in conflict [:sync/conflict :sync/head-revision])))
    (is (= 2 (count (:sync/deltas before-compaction))))
    (is (= {:name "Tower" :status :design :estimate-status :priced}
           (:sync/head-snapshot applied-2)))
    (is (= 1 (get-in compacted [:sync/projects "tower" :sync/compacted-through])))
    (is (= [2] (mapv :sync/revision
                     (get-in compacted [:sync/projects "tower" :sync/deltas]))))
    (is (= :snapshot-required
           (:sync/status (cloud/pull-deltas compacted "tower" 0))))
    (is (= [2] (mapv :sync/revision
                     (:sync/deltas (cloud/pull-deltas compacted "tower" 1)))))))
