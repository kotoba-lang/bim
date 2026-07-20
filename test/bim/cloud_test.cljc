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
