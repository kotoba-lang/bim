(ns bim.worksharing
  "Element leases, role authorization, and optimistic transaction conflict
  detection for BIM worksharing."
  (:require [bim.editor :as editor]))

(def schema-version 1)
(def default-capabilities
  {:viewer #{:read} :reviewer #{:read :comment :review}
   :author #{:read :comment :edit} :manager #{:read :comment :review :edit :admin}})

(defn workspace [document memberships]
  {:worksharing/schema-version schema-version
   :worksharing/editor (editor/editor-state document)
   :worksharing/memberships memberships :worksharing/leases {}
   :worksharing/revision 0 :worksharing/history []})

(defn- capability? [state actor capability]
  (contains? (get default-capabilities (get-in state [:worksharing/memberships actor]) #{})
             capability))

(defn checkout
  "Acquire element leases atomically. Expired leases may be reclaimed."
  [state actor element-ids now lease-duration]
  (when-not (capability? state actor :edit)
    (throw (ex-info "actor is not authorized to edit" {:actor actor})))
  (let [conflicts (keep (fn [element-id]
                          (let [lease (get-in state [:worksharing/leases element-id])]
                            (when (and lease (> (:lease/expires-at lease) now)
                                       (not= actor (:lease/actor lease)))
                              {:element-id element-id :held-by (:lease/actor lease)
                               :expires-at (:lease/expires-at lease)})))
                        element-ids)]
    (when (seq conflicts)
      (throw (ex-info "elements are checked out by another actor"
                      {:actor actor :conflicts (vec conflicts)})))
    (reduce (fn [result element-id]
              (assoc-in result [:worksharing/leases element-id]
                        {:lease/actor actor :lease/acquired-at now
                         :lease/expires-at (+ now lease-duration)}))
            state element-ids)))

(defn relinquish [state actor element-ids]
  (reduce (fn [result element-id]
            (if (= actor (get-in result [:worksharing/leases element-id :lease/actor]))
              (update result :worksharing/leases dissoc element-id)
              result))
          state element-ids))

(defn- path-overlap? [left right]
  (let [common (min (count left) (count right))]
    (= (subvec (vec left) 0 common) (subvec (vec right) 0 common))))

(defn submit
  "Authorize and apply an editor transaction at a base revision. Concurrent
  transactions merge when their paths are disjoint; overlapping paths return
  a semantic conflict without mutating the workspace."
  [state actor base-revision now transaction]
  (when-not (capability? state actor :edit)
    (throw (ex-info "actor is not authorized to edit" {:actor actor})))
  (let [element-ids (set (:transaction/element-ids transaction))
        invalid-leases
        (keep (fn [element-id]
                (let [lease (get-in state [:worksharing/leases element-id])]
                  (when-not (and (= actor (:lease/actor lease))
                                 (> (:lease/expires-at lease 0) now))
                    element-id)))
              element-ids)]
    (when (seq invalid-leases)
      (throw (ex-info "transaction requires active element leases"
                      {:actor actor :element-ids (vec invalid-leases)})))
    (let [incoming-paths (mapv :path (:operations transaction))
          concurrent (filter #(> (:revision %) base-revision) (:worksharing/history state))
          conflicts (vec
                     (for [entry concurrent
                           incoming incoming-paths existing (:paths entry)
                           :when (path-overlap? incoming existing)]
                       {:incoming-path incoming :existing-path existing
                        :revision (:revision entry) :actor (:actor entry)}))]
      (if (seq conflicts)
        {:worksharing/status :conflict :worksharing/workspace state
         :worksharing/conflicts conflicts}
        (let [next-revision (inc (:worksharing/revision state))
              next-state (-> state
                             (update :worksharing/editor editor/transact transaction)
                             (assoc :worksharing/revision next-revision)
                             (update :worksharing/history conj
                                     {:revision next-revision :base-revision base-revision
                                      :actor actor :transaction/id (:id transaction)
                                      :element-ids element-ids :paths incoming-paths
                                      :timestamp now}))]
          {:worksharing/status :applied :worksharing/workspace next-state
           :worksharing/revision next-revision})))))
