(ns bim.editor
  "Pure BIM editor state: atomic serializable transactions, undo/redo,
  selection, and deterministic geometric snapping."
  (:require [clojure.set :as set]))

(def schema-version 1)

(defn editor-state [document]
  {:editor/schema-version schema-version :editor/document document
   :editor/undo [] :editor/redo [] :editor/selection #{}})

(defn- collection-indexed? [value] (or (vector? value) (sequential? value)))

(defn- apply-operation [document {:keys [op path value index]}]
  (case op
    :set
    (let [parent (get-in document (pop path)) key (peek path)
          existed? (if (map? parent) (contains? parent key)
                       (and (collection-indexed? parent) (< -1 key (count parent))))
          old (get-in document path)]
      [(assoc-in document path value)
       (if existed? {:op :set :path path :value old} {:op :dissoc :path path})])

    :dissoc
    (let [parent-path (pop path) key (peek path) parent (get-in document parent-path)]
      (when-not (and (map? parent) (contains? parent key))
        (throw (ex-info "transaction dissoc target does not exist" {:path path})))
      [(if (empty? parent-path) (dissoc document key)
           (update-in document parent-path dissoc key))
       {:op :set :path path :value (get parent key)}])

    :insert
    (let [items (vec (get-in document path)) position (or index (count items))]
      (when-not (<= 0 position (count items))
        (throw (ex-info "transaction insert index is out of bounds"
                        {:path path :index position :count (count items)})))
      [(assoc-in document path (vec (concat (subvec items 0 position) [value]
                                            (subvec items position))))
       {:op :remove :path path :index position}])

    :remove
    (let [items (vec (get-in document path))]
      (when-not (< -1 index (count items))
        (throw (ex-info "transaction remove index is out of bounds"
                        {:path path :index index :count (count items)})))
      [(assoc-in document path (vec (concat (subvec items 0 index)
                                            (subvec items (inc index)))))
       {:op :insert :path path :index index :value (nth items index)}])

    (throw (ex-info "unsupported editor operation" {:operation op}))))

(defn- apply-operations [document operations]
  (reduce (fn [[current inverses] operation]
            (let [[next-document inverse] (apply-operation current operation)]
              [next-document (conj inverses inverse)]))
          [document []] operations))

(defn transact
  "Apply all operations atomically and record their inverses as one undo unit."
  [state {:keys [id label operations] :as transaction}]
  (when-not (seq operations)
    (throw (ex-info "editor transaction requires operations" {:transaction transaction})))
  (let [[document inverses] (apply-operations (:editor/document state) operations)
        entry {:transaction/id id :transaction/label label
               :transaction/operations (vec operations)
               :transaction/inverses (vec (reverse inverses))}]
    (-> state
        (assoc :editor/document document :editor/redo [])
        (update :editor/undo conj entry))))

(defn undo [state]
  (if-let [entry (peek (:editor/undo state))]
    (let [[document _] (apply-operations (:editor/document state)
                                         (:transaction/inverses entry))]
      (-> state (assoc :editor/document document)
          (update :editor/undo pop) (update :editor/redo conj entry)))
    state))

(defn redo [state]
  (if-let [entry (peek (:editor/redo state))]
    (let [[document _] (apply-operations (:editor/document state)
                                         (:transaction/operations entry))]
      (-> state (assoc :editor/document document)
          (update :editor/redo pop) (update :editor/undo conj entry)))
    state))

(defn select
  ([state ids] (select state ids :replace))
  ([state ids mode]
   (let [ids (set ids)]
     (update state :editor/selection
             (fn [selection]
               (case mode :replace ids :add (set/union selection ids)
                     :remove (set/difference selection ids)
                     :toggle (reduce (fn [result id]
                                       (if (contains? result id) (disj result id) (conj result id)))
                                     selection ids)
                     (throw (ex-info "unsupported selection mode" {:mode mode}))))))))

(defn snap-point
  "Return the nearest endpoint/midpoint/intersection/grid candidate in tolerance."
  [point candidates {:keys [grid tolerance] :or {tolerance 0.15}}]
  (let [dimension (count point)
        grid-point (when (and (number? grid) (pos? grid))
                     (mapv #(* grid (#?(:clj Math/round :cljs js/Math.round) (/ % grid))) point))
        candidates (cond-> (vec candidates)
                     grid-point (conj {:snap/kind :grid :snap/point grid-point}))
        distance (fn [candidate]
                   (#?(:clj Math/sqrt :cljs js/Math.sqrt)
                    (reduce + (map (fn [a b] (let [delta (- a b)] (* delta delta)))
                                   point (:snap/point candidate)))))
        ranked (sort-by (juxt distance #(case (:snap/kind %) :endpoint 0 :intersection 1
                                                :midpoint 2 :grid 3 4)) candidates)
        winner (first ranked)]
    (when (and winner (= dimension (count (:snap/point winner)))
               (<= (distance winner) tolerance))
      (assoc winner :snap/distance (distance winner)))))
