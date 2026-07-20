(ns bim.integration
  "Portable contracts that connect BIM authoring, drawings, IFC exchange,
  collaboration, and cloud-itonami. All functions are pure and CLJC-safe."
  (:require [clojure.string :as string]
            [clojure.walk :as walk]
            [bim :as bim]
            [ifc.core :as ifc]
            [kotoba.document.change :as document-change]
            [kotoba.document.collaboration :as collaboration]))

(def schema-version 1)
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))
(defn- sqrt [value] (#?(:clj Math/sqrt :cljs js/Math.sqrt) value))
(defn- math-abs [value] (#?(:clj Math/abs :cljs js/Math.abs) value))
(defn- pow [value exponent] (#?(:clj Math/pow :cljs js/Math.pow) value exponent))
(defn- log10 [value] (#?(:clj Math/log10 :cljs js/Math.log10) value))
(defn- round [value] (#?(:clj Math/round :cljs js/Math.round) value))
(defn- math-ceil [value] (#?(:clj Math/ceil :cljs js/Math.ceil) value))
(defn- math-floor [value] (#?(:clj Math/floor :cljs js/Math.floor) value))
(defn- math-sin [value] (#?(:clj Math/sin :cljs js/Math.sin) value))
(defn- math-cos [value] (#?(:clj Math/cos :cljs js/Math.cos) value))
(defn- math-tan [value] (#?(:clj Math/tan :cljs js/Math.tan) value))
(defn- math-asin [value] (#?(:clj Math/asin :cljs js/Math.asin) value))
(defn- math-acos [value] (#?(:clj Math/acos :cljs js/Math.acos) value))
(defn- math-atan [value] (#?(:clj Math/atan :cljs js/Math.atan) value))
(defn- math-atan2 [y x] (#?(:clj Math/atan2 :cljs js/Math.atan2) y x))

(def ^:private shared-parameter-guid-pattern
  #"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(defn- validate-shared-parameters [shared]
  (let [by-guid (group-by :guid (vals shared))]
    (doseq [[parameter spec] shared]
      (when-not (and (string? (:guid spec))
                     (re-matches shared-parameter-guid-pattern (:guid spec)))
        (throw (ex-info "shared parameter requires a UUID GUID"
                        {:parameter parameter :guid (:guid spec)}))))
    (when-let [[guid definitions] (first (filter #(> (count (val %)) 1) by-guid))]
      (throw (ex-info "duplicate shared parameter GUID"
                      {:guid guid :definitions definitions})))
    shared))

(defn family-definition
  [{:keys [id name category parameters formulas reference-planes sketches constraints
           adaptive host types template shared-parameters]}]
  (let [shared (validate-shared-parameters (or shared-parameters {}))]
    (when-let [parameter (first (filter (set (keys shared)) (keys parameters)))]
      (throw (ex-info "shared and local parameter names conflict"
                      {:parameter parameter})))
    {:family/id id :family/name name :family/category category
     :family/parameters (merge shared (or parameters {}))
     :family/shared-parameters shared :family/formulas (or formulas {})
     :family/reference-planes (or reference-planes {})
     :family/sketches (or sketches {}) :family/adaptive adaptive :family/host host
     :family/constraints (vec constraints) :family/types (or types {})
     :family/template template :family/schema-version schema-version}))

(defn family-catalog [families]
  (let [ids (map :family/id families)
        shared (mapcat (comp vals :family/shared-parameters) families)
        by-guid (group-by :guid shared)]
    (when-not (= (count ids) (count (distinct ids)))
      (throw (ex-info "duplicate family id in catalog" {:ids ids})))
    (doseq [[guid definitions] by-guid]
      (when (> (count (set (map #(select-keys % [:type :name]) definitions))) 1)
        (throw (ex-info "conflicting shared parameter definition"
                        {:guid guid :definitions definitions}))))
    {:family-catalog/schema-version schema-version
     :family-catalog/families (into {} (map (juxt :family/id identity) families))}))

(defn- parse-csv-row [row]
  (loop [characters (seq row) quoted? false field [] fields []]
    (if-let [character (first characters)]
      (let [next-character (second characters)]
        (cond
          (and quoted? (= character \" ) (= next-character \"))
          (recur (nnext characters) quoted? (conj field \" ) fields)
          (= character \" ) (recur (next characters) (not quoted?) field fields)
          (and (= character \,) (not quoted?))
          (recur (next characters) quoted? [] (conj fields (apply str field)))
          :else (recur (next characters) quoted? (conj field character) fields)))
      (conj fields (apply str field)))))

(def ^:private catalog-unit-scale
  {"MILLIMETERS" 0.001 "CENTIMETERS" 0.01 "METERS" 1.0
   "MILLIMETRES" 0.001 "CENTIMETRES" 0.01 "METRES" 1.0
   "DEGREES" (/ pi 180.0) "RADIANS" 1.0})

(defn- catalog-value-scale [spec unit]
  (let [scale (get catalog-unit-scale (string/upper-case (or unit "")) 1.0)]
    (case (:type spec) :area (* scale scale) :volume (* scale scale scale) scale)))

(defn- parse-catalog-value [spec unit value]
  (let [number-value #(when-not (string/blank? %)
                        (#?(:clj Double/parseDouble :cljs js/parseFloat) %))
        scale (catalog-value-scale spec unit)]
    (case (:type spec)
      :boolean (contains? #{"1" "true" "yes"} (string/lower-case value))
      :integer (long (number-value value))
      (:length :angle :area :volume :number) (some-> (number-value value) (* scale))
      :enum (let [allowed (:allowed spec)]
              (or (some #(when (= (name %) value) %) allowed) value))
      value)))

(defn import-family-type-catalog
  "Import a Revit-style type catalog CSV into `:family/types`. Header cells use
  `Parameter##TYPE##UNIT`; the first column contains the type name."
  [family csv]
  (let [rows (mapv parse-csv-row (remove string/blank? (string/split-lines csv)))
        header (first rows)
        _ (when (or (empty? rows) (< (count header) 2))
            (throw (ex-info "type catalog requires a header and parameter columns" {})))
        _ (doseq [row (rest rows)]
            (when-not (= (count header) (count row))
              (throw (ex-info "type catalog row has the wrong column count"
                              {:row row :expected (count header)}))))
        type-keys (mapv #(-> (first %) string/lower-case
                             (string/replace #"[^a-z0-9]+" "-") keyword)
                        (rest rows))
        _ (when-not (= (count type-keys) (count (distinct type-keys)))
            (throw (ex-info "type catalog contains duplicate type names"
                            {:types type-keys})))
        descriptors (mapv (fn [cell]
                            (let [[parameter value-type unit] (string/split cell #"##" -1)]
                              {:parameter (-> parameter string/lower-case
                                              (string/replace #"[^a-z0-9]+" "-") keyword)
                               :value-type value-type :unit unit}))
                          (rest header))
        types
        (into {}
              (map (fn [row]
                     (let [type-name (first row)
                           type-key (-> type-name string/lower-case
                                        (string/replace #"[^a-z0-9]+" "-") keyword)
                           parameters
                           (into {}
                                 (map (fn [descriptor value]
                                        (let [parameter (:parameter descriptor)
                                              spec (get-in family [:family/parameters parameter])]
                                          (when-not spec
                                            (throw (ex-info "type catalog references unknown parameter"
                                                            {:parameter parameter})))
                                          [parameter (parse-catalog-value
                                                      spec (:unit descriptor) value)]))
                                      descriptors (rest row)))]
                       [type-key {:id (str (:family/id family) ":" (name type-key))
                                  :name type-name :parameters parameters}])))
              (rest rows))]
    (update family :family/types merge types)))

(defn- csv-cell [value]
  (let [value (str value)]
    (if (re-find #"[,\"\r\n]" value)
      (str "\"" (string/replace value "\"" "\"\"") "\"") value)))

(defn export-family-type-catalog
  "Export named family types as deterministic Revit-style CSV."
  [family]
  (let [parameters (->> (:family/parameters family)
                        (filter (fn [[_ spec]] (= :type (:scope spec))))
                        (sort-by (comp name key)))
        unit-name (fn [spec]
                    (or (:catalog-unit spec)
                        (case (:type spec) :length "METERS" :angle "RADIANS" "")))
        type-name (fn [spec]
                    (case (:type spec) :length "LENGTH" :angle "ANGLE"
                          :integer "INTEGER" :boolean "YESNO" :text "TEXT"
                          :material "MATERIAL" :number "NUMBER" "OTHER"))
        header (cons "" (map (fn [[parameter spec]]
                               (str (name parameter) "##" (type-name spec) "##" (unit-name spec)))
                             parameters))
        render-value (fn [spec value]
                       (let [scale (catalog-value-scale spec (unit-name spec))
                             value (if (and (number? value) (not= 1.0 scale))
                                     (/ value scale) value)]
                         (if (boolean? value) (if value 1 0) value)))
        rows (for [[type-key type-spec] (sort-by (comp name key) (:family/types family))]
               (cons (or (:name type-spec) (name type-key))
                     (map (fn [[parameter spec]]
                            (render-value spec (get-in type-spec [:parameters parameter])))
                          parameters)))]
    (str (string/join "\n" (map #(string/join "," (map csv-cell %)) (cons header rows))) "\n")))

(defn- eval-expr [params expression]
  (cond
    (and (vector? expression) (= :param (first expression)))
    (get params (second expression))

    (vector? expression)
    (let [[op & operands] expression]
      (case op
        :if (eval-expr params (if (eval-expr params (first operands))
                                (second operands) (nth operands 2)))
        :and (every? true? (map #(boolean (eval-expr params %)) operands))
        :or (boolean (some #(eval-expr params %) operands))
        :not (not (eval-expr params (first operands)))
        (let [values (mapv #(eval-expr params %) operands)]
          (case op
            :+ (reduce + values) :- (reduce - values) :* (reduce * values)
            :/ (reduce / values) :min (reduce min values) :max (reduce max values)
            :pow (pow (first values) (second values))
            :sqrt (sqrt (first values)) :abs (math-abs (first values))
            :round (round (first values)) :ceil (math-ceil (first values))
            :floor (math-floor (first values))
            :sin (math-sin (first values)) :cos (math-cos (first values))
            :tan (math-tan (first values)) :asin (math-asin (first values))
            :acos (math-acos (first values)) :atan (math-atan (first values))
            :atan2 (math-atan2 (first values) (second values))
            := (apply = values) :not= (apply not= values)
            :< (apply < values) :<= (apply <= values)
            :> (apply > values) :>= (apply >= values)
            (throw (ex-info "unsupported family expression" {:expression expression}))))))

    :else expression))

(defn- expression-parameters [expression]
  (cond
    (and (vector? expression) (= :param (first expression))) #{(second expression)}
    (coll? expression) (into #{} (mapcat expression-parameters expression))
    :else #{}))

(defn- validate-parameter! [name spec value]
  (let [valid-type? (case (:type spec)
                      (:length :angle :area :volume :number :integer) (number? value)
                      :boolean (boolean? value)
                      (:text :material) (string? value)
                      :enum (contains? (set (:allowed spec)) value)
                      true)]
    (when-not valid-type?
      (throw (ex-info "family parameter has invalid type"
                      {:parameter name :type (:type spec) :value value})))
    (when (and (number? value) (number? (:min spec)) (< value (:min spec)))
      (throw (ex-info "family parameter is below minimum"
                      {:parameter name :minimum (:min spec) :value value})))
    (when (and (number? value) (number? (:max spec)) (> value (:max spec)))
      (throw (ex-info "family parameter is above maximum"
                      {:parameter name :maximum (:max spec) :value value})))
    value))

(defn- layout-expression-dependencies [expression]
  (cond
    (and (vector? expression) (= :reference (first expression))) #{(second expression)}
    (coll? expression) (into #{} (mapcat layout-expression-dependencies expression))
    :else #{}))

(defn- eval-layout-expression [params planes expression]
  (cond
    (and (vector? expression) (= :reference (first expression)))
    (get-in planes [(second expression) :offset])

    (vector? expression)
    (let [[op & operands] expression
          values (map #(eval-layout-expression params planes %) operands)]
      (case op
        :param (get params (first operands))
        :+ (reduce + values)
        :- (reduce - values)
        :* (reduce * values)
        :/ (reduce / values)
        :min (reduce min values)
        :max (reduce max values)
        (throw (ex-info "unsupported family layout expression" {:expression expression}))))

    :else expression))

(defn resolve-reference-planes
  "Resolve named datum planes in dependency order. Plane offsets may reference
  parameters or already resolved planes. Distance and coincident constraints
  can solve a plane whose offset is intentionally left unspecified."
  [family params]
  (loop [resolved {} pending (:family/reference-planes family)]
    (if (empty? pending)
      resolved
      (let [ready (into {}
                        (filter (fn [[_ plane]]
                                  (and (some? (:offset plane))
                                       (every? #(contains? resolved %)
                                               (layout-expression-dependencies
                                                (:offset plane))))))
                        pending)]
        (if (seq ready)
          (recur (reduce-kv (fn [planes name plane]
                              (let [offset (eval-layout-expression params planes (:offset plane))]
                                (when-not (number? offset)
                                  (throw (ex-info "reference plane offset must be numeric"
                                                  {:plane name :offset offset})))
                                (assoc planes name (assoc plane :name name :offset offset))))
                            resolved ready)
                 (apply dissoc pending (keys ready)))
          (let [solutions
                (reduce
                 (fn [result constraint]
                   (let [kind (:kind constraint)
                         [left-name right-name]
                         (case kind
                           :distance [(:from constraint) (:to constraint)]
                           :coincident [(:left constraint) (:right constraint)]
                           [nil nil])
                         left (get-in resolved [left-name :offset])
                         right (get-in resolved [right-name :offset])
                         expression (when (= :distance kind) (:value constraint))
                         expression-ready? (every? #(contains? resolved %)
                                                   (layout-expression-dependencies expression))
                         distance (when (and (= :distance kind) expression-ready?)
                                    (eval-layout-expression params resolved expression))
                         direction (or (:direction constraint) 1.0)]
                     (cond
                       (and (= :distance kind) (number? distance) (number? left)
                            (contains? pending right-name))
                       (assoc result right-name (+ left (* direction distance)))

                       (and (= :distance kind) (number? distance) (number? right)
                            (contains? pending left-name))
                       (assoc result left-name (- right (* direction distance)))

                       (and (= :coincident kind) (number? left)
                            (contains? pending right-name))
                       (assoc result right-name left)

                       (and (= :coincident kind) (number? right)
                            (contains? pending left-name))
                       (assoc result left-name right)

                       :else result)))
                 {} (:family/constraints family))]
            (when (empty? solutions)
              (throw (ex-info "reference plane dependency cycle or under-constrained layout"
                              {:pending (keys pending)})))
            (recur (reduce-kv (fn [planes name offset]
                                (assoc planes name
                                       (assoc (get pending name) :name name :offset offset
                                              :solved-by :constraint)))
                              resolved solutions)
                   (apply dissoc pending (keys solutions)))))))))

(defn- validate-constraints! [family params planes]
  (doseq [constraint (:family/constraints family)]
    (case (:kind constraint)
      :equal (let [left (eval-expr params (:left constraint))
                   right (eval-expr params (:right constraint))
                   tolerance (or (:tolerance constraint) 1.0e-9)]
               (when (> (#?(:clj Math/abs :cljs js/Math.abs) (- left right)) tolerance)
                 (throw (ex-info "family equality constraint failed"
                                 {:constraint constraint :left left :right right}))))
      :range (let [value (get params (:parameter constraint))]
               (when (or (and (number? (:min constraint)) (< value (:min constraint)))
                         (and (number? (:max constraint)) (> value (:max constraint))))
                 (throw (ex-info "family range constraint failed"
                                 {:constraint constraint :value value}))))
      :distance (let [from (get-in planes [(:from constraint) :offset])
                      to (get-in planes [(:to constraint) :offset])
                      expected (eval-layout-expression params planes (:value constraint))
                      actual (when (and (number? from) (number? to))
                               (math-abs (- to from)))
                      tolerance (or (:tolerance constraint) 1.0e-9)]
                  (when (or (nil? actual) (not (number? expected))
                            (> (math-abs (- actual expected)) tolerance))
                    (throw (ex-info "family distance constraint failed"
                                    {:constraint constraint :actual actual :expected expected}))))
      :coincident (let [left (get-in planes [(:left constraint) :offset])
                        right (get-in planes [(:right constraint) :offset])
                        tolerance (or (:tolerance constraint) 1.0e-9)]
                    (when (or (nil? left) (nil? right) (> (math-abs (- left right)) tolerance))
                      (throw (ex-info "family coincident constraint failed"
                                      {:constraint constraint :left left :right right}))))
      (throw (ex-info "unsupported family constraint" {:constraint constraint}))))
  {:parameters params :reference-planes planes})

(defn resolve-family-sketches
  "Resolve parameterized 2D point loops into closed IFC-compatible profiles."
  [family params planes]
  (into {}
        (map (fn [[sketch-name sketch]]
               (let [points
                     (into {}
                           (map (fn [[point-name coordinates]]
                                  (let [point (mapv #(eval-layout-expression params planes %)
                                                    coordinates)]
                                    (when-not (and (= 2 (count point)) (every? number? point))
                                      (throw (ex-info "family sketch point must resolve to 2D numbers"
                                                      {:sketch sketch-name :point point-name
                                                       :coordinates point})))
                                    [point-name point])))
                           (:points sketch))
                     loop-names (vec (:loop sketch))
                     loop-points (mapv points loop-names)]
                 (when (or (< (count loop-names) 3) (some nil? loop-points)
                           (not= (count loop-names) (count (distinct loop-names))))
                   (throw (ex-info "family sketch must be a closed loop of unique named points"
                                   {:sketch sketch-name :loop loop-names})))
                 (doseq [constraint (:constraints sketch)]
                   (let [point #(get points %)
                         vector-between (fn [[from to]]
                                          (mapv - (point to) (point from)))
                         length (fn [segment]
                                  (sqrt (reduce + (map #(* % %) (vector-between segment)))))
                         [a b] (map point [(:from constraint) (:to constraint)])
                         tolerance (or (:tolerance constraint) 1.0e-9)
                         failed! #(throw (ex-info % {:sketch sketch-name
                                                     :constraint constraint}))]
                     (case (:kind constraint)
                       :horizontal
                       (when (> (math-abs (- (second a) (second b))) tolerance)
                         (failed! "family sketch horizontal constraint failed"))
                       :vertical
                       (when (> (math-abs (- (first a) (first b))) tolerance)
                         (failed! "family sketch vertical constraint failed"))
                       :coincident
                       (when (> (length [(:from constraint) (:to constraint)]) tolerance)
                         (failed! "family sketch coincident constraint failed"))
                       :distance
                       (let [actual (length [(:from constraint) (:to constraint)])
                             expected (eval-layout-expression params planes (:value constraint))]
                         (when (> (math-abs (- actual expected)) tolerance)
                           (failed! "family sketch distance constraint failed")))
                       :equal-length
                       (when (> (math-abs (- (length (:first constraint))
                                             (length (:second constraint)))) tolerance)
                         (failed! "family sketch equal-length constraint failed"))
                       (:parallel :perpendicular)
                       (let [[ax ay] (vector-between (:first constraint))
                             [bx by] (vector-between (:second constraint))
                             cross (- (* ax by) (* ay bx))
                             dot (+ (* ax bx) (* ay by))
                             residual (if (= :parallel (:kind constraint)) cross dot)]
                         (when (> (math-abs residual) tolerance)
                           (failed! (str "family sketch " (name (:kind constraint))
                                         " constraint failed"))))
                       (throw (ex-info "unsupported family sketch constraint"
                                       {:sketch sketch-name :constraint constraint})))))
                 [sketch-name
                  {:kind :arbitrary-closed
                   :name (or (:name sketch) (name sketch-name))
                   :plane (or (:plane sketch) :xy)
                   :points (conj loop-points (first loop-points))}]))
        (:family/sketches family))))

(defn resolve-family-parameters
  "Resolve defaults and overrides, then formulas by dependency order. Cycles,
  missing references, invalid values, and failed dimensional constraints throw."
  ([family overrides] (resolve-family-parameters family nil overrides false))
  ([family type-key overrides enforce-scopes?]
   (let [specs (:family/parameters family)
         type-spec (get-in family [:family/types type-key])
         type-overrides (or (:parameters type-spec) {})
         formula-names (set (keys (:family/formulas family)))]
     (when-let [formula-override
                (first (filter formula-names (concat (keys type-overrides) (keys overrides))))]
       (throw (ex-info "formula-driven parameter cannot be overridden"
                       {:parameter formula-override :type type-key})))
     (when enforce-scopes?
       (doseq [name (keys type-overrides)]
         (when (= :instance (get-in specs [name :scope]))
           (throw (ex-info "type cannot override instance parameter" {:parameter name :type type-key}))))
       (doseq [name (keys overrides)]
         (when (= :type (get-in specs [name :scope]))
           (throw (ex-info "instance cannot override type parameter" {:parameter name :type type-key})))))
     (let [initial (apply dissoc
                          (merge (into {} (map (fn [[k spec]] [k (:default spec)]) specs))
                                 type-overrides overrides)
                          formula-names)
           params
           (loop [params initial pending (:family/formulas family)]
             (if (empty? pending)
               params
               (let [ready (into {} (filter (fn [[_ expression]]
                                              (every? #(contains? params %)
                                                      (expression-parameters expression))) pending))]
                 (when (empty? ready)
                   (throw (ex-info "family formula dependency cycle or missing parameter"
                                   {:pending (keys pending)})))
                 (recur (reduce-kv (fn [result name expression]
                                     (assoc result name (eval-expr result expression)))
                                   params ready)
                        (apply dissoc pending (keys ready))))))]
       (doseq [[name spec] specs]
         (validate-parameter! name spec (get params name)))
       (let [planes (resolve-reference-planes family params)]
         (:parameters (validate-constraints! family params planes)))))))

(declare instantiate-family*)

(defn- array-item [item array index]
  (let [kind (or (:kind array) :linear)
        location (vec (get-in item [:placement :location] [0.0 0.0 0.0]))
        item
        (case kind
          :linear
          (let [raw-direction (vec (or (:direction array) [1.0 0.0 0.0]))
                magnitude (sqrt (reduce + (map #(* % %) raw-direction)))
                direction (mapv #(/ % magnitude) raw-direction)
                spacing (:spacing array)]
            (assoc-in item [:placement :location]
                      (mapv + location (mapv #(* index spacing %) direction))))
          :radial
          (let [[cx cy cz] (vec (or (:center array) [0.0 0.0 0.0]))
                [x y z] location angle (* index (:angle array))
                cosine (math-cos angle) sine (math-sin angle)
                rotated [(+ cx (* (- x cx) cosine) (* -1.0 (- y cy) sine))
                         (+ cy (* (- x cx) sine) (* (- y cy) cosine))
                         (+ cz (- z cz))]
                [rx ry rz] (get-in item [:placement :ref-direction] [1.0 0.0 0.0])]
            (-> item
                (assoc-in [:placement :location] rotated)
                (assoc-in [:placement :ref-direction]
                          [(- (* rx cosine) (* ry sine))
                           (+ (* rx sine) (* ry cosine)) rz])))
          (throw (ex-info "unsupported family array kind" {:kind kind})))]
    (assoc item :id (str (or (:id item) "array-item") "-" index)
                :family/array-index index)))

(defn- materialize-template [catalog value stack]
  (cond
    (and (map? value) (:family/array value))
    (let [{:keys [item] :as array} (:family/array value)
          array-count (:count array)
          kind (or (:kind array) :linear)]
      (when-not (and (number? array-count) (== array-count (long array-count))
                     (<= 1 array-count 10000))
        (throw (ex-info "family array count must be an integer from 1 to 10000"
                        {:count array-count})))
      (when (and (= :linear kind)
                 (or (not (number? (:spacing array)))
                     (not= 3 (count (:direction array)))
                     (not (every? number? (:direction array)))
                     (zero? (reduce + (map #(* % %) (:direction array))))))
        (throw (ex-info "linear family array requires spacing and a 3D direction"
                        {:array array})))
      (when (and (= :radial kind) (not (number? (:angle array))))
        (throw (ex-info "radial family array requires an angle step" {:array array})))
      (mapv (fn [index]
              (array-item (materialize-template catalog item stack) array index))
            (range (long array-count))))

    (and (map? value) (:family/ref value))
    (let [family-id (:family/ref value)]
      (when (contains? stack family-id)
        (throw (ex-info "nested family cycle" {:family-id family-id :stack stack})))
      (let [family (get-in catalog [:family-catalog/families family-id])]
        (when-not family (throw (ex-info "nested family not found" {:family-id family-id})))
        (instantiate-family* catalog family (:family/type value) (:id value)
                             (:overrides value) (conj stack family-id))))
    (map? value) (into (empty value) (map (fn [[k v]] [k (materialize-template catalog v stack)]) value))
    (vector? value) (mapv #(materialize-template catalog % stack) value)
    (seq? value) (map #(materialize-template catalog % stack) value)
    :else value))

(defn- instantiate-family* [catalog family type-key instance-id overrides stack]
  (let [params (resolve-family-parameters family type-key overrides (some? catalog))
        planes (resolve-reference-planes family params)
        sketches (resolve-family-sketches family params planes)
        substituted (walk/postwalk #(cond
                                      (and (vector? %) (= :param (first %)))
                                      (get params (second %))
                                      (and (vector? %) (= :reference (first %)))
                                      (get-in planes [(second %) :offset])
                                      (and (vector? %) (= :sketch-profile (first %)))
                                      (get sketches (second %))
                                      (and (vector? %) (contains? #{:+ :- :* :/ :min :max
                                                                   :pow :sqrt :abs :round
                                                                   :ceil :floor :sin :cos :tan
                                                                   :asin :acos :atan :atan2
                                                                   := :not= :< :<= :> :>=
                                                                   :if :and :or :not}
                                                                  (first %)))
                                      (eval-expr {} %)
                                      :else %)
                                   (:family/template family))
        body (materialize-template catalog substituted stack)
        type-spec (get-in family [:family/types type-key])]
    (cond-> (assoc body :id instance-id
                        :family/id (:family/id family)
                        :family/type type-key
                        :family/parameters params
                        :family/reference-planes planes
                        :family/sketches sketches
                        :family/host (:family/host family))
      type-spec (assoc :type-object
                       {:id (or (:id type-spec) (str (:family/id family) ":" (name type-key)))
                        :global-id (:global-id type-spec)
                        :name (or (:name type-spec) (name type-key))
                        :element-type (:family/name family)
                        :predefined-type (:predefined-type type-spec)}))))

(defn instantiate-family
  "Materialize a serializable family template. A value shaped as [:param :x]
  is replaced by the resolved parameter value."
  [family instance-id overrides]
  (instantiate-family* nil family nil instance-id overrides #{(:family/id family)}))

(defn instantiate-family-type
  "Instantiate a named family type from a catalog with instance overrides and
  recursively materialize nested family references."
  [catalog family-id type-key instance-id overrides]
  (let [family (get-in catalog [:family-catalog/families family-id])]
    (when-not family (throw (ex-info "family not found" {:family-id family-id})))
    (instantiate-family* catalog family type-key instance-id overrides #{family-id})))

(defn instantiate-adaptive-family
  "Drive an adaptive family template from ordered 3D placement points. Markers
  `[:adaptive-point n]` and `[:adaptive-path]` are materialized recursively."
  [family instance-id overrides placement-points]
  (let [{:keys [min-points max-points closed?]} (:family/adaptive family)
        points (mapv vec placement-points)
        count-points (count points)]
    (when-not (:family/adaptive family)
      (throw (ex-info "family is not adaptive" {:family-id (:family/id family)})))
    (when (or (< count-points (or min-points 2))
              (and max-points (> count-points max-points))
              (some #(or (not= 3 (count %)) (not-every? number? %)) points))
      (throw (ex-info "adaptive family placement points are invalid"
                      {:family-id (:family/id family) :point-count count-points
                       :min-points min-points :max-points max-points})))
    (when (some #(= (first %) (second %)) (partition 2 1 points))
      (throw (ex-info "adaptive family has coincident consecutive points"
                      {:family-id (:family/id family)})))
    (let [path (cond-> points closed? (conj (first points)))
          instance (instantiate-family family instance-id overrides)]
      (-> (walk/postwalk (fn [value]
                           (cond
                             (and (vector? value) (= :adaptive-point (first value)))
                             (or (get points (second value))
                                 (throw (ex-info "adaptive point index is out of range"
                                                 {:index (second value) :count count-points})))
                             (and (vector? value) (= [:adaptive-path] value)) path
                             :else value))
                         instance)
          (assoc :family/adaptive-points points :family/adaptive-path path)))))

(defn place-hosted-family
  "Instantiate and attach a hosted family to an allowed host category and face."
  [family instance-id overrides host {:keys [face placement]}]
  (let [{:keys [kinds faces required?]} (:family/host family)
        host-kind (:kind host)]
    (when (and required? (nil? host))
      (throw (ex-info "family requires a host" {:family-id (:family/id family)})))
    (when (and (seq kinds) (not (contains? (set kinds) host-kind)))
      (throw (ex-info "family host kind is not allowed"
                      {:family-id (:family/id family) :host-kind host-kind :allowed kinds})))
    (when (and (seq faces) (not (contains? (set faces) face)))
      (throw (ex-info "family host face is not allowed"
                      {:family-id (:family/id family) :face face :allowed faces})))
    (assoc (instantiate-family family instance-id overrides)
           :host/id (:id host) :host/global-id (:global-id host)
           :host/face face :placement (or placement :identity))))

(defn apply-family-voids
  "Apply a hosted instance's void solids to its host using serializable CSG."
  [host instance]
  (cond
    (empty? (:voids instance)) host

    (= :wall (:kind host))
    (reduce
     (fn [result void]
       (let [points (get-in void [:geometry :profile :points])
             xs (map first points) ys (map second points)
             [offset _ sill] (get-in instance [:placement :location] [0.0 0.0 0.0])]
         (when-not (and (seq points) (every? number? (concat xs ys)))
           (throw (ex-info "hosted wall void requires a resolved 2D profile"
                           {:void-id (:id void)})))
         (bim/add-opening-to-wall
          result
          (bim/rectangular-opening
           {:id (:id void) :offset (+ offset (reduce min xs))
            :sill (+ sill (reduce min ys))
            :width (- (reduce max xs) (reduce min xs))
            :height (- (reduce max ys) (reduce min ys))
            :depth (get-in void [:geometry :depth])
            :filled-by (:id instance)}))))
     host (:voids instance))

    :else
    (update host :geometry
            (fn [geometry]
              (reduce (fn [result void]
                        {:kind :boolean-result :operator :difference
                         :first-operand result :second-operand (:geometry void)
                         :void/id (:id void)})
                      geometry (:voids instance))))))

(defn view-template
  [{:keys [id name discipline scale detail-level hidden-line? show-tags?
           category-visibility category-overrides annotation-style]}]
  {:view-template/id id :view-template/name name
   :view-template/discipline (or discipline :architectural)
   :view-template/scale (or scale 100)
   :view-template/detail-level (or detail-level :medium)
   :view-template/hidden-line? (if (nil? hidden-line?) true hidden-line?)
   :view-template/show-tags? (if (nil? show-tags?) true show-tags?)
   :view-template/category-visibility (or category-visibility {})
   :view-template/category-overrides (or category-overrides {})
   :view-template/annotation-style (or annotation-style {})})

(defn drawing-view
  [{:keys [id kind name scale storey-id building-id section-box cut-plane direction
           discipline annotations template-id overrides]}]
  {:view/id id :view/kind kind :view/name name :view/scale (or scale 100)
   :view/scale-explicit? (some? scale)
   :view/storey-id storey-id :view/building-id building-id
   :view/section-box section-box :view/cut-plane cut-plane :view/direction direction
   :view/annotations (vec annotations)
   :view/discipline (or discipline :architectural)
   :view/template-id template-id :view/overrides (or overrides {})})

(defn apply-view-template
  "Resolve a drawing view and template into renderer options. View overrides
  win without mutating the shared template."
  [view template]
  (when (and (:view/template-id view)
             (not= (:view/template-id view) (:view-template/id template)))
    (throw (ex-info "view template id does not match" {:view (:view/id view)})))
  (merge {:scale (:view-template/scale template)
          :discipline (:view-template/discipline template)
          :detail-level (:view-template/detail-level template)
          :hidden-line? (:view-template/hidden-line? template)
          :show-tags? (:view-template/show-tags? template)
          :category-visibility (:view-template/category-visibility template)
          :category-overrides (:view-template/category-overrides template)
          :annotation-style (:view-template/annotation-style template)
          :annotations (:view/annotations view)}
         (:view/overrides view)
         (when (:view/scale-explicit? view) {:scale (:view/scale view)})))

(defn drawing-sheet [{:keys [id number name size views revisions]}]
  {:sheet/id id :sheet/number number :sheet/name name :sheet/size (or size :a1)
   :sheet/views (vec views) :sheet/revisions (vec revisions)})

(defn element-schedule
  "Create a deterministic quantity schedule grouped by selected element fields."
  [{:keys [id name elements fields] group-fields :group-by}]
  (let [group-keys (vec (or group-fields [:kind :name]))
        fields (vec (or fields
                        [{:key :kind :heading "Category"}
                         {:key :name :heading "Type / Name"}
                         {:key :count :heading "Count"}]))
        rows (->> elements
                  (group-by #(mapv (fn [key] (get % key)) group-keys))
                  (map (fn [[values grouped]]
                         (merge (zipmap group-keys values)
                                {:count (count grouped)
                                 :element-ids (mapv :id grouped)})))
                  (sort-by #(mapv (fn [key] (str (get % key))) group-keys))
                  vec)]
    {:schedule/id id :schedule/name name :schedule/fields fields
     :schedule/group-by group-keys :schedule/rows rows
     :schedule/total-count (reduce + 0 (map :count rows))}))

(defn- all-storeys [project]
  (mapcat :storeys (mapcat :buildings (:sites project))))

(defn- all-elements [project]
  (mapcat :elements (all-storeys project)))

(defn generate-drawing-set
  "Generate deterministic plan, section and elevation view definitions plus
  coordinated sheets. Renderable SVG views are produced by bim.drawing."
  [project]
  (let [buildings (mapcat :buildings (:sites project))
        plans (mapv (fn [storey]
                      (drawing-view {:id (str "plan-" (:id storey)) :kind :floor-plan
                                     :name (str (:name storey) " Plan") :storey-id (:id storey)
                                     :cut-plane {:elevation (+ (:elevation storey) 1.2)}}))
                    (all-storeys project))
        orthographic
        (mapcat (fn [building]
                  [(drawing-view {:id (str "section-" (:id building) "-a") :kind :section
                                  :name (str (:name building) " Section A")
                                  :building-id (:id building) :scale 50
                                  :cut-plane {:axis :x :position 0.0} :direction :north})
                   (drawing-view {:id (str "elevation-" (:id building) "-north") :kind :elevation
                                  :name (str (:name building) " North Elevation")
                                  :building-id (:id building) :scale 100 :direction :north})])
                buildings)
        views (vec (concat plans orthographic))
        schedules [(element-schedule {:id "schedule-elements" :name "Element Schedule"
                                      :elements (all-elements project)})]]
    {:drawing/schema-version schema-version
     :drawing/views views
     :drawing/schedules schedules
     :drawing/sheets [(drawing-sheet {:id "A-001" :number "A-001"
                                      :name "General Arrangements"
                                      :views (conj (mapv :view/id views) "schedule-elements")
                                      :revisions [{:revision "P01" :status :preliminary}]})]}))

(defn- exported-geometry [element]
  (let [geometry (:geometry element)]
    (case (:kind geometry)
      :axis-sweep
      (let [[[x0 y0 z0] [x1 y1 _]] (:axis geometry)
            thickness (get-in geometry [:profile :thickness])
            height (get-in geometry [:profile :height])
            dx (- x1 x0) dy (- y1 y0)
            length (#?(:clj Math/sqrt :cljs js/Math.sqrt) (+ (* dx dx) (* dy dy)))
            px (* (/ (- dy) length) (/ thickness 2.0))
            py (* (/ dx length) (/ thickness 2.0))]
        {:kind :extruded-area-solid
         :profile {:kind :arbitrary-closed :name "Wall footprint"
                   :points [[(+ x0 px) (+ y0 py)] [(+ x1 px) (+ y1 py)]
                            [(- x1 px) (- y1 py)] [(- x0 px) (- y0 py)]
                            [(+ x0 px) (+ y0 py)]]}
         :position {:location [0.0 0.0 z0]} :direction [0.0 0.0 1.0] :depth height})
      :slab-extrusion
      (let [boundary (:boundary geometry) z (nth (first boundary) 2 0.0)]
        {:kind :extruded-area-solid
         :profile {:kind :arbitrary-closed :name "Slab footprint"
                   :points (mapv (fn [[x y _]] [x y])
                                 (conj (vec boundary) (first boundary)))}
         :position {:location [0.0 0.0 z]} :direction [0.0 0.0 1.0]
         :depth (:thickness geometry)})
      geometry)))

(defn- exported-property [value]
  (case (:kind value)
    :enum-list {:kind :enumerated :values (:values value)
                :value-type (or (:value-type value) :ifclabel)
                :enumeration {:name (or (:enumeration-name value) "AllowedValues")
                              :values (vec (:allowed value))}}
    :bounded {:kind :bounded :lower (:lower value) :upper (:upper value)
              :set-point (:set-point value) :value-type (:value-type value)
              :unit (:ifc/unit value)}
    :list {:kind :list :values (:values value) :value-type (:value-type value)
           :unit (:ifc/unit value)}
    :enum {:kind :enumerated :values [(:value value)] :value-type :ifclabel
           :enumeration {:name "AllowedValues" :values (vec (:allowed value))}}
    {:kind :single :value (:value value)
     :value-type (case (:kind value)
                   :bool :ifcboolean :int :ifcinteger :real :ifcreal
                   :measured (case (:unit value) :metre :ifclengthmeasure :ifcreal)
                   :ifclabel)
     :unit (:ifc/unit value)}))

(defn- exported-psets [element]
  (into {}
        (map (fn [[pset-name pset]]
               [pset-name {:name pset-name
                      :properties (into {}
                                        (map (fn [[property-name value]]
                                               [(clojure.core/name property-name)
                                                (exported-property value)]))
                                        (:props pset))}]))
        (:psets element)))

(defn- quantity-kind [quantity-name]
  (let [name (string/lower-case (clojure.core/name quantity-name))]
    (cond
      (string/includes? name "area") :area
      (string/includes? name "volume") :volume
      (string/includes? name "weight") :weight
      (string/includes? name "time") :time
      (string/includes? name "count") :count
      :else :length)))

(defn- exported-quantity-sets [element]
  (if-let [source (:ifc/quantity-sets element)]
    (into {}
          (map (fn [[qset-name qset]]
                 [qset-name
                  (update qset :quantities
                          (fn [quantities]
                            (into {}
                                  (map (fn [[name quantity]]
                                         [name
                                          (if (contains? (:quantities element)
                                                         (keyword name))
                                            (assoc quantity :value
                                                   (get (:quantities element)
                                                        (keyword name)))
                                            quantity)]))
                                  quantities)))]))
          source)
    (when (seq (:quantities element))
      {"Qto_KotobaBaseQuantities"
       {:name "Qto_KotobaBaseQuantities"
        :quantities
        (into {}
              (map (fn [[name value]]
                     [(clojure.core/name name)
                      {:kind (quantity-kind name) :value value}]))
              (:quantities element))}})))

(defn- exported-material [element]
  (when (or (:ifc/material element) (seq (:material-layers element)))
    (let [source (or (:ifc/material element)
                     {:kind :layer-set-usage
                      :direction (if (= :slab (:kind element)) :axis3 :axis2)
                      :direction-sense :positive :offset 0.0
                      :layer-set {:name (str (:name element) " Layers")}})
          source-layers (get-in source [:layer-set :layers])]
      (if-not (seq (:material-layers element))
        source
        (assoc-in
         source [:layer-set :layers]
         (mapv (fn [index layer]
                 (let [original (get source-layers index)]
                   (-> (or original {})
                       (assoc :material
                              (merge (:material original)
                                     {:name (some-> (:material layer) clojure.core/name)
                                      :category (some-> (:category layer) clojure.core/name)}))
                       (assoc :thickness (:thickness layer)
                              :ventilated (:is-ventilated layer)
                              :category (some-> (:category layer) clojure.core/name)))))
               (range) (:material-layers element)))))))

(defn- exported-classifications [element]
  (when-let [classification (:classification element)]
    (let [sources (vec (or (seq (:ifc/classifications element)) [{}]))]
      (assoc sources 0
             (-> (first sources)
                 (assoc :identification (:code classification)
                        :name (:description classification))
                 (assoc-in [:source :name] (:source classification)))))))

(defn- exported-opening [wall opening]
  (let [[[x0 y0 z0] [x1 y1 _]] (get-in wall [:geometry :axis])
        length (get-in wall [:quantities :length-m])
        {:keys [offset sill]} (:placement opening)
        {:keys [width height]} (:profile opening)
        x (+ x0 (* (/ offset length) (- x1 x0)))
        y (+ y0 (* (/ offset length) (- y1 y0)))]
    {:id (:id opening) :global-id (str (:id opening)) :kind :opening :name "Opening"
     :filled-by (:filled-by opening) :placement {:location [x y (+ z0 sill)]}
     :geometry {:kind :extruded-area-solid
                :profile {:kind :rectangle :x-dim width :y-dim (:depth opening)}
                :direction [0.0 0.0 1.0] :depth height}}))

(defn- connector-flow->ifc [flow]
  (case flow :in :sink :out :source :bidirectional :sourceandsink
        :sourceandsink :sourceandsink :notdefined))

(defn- connector-domain->port-type [domain]
  (case domain :duct :duct :hvac :duct :pipe :pipe :piping :pipe
        :electrical :cable :cable :cable :notdefined))

(defn- exported-port [connector]
  {:id (:connector/id connector) :global-id (str (:connector/id connector))
   :name (or (:connector/name connector) (str (:connector/id connector)))
   :placement {:location (:connector/point connector)
               :axis (or (:connector/direction connector) [0.0 0.0 1.0])}
   :flow-direction (connector-flow->ifc (:connector/flow-direction connector))
   :predefined-type (connector-domain->port-type (:connector/domain connector))
   :system-type (or (:connector/system-type connector) :notdefined)})

(defn- exported-element [storey element]
  {:id (:id element) :global-id (or (:global-id element) (str (:id element)))
   :kind (:kind element) :name (:name element) :container-id (:id storey)
   :placement (when (map? (:placement element)) (:placement element))
   :geometry (exported-geometry element)
   :property-sets (exported-psets element)
   :quantity-sets (exported-quantity-sets element)
   :material (exported-material element)
   :classifications (exported-classifications element)
   :type-object (:type-object element)
   :appearance (:appearance element)
   :presentation-layers (:presentation-layers element)
   :ports (mapv exported-port (:mep/connectors element))
   :openings (if (= :wall (:kind element))
               (mapv #(exported-opening element %) (:openings element)) [])})

(defn- exported-connections [elements]
  (let [connectors (mapcat :mep/connectors elements)
        known (set (map :connector/id connectors))]
    (->> connectors
         (keep (fn [connector]
                 (let [source (:connector/id connector)
                       target (:connector/connected-to connector)]
                   (when (and target (contains? known target)
                              (neg? (compare (str source) (str target))))
                     {:id (str source "-" target) :global-id (str source "-" target)
                      :relating-port-global-id (str source)
                      :related-port-global-id (str target)}))))
         vec)))

(defn- decimal-degrees->compound [value]
  (when (number? value)
    (let [absolute (#?(:clj Math/abs :cljs js/Math.abs) value)
          degrees (#?(:clj Math/floor :cljs js/Math.floor) absolute)
          minutes-value (* 60.0 (- absolute degrees))
          minutes (#?(:clj Math/floor :cljs js/Math.floor) minutes-value)
          seconds-value (* 60.0 (- minutes-value minutes))
          seconds (#?(:clj Math/floor :cljs js/Math.floor) seconds-value)
          millionths (#?(:clj Math/round :cljs js/Math.round) (* 1.0e6 (- seconds-value seconds)))]
      [(long (* (if (neg? value) -1 1) degrees)) (long minutes)
       (long seconds) (long millionths)])))

(defn- compound->decimal-degrees [compound]
  (when (seq compound)
    (let [[degrees minutes seconds millionths] compound
          sign (if (neg? degrees) -1.0 1.0)]
      (* sign (+ (#?(:clj Math/abs :cljs js/Math.abs) degrees)
                 (/ minutes 60.0) (/ (+ seconds (/ (or millionths 0) 1.0e6)) 3600.0))))))

(defn- exported-spatial-tree [project]
  (mapv (fn [site]
          {:id (:id site) :global-id (:global-id site) :name (:name site) :type :ifcsite
           :latitude (decimal-degrees->compound (get-in site [:geo :latitude-deg]))
           :longitude (decimal-degrees->compound (get-in site [:geo :longitude-deg]))
           :elevation (get-in site [:geo :elevation-m])
           :placement (when (map? (:placement site)) (:placement site))
           :children
           (mapv (fn [building]
                   {:id (:id building) :global-id (:global-id building) :name (:name building)
                    :type :ifcbuilding
                    :placement (when (map? (:placement building)) (:placement building))
                    :children
                    (mapv (fn [storey]
                            {:id (:id storey) :global-id (:global-id storey) :name (:name storey)
                             :type :ifcbuildingstorey :elevation (:elevation storey)
                             :placement (when (map? (:placement storey)) (:placement storey))
                             :children
                             (mapv (fn [space]
                                     {:id (:id space) :global-id (:global-id space)
                                      :name (:name space) :long-name (:long-name space)
                                      :type :ifcspace :placement (:placement space)
                                      :children []})
                                   (:spaces storey))})
                          (:storeys building))})
                 (:buildings site))})
        (:sites project)))

(defn export-ifc
  "Create a lossless, EDN-native IFC 4.3 semantic exchange document. This is
  the canonical boundary consumed by STEP/JSON adapters; it is not STEP text."
  [project]
  (let [source (:ifc/source-document project)
        storey-elements (mapcat (fn [storey]
                                  (map #(vector storey %) (:elements storey)))
                                (all-storeys project))
        elements (mapv (fn [[storey element]] (exported-element storey element))
                       storey-elements)
        model-elements (mapv second storey-elements)
        exchange (ifc/exchange-document
                  {:project (cond-> {:id (:id project) :global-id (str (:id project))
                                     :name (:name project)
                                     :georeference (or (:ifc/georeference project)
                                                       (:georeference project))
                                     :children (exported-spatial-tree project)}
                              (nil? source) (assoc :model project))
                   :elements elements})
        exchange (assoc exchange
                        :ifc/groups (vec (or (:ifc/groups project)
                                             (:ifc/groups source)))
                        :ifc/connections (let [connections (exported-connections model-elements)]
                                           (if (seq connections) connections
                                               (vec (:ifc/connections source)))))]
    (if source
      (merge exchange
             (select-keys source [:ifc/schema :ifc/source :ifc/raw-spf
                                  :ifc/raw-entities :ifc/raw-entity-count
                                  :ifc/raw-type-frequencies :ifc/import-fingerprint])
             {:ifc/units (:ifc/units source)
              :ifc/georeference (or (:ifc/georeference project)
                                    (:ifc/georeference source))})
      exchange)))

(defn import-ifc [document]
  (when-not (= "IFC4X3_ADD2" (:ifc/schema document))
    (throw (ex-info "unsupported IFC schema" {:schema (:ifc/schema document)})))
  (get-in document [:ifc/project :model]))

(defn- spatial-descendants [node type]
  (filter #(= type (:type %)) (tree-seq #(seq (:children %)) :children node)))

(defn- imported-property-value [property]
  (let [{:keys [value value-type]} property]
    (case (:kind property)
      :enumerated
      (assoc (bim/enum-values (:values property)
                              (get-in property [:enumeration :values]))
             :value-type value-type
             :enumeration-name (get-in property [:enumeration :name]))
      :bounded
      (assoc (bim/bounded-value
              {:lower (:lower property) :upper (:upper property)
               :set-point (:set-point property) :value-type value-type})
             :ifc/unit (:unit property))
      :list
      (assoc (bim/list-value (:values property) nil value-type)
             :ifc/unit (:unit property))
      (assoc
       (case value-type
         :ifcboolean (bim/bool-value value)
         :ifcinteger (bim/int-value value)
         (:ifcreal :ifclengthmeasure :ifcareameasure :ifcvolumemeasure)
         (bim/real-value value)
         (bim/text-value (str value)))
       :ifc/unit (:unit property)))))

(defn- v3+ [a b] (mapv + a b))
(defn- v3- [a b] (mapv - a b))
(defn- v3-scale [factor vector] (mapv #(* factor %) vector))
(defn- v3-dot [a b] (reduce + (map * a b)))
(defn- v3-cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])

(defn- placement-basis [placement]
  (let [x (get placement :ref-direction [1.0 0.0 0.0])
        z (get placement :axis [0.0 0.0 1.0])]
    {:x x :y (v3-cross z x) :z z}))

(defn- imported-psets [source]
  (into {}
        (map (fn [[name pset]]
               [name (bim/property-set name
                                        (into {} (map (fn [[property-name property]]
                                                        [(keyword property-name)
                                                         (imported-property-value property)]))
                                              (:properties pset)))])
             (:property-sets source))))

(defn- imported-quantities [source]
  (into {}
        (mapcat (fn [[_ qset]]
                  (map (fn [[name quantity]] [(keyword name) (:value quantity)])
                       (:quantities qset))))
        (:quantity-sets source)))

(defn- imported-material-layers [source]
  (mapv (fn [layer]
          (let [category (some-> (or (:category layer)
                                     (get-in layer [:material :category]))
                                 string/lower-case keyword)
                category (if (contains? bim/material-categories category)
                           category :other)]
            (bim/material-layer (get-in layer [:material :name])
                                (:thickness layer) (:ventilated layer) category)))
        (get-in source [:material :layer-set :layers])))

(defn- imported-classification [source]
  (when-let [classification (first (:classifications source))]
    (bim/classification-ref (get-in classification [:source :name])
                            (:identification classification)
                            (or (:name classification) (:description classification)))))

(defn- attach-imported-openings [wall source]
  (reduce
   (fn [host opening]
     (let [host-location (get-in source [:placement :location] [0.0 0.0 0.0])
           opening-location (get-in opening [:placement :location] host-location)
           basis (placement-basis (:placement source))
           profile (get-in opening [:geometry :profile])
           width (:x-dim profile) height (get-in opening [:geometry :depth])
           relative (v3- opening-location host-location)
           offset (v3-dot relative (:x basis))
           sill (v3-dot relative (:z basis))]
       (if (and width height)
         (bim/add-opening-to-wall
          host (bim/rectangular-opening {:id (:id opening) :offset offset :sill sill
                                         :width width :height height
                                         :filled-by (or (:filled-by-global-id opening)
                                                        (:filled-by opening))}))
         host)))
   wall (:openings source)))

(defn- ifc-flow->connector [flow]
  (case flow :sink :in :source :out :sourceandsink :bidirectional :notdefined nil nil))

(defn- port-type->connector-domain [port-type]
  (case port-type :duct :hvac :pipe :piping (:cable :cablecarrier) :electrical :other))

(defn- imported-connector [port connected-port]
  {:connector/id (:global-id port)
   :connector/point (get-in port [:placement :location] [0.0 0.0 0.0])
   :connector/direction (get-in port [:placement :axis] [0.0 0.0 1.0])
   :connector/domain (port-type->connector-domain (:predefined-type port))
   :connector/flow-direction (ifc-flow->connector (:flow-direction port))
   :connector/system-type (:system-type port)
   :connector/connected-to connected-port})

(defn- imported-element [source connected-port-by-id]
  (let [[x y z] (get-in source [:placement :location] [0.0 0.0 0.0])
        origin [x y z]
        basis (placement-basis (:placement source))
        geometry (:geometry source)
        profile (:profile geometry)
        x-dim (:x-dim profile) y-dim (:y-dim profile) depth (:depth geometry)
        psets (imported-psets source)
        quantities (imported-quantities source)
        material-layers (imported-material-layers source)
        classification (imported-classification source)
        result
        (case (:kind source)
          :wall (if (and x-dim y-dim depth)
                  (attach-imported-openings
                   (bim/wall {:id (:id source) :name (:name source)
                              :start origin :end (v3+ origin (v3-scale x-dim (:x basis)))
                              :thickness y-dim :height depth}) source)
                  (bim/element {:id (:id source) :kind :wall :name (:name source)
                                :geometry geometry}))
          :slab (if (and x-dim y-dim depth)
                  (bim/slab {:id (:id source) :name (:name source)
                             :boundary [origin
                                        (v3+ origin (v3-scale x-dim (:x basis)))
                                        (v3+ origin
                                             (v3+ (v3-scale x-dim (:x basis))
                                                  (v3-scale y-dim (:y basis))))
                                        (v3+ origin (v3-scale y-dim (:y basis)))]
                             :thickness depth})
                  (bim/element {:id (:id source) :kind :slab :name (:name source)
                                :geometry geometry}))
          (bim/element {:id (:id source) :kind (if (= :proxy (:kind source)) :other (:kind source)) :name (:name source)
                        :placement (:placement source) :geometry geometry}))]
    (assoc result :global-id (:global-id source) :ifc/source-id (:id source)
                  :ifc/kind (:kind source)
                  :type-object (:type-object source)
                  :ifc/property-sets (:property-sets source)
                  :ifc/quantity-sets (:quantity-sets source)
                  :ifc/material (:material source)
                  :ifc/classifications (:classifications source)
                  :appearance (:appearance source)
                  :presentation-layers (:presentation-layers source)
                  :mep/connectors
                  (mapv #(imported-connector % (get connected-port-by-id (:global-id %)))
                        (:ports source))
                  :quantities (merge (:quantities result) quantities)
                  :material-layers (if (seq material-layers)
                                     material-layers (:material-layers result))
                  :classification (or classification (:classification result))
                  :psets (merge (:psets result) psets))))

(defn- imported-unit-system [document]
  (let [{:keys [name prefix]} (get-in document [:ifc/units :lengthunit])
        length (cond
                 (and (= :metre name) (= :milli prefix)) :millimetre
                 (= :metre name) :metre
                 :else :metre)]
    (bim/unit-system {:length length})))

(defn import-external-ifc
  "Map a shared IFC exchange document into the BIM spatial hierarchy."
  [document]
  (if-let [model (get-in document [:ifc/project :model])]
    model
    (let [root (:ifc/project document)
          sites (spatial-descendants root :ifcsite)
          buildings (spatial-descendants root :ifcbuilding)
          storeys (spatial-descendants root :ifcbuildingstorey)
          elements-by-storey (group-by :container-id (:ifc/elements document))
          connected-port-by-id
          (reduce (fn [result connection]
                    (let [a (:relating-port-global-id connection)
                          b (:related-port-global-id connection)]
                      (assoc result a b b a)))
                  {} (:ifc/connections document))
          storey-models (into {}
                              (map (fn [node]
                                     [(:id node)
                                      (assoc
                                       (bim/storey
                                        {:id (:id node) :name (:name node)
                                         :elevation (or (get-in node [:placement :location 2]) 0.0)
                                         :height 3.0 :placement (:placement node)
                                         :spaces
                                         (mapv (fn [space]
                                                 (assoc (bim/space
                                                         {:id (:id space)
                                                          :name (:name space)
                                                          :long-name (:long-name space)
                                                          :category :other
                                                          :boundary [] :height 3.0
                                                          :quantities {} :psets {}})
                                                        :global-id (:global-id space)
                                                        :placement (:placement space)))
                                               (filter (comp #{:ifcspace} :type)
                                                       (:children node)))
                                         :elements (mapv #(imported-element % connected-port-by-id)
                                                         (get elements-by-storey (:id node)))})
                                       :global-id (:global-id node))]))
                              storeys)
          building-models (mapv (fn [node]
                                  (assoc
                                   (bim/building {:id (:id node) :name (:name node)
                                                  :placement (:placement node) :reference-elevation 0.0
                                                  :storeys (mapv #(get storey-models (:id %))
                                                                 (filter (comp #{:ifcbuildingstorey} :type)
                                                                         (:children node)))})
                                   :global-id (:global-id node)))
                                buildings)
          site-node (first sites)
          georeference (:ifc/georeference document)
          true-north (:true-north georeference)
          true-north-rad (if (seq true-north)
                           (#?(:clj Math/atan2 :cljs js/Math.atan2)
                            (first true-north) (second true-north)) 0.0)]
      {:id (or (:global-id root) (:id root)) :name (:name root) :description ""
       :units (imported-unit-system document)
       :world-origin (or (:world-origin georeference) [0.0 0.0 0.0])
       :true-north-rad true-north-rad :ifc/georeference georeference
       :ifc/schema (:ifc/schema document) :ifc/source-document document
       :ifc/groups (:ifc/groups document) :ifc/connections (:ifc/connections document)
       :psets {}
       :sites [(assoc
                (bim/site {:id (or (:id site-node) 1) :name (or (:name site-node) "Site")
                           :geo (when site-node
                                  (bim/geo-ref
                                   (compound->decimal-degrees (:latitude site-node))
                                   (compound->decimal-degrees (:longitude site-node))
                                   (:elevation site-node)))
                           :placement (:placement site-node)
                           :buildings building-models})
                :global-id (:global-id site-node))]})))

(defn import-ifc-spf [text]
  (import-external-ifc (ifc/read-document text)))

(defn structural-node [{:keys [id point restraints]}]
  {:structural.node/id id :structural.node/point (vec point)
   :structural.node/restraints (vec (or restraints [false false]))})

(defn structural-analysis-member
  [{:keys [id start-node end-node area-m2 elastic-modulus-pa yield-strength-pa
           resistance-factor section material density-kg-m3]}]
  {:structural.member/id id :structural.member/start-node start-node
   :structural.member/end-node end-node :structural.member/area-m2 area-m2
   :structural.member/elastic-modulus-pa elastic-modulus-pa
   :structural.member/yield-strength-pa yield-strength-pa
   :structural.member/resistance-factor (or resistance-factor 0.9)
   :structural.member/section section :structural.member/material material
   :structural.member/density-kg-m3 density-kg-m3})

(defn structural-load-case [{:keys [id name kind nodal-loads member-loads gravity]}]
  {:structural.load-case/id id :structural.load-case/name name
   :structural.load-case/kind (or kind :service)
   :structural.load-case/nodal-loads (vec nodal-loads)
   :structural.load-case/member-loads (vec member-loads)
   :structural.load-case/gravity gravity})

(defn structural-load-combination [{:keys [id name factors kind]}]
  {:structural.combination/id id :structural.combination/name name
   :structural.combination/kind (or kind :ultimate)
   :structural.combination/factors factors})

(defn structural-model [{:keys [nodes members load-cases combinations]}]
  {:structural/schema-version schema-version :structural/nodes (vec nodes)
   :structural/members (vec members) :structural/load-cases (vec load-cases)
   :structural/combinations (vec combinations)})

(defn validate-structural-model [model]
  (let [node-list (:structural/nodes model)
        nodes (into {} (map (juxt :structural.node/id identity) node-list))
        member-list (:structural/members model)
        members (into {} (map (juxt :structural.member/id identity) member-list))
        dimensions (set (map (comp count :structural.node/point) node-list))]
    (vec
     (concat
      (when (or (> (count dimensions) 1) (not (contains? #{2 3} (first dimensions))))
        [{:issue/type :structural/inconsistent-node-dimensions :issue/dimensions dimensions}])
      (when-not (= (count node-list) (count nodes))
        [{:issue/type :structural/duplicate-node-id}])
      (when-not (= (count member-list) (count members))
        [{:issue/type :structural/duplicate-member-id}])
      (mapcat (fn [node]
                (when-not (= (count (:structural.node/point node))
                             (count (:structural.node/restraints node)))
                  [{:issue/type :structural/restraint-dimension-mismatch
                    :issue/node (:structural.node/id node)}]))
              node-list)
      (mapcat (fn [member]
               (let [start (nodes (:structural.member/start-node member))
                     end (nodes (:structural.member/end-node member))]
                 (cond
                   (nil? start) [{:issue/type :structural/missing-node
                                  :issue/member (:structural.member/id member)
                                  :issue/node (:structural.member/start-node member)}]
                   (nil? end) [{:issue/type :structural/missing-node
                                :issue/member (:structural.member/id member)
                                :issue/node (:structural.member/end-node member)}]
                   (= (:structural.node/point start) (:structural.node/point end))
                   [{:issue/type :structural/zero-length-member
                     :issue/member (:structural.member/id member)}]
                   (or (not (pos? (or (:structural.member/area-m2 member) 0.0)))
                       (not (pos? (or (:structural.member/elastic-modulus-pa member) 0.0))))
                   [{:issue/type :structural/invalid-member-properties
                     :issue/member (:structural.member/id member)}]
                   :else [])))
              member-list)
      (mapcat
       (fn [load-case]
         (concat
          (for [load (:structural.load-case/nodal-loads load-case)
                :when (not (contains? nodes (:node load)))]
            {:issue/type :structural/load-missing-node
             :issue/load-case (:structural.load-case/id load-case)
             :issue/node (:node load)})
          (for [load (:structural.load-case/member-loads load-case)
                :when (not (contains? members (:member load)))]
            {:issue/type :structural/load-missing-member
             :issue/load-case (:structural.load-case/id load-case)
             :issue/member (:member load)})))
       (:structural/load-cases model))))))

(defn- solve-linear-system [matrix values]
  (let [n (count values)]
    (loop [column 0 matrix (mapv vec matrix) values (vec values)]
      (if (= column n)
        (loop [row (dec n) solution (vec (repeat n 0.0))]
          (if (neg? row)
            solution
            (let [known (reduce + (map (fn [j] (* (get-in matrix [row j]) (nth solution j)))
                                       (range (inc row) n)))
                  value (/ (- (nth values row) known) (get-in matrix [row row]))]
              (recur (dec row) (assoc solution row value)))))
        (let [pivot-row (apply max-key
                               #(math-abs (double (get-in matrix [% column])))
                               (range column n))
              matrix (assoc matrix column (nth matrix pivot-row) pivot-row (nth matrix column))
              values (assoc values column (nth values pivot-row) pivot-row (nth values column))
              pivot (get-in matrix [column column])]
          (when (< (math-abs (double pivot)) 1.0e-12)
            (throw (ex-info "structural stiffness matrix is singular" {:dof column})))
          (let [[matrix values]
                (reduce (fn [[m v] row]
                          (let [factor (/ (get-in m [row column]) pivot)]
                            [(assoc m row
                                    (mapv - (nth m row) (mapv #(* factor %) (nth m column))))
                             (assoc v row (- (nth v row) (* factor (nth v column))))]))
                        [matrix values] (range (inc column) n))]
            (recur (inc column) matrix values)))))))

(defn- structural-member-geometry [node-by-id member]
  (let [a (:structural.node/point (node-by-id (:structural.member/start-node member)))
        b (:structural.node/point (node-by-id (:structural.member/end-node member)))
        delta (mapv - b a) length (sqrt (reduce + (map #(* % %) delta)))]
    {:length length :direction (mapv #(/ % length) delta)}))

(defn- truss-load-vector [model load-case node-index dimension]
  (let [node-by-id (into {} (map (juxt :structural.node/id identity)
                                  (:structural/nodes model)))
        member-by-id (into {} (map (juxt :structural.member/id identity)
                                    (:structural/members model)))
        axes [:fx :fy :fz]
        empty-loads (vec (repeat (* dimension (count node-index)) 0.0))
        add-node-load
        (fn [result node-id values]
          (let [offset (* dimension (node-index node-id))]
            (reduce (fn [loads axis]
                      (update loads (+ offset axis) + (or (nth values axis nil) 0.0)))
                    result (range dimension))))
        nodal (reduce (fn [result load]
                        (add-node-load result (:node load)
                                       (mapv load (take dimension axes))))
                      empty-loads (:structural.load-case/nodal-loads load-case))
        distributed
        (reduce (fn [result load]
                  (let [member (member-by-id (:member load))
                        {:keys [length]} (structural-member-geometry node-by-id member)
                        values (mapv #(* 0.5 length (or (load %) 0.0))
                                     (take dimension [:wx :wy :wz]))]
                    (-> result
                        (add-node-load (:structural.member/start-node member) values)
                        (add-node-load (:structural.member/end-node member) values))))
                nodal (:structural.load-case/member-loads load-case))
        gravity (take dimension (or (:structural.load-case/gravity load-case)
                                    (repeat 0.0)))]
    (reduce (fn [result member]
              (if-let [density (:structural.member/density-kg-m3 member)]
                (let [{:keys [length]} (structural-member-geometry node-by-id member)
                      mass (* density (:structural.member/area-m2 member) length)
                      values (mapv #(* 0.5 mass %) gravity)]
                  (-> result
                      (add-node-load (:structural.member/start-node member) values)
                      (add-node-load (:structural.member/end-node member) values)))
                result))
            distributed (:structural/members model))))

(defn analyze-truss
  "Dimension-independent linear elastic truss solver for 2D and 3D models.
  Supports nodal loads, uniform global member loads, member self-weight,
  support reactions, strain/stress, and axial force results."
  [model load-case-id]
  (when-let [issues (seq (validate-structural-model model))]
    (throw (ex-info "invalid structural model" {:issues issues})))
  (let [nodes (:structural/nodes model)
        dimension (count (:structural.node/point (first nodes)))
        node-index (into {} (map-indexed #(vector (:structural.node/id %2) %1) nodes))
        node-by-id (into {} (map (juxt :structural.node/id identity) nodes))
        dof-count (* dimension (count nodes))
        stiffness
        (reduce
         (fn [matrix member]
           (let [{:keys [length direction]} (structural-member-geometry node-by-id member)
                 factor (/ (* (:structural.member/area-m2 member)
                              (:structural.member/elastic-modulus-pa member)) length)
                 start (* dimension (node-index (:structural.member/start-node member)))
                 end (* dimension (node-index (:structural.member/end-node member)))
                 dofs (vec (concat (range start (+ start dimension))
                                   (range end (+ end dimension))))]
             (reduce
              (fn [result [block-i block-j axis-i axis-j]]
                (let [sign (if (= block-i block-j) 1.0 -1.0)
                      row (+ (* block-i dimension) axis-i)
                      column (+ (* block-j dimension) axis-j)]
                  (update-in result [(nth dofs row) (nth dofs column)] +
                             (* sign factor (nth direction axis-i) (nth direction axis-j)))))
              matrix
              (for [block-i (range 2) block-j (range 2)
                    axis-i (range dimension) axis-j (range dimension)]
                [block-i block-j axis-i axis-j]))))
         (vec (repeat dof-count (vec (repeat dof-count 0.0))))
         (:structural/members model))
        load-case (first (filter #(= load-case-id (:structural.load-case/id %))
                                 (:structural/load-cases model)))
        _ (when-not load-case
            (throw (ex-info "structural load case not found" {:load-case-id load-case-id})))
        loads (truss-load-vector model load-case node-index dimension)
        fixed (into #{} (mapcat (fn [[index node]]
                                  (keep-indexed (fn [axis restrained?]
                                                  (when restrained?
                                                    (+ (* dimension index) axis)))
                                                (:structural.node/restraints node)))
                                (map-indexed vector nodes)))
        free (vec (remove fixed (range dof-count)))
        reduced-matrix (mapv (fn [i] (mapv #(get-in stiffness [i %]) free)) free)
        reduced-loads (mapv #(nth loads %) free)
        reduced-displacements (solve-linear-system reduced-matrix reduced-loads)
        displacements (reduce (fn [result [dof value]] (assoc result dof value))
                              (vec (repeat dof-count 0.0))
                              (map vector free reduced-displacements))
        reactions (mapv -
                        (mapv #(reduce + (map * % displacements)) stiffness)
                        loads)
        member-results
        (into {}
              (map (fn [member]
                     (let [{:keys [length direction]}
                           (structural-member-geometry node-by-id member)
                           start (* dimension (node-index (:structural.member/start-node member)))
                           end (* dimension (node-index (:structural.member/end-node member)))
                           extension (reduce +
                                             (map-indexed
                                              (fn [axis cosine]
                                                (* cosine (- (nth displacements (+ end axis))
                                                             (nth displacements (+ start axis)))))
                                              direction))
                           strain (/ extension length)
                           stress (* (:structural.member/elastic-modulus-pa member) strain)
                           force (* (:structural.member/area-m2 member) stress)]
                       [(:structural.member/id member)
                        {:length-m length :extension-m extension :strain strain
                         :stress-pa stress :force-n force}]))
                   (:structural/members model)))]
    {:structural.analysis/load-case load-case-id
     :structural.analysis/dimension dimension
     :structural.analysis/displacements
     (into {} (map-indexed (fn [index node]
                             [(:structural.node/id node)
                              (subvec displacements (* dimension index)
                                      (* dimension (inc index)))]) nodes))
     :structural.analysis/reactions
     (into {} (map-indexed (fn [index node]
                             [(:structural.node/id node)
                              (subvec reactions (* dimension index)
                                      (* dimension (inc index)))]) nodes))
     :structural.analysis/member-results member-results
     :structural.analysis/member-axial-forces
     (into {} (map (fn [[id result]] [id (:force-n result)]) member-results))}))

(defn analyze-2d-truss [model load-case-id]
  (when-not (= 2 (count (get-in model [:structural/nodes 0 :structural.node/point])))
    (throw (ex-info "2D truss requires 2D nodes" {})))
  (analyze-truss model load-case-id))

(defn analyze-3d-truss [model load-case-id]
  (when-not (= 3 (count (get-in model [:structural/nodes 0 :structural.node/point])))
    (throw (ex-info "3D truss requires 3D nodes" {})))
  (analyze-truss model load-case-id))

(defn analyze-structural-combination
  "Analyze a factored load combination and check axial member resistance."
  [model combination-id]
  (let [combination (first (filter #(= combination-id (:structural.combination/id %))
                                   (:structural/combinations model)))]
    (when-not combination
      (throw (ex-info "structural load combination not found"
                      {:combination-id combination-id})))
    (let [case-by-id (into {} (map (juxt :structural.load-case/id identity)
                                   (:structural/load-cases model)))
          combined-loads
          (->> (:structural.combination/factors combination)
               (mapcat (fn [[case-id factor]]
                         (when-not (contains? case-by-id case-id)
                           (throw (ex-info "structural load case not found"
                                           {:load-case-id case-id
                                            :combination-id combination-id})))
                         (map (fn [{:keys [node fx fy fz]}]
                                {:node node :fx (* factor (or fx 0.0))
                                 :fy (* factor (or fy 0.0))
                                 :fz (* factor (or fz 0.0))})
                              (:structural.load-case/nodal-loads (case-by-id case-id)))))
               (group-by :node)
               (mapv (fn [[node loads]]
                       {:node node :fx (reduce + (map :fx loads))
                        :fy (reduce + (map :fy loads))
                        :fz (reduce + (map :fz loads))})))
          combined-member-loads
          (->> (:structural.combination/factors combination)
               (mapcat (fn [[case-id factor]]
                         (map (fn [{:keys [member wx wy wz]}]
                                {:member member :wx (* factor (or wx 0.0))
                                 :wy (* factor (or wy 0.0)) :wz (* factor (or wz 0.0))})
                              (:structural.load-case/member-loads (case-by-id case-id)))))
               (group-by :member)
               (mapv (fn [[member loads]]
                       {:member member :wx (reduce + (map :wx loads))
                        :wy (reduce + (map :wy loads)) :wz (reduce + (map :wz loads))})))
          dimension (count (get-in model [:structural/nodes 0 :structural.node/point]))
          combined-gravity
          (reduce (fn [result [case-id factor]]
                    (mapv + result
                          (mapv #(* factor %)
                                (take dimension
                                      (or (:structural.load-case/gravity (case-by-id case-id))
                                          (repeat 0.0))))))
                  (vec (repeat dimension 0.0))
                  (:structural.combination/factors combination))
          synthetic-id [:combination combination-id]
          analysis (analyze-truss
                    (update model :structural/load-cases conj
                            (structural-load-case {:id synthetic-id :name (:structural.combination/name combination)
                                                   :kind (:structural.combination/kind combination)
                                                   :nodal-loads combined-loads
                                                   :member-loads combined-member-loads
                                                   :gravity combined-gravity}))
                    synthetic-id)
          member-by-id (into {} (map (juxt :structural.member/id identity)
                                     (:structural/members model)))
          checks
          (into {}
                (map (fn [[member-id force]]
                       (let [member (member-by-id member-id)
                             yield (:structural.member/yield-strength-pa member)
                             resistance (when (number? yield)
                                          (* (:structural.member/area-m2 member) yield
                                             (:structural.member/resistance-factor member)))
                             utilization (when (and resistance (pos? resistance))
                                           (/ (math-abs force) resistance))]
                         [member-id {:force-n force :resistance-n resistance
                                     :utilization utilization
                                     :passes? (when utilization (<= utilization 1.0))}]))
                     (:structural.analysis/member-axial-forces analysis)))]
      (assoc analysis :structural.analysis/combination combination-id
             :structural.analysis/member-checks checks))))

(defn structural-section-properties
  "Calculate gross section properties in SI units. Rectangle dimensions are
  width/depth, circles use diameter, and I sections use overall width/depth,
  web thickness, and flange thickness."
  [section]
  (let [properties
        (case (:kind section)
          :rectangle
          (let [width (:width-m section) depth (:depth-m section)
                area (* width depth)
                strong (/ (* width depth depth depth) 12.0)
                weak (/ (* depth width width width) 12.0)]
            {:area-m2 area :strong-inertia-m4 strong :weak-inertia-m4 weak
             :strong-modulus-m3 (/ (* 2.0 strong) depth)
             :weak-modulus-m3 (/ (* 2.0 weak) width)})
          :circle
          (let [diameter (:diameter-m section)
                area (/ (* pi diameter diameter) 4.0)
                inertia (/ (* pi diameter diameter diameter diameter) 64.0)]
            {:area-m2 area :strong-inertia-m4 inertia :weak-inertia-m4 inertia
             :strong-modulus-m3 (/ (* 2.0 inertia) diameter)
             :weak-modulus-m3 (/ (* 2.0 inertia) diameter)})
          :i-shape
          (let [width (:overall-width-m section) depth (:overall-depth-m section)
                web (:web-thickness-m section) flange (:flange-thickness-m section)
                web-depth (- depth (* 2.0 flange))
                area (+ (* 2.0 width flange) (* web web-depth))
                strong (+ (* 2.0 (+ (/ (* width flange flange flange) 12.0)
                                      (* width flange
                                         (pow (- (/ depth 2.0) (/ flange 2.0)) 2.0))))
                          (/ (* web web-depth web-depth web-depth) 12.0))
                weak (+ (* 2.0 (/ (* flange width width width) 12.0))
                        (/ (* web-depth web web web) 12.0))]
            {:area-m2 area :strong-inertia-m4 strong :weak-inertia-m4 weak
             :strong-modulus-m3 (/ (* 2.0 strong) depth)
             :weak-modulus-m3 (/ (* 2.0 weak) width)})
          (throw (ex-info "unsupported structural section" {:section section})))]
    (when (some #(not (pos? %)) (vals properties))
      (throw (ex-info "structural section dimensions must be positive" {:section section})))
    (merge section properties)))

(defn simply-supported-beam-check
  "Elastic strong-axis check for a simply supported prismatic beam under a
  uniform line load. Combines axial and bending stress and checks service
  deflection against `span / deflection-limit-ratio`."
  [{:keys [span-m section elastic-modulus-pa yield-strength-pa
           uniform-load-n-m axial-force-n resistance-factor
           deflection-limit-ratio]}]
  (let [{:keys [area-m2 strong-inertia-m4 strong-modulus-m3] :as properties}
        (structural-section-properties section)
        _ (when (some #(not (pos? (or % 0.0)))
                      [span-m elastic-modulus-pa yield-strength-pa])
            (throw (ex-info "beam span and material properties must be positive"
                            {:span-m span-m})))
        factor (or resistance-factor 0.9)
        limit-ratio (or deflection-limit-ratio 360.0)
        load (math-abs (or uniform-load-n-m 0.0))
        axial (math-abs (or axial-force-n 0.0))
        moment (/ (* load span-m span-m) 8.0)
        shear (/ (* load span-m) 2.0)
        deflection (/ (* 5.0 load (pow span-m 4.0))
                      (* 384.0 elastic-modulus-pa strong-inertia-m4))
        axial-stress (/ axial area-m2)
        bending-stress (/ moment strong-modulus-m3)
        combined-stress (+ axial-stress bending-stress)
        resistance (* factor yield-strength-pa)
        strength-utilization (/ combined-stress resistance)
        deflection-limit (/ span-m limit-ratio)
        deflection-utilization (/ deflection deflection-limit)
        utilization (max strength-utilization deflection-utilization)]
    {:beam/span-m span-m :beam/section-properties properties
     :beam/reaction-n shear :beam/max-shear-n shear :beam/max-moment-nm moment
     :beam/max-deflection-m deflection :beam/deflection-limit-m deflection-limit
     :beam/axial-stress-pa axial-stress :beam/bending-stress-pa bending-stress
     :beam/combined-stress-pa combined-stress
     :beam/strength-utilization strength-utilization
     :beam/deflection-utilization deflection-utilization
     :beam/utilization utilization :beam/passes? (<= utilization 1.0)}))

(defn structural-member
  "Attach an analytical line, section and load-bearing role to a BIM element."
  [element {:keys [role analytical-axis section material loads]}]
  (assoc element :discipline :structural
                 :structural/role role :structural/analytical-axis analytical-axis
                 :structural/section section :structural/material material
                 :structural/loads (vec loads)))

(defn mep-system [{:keys [id name kind medium design-flow segments]}]
  {:mep/id id :mep/name name :mep/kind kind :mep/medium medium
   :mep/design-flow design-flow :mep/segments (vec segments)})

(defn mep-connector [{:keys [id point direction domain shape size flow-direction connected-to]}]
  {:connector/id id :connector/point (vec point) :connector/direction (vec direction)
   :connector/domain domain :connector/shape shape :connector/size size
   :connector/flow-direction flow-direction :connector/connected-to connected-to})

(defn mep-segment
  [{:keys [id name kind start end diameter width height system-id connectors connected-to]}]
  (let [radius (when diameter (/ diameter 2.0))]
    {:id id :kind :mep-segment :name (or name (str "MEP " id)) :discipline :mep
     :mep/kind kind :mep/system-id system-id :mep/connectors (vec connectors)
     :geometry (if radius
                 {:kind :swept-disk-solid :directrix [start end] :radius radius}
                 {:kind :extruded-area-solid
                  :profile {:kind :rectangle :x-dim width :y-dim height}
                  :position {:location start} :direction (mapv - end start)
                  :depth (sqrt (reduce + (map (fn [a b] (let [d (- b a)] (* d d))) start end)))})
     :connected-to (vec connected-to)}))

(defn route-mep
  "Route an orthogonal path on a 3D grid while avoiding expanded AABB obstacles."
  [start end obstacles {:keys [step clearance max-nodes]
                        :or {step 0.5 clearance 0.1 max-nodes 100000}}]
  (let [grid #(mapv (fn [value] (long (round (/ value step)))) %)
        world #(mapv (fn [value] (* step value)) %)
        start-grid (grid start) end-grid (grid end)
        blocked? (fn [node]
                   (let [point (world node)]
                     (some (fn [{:keys [min max]}]
                             (every? true? (map (fn [value lower upper]
                                                 (<= (- lower clearance) value (+ upper clearance)))
                                               point min max))) obstacles)))
        all-points (concat [start-grid end-grid] (mapcat #(map grid [(:min %) (:max %)]) obstacles))
        lower (mapv #(- (reduce min %) 4) (apply map vector all-points))
        upper (mapv #(+ (reduce max %) 4) (apply map vector all-points))
        directions [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
        inside? (fn [node] (every? true? (map <= lower node upper)))
        result
        (loop [queue [start-grid] cursor 0 came {start-grid nil}]
          (cond
            (>= cursor (count queue)) nil
            (> (count came) max-nodes) nil
            :else
            (let [current (nth queue cursor)]
              (if (= current end-grid)
                (loop [node current path []]
                  (if node (recur (came node) (conj path node)) (vec (reverse path))))
                (let [next-nodes (filter #(and (inside? %)
                                               (or (= % start-grid) (= % end-grid)
                                                   (not (blocked? %)))
                                               (not (contains? came %)))
                                         (map #(mapv + current %) directions))]
                  (recur (into queue next-nodes) (inc cursor)
                         (reduce #(assoc %1 %2 current) came next-nodes)))))))]
    (when result
      (->> result
           (mapv world)
           (reduce (fn [path point]
                     (if (< (count path) 2)
                       (conj path point)
                       (let [a (nth path (- (count path) 2)) b (peek path)
                             direction-a (mapv #(compare %2 %1) a b)
                             direction-b (mapv #(compare %2 %1) b point)]
                         (if (= direction-a direction-b) (conj (pop path) point) (conj path point))))) [])))))

(defn pressure-loss
  "Darcy-Weisbach pressure loss for a circular segment."
  [{:keys [length-m diameter-m roughness-m flow-m3-s density-kg-m3 viscosity-pa-s]}]
  (let [area (* pi (/ (* diameter-m diameter-m) 4.0)) velocity (/ flow-m3-s area)
        reynolds (/ (* density-kg-m3 velocity diameter-m) viscosity-pa-s)
        friction (if (< reynolds 2300.0) (/ 64.0 reynolds)
                     (/ 0.25 (pow
                              (log10 (+ (/ roughness-m (* 3.7 diameter-m))
                                        (/ 5.74 (pow reynolds 0.9)))) 2.0)))
        loss (* friction (/ length-m diameter-m) (/ (* density-kg-m3 velocity velocity) 2.0))]
    {:mep/reynolds reynolds :mep/friction-factor friction :mep/pressure-loss-pa loss}))

(defn size-round-mep-segment
  "Select the minimum circular diameter for a design flow and velocity limit."
  [flow-m3-s max-velocity-m-s available-diameters-m]
  (let [required (sqrt (/ (* 4.0 flow-m3-s) (* pi max-velocity-m-s)))
        selected (first (sort (filter #(>= % required) available-diameters-m)))]
    (when-not selected
      (throw (ex-info "no available MEP diameter satisfies velocity limit"
                      {:required-diameter-m required :available available-diameters-m})))
    {:mep/required-diameter-m required :mep/diameter-m selected
     :mep/velocity-m-s (/ flow-m3-s (* pi (/ (* selected selected) 4.0)))}))

(defn size-and-balance-mep-network
  "Size a directed tree network from terminal demands, calculate segment
  Darcy-Weisbach losses, identify the critical path, and report balancing
  pressure for every terminal plus pump/fan duty."
  [{:keys [source-node segments terminal-demands]} fluid
   {:keys [available-diameters-m max-velocity-m-s equipment-efficiency
           pressure-safety-factor]
    :or {max-velocity-m-s 2.0 equipment-efficiency 0.7 pressure-safety-factor 1.1}}]
  (let [ids (map :id segments)
        children (group-by :from segments)
        parent-count (frequencies (map :to segments))
        all-nodes (set (concat [source-node] (map :from segments) (map :to segments)))
        leaf-nodes (set (remove #(contains? children %) all-nodes))
        downstream-flow
        (fn downstream-flow [node stack]
          (when (contains? stack node)
            (throw (ex-info "MEP network contains a cycle" {:node node :stack stack})))
          (+ (or (get terminal-demands node) 0.0)
             (reduce + (map #(downstream-flow (:to %) (conj stack node))
                            (get children node)))))
        reachable
        (loop [pending [source-node] visited #{}]
          (if-let [node (peek pending)]
            (if (contains? visited node)
              (recur (pop pending) visited)
              (recur (into (pop pending) (map :to (get children node)))
                     (conj visited node)))
            visited))
        _ (when-not (seq segments)
            (throw (ex-info "MEP network requires at least one segment" {})))
        _ (when-not (= (count ids) (count (distinct ids)))
            (throw (ex-info "MEP network contains duplicate segment ids" {:ids ids})))
        _ (when-let [node (first (keep (fn [[node count]] (when (> count 1) node))
                                        parent-count))]
            (throw (ex-info "MEP network must be a directed tree" {:node node})))
        _ (when-not (= reachable all-nodes)
            (throw (ex-info "MEP network has unreachable nodes"
                            {:unreachable (set (remove reachable all-nodes))})))
        _ (when-not (= leaf-nodes (set (keys terminal-demands)))
            (throw (ex-info "terminal demands must cover exactly the network leaves"
                            {:leaf-nodes leaf-nodes
                             :demand-nodes (set (keys terminal-demands))})))
        _ (doseq [[node demand] terminal-demands]
            (when-not (and (number? demand) (pos? demand))
              (throw (ex-info "terminal demand must be positive"
                              {:node node :flow-m3-s demand}))))
        _ (doseq [{:keys [id length-m]} segments]
            (when-not (and (number? length-m) (pos? length-m))
              (throw (ex-info "MEP segment length must be positive"
                              {:segment id :length-m length-m}))))
        _ (when-not (and (seq available-diameters-m)
                         (every? #(and (number? %) (pos? %)) available-diameters-m))
            (throw (ex-info "available MEP diameters must be positive"
                            {:available-diameters-m available-diameters-m})))
        _ (when-not (and (number? max-velocity-m-s) (pos? max-velocity-m-s))
            (throw (ex-info "maximum MEP velocity must be positive"
                            {:max-velocity-m-s max-velocity-m-s})))
        _ (when-not (and (number? equipment-efficiency)
                         (pos? equipment-efficiency) (<= equipment-efficiency 1.0))
            (throw (ex-info "equipment efficiency must be in (0, 1]"
                            {:equipment-efficiency equipment-efficiency})))
        _ (when-not (and (number? pressure-safety-factor)
                         (>= pressure-safety-factor 1.0))
            (throw (ex-info "pressure safety factor must be at least one"
                            {:pressure-safety-factor pressure-safety-factor})))
        _ (doseq [key [:roughness-m :density-kg-m3 :viscosity-pa-s]]
            (when-not (and (number? (get fluid key)) (pos? (get fluid key)))
              (throw (ex-info "fluid properties must be positive"
                              {:property key :value (get fluid key)}))))
        source-flow (downstream-flow source-node #{})
        sized
        (mapv (fn [segment]
                (let [flow (downstream-flow (:to segment) #{})
                      sizing (size-round-mep-segment flow max-velocity-m-s
                                                     available-diameters-m)
                      loss (pressure-loss
                            (merge fluid {:length-m (:length-m segment)
                                          :diameter-m (:mep/diameter-m sizing)
                                          :flow-m3-s flow}))]
                  (merge segment sizing loss {:segment/flow-m3-s flow})))
              segments)
        sized-children (group-by :from sized)
        paths
        (letfn [(visit [node path loss]
                  (let [outgoing (get sized-children node)]
                    (if (seq outgoing)
                      (mapcat (fn [segment]
                                (visit (:to segment) (conj path (:id segment))
                                       (+ loss (:mep/pressure-loss-pa segment))))
                              outgoing)
                      [{:terminal node :segment-ids path :pressure-loss-pa loss}])))]
          (vec (visit source-node [] 0.0)))
        critical-loss (reduce max 0.0 (map :pressure-loss-pa paths))
        paths (mapv #(assoc % :balancing-pressure-pa
                            (- critical-loss (:pressure-loss-pa %))) paths)
        required-pressure (* critical-loss pressure-safety-factor)
        duty-power (/ (* source-flow required-pressure) equipment-efficiency)]
    {:mep.network/source-node source-node :mep.network/source-flow-m3-s source-flow
     :mep.network/segments sized :mep.network/terminal-paths paths
     :mep.network/critical-pressure-loss-pa critical-loss
     :mep.network/required-equipment-pressure-pa required-pressure
     :mep.network/equipment-power-w duty-power
     :mep.network/equipment-efficiency equipment-efficiency}))

(declare validate-mep-system)

(defn analyze-mep-system
  "Build the connector graph and calculate Darcy-Weisbach loss for each
  circular segment and the complete series path."
  [system fluid]
  (when-let [issues (seq (validate-mep-system system))]
    (throw (ex-info "invalid MEP system" {:issues issues})))
  (let [flow (:mep/design-flow system)
        connectors (mapcat :mep/connectors (:mep/segments system))
        graph (into {} (map (fn [connector]
                              [(:connector/id connector)
                               (vec (keep identity [(:connector/connected-to connector)]))])
                            connectors))
        results
        (mapv (fn [segment]
                (let [[start end] (get-in segment [:geometry :directrix])
                      length (sqrt (reduce + (map (fn [a b]
                                                   (let [delta (- b a)] (* delta delta)))
                                                 start end)))
                      diameter (* 2.0 (get-in segment [:geometry :radius]))
                      result (pressure-loss (merge fluid {:length-m length
                                                          :diameter-m diameter
                                                          :flow-m3-s flow}))]
                  (assoc result :segment/id (:id segment) :segment/length-m length
                         :segment/diameter-m diameter)))
              (:mep/segments system))]
    {:mep.analysis/system-id (:mep/id system) :mep.analysis/connector-graph graph
     :mep.analysis/segments results
     :mep.analysis/total-pressure-loss-pa (reduce + (map :mep/pressure-loss-pa results))}))

(defn validate-mep-system
  "Return coordination issues for dangling segments/connectors, non-reciprocal
  links, incompatible domains/shapes/sizes, and invalid flow direction pairs."
  [system]
  (let [ids (set (map :id (:mep/segments system)))
        connectors (mapcat (fn [segment]
                             (map #(assoc % :connector/segment (:id segment))
                                  (:mep/connectors segment)))
                           (:mep/segments system))
        connector-by-id (into {} (map (juxt :connector/id identity) connectors))
        segment-issues
        (mapcat (fn [segment]
                  (for [target (:connected-to segment) :when (not (contains? ids target))]
                    {:issue/type :mep/dangling-connection :issue/segment (:id segment)
                     :issue/target target :issue/system (:mep/id system)}))
                (:mep/segments system))
        connector-issues
        (mapcat
         (fn [connector]
           (when-let [target-id (:connector/connected-to connector)]
             (let [target (connector-by-id target-id)]
               (cond
                 (nil? target)
                 [{:issue/type :mep/dangling-connector :issue/connector (:connector/id connector)
                   :issue/target target-id :issue/system (:mep/id system)}]
                 (not= (:connector/id connector) (:connector/connected-to target))
                 [{:issue/type :mep/non-reciprocal-connection
                   :issue/connector (:connector/id connector) :issue/target target-id}]
                 (not= (:connector/domain connector) (:connector/domain target))
                 [{:issue/type :mep/incompatible-domain
                   :issue/connector (:connector/id connector) :issue/target target-id}]
                 (not= (:connector/shape connector) (:connector/shape target))
                 [{:issue/type :mep/incompatible-shape
                   :issue/connector (:connector/id connector) :issue/target target-id}]
                 (not= (:connector/size connector) (:connector/size target))
                 [{:issue/type :mep/incompatible-size
                   :issue/connector (:connector/id connector) :issue/target target-id}]
                 (= (:connector/flow-direction connector) (:connector/flow-direction target))
                 [{:issue/type :mep/incompatible-flow-direction
                   :issue/connector (:connector/id connector) :issue/target target-id}]
                 :else []))))
         connectors)]
    (vec (concat segment-issues connector-issues))))

(defn federated-design
  [{:keys [architectural structural mep]}]
  {:federation/schema-version schema-version
   :federation/architectural architectural
   :federation/structural (vec structural)
   :federation/mep (vec mep)
   :federation/issues (vec (mapcat validate-mep-system mep))})

(defn change-event
  [{:keys [id actor clock operation path value]}]
  (document-change/event {:id id :actor actor :clock clock :operation operation
                          :path path :value value}))

(defn apply-change
  "Apply a collaboration event. Events remain append-only outside this
  function; deterministic [clock actor id] ordering gives every peer the same result."
  [document event]
  (document-change/apply-event document event))

(defn merge-events [document events]
  (document-change/replay document events))

(defn collaboration-workspace [project]
  (collaboration/workspace {:id (:id project) :document project :default-branch :main}))

(defn create-design-branch [workspace branch-id from-revision]
  (collaboration/branch workspace branch-id from-revision))

(defn commit-design-revision [workspace revision]
  (collaboration/commit workspace revision))

(defn review-design-revision [workspace review]
  (collaboration/review workspace review))

(defn merge-design-branches [workspace merge-request]
  (collaboration/merge-branches workspace merge-request))

(defn resolve-design-merge [merge-result resolution]
  (collaboration/resolve-merge merge-result resolution))

(defn create-coordination-issue [workspace issue]
  (collaboration/create-issue workspace issue))

(defn transition-coordination-issue [workspace issue-id status actor clock comment]
  (collaboration/transition-issue workspace issue-id status actor clock comment))

(defn collaboration-checkpoint [workspace peer-id]
  (collaboration/checkpoint workspace peer-id))

(declare cloud-itonami-payload)
(defn cloud-itonami-sync-payload
  "Create an incremental, replayable cloud-itonami collaboration envelope."
  [workspace checkpoint]
  (let [delta (collaboration/changes-since workspace checkpoint)
        default-branch (:collab/default-branch workspace)
        head (collaboration/branch-head workspace default-branch)
        project (:revision/document (collaboration/revision workspace head))]
    {:itonami/event :design/collaboration-synchronized
     :itonami/contract-version schema-version
     :design/project-id (:id project) :design/project-name (:name project)
     :design/head-revision head :design/branches (:sync/branches delta)
     :design/revisions (:sync/revisions delta)
     :coordination/issues (:sync/issues delta)
     :coordination/reviews (:sync/reviews delta)
     :design/line-items (:design/line-items (cloud-itonami-payload project head))}))

(defn cloud-itonami-payload
  "Project quantities and classifications into a versioned cloud-itonami
  design-change envelope suitable for estimating, procurement and scheduling."
  [project revision]
  {:itonami/event :design/revision-published
   :itonami/contract-version schema-version
   :design/project-id (:id project) :design/project-name (:name project)
   :design/revision revision
   :design/line-items
   (mapv (fn [element]
           {:bim/element-id (:id element) :bim/global-id (:global-id element)
            :design/kind (:kind element)
            :classification/source (get-in element [:classification :source])
            :classification/code (get-in element [:classification :code])
            :quantity/values (:quantities element)
            :procurement/materials (:material-layers element)
            :coordination/connections (:connected-to element)})
         (all-elements project))})
