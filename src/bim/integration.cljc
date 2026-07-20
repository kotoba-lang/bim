(ns bim.integration
  "Portable contracts that connect BIM authoring, drawings, IFC exchange,
  collaboration, and cloud-itonami. All functions are pure and CLJC-safe."
  (:require [clojure.walk :as walk]
            [bim :as bim]
            [ifc.core :as ifc]
            [kotoba.document.change :as document-change]))

(def schema-version 1)

(defn family-definition
  [{:keys [id name category parameters formulas constraints types template]}]
  {:family/id id :family/name name :family/category category
   :family/parameters (or parameters {}) :family/formulas (or formulas {})
   :family/constraints (vec constraints) :family/types (or types {})
   :family/template template :family/schema-version schema-version})

(defn family-catalog [families]
  {:family-catalog/schema-version schema-version
   :family-catalog/families (into {} (map (juxt :family/id identity) families))})

(defn- eval-expr [params expression]
  (cond
    (and (vector? expression) (= :param (first expression)))
    (get params (second expression))

    (vector? expression)
    (let [[op & operands] expression
          values (map #(eval-expr params %) operands)]
      (case op
        :+ (reduce + values)
        :- (reduce - values)
        :* (reduce * values)
        :/ (reduce / values)
        :min (reduce min values)
        :max (reduce max values)
        (throw (ex-info "unsupported family expression" {:expression expression}))))

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

(defn- validate-constraints! [family params]
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
      (throw (ex-info "unsupported family constraint" {:constraint constraint}))))
  params)

(defn resolve-family-parameters
  "Resolve defaults and overrides, then formulas by dependency order. Cycles,
  missing references, invalid values, and failed dimensional constraints throw."
  ([family overrides] (resolve-family-parameters family nil overrides false))
  ([family type-key overrides enforce-scopes?]
   (let [specs (:family/parameters family)
         type-spec (get-in family [:family/types type-key])
         type-overrides (or (:parameters type-spec) {})]
     (when enforce-scopes?
       (doseq [name (keys type-overrides)]
         (when (= :instance (get-in specs [name :scope]))
           (throw (ex-info "type cannot override instance parameter" {:parameter name :type type-key}))))
       (doseq [name (keys overrides)]
         (when (= :type (get-in specs [name :scope]))
           (throw (ex-info "instance cannot override type parameter" {:parameter name :type type-key})))))
     (let [initial (merge (into {} (map (fn [[k spec]] [k (:default spec)]) specs))
                          type-overrides overrides)
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
       (validate-constraints! family params)))))

(declare instantiate-family*)

(defn- materialize-template [catalog value stack]
  (cond
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
        substituted (walk/postwalk #(if (and (vector? %) (= :param (first %)))
                                      (get params (second %)) %)
                                   (:family/template family))
        body (if catalog (materialize-template catalog substituted stack) substituted)
        type-spec (get-in family [:family/types type-key])]
    (cond-> (assoc body :id instance-id
                        :family/id (:family/id family)
                        :family/type type-key
                        :family/parameters params)
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

(defn drawing-view
  [{:keys [id kind name scale storey-id building-id section-box cut-plane direction
           discipline annotations]}]
  {:view/id id :view/kind kind :view/name name :view/scale (or scale 100)
   :view/storey-id storey-id :view/building-id building-id
   :view/section-box section-box :view/cut-plane cut-plane :view/direction direction
   :view/annotations (vec annotations)
   :view/discipline (or discipline :architectural)})

(defn drawing-sheet [{:keys [id number name size views revisions]}]
  {:sheet/id id :sheet/number number :sheet/name name :sheet/size (or size :a1)
   :sheet/views (vec views) :sheet/revisions (vec revisions)})

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
        views (vec (concat plans orthographic))]
    {:drawing/schema-version schema-version
     :drawing/views views
     :drawing/sheets [(drawing-sheet {:id "A-001" :number "A-001"
                                      :name "General Arrangements"
                                      :views (mapv :view/id views)
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
  {:value (:value value)
   :value-type (case (:kind value)
                 :bool :ifcboolean :int :ifcinteger :real :ifcreal
                 :measured (case (:unit value) :metre :ifclengthmeasure :ifcreal)
                 :ifclabel)})

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

(defn- exported-element [storey element]
  {:id (:id element) :global-id (:global-id element)
   :kind (:kind element) :name (:name element) :container-id (:id storey)
   :placement (when (map? (:placement element)) (:placement element))
   :geometry (exported-geometry element)
   :property-sets (exported-psets element)
   :type-object (:type-object element)
   :openings (if (= :wall (:kind element))
               (mapv #(exported-opening element %) (:openings element)) [])})

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
                             :children []})
                          (:storeys building))})
                 (:buildings site))})
        (:sites project)))

(defn export-ifc
  "Create a lossless, EDN-native IFC 4.3 semantic exchange document. This is
  the canonical boundary consumed by STEP/JSON adapters; it is not STEP text."
  [project]
  (ifc/exchange-document
   {:project {:id (:id project) :global-id (str (:id project)) :name (:name project)
              :georeference (or (:ifc/georeference project) (:georeference project))
              :children (exported-spatial-tree project) :model project}
    :elements (mapv (fn [[storey element]] (exported-element storey element))
                    (mapcat (fn [storey] (map #(vector storey %) (:elements storey)))
                            (all-storeys project)))}))

(defn import-ifc [document]
  (when-not (= "IFC4X3_ADD2" (:ifc/schema document))
    (throw (ex-info "unsupported IFC schema" {:schema (:ifc/schema document)})))
  (get-in document [:ifc/project :model]))

(defn- spatial-descendants [node type]
  (filter #(= type (:type %)) (tree-seq #(seq (:children %)) :children node)))

(defn- imported-property-value [{:keys [value value-type]}]
  (case value-type
    :ifcboolean (bim/bool-value value)
    :ifcinteger (bim/int-value value)
    (:ifcreal :ifclengthmeasure :ifcareameasure :ifcvolumemeasure) (bim/real-value value)
    (bim/text-value (str value))))

(defn- imported-psets [source]
  (into {}
        (map (fn [[name pset]]
               [name (bim/property-set name
                                        (into {} (map (fn [[property-name property]]
                                                        [(keyword property-name)
                                                         (imported-property-value property)]))
                                              (:properties pset)))])
             (:property-sets source))))

(defn- attach-imported-openings [wall source]
  (reduce
   (fn [host opening]
     (let [host-location (get-in source [:placement :location] [0.0 0.0 0.0])
           opening-location (get-in opening [:placement :location] host-location)
           profile (get-in opening [:geometry :profile])
           width (:x-dim profile) height (get-in opening [:geometry :depth])
           offset (- (first opening-location) (first host-location))
           sill (- (nth opening-location 2 0.0) (nth host-location 2 0.0))]
       (if (and width height)
         (bim/add-opening-to-wall
          host (bim/rectangular-opening {:id (:id opening) :offset offset :sill sill
                                         :width width :height height
                                         :filled-by (or (:filled-by-global-id opening)
                                                        (:filled-by opening))}))
         host)))
   wall (:openings source)))

(defn- imported-element [source]
  (let [[x y z] (get-in source [:placement :location] [0.0 0.0 0.0])
        geometry (:geometry source)
        profile (:profile geometry)
        x-dim (:x-dim profile) y-dim (:y-dim profile) depth (:depth geometry)
        psets (imported-psets source)
        result
        (case (:kind source)
          :wall (if (and x-dim y-dim depth)
                  (attach-imported-openings
                   (bim/wall {:id (:id source) :name (:name source)
                              :start [x y z] :end [(+ x x-dim) y z]
                              :thickness y-dim :height depth}) source)
                  (bim/element {:id (:id source) :kind :wall :name (:name source)
                                :geometry geometry}))
          :slab (if (and x-dim y-dim depth)
                  (bim/slab {:id (:id source) :name (:name source)
                             :boundary [[x y z] [(+ x x-dim) y z]
                                        [(+ x x-dim) (+ y y-dim) z] [x (+ y y-dim) z]]
                             :thickness depth})
                  (bim/element {:id (:id source) :kind :slab :name (:name source)
                                :geometry geometry}))
          (bim/element {:id (:id source) :kind (if (= :proxy (:kind source)) :other (:kind source)) :name (:name source)
                        :placement (:placement source) :geometry geometry}))]
    (assoc result :global-id (:global-id source) :ifc/source-id (:id source)
                  :ifc/kind (:kind source)
                  :type-object (:type-object source)
                  :ifc/property-sets (:property-sets source)
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
          storey-models (into {}
                              (map (fn [node]
                                     [(:id node)
                                      (bim/storey {:id (:id node) :name (:name node)
                                                   :elevation (or (get-in node [:placement :location 2]) 0.0)
                                                   :height 3.0 :placement (:placement node)
                                                   :spaces []
                                                   :elements (mapv imported-element
                                                                   (get elements-by-storey (:id node)))})]))
                              storeys)
          building-models (mapv (fn [node]
                                  (bim/building {:id (:id node) :name (:name node)
                                                 :placement (:placement node) :reference-elevation 0.0
                                                 :storeys (mapv #(get storey-models (:id %))
                                                                (filter (comp #{:ifcbuildingstorey} :type)
                                                                        (:children node)))}))
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
       :psets {}
       :sites [(bim/site {:id (or (:id site-node) 1) :name (or (:name site-node) "Site")
                          :geo (when site-node
                                 (bim/geo-ref
                                  (compound->decimal-degrees (:latitude site-node))
                                  (compound->decimal-degrees (:longitude site-node))
                                  (:elevation site-node)))
                          :placement (:placement site-node)
                          :buildings building-models})]})))

(defn import-ifc-spf [text]
  (import-external-ifc (ifc/read-document text)))

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

(defn validate-mep-system
  "Return portable coordination issues for dangling segment connections."
  [system]
  (let [ids (set (map :id (:mep/segments system)))]
    (vec
     (mapcat (fn [segment]
               (for [target (:connected-to segment) :when (not (contains? ids target))]
                 {:issue/type :mep/dangling-connection :issue/segment (:id segment)
                  :issue/target target :issue/system (:mep/id system)}))
             (:mep/segments system)))))

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
