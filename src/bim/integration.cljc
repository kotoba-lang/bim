(ns bim.integration
  "Portable contracts that connect BIM authoring, drawings, IFC exchange,
  collaboration, and cloud-itonami. All functions are pure and CLJC-safe."
  (:require [clojure.walk :as walk]
            [ifc.core :as ifc]
            [kotoba.document.change :as document-change]))

(def schema-version 1)

(defn family-definition
  [{:keys [id name category parameters formulas template]}]
  {:family/id id :family/name name :family/category category
   :family/parameters (or parameters {}) :family/formulas (or formulas {})
   :family/template template :family/schema-version schema-version})

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

(defn resolve-family-parameters
  "Resolve parameter defaults, overrides, then formulas in declaration order."
  [family overrides]
  (reduce-kv (fn [params k expression]
               (assoc params k (eval-expr params expression)))
             (merge (into {} (map (fn [[k spec]] [k (:default spec)])
                                  (:family/parameters family)))
                    overrides)
             (:family/formulas family)))

(defn instantiate-family
  "Materialize a serializable family template. A value shaped as [:param :x]
  is replaced by the resolved parameter value."
  [family instance-id overrides]
  (let [params (resolve-family-parameters family overrides)
        body (walk/postwalk #(if (and (vector? %) (= :param (first %)))
                               (get params (second %)) %)
                            (:family/template family))]
    (assoc body :id instance-id
                :family/id (:family/id family)
                :family/parameters params)))

(defn drawing-view
  [{:keys [id kind name scale storey-id section-box discipline]}]
  {:view/id id :view/kind kind :view/name name :view/scale (or scale 100)
   :view/storey-id storey-id :view/section-box section-box
   :view/discipline (or discipline :architectural)})

(defn drawing-sheet [{:keys [id number name size views revisions]}]
  {:sheet/id id :sheet/number number :sheet/name name :sheet/size (or size :a1)
   :sheet/views (vec views) :sheet/revisions (vec revisions)})

(defn- all-storeys [project]
  (mapcat :storeys (mapcat :buildings (:sites project))))

(defn- all-elements [project]
  (mapcat :elements (all-storeys project)))

(defn generate-drawing-set
  "Generate deterministic floor-plan views and one coordination sheet."
  [project]
  (let [views (mapv (fn [storey]
                      (drawing-view {:id (str "plan-" (:id storey)) :kind :floor-plan
                                     :name (:name storey) :storey-id (:id storey)}))
                    (all-storeys project))]
    {:drawing/schema-version schema-version
     :drawing/views views
     :drawing/sheets [(drawing-sheet {:id "A-001" :number "A-001"
                                      :name "General Arrangements"
                                      :views (mapv :view/id views)})]}))

(defn export-ifc
  "Create a lossless, EDN-native IFC 4.3 semantic exchange document. This is
  the canonical boundary consumed by STEP/JSON adapters; it is not STEP text."
  [project]
  (ifc/exchange-document
   {:project {:id (:id project) :name (:name project) :model project}
    :elements (mapv (fn [element]
                      {:id (:id element) :global-id (:global-id element)
                       :kind (:kind element) :name (:name element)})
                    (all-elements project))}))

(defn import-ifc [document]
  (when-not (= "IFC4X3_ADD2" (:ifc/schema document))
    (throw (ex-info "unsupported IFC schema" {:schema (:ifc/schema document)})))
  (get-in document [:ifc/project :model]))

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
