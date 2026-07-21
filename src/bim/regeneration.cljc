(ns bim.regeneration
  "Coordinated incremental regeneration of BIM-derived design artifacts."
  (:require [bim.integration :as integration]
            [ifc.core :as ifc]
            [kotoba.document.artifact-graph :as artifact-graph]))

(def schema-version 1)

(defn- structural-model [project config]
  (case (or (:mode config) (if (:structural/model project) :preserve :derive))
    :preserve (or (:structural/model project)
                  (integration/generate-structural-model project (:options config)))
    :derive (integration/generate-structural-model project (:options config))
    (throw (ex-info "unsupported structural regeneration mode" {:config config}))))

(defn- structural-results [model request]
  (if-not request
    {}
    (case [(:kind request) (boolean (:combination-id request))]
      [:truss false] {(:load-case-id request)
                      (integration/analyze-truss model (:load-case-id request))}
      [:truss true] {(:combination-id request)
                     (integration/analyze-structural-combination
                      model (:combination-id request))}
      [:frame-2d false] {(:load-case-id request)
                         (integration/analyze-2d-frame-model
                          model (:load-case-id request))}
      [:frame-2d true] {(:combination-id request)
                        (integration/analyze-2d-frame-combination
                         model (:combination-id request))}
      [:frame-3d false] {(:load-case-id request)
                         (integration/analyze-3d-frame-model
                          model (:load-case-id request))}
      [:frame-3d true] {(:combination-id request)
                        (integration/analyze-3d-frame-combination
                         model (:combination-id request))}
      (throw (ex-info "unsupported structural analysis request" {:request request})))))

(defn- mep-designs [project requests]
  (let [systems (into {} (map (juxt :mep/id identity) (:mep/systems project)))]
    (into {}
          (map (fn [[system-id request]]
                 (let [system (or (systems system-id)
                                  (throw (ex-info "MEP design system not found"
                                                  {:system-id system-id})))]
                   [system-id
                    (integration/size-and-balance-authored-mep-system
                     system (:source-node request) (:terminal-demands request)
                     (:fluid request) (:options request))])))
          requests)))

(defn- coordinate-project [project structure designs results]
  (let [designed-systems (into {} (map (fn [[id design]]
                                         [id (:mep.design/system design)])) designs)
        systems (mapv #(get designed-systems (:mep/id %) %) (:mep/systems project))]
    (cond-> (assoc project :structural/model structure :mep/systems systems)
      (seq results) (assoc :structural/results results)
      (seq designs) (assoc :mep/design-results
                           (into {} (map (fn [[id design]]
                                          [id (:mep.design/analysis design)])) designs)))))

(def design-graph
  {:structural/model
   {:depends-on [:project :structural/config] :version 1
    :build (fn [{:keys [dependencies]}]
             (structural-model (:project dependencies)
                               (:structural/config dependencies)))}
   :structural/results
   {:depends-on [:structural/model :structural/request] :version 1
    :build (fn [{:keys [dependencies]}]
             (structural-results (:structural/model dependencies)
                                 (:structural/request dependencies)))}
   :mep/designs
   {:depends-on [:project :mep/requests] :version 1
    :build (fn [{:keys [dependencies]}]
             (mep-designs (:project dependencies) (:mep/requests dependencies)))}
   :coordinated/project
   {:depends-on [:project :structural/model :structural/results :mep/designs] :version 1
    :build (fn [{:keys [dependencies]}]
             (coordinate-project (:project dependencies)
                                 (:structural/model dependencies)
                                 (:mep/designs dependencies)
                                 (:structural/results dependencies)))}
   :drawing/set
   {:depends-on [:coordinated/project :drawing/seed] :version 1
    :build (fn [{:keys [dependencies]}]
             (if-let [drawing (:drawing/seed dependencies)]
               (integration/regenerate-drawing-set
                (:coordinated/project dependencies) drawing)
               (integration/generate-drawing-set (:coordinated/project dependencies))))}
   :ifc/document
   {:depends-on [:coordinated/project] :version 1
    :build (fn [{:keys [dependencies]}]
             (integration/export-ifc (:coordinated/project dependencies)))}
   :ifc/spf
   {:depends-on [:ifc/document] :version 1
    :build (fn [{:keys [dependencies]}]
             (ifc/write-spf (:ifc/document dependencies)))}})

(defn regenerate
  "Incrementally regenerate coordinated project, analysis, MEP, drawings, and
  IFC from one authoritative authored project. The returned graph state can be
  passed back as `previous`; unchanged artifacts are reused exactly."
  ([inputs] (regenerate inputs (artifact-graph/state) {}))
  ([inputs previous] (regenerate inputs previous {}))
  ([{:keys [project structural-config structural-request mep-requests drawing-set]}
    previous options]
   (when-not project
     (throw (ex-info "BIM regeneration requires an authored project" {})))
   (let [managed-drawing (get-in previous [:artifact.graph/values :drawing/set])
         sources {:project project
                  :structural/config (or structural-config {})
                  :structural/request structural-request
                  :mep/requests (or mep-requests {})
                  :drawing/seed (or drawing-set managed-drawing)}
         default-tokens (assoc sources :drawing/seed drawing-set)
         tokens (or (:source-tokens options) default-tokens)
         state (artifact-graph/rebuild
                design-graph sources previous
                (merge {:source-tokens tokens} (select-keys options [:targets :invalidate])))]
     (assoc state :design/schema-version schema-version
                  :design/project (get-in state [:artifact.graph/values :coordinated/project])
                  :design/drawing-set (get-in state [:artifact.graph/values :drawing/set])
                  :design/ifc-document (get-in state [:artifact.graph/values :ifc/document])
                  :design/ifc-spf (get-in state [:artifact.graph/values :ifc/spf])))))
