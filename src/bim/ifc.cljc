(ns bim.ifc
  "Compatibility facade. New consumers should require shared `ifc.core`."
  (:require [ifc.core :as ifc]
            [bim.integration :as integration]))

(def schema ifc/schema)
(defn write-spf [project] (ifc/write-spf (integration/export-ifc project)))
(def read-spf ifc/read-spf)
