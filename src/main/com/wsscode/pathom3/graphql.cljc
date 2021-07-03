(ns com.wsscode.pathom3.graphql
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
    [com.wsscode.misc.coll :as coll]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.interface.smart-map :as psm]))

(>def ::ident-map
  (s/map-of string?
            (s/map-of string?
                      (s/tuple string? string?))))

(>def ::namespace string?)

(def schema-query
  [{:__schema
    [{:queryType
      [:name]}

     {:mutationType
      [:name]}

     {:types
      [:name
       :kind
       {:interfaces [:name :kind]}
       {:fields
        [:name
         {:args [:name :defaultValue {:type [:kind :name {:ofType 3}]}]}
         {:type [:kind :name {:ofType 3}]}]}
       {:inputFields
        [:name
         {:type [:kind :name {:ofType 3}]}]}]}]}])

(defn prefixed-key [{::keys [namespace]} p s] (keyword (str namespace "." p) s))
(defn type-key [env s] (prefixed-key env "types" s))
(defn interface-key [env s] (prefixed-key env "interfaces" s))

(defn type-leaf-name [{:strs [kind name ofType]}]
  (case kind
    "NON_NULL" (recur ofType)
    "LIST" (recur ofType)
    "OBJECT" name
    "INTERFACE" name
    nil))

(defn type->field-name [env {:strs [kind name ofType]}]
  (case kind
    "NON_NULL" (recur env ofType)
    "LIST" (recur env ofType)
    "OBJECT" (type-key env name)
    "INTERFACE" (interface-key env name)
    nil))

(defn entity-field-key [{::keys [namespace]} entity field]
  (keyword (str namespace "." entity) field))

; region schema resolvers

(pco/defresolver dynamic-resolver-name [{::keys [namespace]} _]
  {::gql-dynamic-op-name (symbol namespace "pathom-entry-dynamic-resolver")})

(pco/defresolver graphql-schema [{::keys [gql-schema-raw]} _]
  {::gql-schema (get-in gql-schema-raw ["data" "__schema"])})

(pco/defresolver types-index [{::keys [gql-schema]}]
  {::gql-types-index
   (coll/index-by #(get % "name")
     (get gql-schema "types"))})

(pco/defresolver all-types [{::keys [gql-schema]}]
  {::pco/output
   [{::gql-all-types
     [::gql-type-raw]}]}
  {::gql-all-types
   (mapv #(array-map ::gql-type-raw %) (get gql-schema "types"))})

(pco/defresolver indexable-types [{::keys [gql-all-types]}]
  {::pco/input
   [{::gql-all-types
     [::gql-type-raw
      ::gql-type-indexable?]}]

   ::pco/output
   [{::gql-indexable-types
     [::gql-type-raw
      ::gql-type-indexable?]}]}

  {::gql-indexable-types
   (filterv ::gql-type-indexable? gql-all-types)})

(pco/defresolver query-type [{::keys [gql-schema]}]
  {::gql-query-type {::gql-type-name (get-in gql-schema ["queryType" "name"])}})

; endregion

; region type data

(pco/defresolver type-data-raw [{::keys [gql-types-index gql-type-name]}]
  {::gql-type-raw (get gql-types-index gql-type-name)})

(pco/defresolver type-name [{::keys [gql-type-raw]}]
  {::gql-type-name
   (get gql-type-raw "name")})

(pco/defresolver type-qualified-name [env {::keys [gql-type-raw]}]
  {::gql-type-qualified-name
   (type->field-name env gql-type-raw)})

(pco/defresolver type-kind [{::keys [gql-type-raw]}]
  {::gql-type-kind (get gql-type-raw "kind")})

(pco/defresolver type-interfaces [{::keys [gql-type-raw]}]
  {::pco/output
   [{::gql-type-interfaces
     [::gql-type-name]}]}
  {::gql-type-interfaces
   (mapv #(array-map ::gql-type-name (get % "name"))
     (get gql-type-raw "interfaces"))})

(pco/defresolver type-fields [{::keys [gql-type-raw gql-type-name]}]
  {::pco/output
   [{::gql-type-fields
     [::gql-field-raw
      ::gql-type-name]}]}

  {::gql-type-fields
   (mapv #(array-map
            ::gql-field-raw %
            ::gql-type-name gql-type-name)
     (get gql-type-raw "fields"))})

(pco/defresolver indexable-type? [{::keys [gql-type-kind]}]
  {::gql-type-indexable?
   (contains? #{"OBJECT" "INTERFACE"} gql-type-kind)})

; endregion

; region fields

(pco/defresolver field-name [{::keys [gql-field-raw]}]
  {::gql-field-name (get gql-field-raw "name")})

(pco/defresolver field-qualified-name [env {::keys [gql-field-name gql-type-name]}]
  {::gql-field-qualified-name
   (entity-field-key env gql-type-name gql-field-name)})

(pco/defresolver field-type [{::keys [gql-field-raw]}]
  {::pco/output
   [{::gql-field-type
     [::gql-type-name]}]}

  {::gql-field-type
   {::gql-type-name (type-leaf-name (get gql-field-raw "type"))}})

(pco/defresolver field-type-qualified-name [{::keys [gql-field-type]}]
  {::pco/input
   [{::gql-field-type
     [::gql-type-qualified-name]}]}

  {::gql-field-type-qualified-name
   (get gql-field-type ::gql-type-qualified-name)})

; endregion

; region pathom resolvers generation

(pco/defresolver type-resolver-op-name [{::keys [namespace]} {::keys [gql-type-name]}]
  {::gql-type-resolver-op-name
   (symbol namespace (str gql-type-name "-resolver"))})

(pco/defresolver type-resolver-input [{::keys [gql-type-qualified-name]}]
  {::gql-type-resolver-input
   [gql-type-qualified-name]})

(pco/defresolver type-resolver-output
  [{::keys [gql-type-fields
            gql-type-interfaces]}]
  {::pco/input
   [{::gql-type-fields
     [::gql-field-output-entry]}
    {::gql-type-interfaces
     [::gql-type-qualified-name]}]}
  {::gql-type-resolver-output
   (-> (mapv ::gql-type-qualified-name gql-type-interfaces)
       (into (mapv ::gql-field-output-entry gql-type-fields)))})

(pco/defresolver type-resolver
  [{::keys [gql-type-resolver-op-name
            gql-type-resolver-input
            gql-type-resolver-output
            gql-dynamic-op-name]}]
  {::gql-type-resolver
   {::pco/op-name      gql-type-resolver-op-name
    ::pco/dynamic-name gql-dynamic-op-name
    ::pco/input        gql-type-resolver-input
    ::pco/output       gql-type-resolver-output}})

(pco/defresolver field-output-entry
  [{::keys [gql-field-qualified-name
            gql-field-type-qualified-name]}]
  {::pco/input
   [::gql-field-qualified-name
    (pco/? ::gql-field-type-qualified-name)]}
  {::gql-field-output-entry
   (if gql-field-type-qualified-name
     {gql-field-qualified-name
      [gql-field-type-qualified-name]}
     gql-field-qualified-name)})

(pco/defresolver schema->pathom-indexes [{::keys [gql-indexable-types]}]
  {::pco/input
   [{::gql-indexable-types
     [::gql-type-qualified-name
      ::gql-type-resolver]}]}
  {::gql-pathom-indexes
   (pci/register
     {::pci/transient-attrs (into #{} (map ::gql-type-qualified-name) gql-indexable-types)}
     (mapv (comp pco/resolver ::gql-type-resolver) gql-indexable-types))})

; endregion

(def env
  (-> {:com.wsscode.pathom3.connect.planner/plan-cache* (atom {})}
      (pci/register
        [dynamic-resolver-name

         graphql-schema
         types-index
         all-types
         indexable-types
         query-type

         type-data-raw
         type-name
         type-qualified-name
         type-kind
         type-interfaces
         type-fields
         indexable-type?

         type-resolver-op-name
         type-resolver-input
         type-resolver-output
         type-resolver

         field-name
         field-qualified-name
         field-type
         field-type-qualified-name
         field-output-entry

         schema->pathom-indexes])
      ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
       "gql")))

(defn load-schema [env' gql-schema-raw]
  (psm/smart-map
    (-> env
        (merge env')
        (assoc
          ::psm/persistent-cache? true
          ::gql-schema-raw gql-schema-raw))))

(comment
  (tap> env)

  (p.eql/process env
    [{::gql-query-type [::gql-type-name]}]))
