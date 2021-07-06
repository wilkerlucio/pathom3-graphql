(ns com.wsscode.pathom3.graphql
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
    [com.wsscode.misc.coll :as coll]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.connect.planner :as pcp]
    [com.wsscode.pathom3.connect.runner :as pcr]
    [com.wsscode.pathom3.format.eql :as pf.eql]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.interface.smart-map :as psm]
    [com.wsscode.pathom3.plugin :as p.plugin]
    [com.wsscode.promesa.macros :refer [clet]]
    [edn-query-language.eql-graphql :as eql-gql]))

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

(defn convert-back [env response]
  (let [ast (-> env
                :com.wsscode.pathom3.connect.planner/node
                :com.wsscode.pathom3.connect.planner/foreign-ast)]
    (pf.eql/map-select-ast
      (p.plugin/register
        {::p.plugin/id
         `string-select

         ::pf.eql/wrap-map-select-entry
         (fn [select-entry]
           (fn [env source {:keys [key] :as ast}]
             (if-let [entry (select-entry env source
                              (-> ast (update :key name) (update :dispatch-key name)))]
               (coll/make-map-entry key (val entry)))))})
      (get response "data")
      ast)))

(defn process-gql-request [{::keys [request]} env input]
  (let [node     (:com.wsscode.pathom3.connect.planner/node env)
        ast      (:com.wsscode.pathom3.connect.planner/foreign-ast node)
        gql      (-> ast eql-gql/ast->graphql)
        response (request gql)]
    (pcr/merge-node-stats! env node
      {::request gql})
    (convert-back env response)))

(defn next-is-expected-dynamic?
  [{::pcp/keys [node graph]} gql-dynamic-op-name]
  (= gql-dynamic-op-name (as-> node <>
                           (::pcp/run-next <>)
                           (pcp/get-node graph <> ::pco/op-name))))

(defn pull-nested-attribute-resolver
  "Use this to name a nested value or a collection of nested values."
  ([join-entry nested-attribute output-attr]
   (pull-nested-attribute-resolver join-entry nested-attribute output-attr []))
  ([join-entry nested-attribute output-attr coll]
   (let [op-name (pbir/attr->sym output-attr "pull")]
     (pco/resolver op-name
       {::pco/input
        [{join-entry
          [nested-attribute]}]

        ::pco/output
        [output-attr]}
       (fn [_ input]
         (let [result (get input join-entry)]
           {output-attr
            (if (coll/collection? result)
              (into coll (map #(get % nested-attribute)) result)
              (get result nested-attribute))}))))))

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

(pco/defresolver object-types [{::keys [gql-all-types]}]
  {::pco/input
   [{::gql-all-types
     [::gql-type-raw
      ::gql-type-object?]}]

   ::pco/output
   [{::gql-object-types
     [::gql-type-raw
      ::gql-type-object?]}]}

  {::gql-object-types
   (filterv ::gql-type-object? gql-all-types)})

(pco/defresolver interface-types [{::keys [gql-all-types]}]
  {::pco/input
   [{::gql-all-types
     [::gql-type-raw
      ::gql-type-interface?]}]

   ::pco/output
   [{::gql-interface-types
     [::gql-type-raw
      ::gql-type-interface?]}]}

  {::gql-interface-types
   (filterv ::gql-type-interface? gql-all-types)})

(pco/defresolver query-type [{::keys [gql-schema]}]
  {::gql-query-type {::gql-type-name (get-in gql-schema ["queryType" "name"])}})

(pco/defresolver query-type-qualified-name
  [{::keys [gql-query-type]}]
  {::pco/input [{::gql-query-type [::gql-type-qualified-name]}]}
  {::gql-query-type-qualified-name
   (get gql-query-type ::gql-type-qualified-name)})

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

(pco/defresolver type-indexable? [{::keys [gql-type-kind]}]
  {::gql-type-indexable?
   (contains? #{"OBJECT" "INTERFACE"} gql-type-kind)})

(pco/defresolver type-object? [{::keys [gql-type-kind]}]
  {::gql-type-object?
   (= "OBJECT" gql-type-kind)})

(pco/defresolver type-interface? [{::keys [gql-type-kind]}]
  {::gql-type-interface?
   (= "INTERFACE" gql-type-kind)})

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

(def field-type-qualified-name
  (pull-nested-attribute-resolver
    ::gql-field-type
    ::gql-type-qualified-name
    ::gql-field-type-qualified-name))

; endregion

; region query type

(pco/defresolver query-type-field-raw
  [{::keys [gql-field-name
            gql-query-type]}]
  {::pco/input
   [::gql-field-name
    {::gql-query-type
     [::gql-type-name
      {::gql-type-fields
       [::gql-field-name
        ::gql-field-raw]}]}]

   ::pco/output
   [{::gql-query-type-field
     [::gql-field-name
      ::gql-field-raw]}]}

  {::gql-query-type-field
   (-> (coll/find-first
         #(= gql-field-name (::gql-field-name %))
         (->> gql-query-type
              ::gql-type-fields))
       (assoc ::gql-type-name (::gql-type-name gql-query-type)))})

; endregion

; region ident map

(pco/defresolver ident-map-entries
  [{::keys [ident-map]} {::keys [gql-query-type]}]
  {::pco/input
   [{::gql-query-type
     [::gql-type-name]}]

   ::pco/output
   [{::gql-ident-map-entries
     [::gql-ident-map-entry-raw
      ::gql-type-name
      ::gql-field-name]}]}
  {::gql-ident-map-entries
   (mapv #(array-map
            ::gql-ident-map-entry-raw %
            ::gql-type-name (::gql-type-name gql-query-type)
            ::gql-field-name (key %))
     ident-map)})

(pco/defresolver ident-map-entry-params
  [{::keys [gql-ident-map-entry-raw]}]
  {::pco/output
   [{::gql-ident-map-params
     [::gql-type-name
      ::gql-field-name]}]}
  {::gql-ident-map-params
   (mapv #(array-map
            ::gql-type-name (first (val %))
            ::gql-field-name (second (val %)))
     (val gql-ident-map-entry-raw))})

(pco/defresolver ident-map->op-name [{::keys [namespace]} {::keys [gql-field-name]}]
  {::gql-ident-entry-op-name
   (symbol namespace (str gql-field-name "-ident-entry-resolver"))})

(def ident-map->input
  (pull-nested-attribute-resolver
    ::gql-ident-map-params
    ::gql-field-qualified-name
    ::gql-ident-params-op-input))

(pco/defresolver ident-map->output [{::keys [gql-query-type-field]}]
  {::pco/input
   [{::gql-query-type-field
     [::gql-field-type-qualified-name]}]}
  {::gql-ident-params-op-output
   [(get gql-query-type-field ::gql-field-type-qualified-name)]})

(pco/defresolver ident-map->resolve
  [env {::keys [gql-dynamic-op-name
                gql-field-qualified-name]}]
  {::gql-ident-params-op-resolve
   (fn [{::pcp/keys [node] :as env'} input]
     (if-not (next-is-expected-dynamic? env' gql-dynamic-op-name)
       (throw (ex-info "Unexpected node structure. Please report this issue."
                {})))
     (let [{::pcp/keys [node graph] :as env'} (update-in env' [::pcp/graph
                                                               ::pcp/nodes
                                                               (::pcp/run-next node)
                                                               ::pcp/foreign-ast]
                                                (fn [{:keys [children] :as f-ast}]
                                                  (assoc f-ast :children
                                                    [(assoc (pf.eql/prop gql-field-qualified-name)
                                                       :type :join
                                                       :params input
                                                       :children children)])))
           next-node (pcp/get-node graph (::pcp/run-next node))
           response  (process-gql-request env
                       (-> env' (assoc ::pcp/node next-node))
                       input)]
       (get response gql-field-qualified-name)))})

(pco/defresolver ident-map->resolver
  [{::keys [gql-ident-entry-op-name
            gql-ident-params-op-input
            gql-ident-params-op-output
            gql-ident-params-op-resolve]}]
  {::gql-ident-map-entry-resolver
   {::pco/op-name gql-ident-entry-op-name
    ::pco/input   gql-ident-params-op-input
    ::pco/output  gql-ident-params-op-output
    ::pco/resolve gql-ident-params-op-resolve}})

(def schema-ident-map-resolvers
  (pull-nested-attribute-resolver
    ::gql-ident-map-entries
    ::gql-ident-map-entry-resolver
    ::gql-ident-map-resolvers))

; endregion

; region pathom resolvers generation

(pco/defresolver type-resolver-op-name [{::keys [namespace]} {::keys [gql-type-name]}]
  {::gql-type-resolver-op-name
   (symbol namespace (str gql-type-name "-resolver"))})

(pco/defresolver type-resolver-input [{::keys [gql-type-qualified-name]}]
  {::gql-type-resolver-input
   [gql-type-qualified-name]})

(pco/defresolver type-resolver-output
  [{::keys [gql-type-fields gql-type-interfaces]}]
  {::pco/input
   [{::gql-type-fields
     [::gql-field-output-entry]}
    {::gql-type-interfaces
     [::gql-type-qualified-name]}]}
  {::gql-type-resolver-output
   (-> (mapv ::gql-type-qualified-name gql-type-interfaces)
       (into (map ::gql-field-output-entry) gql-type-fields))})

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

(def schema-transient-attrs
  (pull-nested-attribute-resolver
    ::gql-indexable-types
    ::gql-type-qualified-name
    ::gql-pathom-transient-attrs
    #{}))

(pco/defresolver object-type-resolver
  [{::keys [gql-type-resolver-op-name
            gql-type-resolver-input
            gql-type-resolver-output
            gql-dynamic-op-name]}]
  {::pco/input
   [::gql-type-resolver-op-name
    ::gql-type-resolver-input
    ::gql-type-resolver-output
    ::gql-dynamic-op-name]}
  {::gql-indexable-type-resolver
   {::pco/op-name      gql-type-resolver-op-name
    ::pco/dynamic-name gql-dynamic-op-name
    ::pco/input        gql-type-resolver-input
    ::pco/output       gql-type-resolver-output}})

(def schema-object-type-resolvers
  (pull-nested-attribute-resolver
    ::gql-indexable-types
    ::gql-indexable-type-resolver
    ::gql-pathom-indexable-type-resolvers))

(pco/defresolver schema-pathom-main-resolver [env {::keys [gql-dynamic-op-name]}]
  {::gql-pathom-main-resolver
   (pco/resolver gql-dynamic-op-name
     {::pco/dynamic-resolver? true
      ::pco/cache?            false}
     (fn [env' input]
       (process-gql-request env env' input)))})

(pco/defresolver schema-pathom-query-type-entry-resolver
  [{::keys [gql-query-type-qualified-name]}]
  {::gql-pathom-query-type-entry-resolver
   (pbir/constantly-resolver gql-query-type-qualified-name {})})

(pco/defresolver schema->pathom-indexes
  [{::keys [gql-pathom-transient-attrs
            gql-pathom-main-resolver
            gql-pathom-query-type-entry-resolver
            gql-ident-map-resolvers
            gql-pathom-indexable-type-resolvers]}]
  {::gql-pathom-indexes
   (-> {::pci/transient-attrs gql-pathom-transient-attrs}
       (pci/register
         [gql-pathom-main-resolver
          gql-pathom-query-type-entry-resolver
          (mapv pco/resolver gql-ident-map-resolvers)
          (mapv pco/resolver gql-pathom-indexable-type-resolvers)]))})

; endregion

(def env
  (-> {}
      (pci/register
        [dynamic-resolver-name

         graphql-schema
         types-index
         all-types
         indexable-types
         object-types
         interface-types

         query-type
         query-type-qualified-name
         query-type-field-raw

         type-data-raw
         type-name
         type-qualified-name
         type-kind
         type-interfaces
         type-fields
         type-object?
         type-interface?
         type-indexable?

         type-resolver-op-name
         type-resolver-input
         type-resolver-output
         object-type-resolver

         field-name
         field-qualified-name
         field-type
         field-type-qualified-name
         field-output-entry

         ident-map-entries
         ident-map-entry-params
         ident-map->op-name
         ident-map->input
         ident-map->output
         ident-map->resolve
         ident-map->resolver

         schema-ident-map-resolvers
         schema-pathom-main-resolver
         schema-transient-attrs
         schema-object-type-resolvers
         schema-pathom-query-type-entry-resolver
         schema->pathom-indexes])))

(defn load-schema* [config request]
  (clet [gql-schema-raw (request (eql-gql/query->graphql schema-query))]
    (-> env
        (merge config)
        (assoc
          ::pcr/fail-fast? true
          ::request request
          ::gql-schema-raw gql-schema-raw))))

(defn load-schema [config request]
  (clet [env' (load-schema* config request)]
    (psm/smart-map
      (-> env'
          (merge config)
          (assoc
            ::psm/persistent-cache? true)))))

(defn connect-graphql
  "Setup a GraphQL connection on the env.

  The request must be a function that takes a GraphQL string, executes it (or maybe send
  to a server) and return the response as a Clojure map. The Clojure map should be the
  exact same data the GraphQL returns. The keys MUST be encoded as strings, if you convert
  the keys to keywords it wont work.

  Config may include the following keys:

  ::namespace (required) - a namespace (as string) to prefix the entries for this graphql"
  [env config request]
  (clet [env    env
         schema (load-schema config request)]
    (-> env
        (pci/register (::gql-pathom-indexes schema)))))

(comment
  (tap> env)

  (p.eql/process env
    [{::gql-query-type [::gql-type-name]}]))
