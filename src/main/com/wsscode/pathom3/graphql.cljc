(ns com.wsscode.pathom3.graphql
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [com.fulcrologic.guardrails.core :refer [>def]]
    [com.wsscode.misc.coll :as coll]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.connect.planner :as pcp]
    [com.wsscode.pathom3.connect.runner :as pcr]
    [com.wsscode.pathom3.format.eql :as pf.eql]
    [com.wsscode.pathom3.plugin :as p.plugin]
    [com.wsscode.promesa.macros :refer [clet]]
    [edn-query-language.core :as eql]
    [edn-query-language.eql-graphql :as eql-gql]))

(>def ::root-entries-map
  (s/map-of string?
            (s/map-of string?
                      (s/tuple string? string?))))

(>def ::namespace string?)
(>def ::root-entries-map (s/map-of string? (s/map-of string? (s/tuple string? string?))))

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
         {:type [:kind :name {:ofType 3}]}]}
       {:possibleTypes
        [:name :kind]}]}]}])

(defn prefixed-key [{::keys [namespace]} p s] (keyword (str namespace "." p) s))
(defn type-key [env s] (prefixed-key env "types" s))
(defn interface-key [env s] (prefixed-key env "interfaces" s))
(defn union-key [env s] (prefixed-key env "unions" s))

(defn entity-field-key [{::keys [namespace]} entity field]
  (keyword (str namespace "." entity) field))

(defn type-leaf-name [{:strs [kind name ofType]}]
  (case kind
    "NON_NULL" (recur ofType)
    "LIST" (recur ofType)
    "OBJECT" name
    "INTERFACE" name
    "UNION" name
    "SCALAR" name
    "ENUM" name
    nil))

(defn type->field-name [env {:strs [kind name ofType]}]
  (case kind
    "NON_NULL" (recur env ofType)
    "LIST" (recur env ofType)
    "OBJECT" (type-key env name)
    "INTERFACE" (interface-key env name)
    "UNION" (union-key env name)
    nil))

(defn map-children
  [f node]
  (let [union? (eql/union-children? node)
        node'  (cond-> node (not union?) f)]
    (cond-> node'
      (seq (:children node'))
      (update :children
        (fn [children]
          (if union?
            (update-in children [0 :children]
              (fn [children]
                (mapv #(map-children f %) children)))
            (mapv #(map-children f %) children)))))))

(defn set-union-path
  [schema-env entity]
  (let [type (and entity (map? entity) (get entity "__typename"))]
    (cond-> entity
      type
      (vary-meta assoc ::pf.eql/union-entry-key (type-key schema-env type)))))

(defn convert-back [env response]
  (let [ast (-> env
                ::pcp/node
                ::pcp/foreign-ast)]
    (pf.eql/map-select-ast
      (p.plugin/register
        {::p.plugin/id
         `string-select

         ::pf.eql/wrap-map-select-entry
         (fn [select-entry]
           (fn [env source {:keys [key] :as ast}]
             (when-let [entry (select-entry env source
                                            (-> ast (update :key name) (update :dispatch-key name)))]
               (coll/make-map-entry key (val entry)))))})
      (get response "data")
      ast)))

(defn inject-gql-on [parent-type {:keys [dispatch-key] :as node}]
  (if (keyword? dispatch-key)
    (if-let [type-name (some-> (namespace dispatch-key)
                               (str/split #"\.")
                               last)]
      (cond-> node
        (not= type-name parent-type)
        (assoc-in [:params ::eql-gql/on] type-name))
      node)
    node))

(defn prepare-gql-ast [env ast]
  (map-children
    (fn [{:keys [type dispatch-key] :as node}]
      (if (= type :union-entry)
        node
        (let [value-type (if (= type :root)
                           (get-in env [::gql-query-type "name"])
                           (get-in env [::gql-fields-index dispatch-key]))]
          (coll/update-if node :children
                          (fn [children]
                            (mapv #(inject-gql-on value-type %) children))))))
    ast))

(defn format-error [{:strs [message path]}]
  (str message " at path " (pr-str path)))

(defn process-gql-request [{::keys [request] :as schema-env} env input]
  (clet [node (::pcp/node env)
         ast (->> (or (::pcp/foreign-ast node)
                      (::pcp/foreign-ast input))
                  (prepare-gql-ast schema-env))
         gql (-> ast eql-gql/ast->graphql)
         response (request env gql)
         response (walk/postwalk #(set-union-path schema-env %) response)]
    (pcr/merge-node-stats! env node
      {::request gql})
    (if-let [errors (get response "errors")]
      (throw (ex-info (str "GraphQL Error: " (format-error (first errors))) {:errors errors}))
      (convert-back
        (assoc-in env [::pcp/node ::pcp/foreign-ast] ast)
        response))))

(defn next-is-expected-dynamic?
  [{::pcp/keys [node graph]} gql-dynamic-op-name]
  (= gql-dynamic-op-name (as-> node <>
                           (::pcp/run-next <>)
                           (pcp/get-node graph <> ::pco/op-name))))

(defn type-indexable? [{::keys [gql-mutation-type-name]} {:strs [name kind]}]
  (and (contains? #{"OBJECT" "INTERFACE" "UNION"} kind)
       (not= gql-mutation-type-name name)))

(defn type-chain [type]
  (->> (coll/iterate-while
         #(get % "ofType")
         type)))

(defn adapt-field [{::keys [gql-types-index] :as env} {type-name "name"} {:strs [type name args] :as field}]
  (let [field-type-name    (type-leaf-name type)
        field-type-fq-name (get-in gql-types-index [field-type-name ::gql-type-name])
        field-name         (entity-field-key env type-name name)]
    (-> field
        (assoc
          ::gql-field-name field-name
          ::gql-field-leaf-type field-type-name
          ::gql-field-leaf-fq-type field-type-fq-name
          ::gql-list-type? (->> (type-chain type)
                                (some #(= "LIST" (get % "kind")))
                                boolean)
          ::gql-id-arg (coll/find-first #(-> % (get "name") (= "id")) args)
          ::gql-field-output-entry (if field-type-fq-name
                                     {field-name [field-type-fq-name]}
                                     field-name)))))

(defn adapt-type [env type]
  (-> type
      (assoc
        ::gql-type-name (type->field-name env type)
        ::gql-type-indexable? (type-indexable? env type))
      (as-> <>
        (assoc <>
          ::gql-type-id-field (coll/find-first #(-> % (get "name") (= "id")) (get <> "fields"))))))

(defn inferred-root-entries-map [{::keys [gql-types-index]} {:strs [fields]}]
  (into {}
        (keep
          (fn [{::keys     [gql-field-leaf-type gql-id-arg gql-list-type?]
                field-name "name"}]
            (let [{::keys [gql-type-id-field gql-type-indexable?]
                   :strs  [name]}
                  (get gql-types-index gql-field-leaf-type)]
              (if (and (not gql-list-type?)
                       gql-type-indexable?
                       gql-type-id-field
                       gql-id-arg)
                [field-name {(get gql-id-arg "name") [name (get gql-type-id-field "name")]}]))))
        fields))

(defn interfaces-usage-index [{::keys [gql-object-types gql-types-index]}]
  (reduce
    (fn [idx
         {:strs     [interfaces]
          type-name ::gql-type-name}]
      (reduce
        (fn [idx {:strs [name]}]
          (let [interface-name (get-in gql-types-index [name ::gql-type-name])]
            (update idx interface-name coll/sconj type-name)))
        idx
        interfaces))
    {}
    gql-object-types))

(defn fields-index [{::keys [gql-types-index]}]
  (into {}
        (comp (mapcat #(get % "fields"))
              (map (juxt ::gql-field-name ::gql-field-leaf-type)))
        (vals gql-types-index)))

(defn index-root-entries-fields [env]
  (update env
    ::root-entries-map
    (fn [root-entries]
      (coll/map-vals
        (fn [args]
          (into args
                (map (fn [[arg [type field]]]
                       (coll/make-map-entry (entity-field-key env type field) arg)))
                args))
        root-entries))))

(defn pathom-main-resolver [env gql-dynamic-op-name]
  (pco/resolver gql-dynamic-op-name
    {::pco/dynamic-resolver? true
     ::pco/cache?            false}
    (fn [env' input]
      (process-gql-request env env' input))))

(defn pathom-query-entry-resolver [type-name]
  (pbir/constantly-resolver type-name {}))

(defn pathom-root-entries-map-resolve
  [{::keys [gql-dynamic-op-name root-entries-map] :as env}
   {::keys [gql-field-name] :strs [name]}]
  (fn root-entries-resolve [{::pcp/keys [node] :as env'} input]
    (if-not (next-is-expected-dynamic? env' gql-dynamic-op-name)
      (throw (ex-info "Unexpected node structure. Please report this issue."
                      {})))
    (clet [input (set/rename-keys input (get root-entries-map name))

           {::pcp/keys [node graph] :as env'}
           (update-in env' [::pcp/graph
                            ::pcp/nodes
                            (::pcp/run-next node)
                            ::pcp/foreign-ast]
             (fn [{:keys [children] :as f-ast}]
               (assoc f-ast :children
                 [(assoc (pf.eql/prop gql-field-name)
                    :type :join
                    :params input
                    :children children)])))
           next-node (pcp/get-node graph (::pcp/run-next node))
           response (process-gql-request env
                                         (-> env' (assoc ::pcp/node next-node))
                                         input)]
      (get response gql-field-name))))

(defn pathom-root-entries-resolvers
  [{::keys [gql-query-type
            gql-types-index
            root-entries-map
            namespace]
    :as    env}]
  (let [query-fields (get gql-query-type "fields")
        find-field   (fn [field]
                       (coll/find-first #(-> % (get "name") (= field))
                                        query-fields))]
    (mapv
      (fn [[field-name params]]
        (let [op-name     (symbol namespace (str field-name "-ident-entry-resolver"))

              {::keys [gql-field-leaf-type]
               :as    field}
              (find-field field-name)

              input       (->> params
                               (coll/filter-keys string?)
                               (vals)
                               (mapv #(entity-field-key env (first %) (second %))))
              output-type (get-in gql-types-index [gql-field-leaf-type ::gql-type-name])
              resolve     (pathom-root-entries-map-resolve env field)]
          {::pco/op-name op-name
           ::pco/input   input
           ::pco/output  [output-type]
           ::pco/cache?  false
           ::pco/resolve resolve}))
      root-entries-map)))

(defn pathom-type-resolvers
  [{::keys [namespace
            gql-types-index
            gql-indexable-types
            gql-interface-usages-index
            gql-dynamic-op-name]}]
  (mapv
    (fn [{:strs [name fields possibleTypes] ::keys [gql-type-name]}]
      (let [gql-type-resolver-op-name (symbol namespace (str name "-resolver"))
            gql-type-resolver-input   [gql-type-name]
            gql-interface-usages      (get gql-interface-usages-index gql-type-name)
            gql-type-resolver-output  (-> (or (some-> gql-interface-usages vec) [])
                                          (into (map #(-> (get-in gql-types-index [(get % "name") ::gql-type-name]))) possibleTypes)
                                          (into (map ::gql-field-output-entry) fields))]
        {::pco/op-name      gql-type-resolver-op-name
         ::pco/dynamic-name gql-dynamic-op-name
         ::pco/input        gql-type-resolver-input
         ::pco/output       gql-type-resolver-output}))
    gql-indexable-types))

(defn pathom-mutations
  [{::keys [gql-dynamic-op-name
            gql-mutation-type]}]
  (let [{:strs [fields]} gql-mutation-type]
    (mapv
      (fn [{::keys [gql-field-name
                    gql-field-leaf-fq-type]}]
        {::pco/op-name      (symbol gql-field-name)
         ::pco/dynamic-name gql-dynamic-op-name
         ::pco/output       [gql-field-leaf-fq-type]})
      fields)))

(defn build-indexes [{::keys [namespace root-entries-map] :as env} schema]
  (let [{:strs [queryType mutationType types]} (get-in schema ["data" "__schema"])

        env'             (assoc env ::gql-mutation-type-name (get mutationType "name"))

        dynamic-op-name  (symbol namespace "pathom-entry-dynamic-resolver")

        types-index      (coll/index-by #(get % "name")
                                        (into [] (comp (map #(adapt-type env' %))
                                                       (filter ::gql-type-name)) types))

        env'             (assoc env'
                           ::gql-dynamic-op-name dynamic-op-name
                           ::gql-types-index types-index)

        types-index      (coll/map-vals
                           (fn [type]
                             (let [field-adapter (partial adapt-field env' type)]
                               (update type "fields" #(mapv field-adapter %)))) types-index)

        env'             (assoc env' ::gql-types-index types-index)

        indexable-types  (into [] (filter ::gql-type-indexable?) (vals types-index))
        object-types     (into [] (filter #(-> % (get "kind") (= "OBJECT"))) (vals types-index))
        interface-types  (into [] (filter #(-> % (get "kind") (= "INTERFACE"))) (vals types-index))

        query-type       (get types-index (get queryType "name"))
        mutation-type    (get types-index (get mutationType "name"))

        root-entries-map (merge
                           (inferred-root-entries-map
                             (assoc env' ::gql-types-index types-index)
                             query-type)
                           root-entries-map)

        transients       (cond-> (into #{} (map ::gql-type-name) indexable-types)
                           mutation-type
                           (conj (get mutation-type ::gql-type-name)))

        env'             (assoc env'
                           ::root-entries-map root-entries-map
                           ::gql-indexable-types indexable-types
                           ::gql-object-types object-types
                           ::gql-interface-types interface-types
                           ::gql-query-type query-type
                           ::gql-mutation-type mutation-type
                           ::gql-fields-index (fields-index env'))

        env'             (assoc env' ::gql-interface-usages-index (interfaces-usage-index env'))
        env'             (index-root-entries-fields env')]
    (assoc env'
      ::gql-pathom-indexes
      (-> {::pci/transient-attrs transients}
          (pci/register
            [(pathom-main-resolver env' dynamic-op-name)
             (pathom-query-entry-resolver (::gql-type-name query-type))
             (mapv pco/resolver (pathom-root-entries-resolvers env'))
             (mapv pco/resolver (pathom-type-resolvers env'))
             (mapv pco/mutation (pathom-mutations env'))])))))

(defn load-schema [env config request]
  (clet [gql-schema-raw (request env (eql-gql/query->graphql schema-query))]
    (build-indexes
      (assoc config ::request request)
      gql-schema-raw)))

(defn connect-graphql
  "Setup a GraphQL connection on the env.

  The request must be a function that takes a pathom env and a GraphQL string, executes it (or maybe send
  to a server) and return the response as a Clojure map. The Clojure map should be the
  exact same data the GraphQL returns. The keys MUST be encoded as strings, if you convert
  the keys to keywords it wont work.

  Config may include the following keys:

  ::namespace (required) - a namespace (as string) to prefix the entries for this graphql"
  [env {::keys [namespace] :as config} request]
  (if-not (seq namespace)
    (throw (ex-info "Namespace is required to pull a GraphQL API." {})))
  (clet [{::keys [gql-pathom-indexes]} (load-schema env config request)
         env env]
    (-> env
        (pci/register gql-pathom-indexes))))
