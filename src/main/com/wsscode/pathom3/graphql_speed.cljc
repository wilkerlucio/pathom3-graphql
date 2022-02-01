(ns com.wsscode.pathom3.graphql-speed
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
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

(defn entity-field-key [{::keys [namespace]} entity field]
  (keyword (str namespace "." entity) field))

(defn type-leaf-name [{:strs [kind name ofType]}]
  (case kind
    "NON_NULL" (recur ofType)
    "LIST" (recur ofType)
    "OBJECT" name
    "INTERFACE" name
    "SCALAR" name
    nil))

(defn type->field-name [env {:strs [kind name ofType]}]
  (case kind
    "NON_NULL" (recur env ofType)
    "LIST" (recur env ofType)
    "OBJECT" (type-key env name)
    "INTERFACE" (interface-key env name)
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

(defn inject-gql-on [{:keys [dispatch-key] :as node}]
  (if (keyword? dispatch-key)
    (let [type-name (-> (namespace dispatch-key)
                        (str/split #"\.")
                        last)]
      (assoc-in node [:params ::eql-gql/on] type-name))
    node))

(defn prepare-gql-ast [ast]
  (map-children
    (fn [{:keys [type] :as node}]
      (if (= type :union-entry)
        node
        (coll/update-if node :children
                        (fn [children]
                          (-> (mapv inject-gql-on children)
                              (conj (pf.eql/prop :__typename)))))))
    ast))

(defn format-error [{:strs [message path]}]
  (str message " at path " (pr-str path)))

(defn process-gql-request [{::keys [request] :as schema-env} env _input]
  (let [node     (::pcp/node env)
        ast      (-> node
                     ::pcp/foreign-ast
                     prepare-gql-ast)
        gql      (-> ast eql-gql/ast->graphql)
        response (->> (request gql)
                      (walk/postwalk #(set-union-path schema-env %)))]
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
  (and (contains? #{"OBJECT" "INTERFACE"} kind)
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

(defn inferred-ident-map [{::keys [gql-types-index]} {:strs [fields]}]
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

(defn pathom-main-resolver [env gql-dynamic-op-name]
  (pco/resolver gql-dynamic-op-name
    {::pco/dynamic-resolver? true
     ::pco/cache?            false}
    (fn [env' input]
      (process-gql-request env env' input))))

(defn pathom-query-entry-resolver [type-name]
  (pbir/constantly-resolver type-name {}))

(defn pathom-ident-map-resolvers
  [{::keys [gql-dynamic-op-name
            gql-query-type
            gql-types-index
            ident-map
            namespace]
    :as    env}]
  (mapv
    (fn [[field-name params]]
      (let [op-name                  (symbol namespace (str field-name "-ident-entry-resolver"))
            input                    (mapv #(entity-field-key env (first %) (second %)) (vals params))
            output-type              (as-> gql-query-type <>
                                       (get <> "fields")
                                       (coll/find-first #(-> % (get "name") (= field-name)) <>)
                                       (get <> ::gql-field-leaf-type)
                                       (get-in gql-types-index [<> ::gql-type-name]))

            gql-field-qualified-name (first input)

            resolve                  (fn ident-map-resolve [{::pcp/keys [node] :as env'} input]
                                       (if-not (next-is-expected-dynamic? env' gql-dynamic-op-name)
                                         (throw (ex-info "Unexpected node structure. Please report this issue."
                                                         {})))
                                       (let [{::pcp/keys [node graph] :as env'}
                                             (update-in env' [::pcp/graph
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
                                         (get response gql-field-qualified-name)))]
        {::pco/op-name op-name
         ::pco/input   input
         ::pco/output  [output-type]
         ::pco/resolve resolve}))
    ident-map))

(defn pathom-type-resolvers
  [{::keys [namespace
            gql-indexable-types
            gql-interface-usages-index
            gql-dynamic-op-name]}]
  (mapv
    (fn [{:strs [name fields] ::keys [gql-type-name]}]
      (let [gql-type-resolver-op-name (symbol namespace (str name "-resolver"))
            gql-type-resolver-input   [gql-type-name]
            gql-interface-usages      (get gql-interface-usages-index gql-type-name)
            gql-type-resolver-output  (-> (or (some-> gql-interface-usages vec) [])
                                          (into (map ::gql-field-output-entry) fields))]
        {::pco/op-name      gql-type-resolver-op-name
         ::pco/dynamic-name gql-dynamic-op-name
         ::pco/input        gql-type-resolver-input
         ::pco/output       gql-type-resolver-output}))
    gql-indexable-types))

(defn build-pathom-indexes [{::keys [namespace ident-map] :as env} schema]
  (let [{:strs [queryType mutationType types]} (get-in schema ["data" "__schema"])

        env'            (assoc env ::gql-mutation-type-name (get mutationType "name"))

        dynamic-op-name (symbol namespace "pathom-entry-dynamic-resolver")

        types-index     (coll/index-by #(get % "name")
                                       (into [] (comp (map #(adapt-type env' %))
                                                      (filter ::gql-type-name)) types))

        env'            (assoc env'
                          ::gql-dynamic-op-name dynamic-op-name
                          ::gql-types-index types-index)

        types-index     (coll/map-vals
                          (fn [type]
                            (let [field-adapter (partial adapt-field env' type)]
                              (update type "fields" #(mapv field-adapter %)))) types-index)

        env'            (assoc env' ::gql-types-index types-index)

        indexable-types (into [] (filter ::gql-type-indexable?) (vals types-index))
        object-types    (into [] (filter #(-> % (get "kind") (= "OBJECT"))) (vals types-index))
        interface-types (into [] (filter #(-> % (get "kind") (= "INTERFACE"))) (vals types-index))

        query-type      (get types-index (get queryType "name"))
        mutation-type   (get types-index (get mutationType "name"))

        ident-map       (merge
                          (inferred-ident-map (assoc env' ::gql-types-index types-index)
                                              query-type)
                          ident-map)

        transients      (cond-> (into #{} (map ::gql-type-name) indexable-types)
                          mutation-type
                          (conj (get mutation-type ::gql-type-name)))

        env'            (assoc env'
                          ::ident-map ident-map
                          ::gql-indexable-types indexable-types
                          ::gql-object-types object-types
                          ::gql-interface-types interface-types
                          ::gql-query-type query-type
                          ::gql-mutation-type mutation-type)

        env'            (assoc env' ::gql-interface-usages-index (interfaces-usage-index env'))]
    (assoc env'
      ::gql-pathom-indexes
      (-> {::pci/transient-attrs transients}
          (pci/register
            [(pathom-main-resolver env' dynamic-op-name)
             (pathom-query-entry-resolver (::gql-type-name query-type))
             (mapv pco/resolver (pathom-ident-map-resolvers env'))
             (mapv pco/resolver (pathom-type-resolvers env'))])))))

(defn connect-graphql
  "Setup a GraphQL connection on the env.

  The request must be a function that takes a GraphQL string, executes it (or maybe send
  to a server) and return the response as a Clojure map. The Clojure map should be the
  exact same data the GraphQL returns. The keys MUST be encoded as strings, if you convert
  the keys to keywords it wont work.

  Config may include the following keys:

  ::namespace (required) - a namespace (as string) to prefix the entries for this graphql"
  [env {::keys [namespace] :as config} request]
  (if-not (seq namespace)
    (throw (ex-info "Namespace is required to pull a GraphQL API." {})))
  (clet [env            env
         gql-schema-raw (request (eql-gql/query->graphql schema-query))

         {::keys [gql-pathom-indexes]}
         (build-pathom-indexes
           (assoc config ::request request)
           gql-schema-raw)]
    (-> env
        (pci/register gql-pathom-indexes))))
