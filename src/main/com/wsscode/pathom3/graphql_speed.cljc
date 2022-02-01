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
    [com.wsscode.pathom3.interface.eql :as p.eql]
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

(comment
  (time
    (let [{:strs [queryType mutationType types]} (get-in schema-demo ["data" "__schema"])

          namespace       "demo"

          env             {::namespace              namespace
                           ::gql-mutation-type-name (get mutationType "name")}

          dynamic-op-name (symbol namespace "pathom-entry-dynamic-resolver")

          types-index     (coll/index-by #(get % "name")
                            (into [] (comp (map #(adapt-type env %))
                                           (filter ::gql-type-name)) types))

          env'            (assoc env
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

          ident-map       (inferred-ident-map (assoc env ::gql-types-index types-index)
                            query-type)

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

          env'            (assoc env' ::gql-interface-usages-index (interfaces-usage-index env'))
          env'            (assoc env'
                            :indexable-type-resolvers (pathom-type-resolvers env')
                            :ident-map-resolvers (pathom-ident-map-resolvers env'))]
      (-> (assoc env'
            ::gql-pathom-indexes
            (-> {::pci/transient-attrs transients}
                (pci/register
                  [(pathom-main-resolver env' dynamic-op-name)
                   (pathom-query-entry-resolver (::gql-type-name query-type))
                   (mapv pco/resolver (pathom-ident-map-resolvers env'))
                   (mapv pco/resolver (pathom-type-resolvers env'))])))

        (doto tap>))

      nil)))

; region temp

(def schema-demo
  {"data"
   {"__schema"
    {"mutationType" nil,
     "queryType"    {"name" "Root"},
     "types"
     [{"interfaces"  [],
       "inputFields" nil,
       "name"        "Root",
       "kind"        "OBJECT",
       "fields"
       [{"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "allFilms",
         "type" {"ofType" nil, "name" "FilmsConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "id",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "filmID",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "film",
         "type" {"ofType" nil, "name" "Film", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "allPeople",
         "type"
         {"ofType" nil, "name" "PeopleConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "id",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "personID",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "person",
         "type" {"ofType" nil, "name" "Person", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "allPlanets",
         "type"
         {"ofType" nil, "name" "PlanetsConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "id",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "planetID",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "planet",
         "type" {"ofType" nil, "name" "Planet", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "allSpecies",
         "type"
         {"ofType" nil, "name" "SpeciesConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "id",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "speciesID",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "species",
         "type" {"ofType" nil, "name" "Species", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "allStarships",
         "type"
         {"ofType" nil, "name" "StarshipsConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "id",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "starshipID",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "starship",
         "type" {"ofType" nil, "name" "Starship", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "allVehicles",
         "type"
         {"ofType" nil, "name" "VehiclesConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "id",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "vehicleID",
           "type"         {"ofType" nil, "name" "ID", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "vehicle",
         "type" {"ofType" nil, "name" "Vehicle", "kind" "OBJECT"}}
        {"args"
         [{"name"         "id",
           "type"
           {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
            "name"   nil,
            "kind"   "NON_NULL"},
           "defaultValue" nil}],
         "name" "node",
         "type" {"ofType" nil, "name" "Node", "kind" "INTERFACE"}}]}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "String",
       "kind"        "SCALAR",
       "fields"      nil}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "Int",
       "kind"        "SCALAR",
       "fields"      nil}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType" {"ofType" nil, "name" "FilmsEdge", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "films",
         "type"
         {"ofType" {"ofType" nil, "name" "Film", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PageInfo",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "hasNextPage",
         "type"
         {"ofType" {"ofType" nil, "name" "Boolean", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "hasPreviousPage",
         "type"
         {"ofType" {"ofType" nil, "name" "Boolean", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "startCursor",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "endCursor",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}]}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "Boolean",
       "kind"        "SCALAR",
       "fields"      nil}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Film", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [{"name" "Node", "kind" "INTERFACE"}],
       "inputFields" nil,
       "name"        "Film",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "title",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "episodeID",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "openingCrawl",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "director",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "producers",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "releaseDate",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "speciesConnection",
         "type"
         {"ofType" nil, "name" "FilmSpeciesConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "starshipConnection",
         "type"
         {"ofType" nil,
          "name"   "FilmStarshipsConnection",
          "kind"   "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "vehicleConnection",
         "type"
         {"ofType" nil, "name" "FilmVehiclesConnection", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "characterConnection",
         "type"
         {"ofType" nil,
          "name"   "FilmCharactersConnection",
          "kind"   "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "planetConnection",
         "type"
         {"ofType" nil, "name" "FilmPlanetsConnection", "kind" "OBJECT"}}
        {"args" [],
         "name" "created",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "edited",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "id",
         "type"
         {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "Node",
       "kind"        "INTERFACE",
       "fields"
       [{"args" [],
         "name" "id",
         "type"
         {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "ID",
       "kind"        "SCALAR",
       "fields"      nil}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmSpeciesConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "FilmSpeciesEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "species",
         "type"
         {"ofType" {"ofType" nil, "name" "Species", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmSpeciesEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Species", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [{"name" "Node", "kind" "INTERFACE"}],
       "inputFields" nil,
       "name"        "Species",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "classification",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "designation",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "averageHeight",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "averageLifespan",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "eyeColors",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "hairColors",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "skinColors",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "language",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "homeworld",
         "type" {"ofType" nil, "name" "Planet", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "personConnection",
         "type"
         {"ofType" nil,
          "name"   "SpeciesPeopleConnection",
          "kind"   "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "filmConnection",
         "type"
         {"ofType" nil, "name" "SpeciesFilmsConnection", "kind" "OBJECT"}}
        {"args" [],
         "name" "created",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "edited",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "id",
         "type"
         {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "Float",
       "kind"        "SCALAR",
       "fields"      nil}
      {"interfaces"  [{"name" "Node", "kind" "INTERFACE"}],
       "inputFields" nil,
       "name"        "Planet",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "diameter",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "rotationPeriod",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "orbitalPeriod",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "gravity",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "population",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "climates",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "terrains",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "surfaceWater",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "residentConnection",
         "type"
         {"ofType" nil,
          "name"   "PlanetResidentsConnection",
          "kind"   "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "filmConnection",
         "type"
         {"ofType" nil, "name" "PlanetFilmsConnection", "kind" "OBJECT"}}
        {"args" [],
         "name" "created",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "edited",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "id",
         "type"
         {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PlanetResidentsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "PlanetResidentsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "residents",
         "type"
         {"ofType" {"ofType" nil, "name" "Person", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PlanetResidentsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Person", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [{"name" "Node", "kind" "INTERFACE"}],
       "inputFields" nil,
       "name"        "Person",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "birthYear",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "eyeColor",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "gender",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "hairColor",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "height",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "mass",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "skinColor",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "homeworld",
         "type" {"ofType" nil, "name" "Planet", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "filmConnection",
         "type"
         {"ofType" nil, "name" "PersonFilmsConnection", "kind" "OBJECT"}}
        {"args" [],
         "name" "species",
         "type" {"ofType" nil, "name" "Species", "kind" "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "starshipConnection",
         "type"
         {"ofType" nil,
          "name"   "PersonStarshipsConnection",
          "kind"   "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "vehicleConnection",
         "type"
         {"ofType" nil,
          "name"   "PersonVehiclesConnection",
          "kind"   "OBJECT"}}
        {"args" [],
         "name" "created",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "edited",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "id",
         "type"
         {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PersonFilmsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "PersonFilmsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "films",
         "type"
         {"ofType" {"ofType" nil, "name" "Film", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PersonFilmsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Film", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PersonStarshipsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "PersonStarshipsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "starships",
         "type"
         {"ofType" {"ofType" nil, "name" "Starship", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PersonStarshipsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Starship", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [{"name" "Node", "kind" "INTERFACE"}],
       "inputFields" nil,
       "name"        "Starship",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "model",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "starshipClass",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "manufacturers",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "costInCredits",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "length",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "crew",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "passengers",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "maxAtmospheringSpeed",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "hyperdriveRating",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "MGLT",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "cargoCapacity",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "consumables",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "pilotConnection",
         "type"
         {"ofType" nil,
          "name"   "StarshipPilotsConnection",
          "kind"   "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "filmConnection",
         "type"
         {"ofType" nil,
          "name"   "StarshipFilmsConnection",
          "kind"   "OBJECT"}}
        {"args" [],
         "name" "created",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "edited",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "id",
         "type"
         {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "StarshipPilotsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "StarshipPilotsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "pilots",
         "type"
         {"ofType" {"ofType" nil, "name" "Person", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "StarshipPilotsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Person", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "StarshipFilmsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "StarshipFilmsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "films",
         "type"
         {"ofType" {"ofType" nil, "name" "Film", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "StarshipFilmsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Film", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PersonVehiclesConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "PersonVehiclesEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "vehicles",
         "type"
         {"ofType" {"ofType" nil, "name" "Vehicle", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PersonVehiclesEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Vehicle", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [{"name" "Node", "kind" "INTERFACE"}],
       "inputFields" nil,
       "name"        "Vehicle",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "model",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "vehicleClass",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "manufacturers",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "costInCredits",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "length",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "crew",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "passengers",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "maxAtmospheringSpeed",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "cargoCapacity",
         "type" {"ofType" nil, "name" "Float", "kind" "SCALAR"}}
        {"args" [],
         "name" "consumables",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "pilotConnection",
         "type"
         {"ofType" nil,
          "name"   "VehiclePilotsConnection",
          "kind"   "OBJECT"}}
        {"args"
         [{"name"         "after",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "first",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "before",
           "type"         {"ofType" nil, "name" "String", "kind" "SCALAR"},
           "defaultValue" nil}
          {"name"         "last",
           "type"         {"ofType" nil, "name" "Int", "kind" "SCALAR"},
           "defaultValue" nil}],
         "name" "filmConnection",
         "type"
         {"ofType" nil, "name" "VehicleFilmsConnection", "kind" "OBJECT"}}
        {"args" [],
         "name" "created",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "edited",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "id",
         "type"
         {"ofType" {"ofType" nil, "name" "ID", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "VehiclePilotsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "VehiclePilotsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "pilots",
         "type"
         {"ofType" {"ofType" nil, "name" "Person", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "VehiclePilotsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Person", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "VehicleFilmsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "VehicleFilmsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "films",
         "type"
         {"ofType" {"ofType" nil, "name" "Film", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "VehicleFilmsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Film", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PlanetFilmsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "PlanetFilmsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "films",
         "type"
         {"ofType" {"ofType" nil, "name" "Film", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PlanetFilmsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Film", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "SpeciesPeopleConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "SpeciesPeopleEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "people",
         "type"
         {"ofType" {"ofType" nil, "name" "Person", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "SpeciesPeopleEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Person", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "SpeciesFilmsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "SpeciesFilmsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "films",
         "type"
         {"ofType" {"ofType" nil, "name" "Film", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "SpeciesFilmsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Film", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmStarshipsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "FilmStarshipsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "starships",
         "type"
         {"ofType" {"ofType" nil, "name" "Starship", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmStarshipsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Starship", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmVehiclesConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "FilmVehiclesEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "vehicles",
         "type"
         {"ofType" {"ofType" nil, "name" "Vehicle", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmVehiclesEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Vehicle", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmCharactersConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "FilmCharactersEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "characters",
         "type"
         {"ofType" {"ofType" nil, "name" "Person", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmCharactersEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Person", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmPlanetsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "FilmPlanetsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "planets",
         "type"
         {"ofType" {"ofType" nil, "name" "Planet", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "FilmPlanetsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Planet", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PeopleConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType" {"ofType" nil, "name" "PeopleEdge", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "people",
         "type"
         {"ofType" {"ofType" nil, "name" "Person", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PeopleEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Person", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PlanetsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType" {"ofType" nil, "name" "PlanetsEdge", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "planets",
         "type"
         {"ofType" {"ofType" nil, "name" "Planet", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "PlanetsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Planet", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "SpeciesConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType" {"ofType" nil, "name" "SpeciesEdge", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "species",
         "type"
         {"ofType" {"ofType" nil, "name" "Species", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "SpeciesEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Species", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "StarshipsConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType"
          {"ofType" nil, "name" "StarshipsEdge", "kind" "OBJECT"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "starships",
         "type"
         {"ofType" {"ofType" nil, "name" "Starship", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "StarshipsEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Starship", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "VehiclesConnection",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "pageInfo",
         "type"
         {"ofType" {"ofType" nil, "name" "PageInfo", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "edges",
         "type"
         {"ofType" {"ofType" nil, "name" "VehiclesEdge", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}
        {"args" [],
         "name" "totalCount",
         "type" {"ofType" nil, "name" "Int", "kind" "SCALAR"}}
        {"args" [],
         "name" "vehicles",
         "type"
         {"ofType" {"ofType" nil, "name" "Vehicle", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "LIST"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "VehiclesEdge",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "node",
         "type" {"ofType" nil, "name" "Vehicle", "kind" "OBJECT"}}
        {"args" [],
         "name" "cursor",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "__Schema",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "types",
         "type"
         {"ofType"
          {"ofType"
           {"ofType" {"name" "__Type", "kind" "OBJECT"},
            "name"   nil,
            "kind"   "NON_NULL"},
           "name" nil,
           "kind" "LIST"},
          "name" nil,
          "kind" "NON_NULL"}}
        {"args" [],
         "name" "queryType",
         "type"
         {"ofType" {"ofType" nil, "name" "__Type", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "mutationType",
         "type" {"ofType" nil, "name" "__Type", "kind" "OBJECT"}}
        {"args" [],
         "name" "subscriptionType",
         "type" {"ofType" nil, "name" "__Type", "kind" "OBJECT"}}
        {"args" [],
         "name" "directives",
         "type"
         {"ofType"
          {"ofType"
           {"ofType" {"name" "__Directive", "kind" "OBJECT"},
            "name"   nil,
            "kind"   "NON_NULL"},
           "name" nil,
           "kind" "LIST"},
          "name" nil,
          "kind" "NON_NULL"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "__Type",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "kind",
         "type"
         {"ofType" {"ofType" nil, "name" "__TypeKind", "kind" "ENUM"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "name",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "description",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args"
         [{"name"         "includeDeprecated",
           "type"         {"ofType" nil, "name" "Boolean", "kind" "SCALAR"},
           "defaultValue" "false"}],
         "name" "fields",
         "type"
         {"ofType"
          {"ofType" {"ofType" nil, "name" "__Field", "kind" "OBJECT"},
           "name"   nil,
           "kind"   "NON_NULL"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "interfaces",
         "type"
         {"ofType"
          {"ofType" {"ofType" nil, "name" "__Type", "kind" "OBJECT"},
           "name"   nil,
           "kind"   "NON_NULL"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "possibleTypes",
         "type"
         {"ofType"
          {"ofType" {"ofType" nil, "name" "__Type", "kind" "OBJECT"},
           "name"   nil,
           "kind"   "NON_NULL"},
          "name" nil,
          "kind" "LIST"}}
        {"args"
         [{"name"         "includeDeprecated",
           "type"         {"ofType" nil, "name" "Boolean", "kind" "SCALAR"},
           "defaultValue" "false"}],
         "name" "enumValues",
         "type"
         {"ofType"
          {"ofType" {"ofType" nil, "name" "__EnumValue", "kind" "OBJECT"},
           "name"   nil,
           "kind"   "NON_NULL"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "inputFields",
         "type"
         {"ofType"
          {"ofType"
           {"ofType" nil, "name" "__InputValue", "kind" "OBJECT"},
           "name" nil,
           "kind" "NON_NULL"},
          "name" nil,
          "kind" "LIST"}}
        {"args" [],
         "name" "ofType",
         "type" {"ofType" nil, "name" "__Type", "kind" "OBJECT"}}]}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "__TypeKind",
       "kind"        "ENUM",
       "fields"      nil}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "__Field",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "description",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "args",
         "type"
         {"ofType"
          {"ofType"
           {"ofType" {"name" "__InputValue", "kind" "OBJECT"},
            "name"   nil,
            "kind"   "NON_NULL"},
           "name" nil,
           "kind" "LIST"},
          "name" nil,
          "kind" "NON_NULL"}}
        {"args" [],
         "name" "type",
         "type"
         {"ofType" {"ofType" nil, "name" "__Type", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "isDeprecated",
         "type"
         {"ofType" {"ofType" nil, "name" "Boolean", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "deprecationReason",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "__InputValue",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "description",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "type",
         "type"
         {"ofType" {"ofType" nil, "name" "__Type", "kind" "OBJECT"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "defaultValue",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "__EnumValue",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "description",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "isDeprecated",
         "type"
         {"ofType" {"ofType" nil, "name" "Boolean", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "deprecationReason",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}]}
      {"interfaces"  [],
       "inputFields" nil,
       "name"        "__Directive",
       "kind"        "OBJECT",
       "fields"
       [{"args" [],
         "name" "name",
         "type"
         {"ofType" {"ofType" nil, "name" "String", "kind" "SCALAR"},
          "name"   nil,
          "kind"   "NON_NULL"}}
        {"args" [],
         "name" "description",
         "type" {"ofType" nil, "name" "String", "kind" "SCALAR"}}
        {"args" [],
         "name" "locations",
         "type"
         {"ofType"
          {"ofType"
           {"ofType" {"name" "__DirectiveLocation", "kind" "ENUM"},
            "name"   nil,
            "kind"   "NON_NULL"},
           "name" nil,
           "kind" "LIST"},
          "name" nil,
          "kind" "NON_NULL"}}
        {"args" [],
         "name" "args",
         "type"
         {"ofType"
          {"ofType"
           {"ofType" {"name" "__InputValue", "kind" "OBJECT"},
            "name"   nil,
            "kind"   "NON_NULL"},
           "name" nil,
           "kind" "LIST"},
          "name" nil,
          "kind" "NON_NULL"}}]}
      {"interfaces"  nil,
       "inputFields" nil,
       "name"        "__DirectiveLocation",
       "kind"        "ENUM",
       "fields"      nil}]}}})

; endregion
