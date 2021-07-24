(ns com.wsscode.pathom3.graphql
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

(defn entity-field-key [{::keys [namespace]} entity field]
  (keyword (str namespace "." entity) field))

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

(defn <<
  ([prop] (<< prop prop))
  ([prop name]
   (eql/update-property-param prop assoc ::root-alias name)))

(defn extract-marked-paths
  ([input-eql]
   (extract-marked-paths (eql/query->ast input-eql) [] {}))
  ([{:keys [children]} path out]
   (reduce
     (fn [out {:keys [params key] :as ast'}]
       (let [path' (conj path key)]
         (->> (if-let [alias (::root-alias params)]
                (if (contains? out alias)
                  (throw (ex-info (str "Duplicated alias " alias) {:alias alias}))
                  (assoc out alias path'))
                out)
              (extract-marked-paths ast' path'))))
     out
     children)))

(defn pull-nested-path [x [p & ps]]
  (let [val (get x p)]
    (if (seq ps)
      (cond
        (coll/collection? val)
        (map #(pull-nested-path % ps) val)

        (nil? val)
        nil

        :else
        (pull-nested-path val ps))
      val)))

(defn pull-nested-data [input paths]
  (reduce
    (fn [i [out path]]
      (assoc i out (pull-nested-path i path)))
    input
    paths))

(defn pull-nested [resolver]
  (let [paths (extract-marked-paths (::pco/input resolver))]
    (-> resolver
        (assoc
          ::paths paths
          ::pco/disable-validate-input-destructuring? true)
        (update ::pco/resolve
          (fn [resolve]
            (fn [env input]
              (let [input' (pull-nested-data input paths)]
                (resolve env input'))))))))

(defn set-union-path
  [schema-env entity]
  (let [type (and entity (map? entity) (get entity "__typename"))]
    (cond-> entity
      type
      (vary-meta assoc ::pf.eql/union-entry-key
                 (p.eql/process-one schema-env {::gql-type-name type} ::gql-type-qualified-name)))))

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

;; region schema resolvers

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

(pco/defresolver interface-usages-index [{::keys [gql-object-types]}]
  {::pco/input
   [{::gql-object-types
     [::gql-type-qualified-name
      {::gql-type-interfaces
       [::gql-type-qualified-name]}]}]}
  {::gql-interface-usages-index
   (reduce
     (fn [idx
          {::keys    [gql-type-interfaces]
           type-name ::gql-type-qualified-name}]
       (reduce
         (fn [idx {interface-name ::gql-type-qualified-name}]
           (update idx interface-name coll/sconj type-name))
         idx
         gql-type-interfaces))
     {}
     gql-object-types)})

(pco/defresolver query-type [{::keys [gql-schema]}]
  {::gql-query-type {::gql-type-name (get-in gql-schema ["queryType" "name"])}})

(pco/defresolver query-type-qualified-name
  [{::keys [gql-query-type]}]
  {::pco/input [{::gql-query-type [::gql-type-qualified-name]}]}
  {::gql-query-type-qualified-name
   (get gql-query-type ::gql-type-qualified-name)})

(pco/defresolver mutation-type-name [{::keys [gql-schema]}]
  {::gql-mutation-type-name (get-in gql-schema ["mutationType" "name"])})

(pco/defresolver mutation-type [{::keys [gql-mutation-type-name]}]
  {::gql-mutation-type {::gql-type-name gql-mutation-type-name}})

(pco/defresolver mutation-type-qualified-name
  [{::keys [gql-mutation-type]}]
  {::pco/input [{::gql-mutation-type [::gql-type-qualified-name]}]}
  {::gql-mutation-type-qualified-name
   (get gql-mutation-type ::gql-type-qualified-name)})

;; endregion

;; region type data

(pco/defresolver type-data-raw [{::keys [gql-types-index gql-type-name]}]
  {::gql-type-raw (get gql-types-index gql-type-name)})

(pco/defresolver type-name [{::keys [gql-type-raw]}]
  {::gql-type-name
   (get gql-type-raw "name")})

(pco/defresolver type-qualified-name [env {::keys [gql-type-raw]}]
  {::gql-type-qualified-name
   (type->field-name env gql-type-raw)})

(pco/defresolver type-chain-of-type [{::keys [gql-type-raw]}]
  {::pco/output
   [{::gql-type-chain
     [::gql-type-raw]}]}
  {::gql-type-chain
   (->> (coll/iterate-while
          #(get % "ofType")
          gql-type-raw)
        (map #(array-map ::gql-type-raw %)))})

(pco/defresolver type-chain-has-list? [{::keys [gql-type-chain]}]
  {::pco/input
   [{::gql-type-chain
     [::gql-type-kind]}]}
  {::gql-type-chain-list?
   (boolean (first (filter (comp #{"LIST"} ::gql-type-kind) gql-type-chain)))})

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

(pco/defresolver type-indexable? [{::keys [gql-type-kind gql-type-name gql-mutation-type-name]}]
  {::gql-type-indexable?
   (and (contains? #{"OBJECT" "INTERFACE"} gql-type-kind)
        (not= gql-mutation-type-name gql-type-name))})

(pco/defresolver type-object? [{::keys [gql-type-kind]}]
  {::gql-type-object?
   (= "OBJECT" gql-type-kind)})

(pco/defresolver type-interface? [{::keys [gql-type-kind]}]
  {::gql-type-interface?
   (= "INTERFACE" gql-type-kind)})

(pco/defresolver interface-usages
  [{::keys [gql-interface-usages-index
            gql-type-qualified-name]}]
  {::gql-interface-usages (get gql-interface-usages-index gql-type-qualified-name)})

;; endregion

;; region fields

(pco/defresolver field-name [{::keys [gql-field-raw]}]
  {::gql-field-name (get gql-field-raw "name")})

(pco/defresolver field-qualified-name [env {::keys [gql-field-name gql-type-name]}]
  {::gql-field-qualified-name
   (entity-field-key env gql-type-name gql-field-name)})

(pco/defresolver field-raw-type [{::keys [gql-field-raw]}]
  {::pco/output
   [{::gql-field-raw-type
     [::gql-type-raw]}]}
  {::gql-field-raw-type
   {::gql-type-raw (get gql-field-raw "type")}})

(pco/defresolver field-type [{::keys [gql-field-raw]}]
  {::pco/output
   [{::gql-field-type
     [::gql-type-name]}]}

  {::gql-field-type
   {::gql-type-name (type-leaf-name (get gql-field-raw "type"))}})

(pco/defresolver field-args [{::keys [gql-field-raw]}]
  {::pco/output
   [{::gql-field-args
     [::gql-argument-raw]}]}
  {::gql-field-args
   (mapv #(array-map ::gql-argument-raw %) (get gql-field-raw "args"))})

(pco/defresolver argument-name [{::keys [gql-argument-raw]}]
  {::pco/output
   [::gql-argument-name]}
  {::gql-argument-name (get gql-argument-raw "name")})

(pco/defresolver argument-type [{::keys [gql-argument-raw]}]
  {::pco/output
   [{::gql-argument-type
     [::gql-type-raw]}]}
  {::gql-argument-type
   {::gql-type-raw (get gql-argument-raw "type")}})

(def field-type-qualified-name
  (pull-nested-attribute-resolver
    ::gql-field-type
    ::gql-type-qualified-name
    ::gql-field-type-qualified-name))

;; endregion

;; region query type

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

;; endregion

;; region ident map

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

;; endregion

;; region inferred ident map

(pco/defresolver ident-map-type-id-field [{::keys [gql-type-fields]}]
  {::pco/input
   [{::gql-type-fields
     [::gql-field-name
      ::gql-field-raw]}]

   ::pco/output
   [{::gql-type-id-field
     [::gql-field-raw]}]}
  {::gql-type-id-field
   (coll/find-first #(-> % ::gql-field-name (= "id")) gql-type-fields)})

(pco/defresolver ident-map-field-id-arg [{::keys [gql-field-args]}]
  {::pco/input
   [{::gql-field-args
     [::gql-argument-raw
      ::gql-argument-name]}]

   ::pco/output
   [{::gql-field-id-argument
     [::gql-argument-raw]}]}
  {::gql-field-id-argument
   (coll/find-first #(-> % ::gql-argument-name (= "id")) gql-field-args)})

(pco/defresolver ident-map-field-type-inferred-return
  [{::keys [gql-type-chain-list?
            gql-type-indexable?
            gql-type-name
            gql-type-id-field]}]
  {::pco/input
   [{::gql-field-raw-type
     [(<< ::gql-type-chain-list?)]}
    {::gql-field-type
     [(<< ::gql-type-indexable?)
      (<< ::gql-type-name)
      (<< ::gql-type-id-field)]}]

   ::pco/transform
   pull-nested}
  {::gql-field-type-inferred-return
   (or (and (not gql-type-chain-list?)
            gql-type-indexable?
            gql-type-id-field
            gql-type-name)
       nil)})

(pco/defresolver ident-map-field-args-name
  [{:keys [arg-type field-type]}]
  {::pco/input
   [{(pco/? ::gql-field-id-argument)
     [{::gql-argument-type
       [(<< ::gql-type-name :arg-type)]}]}
    {::gql-field-type
     [::gql-type-name
      {::gql-type-id-field
       [{::gql-field-type
         [(<< ::gql-type-name :field-type)]}]}]}]

   ::pco/transform
   pull-nested}
  {::gql-ident-map-field-id-arg
   (if (and arg-type (= arg-type field-type)) "id")})

(pco/defresolver ident-map-for-query-field
  [{::keys [gql-field-name
            gql-ident-map-field-id-arg
            gql-field-type-inferred-return]}]
  {::gql-query-field-ident-map
   (if (and gql-field-name gql-ident-map-field-id-arg gql-field-type-inferred-return)
     {gql-field-name {gql-ident-map-field-id-arg [gql-field-type-inferred-return gql-ident-map-field-id-arg]}})})

(pco/defresolver inferred-ident-map
  [{::keys [gql-query-field-ident-map]}]
  {::pco/input
   [{::gql-query-type
     [{::gql-type-fields
       [(<< ::gql-query-field-ident-map)]}]}]

   ::pco/transform
   pull-nested}
  {::gql-inferred-ident-map
   (into {} (filter some?) gql-query-field-ident-map)})

;; endregion

;; region pathom operations generation

(pco/defresolver type-resolver-op-name [{::keys [namespace]} {::keys [gql-type-name]}]
  {::gql-type-resolver-op-name
   (symbol namespace (str gql-type-name "-resolver"))})

(pco/defresolver type-resolver-input [{::keys [gql-type-qualified-name]}]
  {::gql-type-resolver-input
   [gql-type-qualified-name]})

(pco/defresolver type-resolver-output
  [{::keys [gql-type-fields gql-interface-usages]}]
  {::pco/input
   [::gql-interface-usages
    {::gql-type-fields
     [::gql-field-output-entry]}]}
  {::gql-type-resolver-output
   (-> (or (some-> gql-interface-usages vec) [])
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
    ::gql-pathom-schema-transient-attrs
    #{}))

(pco/defresolver schema-all-transient-attrs
  [{::keys [gql-pathom-schema-transient-attrs
            gql-mutation-type-qualified-name]}]
  {::pco/input
   [::gql-pathom-schema-transient-attrs
    (pco/? ::gql-mutation-type-qualified-name)]}
  {::gql-pathom-transient-attrs
   (cond-> gql-pathom-schema-transient-attrs
     gql-mutation-type-qualified-name
     (conj gql-mutation-type-qualified-name))})

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

(pco/defresolver schema-pathom-mutations
  [{::keys [gql-dynamic-op-name] :as input}]
  {::pco/input
   [::gql-dynamic-op-name
    {::gql-mutation-type
     [{::gql-type-fields
       [::gql-field-qualified-name
        ::gql-field-type-qualified-name]}]}]}
  {::gql-pathom-mutations
   (mapv
     (fn [{::keys [gql-field-qualified-name
                   gql-field-type-qualified-name]}]
       {::pco/op-name      (symbol gql-field-qualified-name)
        ::pco/dynamic-name gql-dynamic-op-name
        ::pco/output       [gql-field-type-qualified-name]})
     (-> input ::gql-mutation-type ::gql-type-fields))})

(pco/defresolver schema->pathom-indexes
  [{::keys [gql-pathom-transient-attrs
            gql-pathom-main-resolver
            gql-pathom-query-type-entry-resolver
            gql-ident-map-resolvers
            gql-pathom-indexable-type-resolvers
            gql-pathom-mutations]}]
  {::gql-pathom-indexes
   (-> {::pci/transient-attrs gql-pathom-transient-attrs}
       (pci/register
         [gql-pathom-main-resolver
          gql-pathom-query-type-entry-resolver
          (mapv pco/resolver gql-ident-map-resolvers)
          (mapv pco/resolver gql-pathom-indexable-type-resolvers)
          (mapv pco/mutation gql-pathom-mutations)]))})

;; endregion

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

         interface-usages-index

         query-type
         query-type-qualified-name
         query-type-field-raw

         mutation-type-name
         mutation-type
         mutation-type-qualified-name

         type-data-raw
         type-name
         type-qualified-name
         type-chain-of-type
         type-chain-has-list?
         type-kind
         type-interfaces
         type-fields
         type-object?
         type-interface?
         type-indexable?

         interface-usages

         type-resolver-op-name
         type-resolver-input
         type-resolver-output
         object-type-resolver

         field-name
         field-qualified-name
         field-raw-type
         field-type
         field-type-qualified-name
         field-output-entry
         field-args
         argument-name
         argument-type

         ident-map-entries
         ident-map-entry-params
         ident-map->op-name
         ident-map->input
         ident-map->output
         ident-map->resolve
         ident-map->resolver

         ident-map-type-id-field
         ident-map-field-id-arg
         ident-map-field-type-inferred-return
         ident-map-field-args-name
         ident-map-for-query-field
         inferred-ident-map

         schema-ident-map-resolvers
         schema-pathom-main-resolver
         schema-transient-attrs
         schema-all-transient-attrs
         schema-object-type-resolvers
         schema-pathom-query-type-entry-resolver
         schema-pathom-mutations
         schema->pathom-indexes])))

(defn load-schema [config request]
  (clet [gql-schema-raw (request (eql-gql/query->graphql schema-query))]
    (-> env
        (merge config)
        (pcp/with-plan-cache (atom {}))
        (assoc
          ::pcr/resolver-cache* (atom {})
          ::request request
          ::gql-schema-raw gql-schema-raw))))

(defn connect-graphql
  "Setup a GraphQL connection on the env.

  The request must be a function that takes a GraphQL string, executes it (or maybe send
  to a server) and return the response as a Clojure map. The Clojure map should be the
  exact same data the GraphQL returns. The keys MUST be encoded as strings, if you convert
  the keys to keywords it wont work.

  Config may include the following keys:

  ::namespace (required) - a namespace (as string) to prefix the entries for this graphql"
  [env {::keys [ident-map] :as config} request]
  (clet [env        env
         schema-env (load-schema config request)
         ident-map' (p.eql/process-one schema-env ::gql-inferred-ident-map)]
    (-> env
        (pci/register
          (p.eql/process-one
            (assoc schema-env ::ident-map (merge ident-map' ident-map))
            ::gql-pathom-indexes)))))

(comment
  (tap> env)

  (p.eql/process env
    [{::gql-query-type [::gql-type-name]}]))
