(ns com.wsscode.pathom3.graphql
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
    [com.wsscode.misc.coll :as coll]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]))

(>def ::schema map?)

(>def ::ident-map
  (s/map-of string?
            (s/map-of string?
                      (s/tuple string? string?))))

(>def ::resolver ::pco/op-name)
(>def ::prefix string?)

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

(defn prefixed-key [{::keys [prefix]} p s] (keyword (str prefix "." p) s))
(defn type-key [env s] (prefixed-key env "types" s))
(defn interface-key [env s] (prefixed-key env "interfaces" s))
(defn mutation-key [{::keys [prefix]} s] (symbol prefix s))
(defn service-resolver-key [env] (mutation-key env "resolver"))
(defn service-mutation-key [{::keys [prefix]}] (symbol "com.wsscode.pathom.connect.graphql.service-mutations" prefix))

(defn type-leaf-name [{:keys [kind name ofType]}]
  (case kind
    "NON_NULL" (recur ofType)
    "LIST" (recur ofType)
    "OBJECT" name
    "INTERFACE" name
    nil))

(defn type->field-name [env {:keys [kind name ofType]}]
  (case kind
    "NON_NULL" (recur env ofType)
    "LIST" (recur env ofType)
    "OBJECT" (type-key env name)
    "INTERFACE" (interface-key env name)
    nil))

(defn type->field-entry
  "Given a type, return the keyword key to represent it."
  [env type]
  (if-let [name (type->field-name env type)]
    {name {}}
    {}))

(defn entity-field-key [{::keys [prefix]} entity field]
  (keyword (str prefix "." entity) field))

(defn index-type [env {:keys [fields name interfaces] :as entry}]
  {#{(type->field-name env entry)}
   (-> {}
       ; fields
       (into (map #(coll/make-map-entry
                     (entity-field-key env name (:name %))
                     (type->field-entry env (:type %))))
             fields)
       ; interfaces
       (into (map #(coll/make-map-entry
                     (interface-key env (:name %))
                     {}))
             interfaces))})

(defn ident-map-entry [env item]
  (cond
    (keyword? item) item
    (vector? item) (entity-field-key env (first item) (second item))))

(defn ident-map-params->io [env params]
  (->> params vals (into #{} (map (fn [item] (ident-map-entry env item))))))

(defn index-schema-types [schema]
  (let [index (->> schema :__schema :types
                   (group-by :name)
                   (into {} (map (fn [[k v]] [k (first v)]))))]
    (-> (assoc schema ::types-index index)
        (update-in [:__schema :queryType] #(merge % (get index (:name %))))
        (update-in [:__schema :mutationType] #(merge % (get index (:name %)))))))

(defn index-schema-io [{::keys [prefix schema ident-map] :as config}]
  (let [schema (:__schema schema)]
    (-> {}
        (into (comp (filter (comp #{"OBJECT" "INTERFACE"} :kind))
                    (map (partial index-type config)))
              (:types schema))
        (as-> <>
          (reduce (fn [idx {:keys [name type]}]
                    (if-let [params (get ident-map name)]
                      (let [input-set (ident-map-params->io config params)]
                        (update idx input-set coll/merge-grow {(ffirst (type->field-entry config type)) {}}))
                      (update idx #{} coll/merge-grow
                        {(keyword prefix name) {(ffirst (type->field-entry config type)) {}}})))
            <>
            (->> schema :queryType :fields))))))

(defn args-translate [{::keys [ident-map]} {:keys [name args]}]
  (if-let [mapping-table (get ident-map name)]
    (let [inputs (into []
                       (keep (fn [arg]
                               (get mapping-table (:name arg))))
                       args)]
      (if (not= (count inputs) (count args))
        (throw (ex-info (str "Invalid attribute mapping on entry " name)
                        {:name          name
                         :args          args
                         :mapping-table mapping-table
                         :inputs        inputs})))
      inputs)
    []))

;; how to convert root entries

;; 1. global, no args: put nested under the entry name from graphql
;; 2. with args

;; to wrap or no to wrap??
;; -- when there is ident map, no wrap

(defn get-type [env entry]
  (get-in env [::schema ::types-index (type-leaf-name entry)]))

(defn type-expand-properties [env {:keys [fields name interfaces]}]
  (-> []
      ; fields
      (into (map #(entity-field-key env name (:name %)))
            fields)
      ; interfaces
      (into (mapcat #(type-expand-properties env (get-type env %)))
            interfaces)))

(defn index-aux-resolvers
  "Creates auxiliary resolvers to drive the requests to the GraphQL.

  To do this, the algorithm looks for the query roots from the GraphQL Schema.

  There are two distinct ways in which this function generates the attributes.

  If there is a ident mapping to that root entry, the type is pulled of and made
  available from any root.

  When there is not a mapping, we wrap the type output attributes under the name
  of the query root entry."
  [{::keys [prefix schema resolver ident-map]
    :as    env}]
  (let [schema (:__schema schema)
        roots  (-> schema :queryType :fields)]
    (into
      []
      (map (fn [entry]
             (let [op-name    (symbol (str prefix ".root-entry") (:name entry))
                   input      (args-translate env entry)
                   type-props (type-expand-properties env (get-type env (:type entry)))
                   output     (if (get ident-map (:name entry))
                                type-props
                                [{(keyword prefix (:name entry)) type-props}])]
               (pco/resolver
                 {::pco/op-name      op-name
                  ::pco/dynamic-name resolver
                  ::pco/input        input
                  ::pco/output       output}))))
      roots)))

(defn index-graphql-idents
  [{::keys     [schema ident-map]
    ::pci/keys [index-io]
    :as        env}]
  (let [schema (:__schema schema)
        fields (-> schema :queryType :fields)
        idents (filter (comp ident-map :name) fields)]
    (-> {}
        (into (mapcat (fn [{:keys [name type]}]
                        (let [params        (get ident-map name)
                              ident-key     (keyword name (str/join "-and-" (keys params)))
                              entity-fields (mapv (fn [item] (ident-map-entry env item)) (vals params))
                              entity-field  (cond-> entity-fields (= 1 (count entity-fields)) first)
                              fields        (-> (get index-io #{(ffirst (type->field-entry env type))})
                                                keys)]
                          (mapv (fn [field]
                                  [field {::entity-field entity-field
                                          ::ident-key    ident-key}])
                            fields))))
              idents))))

(defn index-schema [{::keys [resolver] :as config}]
  (let [resolver (or resolver (service-resolver-key config))
        config   (update config ::schema index-schema-types)
        index-io (index-schema-io config)
        config   (assoc config ::pci/index-io index-io
                   ::resolver resolver)]
    (pci/register
      {::pci/index-resolvers
       {resolver
        (pco/resolver
          {::pco/op-name           resolver
           ::pco/cache?            false
           ::graphql?              true
           ::pco/dynamic-resolver? true
           ::pco/resolve           (fn [_env _]
                                     {}
                                     #_(graphql-resolve config env))})}

       ::pci/index-io
       index-io

       ::field->ident
       (index-graphql-idents config)}
      (index-aux-resolvers config))))

(defn normalize-schema
  "Depending on encoding settings sometimes the :kind can come as a keyword, the indexer expects it to
  be a string, this function ensures all :kind fields are strings."
  [schema]
  (walk/postwalk
    (fn [x]
      (if (and (map? x)
               (contains? x :kind))
        (update x :kind #(some-> % name))
        x))
    schema))
