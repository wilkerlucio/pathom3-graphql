(ns com.wsscode.pathom3.graphql.test.server
  (:require [clojure.data.json :as json]
            [org.httpkit.client :as http]
            [edn-query-language.eql-graphql :as eql-gql]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [com.wsscode.pathom3.graphql :as p.gql]
            [com.walmartlabs.lacinia :refer [execute]]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.util :as util]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.connect.indexes :as pci]))

(def schema
  '{:enums
    {:episode
     {:description "The episodes of the original Star Wars trilogy."
      :values      [:NEWHOPE :EMPIRE :JEDI]}}

    :interfaces
    {:character
     {:fields {:id         {:type String}
               :name       {:type String}
               :appears_in {:type (list :episode)}
               :friends    {:type (list :character)}}}}

    :objects
    {:droid
     {:implements [:character]
      :fields     {:id               {:type String}
                   :name             {:type String}
                   :appears_in       {:type (list :episode)}
                   :friends          {:type    (list :character)
                                      :resolve :friends}
                   :primary_function {:type (list String)}}}

     :human
     {:implements [:character]
      :fields     {:id          {:type String}
                   :name        {:type String}
                   :appears_in  {:type (list :episode)}
                   :friends     {:type    (list :character)
                                 :resolve :friends}
                   :home_planet {:type String}}}}

    :queries
    {:hero  {:type    (non-null :character)
             :args    {:episode {:type :episode}}
             :resolve :hero}

     :human {:type    (non-null :human)
             :args    {:id {:type          String
                            :default-value "1001"}}
             :resolve :human}

     :droid {:type    :droid
             :args    {:id {:type          String
                            :default-value "2001"}}
             :resolve :droid}}})

(defn resolve-hero [context arguments value]
  (let [{:keys [id]} arguments]
    (schema/tag-with-type
      (if (= id 1000)
        {:id          1000
         :name        "Luke"
         :home_planet "Tatooine"
         :appears_in  ["NEWHOPE" "EMPIRE" "JEDI"]}
        {:id          2000
         :name        "Lando Calrissian"
         :home_planet "Socorro"
         :appears_in  ["EMPIRE" "JEDI"]})
      :human)))

(defn resolve-human [context arguments value]
  (let [{:keys [id]} arguments]
    (if (= id "1000")
      {:id          1000
       :name        "Luke"
       :home_planet "Tatooine"
       :appears_in  ["NEWHOPE" "EMPIRE" "JEDI"]}
      {:id          2000
       :name        "Lando Calrissian"
       :home_planet "Socorro"
       :appears_in  ["EMPIRE" "JEDI"]})))

(defn resolve-droid [context arguments value]
  (let [{:keys [id]} arguments]
    {:id               1
     :name             "Droid Sample"
     :primary_function ["Work"]}))

(defn resolve-friends [context arguments value]
  )

(def star-wars-schema
  (-> schema
      (util/attach-resolvers {:hero    resolve-hero
                              :human   resolve-human
                              :droid   resolve-droid
                              :friends resolve-friends})
      schema/compile))

(defn request [query]
  (json/read-str (json/write-str (execute star-wars-schema query nil nil))))

(defn load-schema [request]
  (p.gql/load-schema {::p.gql/namespace "acme.stars"} request))

(def schema
  (load-schema request))

(comment
  (load-schema request2)

  (-> @(http/request
         {:url     "https://swapi-graphql.netlify.app/.netlify/functions/index"
          :method  :post
          :headers {"Content-Type" "application/json"
                    "Accept"       "*/*"}
          :body    (json/write-str {:query "{\n  allPeople {\n    people {\n      id\n      name\n    }\n  }\n}"})})
      :body
      json/read-str)

  (-> (load-schema request)
      ::p.gql/gql-query-type
      ::p.gql/gql-type-indexable?)

  (-> (load-schema request)
      ::p.gql/gql-query-type
      ::p.gql/gql-type-qualified-name)

  (-> (load-schema request)
      ::p.gql/gql-types-index)

  (->> (load-schema request)
       ::p.gql/gql-all-types
       (mapv ::p.gql/gql-type-qualified-name))

  (->> (load-schema request)
       ::p.gql/gql-all-types)

  (->> (load-schema request)
       ::p.gql/gql-indexable-types
       )

  (->> (psm/sm-replace-context schema
         {::p.gql/gql-type-name "human"})
       ::p.gql/gql-type-interfaces
       (mapv ::p.gql/gql-type-qualified-name))

  (-> schema
      (assoc ::p.gql/gql-type-name "human")
      ::p.gql/gql-indexable-type-resolver)

  (tap> (psm/sm-entity schema))

  (->> schema
       ::p.gql/gql-indexable-types
       (mapv ::p.gql/gql-indexable-type-resolver))

  (->> schema
       ::p.gql/gql-pathom-indexes)

  (tap> (load-schema request2))

  (->> (load-schema request2)
       ::p.gql/gql-pathom-indexes)

  (json/read-str (request (eql-gql/query->graphql p.gql/schema-query)))
  (request "{\n  human {\n    id\n    name\n    friends {\n      name\n    }\n  }\n}")

  (request "query {\n  human {\n    id\n    name\n    __typename\n  }\n  __typename\n}\n"
    ))
