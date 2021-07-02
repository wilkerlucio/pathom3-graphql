(ns com.wsscode.pathom3.graphql.test.server
  (:require [clojure.data.json :as json]
            [com.walmartlabs.lacinia :refer [execute]]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.util :as util]))

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
  (let [{:keys [episode]} arguments]
    (schema/tag-with-type
      (if (= episode :NEWHOPE)
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
  (let [{:keys [episode]} arguments]
    (if (= episode :NEWHOPE)
      {:id          1000
       :name        "Luke"
       :home_planet "Tatooine"
       :appears_in  ["NEWHOPE" "EMPIRE" "JEDI"]}
      {:id          2000
       :name        "Lando Calrissian"
       :home_planet "Socorro"
       :appears_in  ["EMPIRE" "JEDI"]})))

(defn resolve-droid [context arguments value]
  )

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
  (json/write-str (execute star-wars-schema query nil nil)))

(comment
  (request "{\n  human {\n    id\n    name\n    friends {\n      name\n    }\n  }\n}"))
