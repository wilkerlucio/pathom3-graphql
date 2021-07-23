# Pathom 3 GraphQL

This library provides an implementation to integrate GraphQL services in Pathom environments.

## Example setup

```clojure
(ns com.wsscode.pathom3.graphql.test.swapi
  (:require
    [clojure.data.json :as json]
    [com.wsscode.misc.coll :as coll]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [org.httpkit.client :as http]))

(defn request [query]
  (-> @(http/request
         {:url     "https://swapi-graphql.netlify.app/.netlify/functions/index"
          :method  :post
          :headers {"Content-Type" "application/json"
                    "Accept"       "*/*"}
          :body    (json/write-str {:query query})})
      :body
      json/read-str))

(def env
  (-> {}
      (p.gql/connect-graphql
        {::p.gql/namespace "swapi"
         ::p.gql/ident-map {"film"     {"id" ["Film" "id"]}
                            "person"   {"id" ["Person" "id"]}
                            "planet"   {"id" ["Planet" "id"]}
                            "species"  {"id" ["Species" "id"]}
                            "starship" {"id" ["Starship" "id"]}
                            "vehicle"  {"id" ["Vehicle" "id"]}}}
        request)))

(p.eql/process
  env
  [{:swapi.Root/allFilms
    [{:swapi.FilmsConnection/films
      [:swapi.Film/director
       :swapi.Film/title]}]}])

(p.eql/process
  env
  {:swapi.Film/id "ZmlsbXM6Mg=="}
  [:swapi.Film/title])
```
