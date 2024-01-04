# Pathom 3 GraphQL [![Clojars Project](https://img.shields.io/clojars/v/com.wsscode/pathom3-graphql.svg)](https://clojars.org/com.wsscode/pathom3-graphql) ![Test](https://github.com/wilkerlucio/pathom3-graphql/workflows/Test/badge.svg)

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

(defn request [_ query]
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
        {::p.gql/namespace "swapi"}
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

Check the [demos folder](https://github.com/wilkerlucio/pathom3-graphql/tree/main/demos/com/wsscode/pathom3/graphql/demos) for more example setups.
