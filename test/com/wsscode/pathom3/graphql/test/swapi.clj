(ns com.wsscode.pathom3.graphql.test.swapi
  (:require [clojure.test :refer :all]
            [org.httpkit.client :as http]
            [clojure.data.json :as json]
            [com.wsscode.pathom3.graphql :as p.gql]
            [com.wsscode.pathom3.interface.eql :as p.eql]))

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
        {::p.gql/namespace "swapi"}
        request)))

(comment
  (p.eql/process
    env
    {:swapi.types/Root {}}
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/director
         :myapp.generic/title-display]}]}]))
