(ns com.wsscode.pathom3.graphql.demos.swapi
  (:require
    [clojure.data.json :as json]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.graphql-speed :as p.gql2]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [org.httpkit.client :as http]))

(defn request [query]
  (-> @(http/request
         {:url     "https://swapi-graphql.netlify.app/.netlify/functions/index"
          :method  :post
          :headers {"Content-Type" "application/json"
                    "Accept"       "*/*"}
          :body    (json/write-str {:query query})})
      (doto tap>)
      :body
      json/read-str))

(defn make-env []
  (-> {}
      (p.gql/connect-graphql
        {::p.gql/namespace "swapi"}
        request)))

(defn make-env2 []
  (-> {}
      (p.gql2/connect-graphql
        {::p.gql2/namespace "swapi"}
        request)))


(comment
  (def env (make-env))
  (def env2 (make-env2))

  (p.eql/process
    env2
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/director
         :swapi.Film/title]}]}])

  (p.eql/process
    env
    {:swapi.Film/id "ZmlsbXM6Mg=="}
    [:swapi.Film/title]))
