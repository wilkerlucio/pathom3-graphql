(ns com.wsscode.pathom3.graphql.demos.swapi
  (:require
    [clojure.data.json :as json]
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
      (doto tap>)
      :body
      json/read-str))

(defn make-env []
  (-> {}
      (p.gql/connect-graphql
        {::p.gql/namespace "swapi"}
        request)))


(comment
  (def env (make-env))

  (p.eql/process
    env
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/director
         :swapi.Film/title]}]}])

  (p.eql/process
    env
    {:swapi.Film/id "ZmlsbXM6Mg=="}
    [:swapi.Film/title]))
