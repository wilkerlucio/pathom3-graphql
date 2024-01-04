(ns com.wsscode.pathom3.graphql.demos.swapi
  (:require
   [com.wsscode.pathom3.graphql :as p.gql]
   [com.wsscode.pathom3.interface.async.eql :as p.eql]
   [promesa.core :as p]))

(defn request-swapi-graphql
  [_ query]
  (p/let [url "https://swapi-graphql.netlify.app/.netlify/functions/index"
          headers #js {"Content-Type" "application/json"
                       "Accept"       "*/*"}
          body (js/JSON.stringify (clj->js {:query query}))
          resp (js/fetch url #js {:method  "POST"
                                  :headers headers
                                  :body    body})
          js-data (.json resp)]
    (js->clj js-data)))

(def env
  (-> {}
    (p.gql/connect-graphql
      {::p.gql/namespace "swapi"}
      request-swapi-graphql)))

(comment
  (p/let [env env]
    (tap> env))

  (p/let [result (p.eql/process env
                   [{:swapi.Root/allPeople
                     [{:swapi.PeopleConnection/people
                       [:swapi.Person/name
                        {:swapi.Person/filmConnection
                         [{:swapi.PersonFilmsConnection/films
                           [:swapi.Film/title]}]}]}]}])]
    (js/console.log "!! " result))
  )
