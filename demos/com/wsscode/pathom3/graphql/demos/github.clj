(ns com.wsscode.pathom3.graphql.demos.github
  (:require
    [clojure.data.json :as json]
    [com.wsscode.pathom3.graphql :as p.gql]
    [org.httpkit.client :as http]))

(def token (System/getenv "GITHUB_TOKEN"))

(defn request [query]
  (-> @(http/request
         {:url     "https://api.github.com/graphql"
          :method  :post
          :headers {"Content-Type"  "application/json"
                    "Accept"        "*/*"
                    "Authorization" (str "Bearer " token)}
          :body    (json/write-str {:query query})})
      :body
      (doto tap>)
      json/read-str))

(defn make-env []
  (-> {}
      (p.gql/connect-graphql
        {::p.gql/namespace "github"}
        request)
      ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
       "gql-github")))

(comment
  (time
    (def env (make-env))))
