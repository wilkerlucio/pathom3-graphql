(ns com.wsscode.pathom3.graphql.demos.gitlab
  (:require
    [clojure.data.json :as json]
    [com.wsscode.pathom3.graphql :as p.gql]
    [org.httpkit.client :as http]
    [org.httpkit.sni-client :as sni-client]))

(alter-var-root #'org.httpkit.client/*default-client* (fn [_] sni-client/default-client))

(def token (System/getenv "GITLAB_TOKEN"))

(defn request [query]
  (-> @(http/request
         {:url     "https://gitlab.com/api/graphql"
          :method  :post
          :headers {"Content-Type" "application/json"
                    "Accept"       "*/*"
                    "Authorization" (str "Bearer " token)}
          :body    (json/write-str {:query query})})
      :body
      json/read-str))

(defn make-env []
  (-> {}
      (p.gql/connect-graphql
        {::p.gql/namespace "gitlab"}
        request)
      ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
       "gql-gitlab")))

(comment
  (time
    (def env (make-env))))
