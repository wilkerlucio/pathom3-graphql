(ns com.wsscode.pathom3.graphql.demos.github
  (:require
    [clojure.data.json :as json]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [org.httpkit.client :as http]))

(def token (System/getenv "GITHUB_TOKEN"))

(defn request [query]
  (tap> ["Q" query])
  (-> @(http/request
         {:url     "https://api.github.com/graphql"
          :method  :post
          :headers {"Content-Type"  "application/json"
                    "Accept"        "*/*"
                    "Authorization" (str "Bearer " token)}
          :body    (json/write-str {:query query})})
      :body
      json/read-str
      (doto tap>)))

(pco/defresolver shortcut
  [{{edges :github.RepositoryConnection/edges} :github.Organization/repositories}]
  {::pco/input  ['{(:github.Organization/repositories {:first 1})
                   [{:github.RepositoryConnection/edges
                     [{:github.RepositoryEdge/node
                       [:github.Repository/name
                        :github.Repository/nameWithOwner
                        {:github.Repository/owner [:github.RepositoryOwner/id]}]}]}]}]
   ::pco/output [{:github.Organization/all-repos
                  [:github.Repository/id
                   :github.Repository/name]}]}
  {:github.Organization/all-repos (mapv :github.RepositoryEdge/node edges)})

(pco/defresolver shortcut2
  [{:github.Repository/keys [owner]}]
  {::pco/input  [:github.Repository/name
                 :github.Repository/nameWithOwner
                 {:github.Repository/owner [:github.RepositoryOwner/id]}]
   ::pco/output [{:owner
                  [:github.Repository/name
                   :github.Repository/nameWithOwner]}]}
  {:owner owner})

(defn make-env []
  (-> {}
      (p.gql/connect-graphql
        {::p.gql/namespace        "github"
         ::p.gql/root-entries-map {"repository"   {"name"  ["Repository" "name"]
                                                   "owner" ["User" "login"]}
                                   "user"         {"login" ["User" "login"]}
                                   "organization" {"login" ["Organization" "login"]}}}
        request)))

(defonce env
  (make-env))

(def env'
  (-> (pci/register env
                    [shortcut
                     shortcut2])
      ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
       "gql-github")))

(comment
  (tap> env)
  (throw (ex-info "A" {}))

  (p.eql/process env'
    {:github.Repository/name "fulcro"
     :github.User/login      "fulcrologic"}
    [:owner])

  (p.eql/process env'
    '[{(:github.Query/search {:query "sheluchin" :type USER :first 1})
       [:github.SearchResultItemConnection/userCount
        {:github.SearchResultItemConnection/edges
         [{:github.SearchResultItemEdge/node
           [:github.User/login]}]}]}])

  (p.eql/process env
    {:github.Organization/login "fulcrologic"}
    ['{(:github.Organization/repositories {:first 1})
       [{:github.RepositoryConnection/edges
         [{:github.RepositoryEdge/node
           [:github.Repository/name
            :github.Repository/nameWithOwner
            {:github.Repository/owner [:github.RepositoryOwner/id]}]}]}]}])

  (p.eql/process env'
    {:github.Organization/login "fulcrologic"}
    [{:>/other
      [:github.Organization/all-repos]}
     {:>/sub
      ['{(:github.Organization/repositories {:first 10})
         [{:github.RepositoryConnection/edges
           [{:github.RepositoryEdge/node
             [:github.Repository/name
              :github.Repository/nameWithOwner
              {:github.Repository/owner [:github.RepositoryOwner/id
                                         :github.RepositoryOwner/login]}]}]}]}]}])

  (tap> env')

  (time
    (def env (make-env))))

