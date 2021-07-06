(ns com.wsscode.pathom3.graphql.test.swapi
  (:require [clojure.test :refer :all]
            [org.httpkit.client :as http]
            [clojure.data.json :as json]
            [com.wsscode.pathom3.graphql :as p.gql]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
            [com.wsscode.misc.coll :as coll]
            [com.wsscode.pathom3.connect.indexes :as pci]))

(defn request [query]
  (-> @(http/request
         {:url     "https://swapi-graphql.netlify.app/.netlify/functions/index"
          :method  :post
          :headers {"Content-Type" "application/json"
                    "Accept"       "*/*"}
          :body    (json/write-str {:query query})})
      :body
      json/read-str))

(defn request-tmdb [query]
  (-> @(http/request
         {:url     "https://tmdb.apps.quintero.io/"
          :method  :post
          :headers {"Content-Type" "application/json"
                    "Accept"       "*/*"}
          :body    (json/write-str {:query query})})
      :body
      json/read-str))

(def relations
  [{:swapi.Film/id "ZmlsbXM6MQ=="
    :tmdb.Movie/id "MDoxMQ=="}
   {:swapi.Film/id "ZmlsbXM6Mg=="
    :tmdb.Movie/id "MDoxODkx"}
   {:swapi.Film/id "ZmlsbXM6Mw=="
    :tmdb.Movie/id "MDoxODky"}
   {:swapi.Film/id "ZmlsbXM6NA=="
    :tmdb.Movie/id "MDoxODkz"}
   {:swapi.Film/id "ZmlsbXM6NQ=="
    :tmdb.Movie/id "MDoxODk0"}
   {:swapi.Film/id "ZmlsbXM6Ng=="
    :tmdb.Movie/id "MDoxODk1"}])

(defn connect-foreign-keys [relations from to]
  (pbir/static-attribute-map-resolver
    from
    to
    (->> (coll/index-by from relations)
         (coll/map-vals to))))

(def env
  (-> {}
      (pci/register
        [(connect-foreign-keys relations :swapi.Film/id :tmdb.Movie/id)
         (connect-foreign-keys relations :tmdb.Movie/id :swapi.Film/id)])
      (p.gql/connect-graphql
        {::p.gql/namespace "swapi"}
        request)
      (p.gql/connect-graphql
        {::p.gql/namespace "tmdb"
         ::p.gql/ident-map {"node" {"id" ["Node" "id"]}}}
        request-tmdb)
      ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
       "swapi")))

(comment
  (p.eql/process
    env
    {}
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/director
         :swapi.Film/title]}]}])

  (p.eql/process
    env
    {}
    [{(:tmdb.Query/node {:tmdb.Node/id "MDoxODk1"})
      {:tmdb.types/Movie
       [:tmdb.Movie/title]}}]))

(comment
  (->> (coll/index-by :swapi.Film/id relations)
       (coll/map-vals :tmdb.Movie/id))

  (->> (coll/index-by :tmdb.Movie/id relations)
       (coll/map-vals :swapi.Film/id))

  (pbir/single-attr-resolver))
