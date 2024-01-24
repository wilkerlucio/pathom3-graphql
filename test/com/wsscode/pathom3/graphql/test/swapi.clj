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

(defn request-tmdb [_ query]
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
        {::p.gql/namespace        "tmdb"
         ::p.gql/root-entries-map {"node" {"id" ["Node" "id"]}}}
        request-tmdb)
      (pci/register
        [(pbir/alias-resolver :tmdb.Movie/id :tmdb.Node/id)
         (pbir/alias-resolver :tmdb.Movie/budget :budget)])
      ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
       "swapi")))

(comment
  (def schema (p.gql/load-schema {} {::p.gql/namespace "swapi"}
     request))

  (p.eql/process-one schema ::p.gql/gql-inferred-ident-map)

  (p.eql/process
    env
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/title
         :budget]}]}])

  (p.eql/process
    env
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/title]}]}])

  (p.eql/process
    env
    {:swapi.Film/id "ZmlsbXM6Mg=="}
    [:swapi.Film/title
     :tmdb.Movie/budget])

  (p.eql/process
    env
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/title
         :tmdb.Movie/budget]}]}])

  (p.eql/process
    env
    {}
    [{'(:tmdb.Query/node {:tmdb.Node/id "MDoxODk1"})
      {:tmdb.types/Movie
       [:tmdb.Movie/title
        {:tmdb.Movie/images
         [{:tmdb.MediaImages/backdrops
           ['(:tmdb.BackdropSizeDetailImage/image
               {:size W1280})]}]}]}}]))

(comment
  (->> (coll/index-by :swapi.Film/id relations)
       (coll/map-vals :tmdb.Movie/id))

  (->> (coll/index-by :tmdb.Movie/id relations)
       (coll/map-vals :swapi.Film/id)))
