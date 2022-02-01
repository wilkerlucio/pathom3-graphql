(ns com.wsscode.pathom3.graphql-test
  (:require
    [clojure.test :refer [deftest is are run-tests testing]]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.graphql.test.server :as t-server]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.interface.smart-map :as psm]
    [edn-query-language.core :as eql]))

(def schema-config
  {::p.gql/namespace "acme.sw"})

(def schema (p.gql/load-schema schema-config t-server/request))

(def gql-env
  (-> {}
      (p.gql/connect-graphql
        schema-config
        t-server/request)))

(defn prepare-gql-query [query]
  (-> query
      eql/query->ast
      p.gql/prepare-gql-ast
      eql/ast->query))

(deftest prepare-gql-query-test
  (is (= (prepare-gql-query
           [{:acme.sw.Query/hero
             [:acme.sw.character/id
              :acme.sw.character/name
              :acme.sw.human/home_planet]}])
         '[({:acme.sw.Query/hero [(:acme.sw.character/id
                                    {:edn-query-language.eql-graphql/on "character"})
                                  (:acme.sw.character/name
                                    {:edn-query-language.eql-graphql/on "character"})
                                  (:acme.sw.human/home_planet
                                    {:edn-query-language.eql-graphql/on "human"})
                                  :__typename]}
            {:edn-query-language.eql-graphql/on "Query"})
           :__typename]))

  (testing "union"
    (is (= (prepare-gql-query
             [{:acme.sw.Query/hero
               {:acme.sw.types/human
                [:acme.sw.human/name]}}])
           '[({:acme.sw.Query/hero
               {:acme.sw.types/human [:acme.sw.human/name]}}
              {:edn-query-language.eql-graphql/on "Query"})
             :__typename]))))

(deftest integration-tests
  (testing "running query root entry"
    (is (= (p.eql/process
             gql-env
             [{:acme.sw.Query/human
               [:acme.sw.human/id
                :acme.sw.human/name]}])
           {:acme.sw.Query/human
            {:acme.sw.human/id   "2000"
             :acme.sw.human/name "Lando Calrissian"}})))

  (testing "interface entry"
    (is (= (p.eql/process
             gql-env
             [{:acme.sw.Query/hero
               [:acme.sw.character/id
                :acme.sw.character/name]}])
           {:acme.sw.Query/hero
            {:acme.sw.character/id   "2000",
             :acme.sw.character/name "Lando Calrissian"}}))

    (testing "specific type data"
      (is (= (p.eql/process
               gql-env
               [{:acme.sw.Query/hero
                 [:acme.sw.character/id
                  :acme.sw.character/name
                  :acme.sw.human/home_planet]}])
             {:acme.sw.Query/hero
              {:acme.sw.character/id      "2000",
               :acme.sw.character/name    "Lando Calrissian",
               :acme.sw.human/home_planet "Socorro"}})))

    (testing "union query"
      (is (= (p.eql/process
               (pci/register gql-env
                             (pbir/alias-resolver :acme.sw.human/home_planet :planet))
               [{:acme.sw.Query/hero
                 {:acme.sw.types/human
                  [:planet]}}])
             {:acme.sw.Query/hero {:planet "Socorro"}}))))

  (testing "starting from known root entry mapping"
    (is (= (p.eql/process
             gql-env
             {:acme.sw.human/id "1000"}
             [:acme.sw.human/id
              :acme.sw.human/name])
           {:acme.sw.human/id   "1000"
            :acme.sw.human/name "Luke"})))

  (testing "can handle multiple idents at once"
    (is (= (p.eql/process
             gql-env
             {:acme.sw.human/id "1000"
              :acme.sw.droid/id "3"}
             [:acme.sw.human/id
              :acme.sw.human/name

              :acme.sw.droid/name
              :acme.sw.droid/primary_function])
           {:acme.sw.human/id               "1000",
            :acme.sw.human/name             "Luke",
            :acme.sw.droid/name             "Droid Sample",
            :acme.sw.droid/primary_function ["Work"]})))

  (testing "call mutation"
    (is (= (p.eql/process
             gql-env
             [{'(acme.sw.Mutation/create_human {:name "Human"})
               [:acme.sw.human/id
                :acme.sw.human/name]}])
           '{acme.sw.Mutation/create_human {:acme.sw.human/id "3000", :acme.sw.human/name "Human"}}))))

(comment

  (tap> (::p.gql/gql-pathom-indexes schema))
  (::p.gql/gql-ident-map-resolvers schema)
  (mapv ::p.gql/gql-type-qualified-name (::p.gql/gql-all-types schema))
  (p.eql/process
    gql-env
    {:acme.sw.human/id "1000"}
    [:acme.sw.human/id
     :acme.sw.human/name
     :acme.sw.human/home_planet])

  #_(-> schema
        ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
         "debug"))

  #_#_(p.eql/process
        (-> gql-env
            ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
             "gql"))
        {:acme.sw.human/id "1000"
         :acme.sw.droid/id "3"}
        [:acme.sw.human/id
         :acme.sw.human/name

         :acme.sw.droid/name
         :acme.sw.droid/primary_function])

          (p.eql/process
            (-> gql-env
                ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
                 "gql"))
            {:acme.sw.droid/id "3"}
            [:acme.sw.droid/name])

  (psm/datafy-smart-map schema)

  (tap> schema)

  (-> schema
      psm/sm-env
      (assoc :com.wsscode.pathom3.connect.runner/fail-fast? true)
      (p.eql/process [{::p.gql/gql-ident-map-entries
                       [::p.gql/gql-ident-map-entry-resolver]}]))

  (-> schema
      psm/sm-env
      (p.eql/process
        [::p.gql/gql-pathom-indexes]))

  (-> schema (::p.gql/gql-pathom-indexes)
      ::pci/index-attributes
      keys)

  (p.eql/process
    gql-env
    {:swapi.types/Root {}}
    [{:swapi.Root/allFilms
      [{:swapi.FilmsConnection/films
        [:swapi.Film/director
         :myapp.generic/title-display]}]}])

  (p.eql/process
    (pci/register
      gql-env
      (pbir/alias-resolver :acme.sw.human/name :uname))
    {:acme.sw.types/Query {}}
    [{'(:acme.sw.Query/human {:acme.sw.human/id "1000"})
      [:acme.sw.human/id
       :acme.sw.human/name
       {:acme.sw.human/friends
        [:acme.sw.character/name]}]}]))
