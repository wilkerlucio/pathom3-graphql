(ns com.wsscode.pathom3.graphql-test
  (:require
    [clojure.test :refer [deftest is are run-tests testing]]
    [com.wsscode.pathom3.connect.built-in.plugins :as pbip]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.graphql.test.server :as t-server]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.interface.smart-map :as psm]
    [com.wsscode.pathom3.plugin :as p.plugin]
    [promesa.core :as p]
    [com.wsscode.pathom3.connect.operation :as pco]))

(def schema-config
  {::p.gql/namespace "acme.sw"
   ::p.gql/ident-map {"human" {"id" ["human" "id"]}
                      "hero"  {"id" ["character" "id"]}
                      "droid" {"id" ["droid" "id"]}}})

(def schema (p.gql/load-schema schema-config t-server/request))

(psm/sm-get-with-stats (psm/sm-update-env schema assoc :com.wsscode.pathom3.connect.runner/omit-run-stats-resolver-io? true) ::p.gql/gql-pathom-indexable-type-resolvers)

(deftest index-schema-test
  (testing "::p.gql/gql-pathom-transient-attrs"
    (is (= (::p.gql/gql-pathom-transient-attrs schema)
           #{:acme.sw.types/human
             :acme.sw.types/Query
             :acme.sw.interfaces/character
             :acme.sw.types/droid})))

  (testing "type aux resolvers"
    (is (= (::p.gql/gql-pathom-indexable-type-resolvers schema)
           '[{::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.types/Query]
              ::pco/op-name      acme.sw/Query-resolver
              ::pco/output       [{:acme.sw.Query/droid [:acme.sw.types/droid]}
                                  {:acme.sw.Query/hero [:acme.sw.interfaces/character]}
                                  {:acme.sw.Query/human [:acme.sw.types/human]}]}
             {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.interfaces/character]
              ::pco/op-name      acme.sw/character-resolver
              ::pco/output       [:acme.sw.character/appears_in
                                  {:acme.sw.character/friends [:acme.sw.interfaces/character]}
                                  :acme.sw.character/id
                                  :acme.sw.character/name]}
             {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.types/droid]
              ::pco/op-name      acme.sw/droid-resolver
              ::pco/output       [:acme.sw.interfaces/character
                                  :acme.sw.droid/appears_in
                                  {:acme.sw.droid/friends [:acme.sw.interfaces/character]}
                                  :acme.sw.droid/id
                                  :acme.sw.droid/name
                                  :acme.sw.droid/primary_function]}
             {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.types/human]
              ::pco/op-name      acme.sw/human-resolver
              ::pco/output       [:acme.sw.interfaces/character
                                  :acme.sw.human/appears_in
                                  {:acme.sw.human/friends [:acme.sw.interfaces/character]}
                                  :acme.sw.human/home_planet
                                  :acme.sw.human/id
                                  :acme.sw.human/name]}]))

    (testing "load async"
      (is (= (-> @(p.gql/load-schema schema-config #(p/do! (t-server/request %)))
                 ::p.gql/gql-pathom-indexable-type-resolvers)
             '[{::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.types/Query]
                ::pco/op-name      acme.sw/Query-resolver
                ::pco/output       [{:acme.sw.Query/droid [:acme.sw.types/droid]}
                                    {:acme.sw.Query/hero [:acme.sw.interfaces/character]}
                                    {:acme.sw.Query/human [:acme.sw.types/human]}]}
               {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.interfaces/character]
                ::pco/op-name      acme.sw/character-resolver
                ::pco/output       [:acme.sw.character/appears_in
                                    {:acme.sw.character/friends [:acme.sw.interfaces/character]}
                                    :acme.sw.character/id
                                    :acme.sw.character/name]}
               {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.types/droid]
                ::pco/op-name      acme.sw/droid-resolver
                ::pco/output       [:acme.sw.interfaces/character
                                    :acme.sw.droid/appears_in
                                    {:acme.sw.droid/friends [:acme.sw.interfaces/character]}
                                    :acme.sw.droid/id
                                    :acme.sw.droid/name
                                    :acme.sw.droid/primary_function]}
               {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.types/human]
                ::pco/op-name      acme.sw/human-resolver
                ::pco/output       [:acme.sw.interfaces/character
                                    :acme.sw.human/appears_in
                                    {:acme.sw.human/friends [:acme.sw.interfaces/character]}
                                    :acme.sw.human/home_planet
                                    :acme.sw.human/id
                                    :acme.sw.human/name]}])))))

(def gql-env
  (-> {}
      ;(p.plugin/register (pbip/attribute-errors-plugin))
      (p.gql/connect-graphql
        schema-config
        t-server/request)))

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
           {:acme.sw.human/id "1000",
            :acme.sw.human/name "Luke",
            :acme.sw.droid/name "Droid Sample",
            :acme.sw.droid/primary_function ["Work"]}))))

(comment
  (::p.gql/gql-pathom-indexes schema)
  (::p.gql/gql-ident-map-resolvers schema)
  (mapv ::p.gql/gql-type-qualified-name (::p.gql/gql-all-types schema))
  (p.eql/process
    (-> gql-env
        ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
         "gql"))
    {:acme.sw.human/id "1000"}
    [:acme.sw.human/id
     :acme.sw.human/name
     :acme.sw.human/home_planet])


  (p.eql/process
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

  (-> (p.gql/load-schema* schema-config t-server/request)
      (p.eql/process
        [::p.gql/gql-pathom-indexable-type-resolvers]))

  (-> (p.gql/load-schema* schema-config t-server/request)
      ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
       "schema"))

  (-> schema
      psm/sm-env
      (assoc :com.wsscode.pathom3.connect.runner/fail-fast? true)
      (p.plugin/register (pbip/attribute-errors-plugin))
      (p.eql/process [{::p.gql/gql-ident-map-entries
                       [::p.gql/gql-ident-map-entry-resolver]}]))

  (-> schema
      psm/sm-env
      (p.plugin/register (pbip/attribute-errors-plugin))
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
