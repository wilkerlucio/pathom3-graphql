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
    [promesa.core :as p]))

(def schema-config
  {::p.gql/namespace "acme.sw"
   ::p.gql/ident-map {"human" {"id" ["human" "id"]}
                      "hero"  {"id" ["character" "id"]}
                      "droid" {"id" ["droid" "id"]}}})

(def schema (p.gql/load-schema schema-config t-server/request))

(deftest index-schema-test
  (testing "::p.gql/gql-pathom-transient-attrs"
    (is (= (::p.gql/gql-pathom-transient-attrs schema)
           #{:acme.sw.types/human
             :acme.sw.types/Query
             :acme.sw.interfaces/character
             :acme.sw.types/droid})))

  (testing "type aux resolvers"
    (is (= (::p.gql/gql-pathom-type-resolvers schema)
           '[{:com.wsscode.pathom3.connect.operation/op-name      acme.sw/Query-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.sw.types/Query],
              :com.wsscode.pathom3.connect.operation/output       [{:acme.sw.Query/droid [:acme.sw.types/droid]}
                                                                   {:acme.sw.Query/hero [:acme.sw.interfaces/character]}
                                                                   {:acme.sw.Query/human [:acme.sw.types/human]}]}
             {:com.wsscode.pathom3.connect.operation/op-name      acme.sw/character-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.sw.interfaces/character],
              :com.wsscode.pathom3.connect.operation/output       [:acme.sw.character/appears_in
                                                                   {:acme.sw.character/friends [:acme.sw.interfaces/character]}
                                                                   :acme.sw.character/id
                                                                   :acme.sw.character/name]}
             {:com.wsscode.pathom3.connect.operation/op-name      acme.sw/droid-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.sw.types/droid],
              :com.wsscode.pathom3.connect.operation/output       [:acme.sw.interfaces/character
                                                                   :acme.sw.droid/appears_in
                                                                   {:acme.sw.droid/friends [:acme.sw.interfaces/character]}
                                                                   :acme.sw.droid/id
                                                                   :acme.sw.droid/name
                                                                   :acme.sw.droid/primary_function]}
             {:com.wsscode.pathom3.connect.operation/op-name      acme.sw/human-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.sw.types/human],
              :com.wsscode.pathom3.connect.operation/output       [:acme.sw.interfaces/character
                                                                   :acme.sw.human/appears_in
                                                                   {:acme.sw.human/friends [:acme.sw.interfaces/character]}
                                                                   :acme.sw.human/home_planet
                                                                   :acme.sw.human/id
                                                                   :acme.sw.human/name]}]))

    (testing "load async"
      (is (= (-> @(p.gql/load-schema schema-config #(p/do! (t-server/request %)))
                 ::p.gql/gql-pathom-type-resolvers)
             '[{:com.wsscode.pathom3.connect.operation/op-name      acme.sw/Query-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.sw.types/Query],
                :com.wsscode.pathom3.connect.operation/output       [{:acme.sw.Query/droid [:acme.sw.types/droid]}
                                                                     {:acme.sw.Query/hero [:acme.sw.interfaces/character]}
                                                                     {:acme.sw.Query/human [:acme.sw.types/human]}]}
               {:com.wsscode.pathom3.connect.operation/op-name      acme.sw/character-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.sw.interfaces/character],
                :com.wsscode.pathom3.connect.operation/output       [:acme.sw.character/appears_in
                                                                     {:acme.sw.character/friends [:acme.sw.interfaces/character]}
                                                                     :acme.sw.character/id
                                                                     :acme.sw.character/name]}
               {:com.wsscode.pathom3.connect.operation/op-name      acme.sw/droid-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.sw.types/droid],
                :com.wsscode.pathom3.connect.operation/output       [:acme.sw.interfaces/character
                                                                     :acme.sw.droid/appears_in
                                                                     {:acme.sw.droid/friends [:acme.sw.interfaces/character]}
                                                                     :acme.sw.droid/id
                                                                     :acme.sw.droid/name
                                                                     :acme.sw.droid/primary_function]}
               {:com.wsscode.pathom3.connect.operation/op-name      acme.sw/human-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.sw.types/human],
                :com.wsscode.pathom3.connect.operation/output       [:acme.sw.interfaces/character
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
