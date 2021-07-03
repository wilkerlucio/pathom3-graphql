(ns com.wsscode.pathom3.graphql-test
  (:require
    [clojure.test :refer [deftest is are run-tests testing]]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.graphql.test.server :as t-server]
    [promesa.core :as p]))

(def schema-config
  {::p.gql/namespace "acme.stars"})

(defn load-schema []
  (p.gql/load-schema schema-config t-server/request))

(def schema (load-schema))

(deftest index-schema-test
  (testing "::p.gql/gql-pathom-transient-attrs"
    (is (= (::p.gql/gql-pathom-transient-attrs schema)
           #{:acme.stars.types/human
             :acme.stars.types/Query
             :acme.stars.interfaces/character
             :acme.stars.types/droid})))

  (testing "type aux resolvers"
    (is (= (::p.gql/gql-pathom-type-resolvers schema)
           '[{:com.wsscode.pathom3.connect.operation/op-name      acme.stars/Query-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.stars.types/Query],
              :com.wsscode.pathom3.connect.operation/output       [{:acme.stars.Query/droid [:acme.stars.types/droid]}
                                                                   {:acme.stars.Query/hero [:acme.stars.interfaces/character]}
                                                                   {:acme.stars.Query/human [:acme.stars.types/human]}]}
             {:com.wsscode.pathom3.connect.operation/op-name      acme.stars/character-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.stars.interfaces/character],
              :com.wsscode.pathom3.connect.operation/output       [:acme.stars.character/appears_in
                                                                   {:acme.stars.character/friends [:acme.stars.interfaces/character]}
                                                                   :acme.stars.character/id
                                                                   :acme.stars.character/name]}
             {:com.wsscode.pathom3.connect.operation/op-name      acme.stars/droid-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.stars.types/droid],
              :com.wsscode.pathom3.connect.operation/output       [:acme.stars.interfaces/character
                                                                   :acme.stars.droid/appears_in
                                                                   {:acme.stars.droid/friends [:acme.stars.interfaces/character]}
                                                                   :acme.stars.droid/id
                                                                   :acme.stars.droid/name
                                                                   :acme.stars.droid/primary_function]}
             {:com.wsscode.pathom3.connect.operation/op-name      acme.stars/human-resolver,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/input        [:acme.stars.types/human],
              :com.wsscode.pathom3.connect.operation/output       [:acme.stars.interfaces/character
                                                                   :acme.stars.human/appears_in
                                                                   {:acme.stars.human/friends [:acme.stars.interfaces/character]}
                                                                   :acme.stars.human/home_planet
                                                                   :acme.stars.human/id
                                                                   :acme.stars.human/name]}]))

    (testing "load async"
      (is (= (-> @(p.gql/load-schema schema-config #(p/do! (t-server/request %)))
                 ::p.gql/gql-pathom-type-resolvers)
             '[{:com.wsscode.pathom3.connect.operation/op-name      acme.stars/Query-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.stars.types/Query],
                :com.wsscode.pathom3.connect.operation/output       [{:acme.stars.Query/droid [:acme.stars.types/droid]}
                                                                     {:acme.stars.Query/hero [:acme.stars.interfaces/character]}
                                                                     {:acme.stars.Query/human [:acme.stars.types/human]}]}
               {:com.wsscode.pathom3.connect.operation/op-name      acme.stars/character-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.stars.interfaces/character],
                :com.wsscode.pathom3.connect.operation/output       [:acme.stars.character/appears_in
                                                                     {:acme.stars.character/friends [:acme.stars.interfaces/character]}
                                                                     :acme.stars.character/id
                                                                     :acme.stars.character/name]}
               {:com.wsscode.pathom3.connect.operation/op-name      acme.stars/droid-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.stars.types/droid],
                :com.wsscode.pathom3.connect.operation/output       [:acme.stars.interfaces/character
                                                                     :acme.stars.droid/appears_in
                                                                     {:acme.stars.droid/friends [:acme.stars.interfaces/character]}
                                                                     :acme.stars.droid/id
                                                                     :acme.stars.droid/name
                                                                     :acme.stars.droid/primary_function]}
               {:com.wsscode.pathom3.connect.operation/op-name      acme.stars/human-resolver,
                :com.wsscode.pathom3.connect.operation/dynamic-name acme.stars/pathom-entry-dynamic-resolver,
                :com.wsscode.pathom3.connect.operation/input        [:acme.stars.types/human],
                :com.wsscode.pathom3.connect.operation/output       [:acme.stars.interfaces/character
                                                                     :acme.stars.human/appears_in
                                                                     {:acme.stars.human/friends [:acme.stars.interfaces/character]}
                                                                     :acme.stars.human/home_planet
                                                                     :acme.stars.human/id
                                                                     :acme.stars.human/name]}])))))

