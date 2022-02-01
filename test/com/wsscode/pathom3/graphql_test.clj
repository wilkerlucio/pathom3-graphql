(ns com.wsscode.pathom3.graphql-test
  (:require
    [clojure.test :refer [deftest is are run-tests testing]]
    [com.wsscode.misc.coll :as coll]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.format.eql :as pf.eql]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.graphql.test.server :as t-server]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.interface.smart-map :as psm]
    [edn-query-language.core :as eql]
    [promesa.core :as p]))

(def schema-config
  {::p.gql/namespace "acme.sw"})

(def schema (p.gql/load-schema schema-config t-server/request))

(deftest extract-marked-paths-test
  (is (= (p.gql/extract-marked-paths
           [:foo])
         {}))

  (is (= (p.gql/extract-marked-paths
           [{:foo [:bar]}])
         {}))

  (is (= (p.gql/extract-marked-paths
           [{:foo [(p.gql/<< :bar)]}])
         {:bar [:foo :bar]}))

  (is (thrown-with-msg?
        Throwable
        #"Duplicated alias :bar"
        (p.gql/extract-marked-paths
          [{:foo [(p.gql/<< :bar)]}
           {:baz [(p.gql/<< :bar)]}])))

  (is (= (p.gql/extract-marked-paths
           [{:foo [(p.gql/<< :bar :b1)]}
            {:baz [(p.gql/<< :bar :b2)]}])
         {:b1 [:foo :bar], :b2 [:baz :bar]})))

(deftest pull-nested-data-test
  (is (= (p.gql/pull-nested-data {:foo {:bar "baz"}}
                                 {:b [:foo :bar]})
         {:foo {:bar "baz"}
          :b   "baz"}))

  (is (= (p.gql/pull-nested-data {:foo {:bar "baz"}}
                                 {:b [:foo :bar :baz :other :vai]})
         {:foo {:bar "baz"}
          :b   nil}))

  (is (= (p.gql/pull-nested-data {:foo [{:bar "baz"}
                                        {:bar "baz2"}]}
                                 {:b [:foo :bar]})
         {:foo [{:bar "baz"}
                {:bar "baz2"}]
          :b   ["baz" "baz2"]}))

  (is (= (p.gql/pull-nested-data {:foo [{:bar [{:baz "baz"}
                                               {:baz "baz2"}]}
                                        {:bar [{:baz "baz2"}
                                               {:baz "baz3"}]}]}
                                 {:b [:foo :bar :baz]})
         {:foo [{:bar [{:baz "baz"} {:baz "baz2"}]} {:bar [{:baz "baz2"} {:baz "baz3"}]}],
          :b   [["baz" "baz2"] ["baz2" "baz3"]]})))

(defn map-children-query [f query]
  (->> (eql/query->ast query)
       (p.gql/map-children f)
       (eql/ast->query)))

(deftest map-children-test
  (is (= (map-children-query #(coll/update-if % :children conj (pf.eql/prop :test))
                             [:foo {:bar [:baz]}])
         [:foo {:bar [:baz :test]} :test]))

  (testing "unions"
    (is (= (map-children-query #(coll/update-if % :children conj (pf.eql/prop :test))
                               [:foo {:bar {:a [:baz]
                                            :b [:other]}}])
           [:foo
            {:bar {:a [:baz :test]
                   :b [:other :test]}}
            :test]))))

(deftest index-schema-test
  (testing "::p.gql/gql-mutation-type"
    (is (= (p.eql/process-one schema {::p.gql/gql-mutation-type
                                      [::p.gql/gql-mutation-type-qualified-name]})
           {:com.wsscode.pathom3.graphql/gql-mutation-type-qualified-name :acme.sw.types/Mutation})))

  (testing "::p.gql/gql-interface-usages-index"
    (is (= (p.eql/process-one schema ::p.gql/gql-interface-usages-index)
           {:acme.sw.interfaces/character #{:acme.sw.types/human
                                            :acme.sw.types/droid}})))

  (testing "::p.gql/gql-interface-usages"
    (is (= (p.eql/process-one schema {::p.gql/gql-type-name "character"} ::p.gql/gql-interface-usages)
           #{:acme.sw.types/droid :acme.sw.types/human})))

  (testing "::p.gql/gql-type-resolver-output"
    (is (= (p.eql/process-one schema {::p.gql/gql-type-name "human"}
                              ::p.gql/gql-type-resolver-output)
           [:acme.sw.human/appears_in
            {:acme.sw.human/friends [:acme.sw.interfaces/character]}
            :acme.sw.human/home_planet
            :acme.sw.human/id
            :acme.sw.human/name]))

    (testing "interface include links to types"
      (is (= (p.eql/process-one schema {::p.gql/gql-type-name "character"}
                                ::p.gql/gql-type-resolver-output)
             [:acme.sw.types/droid
              :acme.sw.types/human
              :acme.sw.character/appears_in
              {:acme.sw.character/friends [:acme.sw.interfaces/character]}
              :acme.sw.character/id
              :acme.sw.character/name]))))

  (testing "::p.gql/gql-pathom-transient-attrs"
    (is (= (p.eql/process-one schema ::p.gql/gql-pathom-transient-attrs)
           #{:acme.sw.types/human
             :acme.sw.types/Query
             :acme.sw.types/Mutation
             :acme.sw.interfaces/character
             :acme.sw.types/droid})))

  (testing "::p.gql/gql-pathom-mutations"
    (is (= (p.eql/process-one schema ::p.gql/gql-pathom-mutations)
           '[{:com.wsscode.pathom3.connect.operation/op-name      acme.sw.Mutation/create_human,
              :com.wsscode.pathom3.connect.operation/dynamic-name acme.sw/pathom-entry-dynamic-resolver,
              :com.wsscode.pathom3.connect.operation/output       [:acme.sw.types/human]}])))

  (testing "::p.gql/gql-inferred-ident-map"
    (is (= (p.eql/process-one schema ::p.gql/gql-inferred-ident-map)
           {"human" {"id" ["human" "id"]}
            "droid" {"id" ["droid" "id"]}})))

  (testing "type aux resolvers"
    (is (= (p.eql/process-one schema ::p.gql/gql-pathom-indexable-type-resolvers)
           '[{::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.types/Query]
              ::pco/op-name      acme.sw/Query-resolver
              ::pco/output       [{:acme.sw.Query/allHumans [:acme.sw.types/human]}
                                  {:acme.sw.Query/droid [:acme.sw.types/droid]}
                                  {:acme.sw.Query/hero [:acme.sw.interfaces/character]}
                                  {:acme.sw.Query/human [:acme.sw.types/human]}]}
             {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.interfaces/character]
              ::pco/op-name      acme.sw/character-resolver
              ::pco/output       [:acme.sw.types/droid
                                  :acme.sw.types/human
                                  :acme.sw.character/appears_in
                                  {:acme.sw.character/friends [:acme.sw.interfaces/character]}
                                  :acme.sw.character/id
                                  :acme.sw.character/name]}
             {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.types/droid]
              ::pco/op-name      acme.sw/droid-resolver
              ::pco/output       [:acme.sw.droid/appears_in
                                  {:acme.sw.droid/friends [:acme.sw.interfaces/character]}
                                  :acme.sw.droid/id
                                  :acme.sw.droid/name
                                  :acme.sw.droid/primary_function]}
             {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
              ::pco/input        [:acme.sw.types/human]
              ::pco/op-name      acme.sw/human-resolver
              ::pco/output       [:acme.sw.human/appears_in
                                  {:acme.sw.human/friends [:acme.sw.interfaces/character]}
                                  :acme.sw.human/home_planet
                                  :acme.sw.human/id
                                  :acme.sw.human/name]}]))

    (testing "load async"
      (is (= (-> @(p.gql/load-schema schema-config #(p/do! (t-server/request %)))
                 psm/smart-map
                 ::p.gql/gql-pathom-indexable-type-resolvers)
             '[{::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.types/Query]
                ::pco/op-name      acme.sw/Query-resolver
                ::pco/output       [{:acme.sw.Query/allHumans [:acme.sw.types/human]}
                                    {:acme.sw.Query/droid [:acme.sw.types/droid]}
                                    {:acme.sw.Query/hero [:acme.sw.interfaces/character]}
                                    {:acme.sw.Query/human [:acme.sw.types/human]}]}
               {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.interfaces/character]
                ::pco/op-name      acme.sw/character-resolver
                ::pco/output       [:acme.sw.types/droid
                                    :acme.sw.types/human
                                    :acme.sw.character/appears_in
                                    {:acme.sw.character/friends [:acme.sw.interfaces/character]}
                                    :acme.sw.character/id
                                    :acme.sw.character/name]}
               {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.types/droid]
                ::pco/op-name      acme.sw/droid-resolver
                ::pco/output       [:acme.sw.droid/appears_in
                                    {:acme.sw.droid/friends [:acme.sw.interfaces/character]}
                                    :acme.sw.droid/id
                                    :acme.sw.droid/name
                                    :acme.sw.droid/primary_function]}
               {::pco/dynamic-name acme.sw/pathom-entry-dynamic-resolver
                ::pco/input        [:acme.sw.types/human]
                ::pco/op-name      acme.sw/human-resolver
                ::pco/output       [:acme.sw.human/appears_in
                                    {:acme.sw.human/friends [:acme.sw.interfaces/character]}
                                    :acme.sw.human/home_planet
                                    :acme.sw.human/id
                                    :acme.sw.human/name]}])))))

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

  #_
  (testing "call mutation"
    (is (= (p.eql/process
             gql-env
             [{'(acme.sw.Mutation/create_human {:name "Human"})
               [:acme.sw.human/id
                :acme.sw.human/name]}])
           '{acme.sw.Mutation/create_human {:acme.sw.human/id "3000", :acme.sw.human/name "Human"}}))))

(comment

  (tap> (p.eql/process-one schema ::p.gql/gql-pathom-indexes))
  (p.eql/process-one schema ::p.gql/gql-types-index)
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

  (-> schema
   ((requiring-resolve 'com.wsscode.pathom.viz.ws-connector.pathom3/connect-env)
    "debug"))

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
