(ns com.wsscode.eql.graphql-test
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is are run-tests testing]]
    [com.wsscode.eql.graphql :as eql.gql])
  #?(:clj
     (:import
       (java.util
         UUID))))

(defn query->graphql [query]
  (-> (eql.gql/eql->graphql query)
      (str/replace #"\s+" " ")
      (str/trim)))

(defn uuid* [s]
  #?(:clj  (UUID/fromString s)
     :cljs (uuid s)))

(deftest test-query->graphql
  (testing "properties"
    (is (= (query->graphql []) "query { }"))
    (is (= (query->graphql [:property]) "query { property }"))
    (is (= (query->graphql [:qualified/property]) "query { property }")))

  (testing "on"
    '[:hello (:other {::eql.gql/on "User"})] "query { hello ... on User { other } }")

  (testing "params"
    (is (= (query->graphql '[(:parameterized {:foo "bar"})]) "query { parameterized(foo: \"bar\") }"))

    (is (= (query->graphql '[(:parameterized {:foo [a b]})]) "query { parameterized(foo: [a, b]) }"))

    (is (= (query->graphql `[(:parameterized {:foo ~(uuid* "ead34300-0ef6-4c31-9626-90bf18fa22c0")})])
           "query { parameterized(foo: \"ead34300-0ef6-4c31-9626-90bf18fa22c0\") }")))

  (testing "aliasing"
    (is (= (query->graphql '[(:property {::eql.gql/alias "aliased"})]) "query { aliased: property }"))
    (is (= (query->graphql '[{(:property {::eql.gql/alias "aliased" :another "param"})
                              [:subquery]}]) "query { aliased: property(another: \"param\") { subquery } }")))

  (testing "ident"
    (is (= (query->graphql [{[:Item/id 123] [:id :name]}])
           "query { _Item_id_123: Item(id: 123) { id name } }"))

    (is (= (query->graphql [{[:service.Item/id 123] [:id :name]}])
           "query { _service_Item_id_123: Item(id: 123) { id name } }"))

    (is (= (query->graphql [{[:item/name-and-owner ["NAM" "OWN"]] [:id :name]}])
           "query { _item_name_and_owner_NAM_OWN: item(name: \"NAM\", owner: \"OWN\") { id name } }"))

    (is (= (query->graphql [{[:Item/slug "some-post"] [:id :slug]}])
           "query { _Item_slug_some_post: Item(slug: \"some-post\") { id slug } }"))

    (is (= (query->graphql [{[:Item/id "123,45"] [:id :name]}])
           "query { _Item_id_123_45: Item(id: \"123,45\") { id name } }"))

    (is (= (query->graphql [{[:Item/id 123] [:id :name]}
                            {[:Item/id 321] [:id :name]}])
           "query { _Item_id_123: Item(id: 123) { id name } _Item_id_321: Item(id: 321) { id name } }"))

    (is (= (query->graphql '[({[:Item/id 123] [:id :name]} {:name "bla"})])
           "query { _Item_id_123: Item(id: 123, name: \"bla\") { id name } }")))

  (testing "joins"
    (is (= (query->graphql [{:all-items [:id :name]}])
           "query { all-items { id name } }"))

    (is (= (query->graphql '[{:all-items [:hello
                                          (:other {::eql.gql/on "User"})
                                          (:foo {::eql.gql/on "User"})
                                          (:location {::eql.gql/on "Place"})]}])
           "query { all-items { hello ... on User { other foo } ... on Place { location } } }"))

    (is (= (query->graphql '[({:nodes [:id :user/name]} {:last 10})])
           "query { nodes(last: 10) { id name } }")))

  (testing "union queries"
    (is (= (query->graphql [{:search
                             {:User  [:username]
                              :Movie [:director]
                              :Book  [:author]}}])
           "query { search { __typename ... on User { username } ... on Movie { director } ... on Book { author } } }")))

  (testing "recursive queries"
    (is (= (query->graphql [:id {:parent 3}])
           "query { id parent { id parent { id parent { id } } } }"))

    (is (= (query->graphql [:id {:parent '...}])
           "query { id parent { id parent { id parent { id parent { id parent { id } } } } } }")))

  (testing "mutations"
    (is (= (query->graphql '[(call {:param "value"})])
           "mutation { call(param: \"value\")}"))

    (is (= (query->graphql '[(call {:enum HEY})])
           "mutation { call(enum: HEY)}"))

    (is (= (query->graphql '[{(call {:param "value" :item/value 42}) [:id :foo]}])
           "mutation { call(param: \"value\", value: 42) { id foo } }"))

    (is (= (query->graphql '[{(call {:param "value" :item/value 42}) [*]}])
           "mutation { call(param: \"value\", value: 42)}"))

    (is (= (query->graphql '[(call {:param {:nested "value"}})])
           "mutation { call(param: {nested: \"value\"})}"))

    (is (= (query->graphql '[(call {:param "value" :item/value 42 ::eql.gql/mutate-join [:id :foo]})])
           "mutation { call(param: \"value\", value: 42) { id foo } }"))

    (is (= (query->graphql '[{(call {:param "value" :item/value 42}) [:id :foo (:other {::eql.gql/on "User"})]}])
           "mutation { call(param: \"value\", value: 42) { id foo ... on User { other } } }"))))

(comment
  (query->graphql '[(:property {::eql.gql/alias "aliased"})])
  (query->graphql '[{(:property {::eql.gql/alias "aliased" :another "param"})
                     [:subquery]}])

  (-> '[{(call {:param "value" :item/value 42}) [*]}]
      (eql.gql/eql->graphql {::eql.gql/tempid? fp/tempid?})
      (str/replace #"\s+" " ")
      (str/trim))

  (-> '[{:app/timeline
         [:entity/id
          (:user/name {::eql.gql/on :app/User})
          {(:activity/user {::eql.gql/on :app/User})
           [:user/name]}]}]
      (eql.gql/eql->graphql)
      (println))

  (-> (eql.gql/eql->graphql [{:search
                              ^{::eql.gql/union-query [:__typename]}
                              {:User  [:username]
                               :Movie [:director]
                               :Book  [:author]}}])

      (println))

  (-> (eql.gql/eql->graphql [{[:customer/customer-id "123"]
                              [:stormshield.customer/cpf]}])

      (println))

  (-> (fp/query->ast [{:search
                       ^{::eql.gql/union-query [:__typename]}
                       {:User  [:username]
                        :Movie [:director]
                        :Book  [:author]}}]))

  (fp/query->ast [{:search
                   {:User  [:username]
                    :Movie [:director]
                    :Book  [:author]}}])
  (fp/ast->query (fp/query->ast '[{(call {:param "value" :item/value 42}) [:id :foo]}]))
  (eql.gql/eql->graphql `[(call {:id ~(fp/tempid) :param "value"})]))
