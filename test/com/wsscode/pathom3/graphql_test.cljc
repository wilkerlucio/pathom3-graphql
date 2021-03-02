(ns com.wsscode.pathom3.graphql-test
  (:require
    [clojure.test :refer [deftest is are run-tests testing]]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.graphql :as p.gql]
    [com.wsscode.pathom3.interface.eql :as p.eql]))

(pci/register [])

; region GraphQL demo schema

(def query-root-type
  (p.gql/normalize-schema
    {:name   "QueryRoot"
     :fields [{:name "banks" :args [] :type {:kind "LIST" :name nil :ofType {:kind "OBJECT" :name "Bank"}}}
              {:name "creditCardAccount"
               :args [{:name "customerId" :defaultValue nil :type {:kind "SCALAR" :name "ID"}}]
               :type {:kind "OBJECT" :name "CreditCardAccount" :ofType nil}}
              {:name "customer"
               :args [{:name "customerId" :defaultValue nil :type {:kind "SCALAR" :name "ID"}}]
               :type {:kind :OBJECT :name "Customer" :ofType nil}}
              {:name "repository"
               :args [{:name "owner" :defaultValue nil :type {:kind "SCALAR" :name "String"}}
                      {:name "name" :defaultValue nil :type {:kind "SCALAR" :name "String"}}]
               :type {:kind "OBJECT" :name "Repository" :ofType nil}}
              {:name "savingsAccount"
               :args [{:name "customerId" :defaultValue nil :type {:kind "SCALAR" :name "ID"}}]
               :type {:kind "OBJECT" :name "SavingsAccount" :ofType nil}}
              {:name "viewer" :args [] :type {:kind "OBJECT" :name "Customer" :ofType nil}}]}))

(def bank-type
  {:name       "Bank"
   :kind       "OBJECT"
   :interfaces []
   :fields     [{:name "id" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "ID"}}}
                {:name "name" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}]})

(def customer-type
  {:name       "Customer"
   :kind       "OBJECT"
   :interfaces []
   :fields     [{:name "id" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "ID"}}}
                {:name "cpf" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}
                {:name "creditCardAccount" :args [] :type {:kind "OBJECT" :name "CreditCardAccount" :ofType nil}}
                {:name "feed" :args [] :type {:kind "LIST" :name nil :ofType {:kind "NON_NULL" :name nil :ofType {:kind "INTERFACE" :name "FeedEvent"}}}}
                {:name "name" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}
                {:name "preferredName" :args [] :type {:kind "SCALAR" :name "String" :ofType nil}}
                {:name "savingsAccount" :args [] :type {:kind "OBJECT" :name "SavingsAccount" :ofType nil}}]})

(def credit-card-account-type
  {:name       "CreditCardAccount"
   :kind       "OBJECT"
   :interfaces []
   :fields     [{:name "id" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "ID"}}}
                {:name "number" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}]})

(def savings-card-account-type
  {:name       "SavingsAccount"
   :kind       "OBJECT"
   :interfaces []
   :fields     [{:name "id" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "ID"}}}
                {:name "number" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}]})

(def repository-type
  {:name       "Repository"
   :kind       "OBJECT"
   :interfaces []
   :fields     [{:name "id" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "ID"}}}
                {:name "name" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}]})

(def feed-event-interface
  {:name       "FeedEvent"
   :kind       "INTERFACE"
   :interfaces []
   :fields     [{:name "detail" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}
                {:name "id" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "ID"}}}
                {:name "postDate" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "Date"}}}
                {:name "title" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}]})

(def onboarding-event-type
  {:name       "OnboardingEvent"
   :kind       "OBJECT"
   :interfaces [{:name "FeedEvent" :kind "INTERFACE"}]
   :fields     [{:name "detail" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}
                {:name "id" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "ID"}}}
                {:name "postDate" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "Date"}}}
                {:name "title" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}}}]})

(def mutation-type
  {:name   "Mutation"
   :kind   "OBJECT"
   :fields [{:name "addStar"
             :args [{:name         "input"
                     :defaultValue nil
                     :type         {:kind   "NON_NULL"
                                    :name   nil
                                    :ofType {:kind   "INPUT_OBJECT"
                                             :name   "UserBlockProductInput"
                                             :ofType nil}}}]
             :type {:kind   "NON_NULL"
                    :name   nil
                    :ofType {:kind   "OBJECT"
                             :name   "Customer"
                             :ofType nil}}}
            {:name "removeStar"}
            {:name "requestReviews"}]})

(def types
  [{:name       "CreditCardBalances"
    :kind       "OBJECT"
    :interfaces []
    :fields     [{:name "available" :args [] :type {:kind "SCALAR" :name "Float" :ofType nil}}
                 {:name "due" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "Float"}}}
                 {:name "future" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "Float"}}}
                 {:name "open" :args [] :type {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "Float"}}}
                 {:name "prepaid" :args [] :type {:kind "SCALAR" :name "Float" :ofType nil}}]}
   query-root-type
   bank-type
   savings-card-account-type
   customer-type
   credit-card-account-type
   repository-type
   feed-event-interface
   onboarding-event-type
   mutation-type])

(def schema
  {:__schema
   {:queryType    {:name "QueryRoot"}
    :mutationType {:name "Mutation"}
    :types        types}})

; endregion

(def prefix "service")
(def env {::p.gql/prefix "service"})

(deftest type-key-test
  (is (= (p.gql/type-key env "CreditCardBalances")
         :service.types/CreditCardBalances)))

(deftest interface-key-test
  (is (= (p.gql/interface-key env "FeedEvent")
         :service.interfaces/FeedEvent)))

(deftest type-leaf-name-test
  (is (= (p.gql/type-leaf-name {:kind "SCALAR" :name "Float" :ofType nil})
         nil))

  (is (= (p.gql/type-leaf-name {:kind "OBJECT" :name "CreditCardAccount" :ofType nil})
         "CreditCardAccount"))

  (is (= (p.gql/type-leaf-name {:kind "NON_NULL" :name nil :ofType {:kind "OBJECT" :name "CreditCardAccount" :ofType nil}})
         "CreditCardAccount"))

  (is (= (p.gql/type-leaf-name {:kind "NON_NULL" :name nil :ofType {:kind "INTERFACE" :name "Runnable" :ofType nil}})
         "Runnable")))

(deftest type->field-name-test
  (is (= (p.gql/type->field-name env {:kind "SCALAR" :name "Float" :ofType nil})
         nil))

  (is (= (p.gql/type->field-name env {:kind "OBJECT" :name "CreditCardAccount" :ofType nil})
         :service.types/CreditCardAccount))

  (is (= (p.gql/type->field-name env {:kind "NON_NULL" :name nil :ofType {:kind "OBJECT" :name "CreditCardAccount" :ofType nil}})
         :service.types/CreditCardAccount))

  (is (= (p.gql/type->field-name env {:kind "NON_NULL" :name nil :ofType {:kind "INTERFACE" :name "Runnable" :ofType nil}})
         :service.interfaces/Runnable)))

(deftest type->field-entry-test
  (is (= (p.gql/type->field-entry env {:kind "SCALAR" :name "Float" :ofType nil})
         {}))
  (is (= (p.gql/type->field-entry env {:kind "OBJECT" :name "CreditCardAccount" :ofType nil})
         {:service.types/CreditCardAccount {}}))
  (is (= (p.gql/type->field-entry env {:kind "INTERFACE" :name "FeedEvent" :ofType nil})
         {:service.interfaces/FeedEvent {}}))
  (is (= (p.gql/type->field-entry env {:kind "NON_NULL" :name nil :ofType {:kind "SCALAR" :name "String"}})
         {}))
  (is (= (p.gql/type->field-entry env {:kind "NON_NULL" :name nil :ofType {:kind "OBJECT" :name "CreditCardAccount" :ofType nil}})
         {:service.types/CreditCardAccount {}}))
  (is (= (p.gql/type->field-entry env {:kind "LIST" :name nil :ofType {:kind "OBJECT" :name "Bank"}})
         {:service.types/Bank {}})))

(deftest entity-field-key-test
  (is (= (p.gql/entity-field-key env "Customer" "name")
         :service.Customer/name)))

(deftest index-type-test
  (is (= (p.gql/index-type env customer-type)
         {#{:service.types/Customer} #:service.Customer{:cpf               {}
                                                        :creditCardAccount #:service.types{:CreditCardAccount {}}
                                                        :feed              #:service.interfaces{:FeedEvent {}}
                                                        :id                {}
                                                        :name              {}
                                                        :preferredName     {}
                                                        :savingsAccount    #:service.types{:SavingsAccount {}}}}))

  (is (= (p.gql/index-type env feed-event-interface)
         {#{:service.interfaces/FeedEvent} #:service.FeedEvent{:detail   {}
                                                               :id       {}
                                                               :postDate {}
                                                               :title    {}}}))

  (is (= (p.gql/index-type env onboarding-event-type)
         {#{:service.types/OnboardingEvent} {:service.OnboardingEvent/detail   {}
                                             :service.OnboardingEvent/id       {}
                                             :service.OnboardingEvent/postDate {}
                                             :service.OnboardingEvent/title    {}
                                             :service.interfaces/FeedEvent     {}}})))

(def graphql-sampler-resolver nil)

(def indexes
  `{:com.wsscode.pathom.connect.graphql2/field->ident {:service.Customer/cpf               #:com.wsscode.pathom.connect.graphql2{:entity-field :service.Customer/id
                                                                                                                                 :ident-key    :customer/customerId}
                                                       :service.Customer/creditCardAccount #:com.wsscode.pathom.connect.graphql2{:entity-field :service.Customer/id
                                                                                                                                 :ident-key    :customer/customerId}
                                                       :service.Customer/feed              #:com.wsscode.pathom.connect.graphql2{:entity-field :service.Customer/id
                                                                                                                                 :ident-key    :customer/customerId}
                                                       :service.Customer/id                #:com.wsscode.pathom.connect.graphql2{:entity-field :service.Customer/id
                                                                                                                                 :ident-key    :customer/customerId}
                                                       :service.Customer/name              #:com.wsscode.pathom.connect.graphql2{:entity-field :service.Customer/id
                                                                                                                                 :ident-key    :customer/customerId}
                                                       :service.Customer/preferredName     #:com.wsscode.pathom.connect.graphql2{:entity-field :service.Customer/id
                                                                                                                                 :ident-key    :customer/customerId}
                                                       :service.Customer/savingsAccount    #:com.wsscode.pathom.connect.graphql2{:entity-field :service.Customer/id
                                                                                                                                 :ident-key    :customer/customerId}
                                                       :service.Repository/id              #:com.wsscode.pathom.connect.graphql2{:entity-field [:service.Customer/name
                                                                                                                                                :service.Repository/name]
                                                                                                                                 :ident-key    :repository/owner-and-name}
                                                       :service.Repository/name            #:com.wsscode.pathom.connect.graphql2{:entity-field [:service.Customer/name
                                                                                                                                                :service.Repository/name]
                                                                                                                                 :ident-key    :repository/owner-and-name}}
    :com.wsscode.pathom.connect/autocomplete-ignore   #{:service.interfaces/FeedEvent
                                                        :service.types/CreditCardBalances
                                                        :service.types/CreditCardAccount
                                                        :service.types/Customer
                                                        :service.types/Mutation
                                                        :service.types/OnboardingEvent
                                                        :service.types/Repository}
    :com.wsscode.pathom.connect/idents                #{:service.Customer/id}
    :com.wsscode.pathom.connect/index-io              {#{:service.Customer/id}              #:service.types{:Customer       {}
                                                                                                            :SavingsAccount {}}
                                                       #{:service.types/CreditCardAccount}  #:service.CreditCardAccount{:id     {}
                                                                                                                        :number {}}
                                                       #{:service.Customer/name
                                                         :service.Repository/name}          #:service.types{:Repository {}}
                                                       #{:service.interfaces/FeedEvent}     #:service.FeedEvent{:detail   {}
                                                                                                                :id       {}
                                                                                                                :postDate {}
                                                                                                                :title    {}}
                                                       #{:service.types/CreditCardBalances} #:service.CreditCardBalances{:available {}
                                                                                                                         :due       {}
                                                                                                                         :future    {}
                                                                                                                         :open      {}
                                                                                                                         :prepaid   {}}
                                                       #{:service.types/Customer}           #:service.Customer{:cpf               {}
                                                                                                               :creditCardAccount #:service.types{:CreditCardAccount {}}
                                                                                                               :feed              #:service.interfaces{:FeedEvent {}}
                                                                                                               :id                {}
                                                                                                               :name              {}
                                                                                                               :preferredName     {}
                                                                                                               :savingsAccount    #:service.types{:SavingsAccount {}}}
                                                       #{:service.types/Mutation}           #:service.Mutation{:addStar        #:service.types{:Customer {}}
                                                                                                               :removeStar     {}
                                                                                                               :requestReviews {}}
                                                       #{:service.types/OnboardingEvent}    {:service.OnboardingEvent/detail   {}
                                                                                             :service.OnboardingEvent/id       {}
                                                                                             :service.OnboardingEvent/postDate {}
                                                                                             :service.OnboardingEvent/title    {}
                                                                                             :service.interfaces/FeedEvent     {}}
                                                       #{:service.types/Repository}         #:service.Repository{:id   {}
                                                                                                                 :name {}}
                                                       #{}                                  #:service{:banks             #:service.types{:Bank {}}
                                                                                                      :creditCardAccount #:service.types{:CreditCardAccount {}}
                                                                                                      :customer          #:service.types{:Customer {}}
                                                                                                      :repository        #:service.types{:Repository {}}
                                                                                                      :savingsAccount    #:service.types{:SavingsAccount {}}
                                                                                                      :viewer            #:service.types{:Customer {}}}}
    :com.wsscode.pathom.connect/index-mutations       {com.wsscode.pathom.connect.graphql.service-mutations/service #:com.wsscode.pathom.connect{:sym com.wsscode.pathom.connect.graphql.service-mutations/service}
                                                       service/addStar                                              {:com.wsscode.pathom.connect.graphql2/output-type :service.types/Customer
                                                                                                                     :com.wsscode.pathom.connect/sym                  com.wsscode.pathom.connect.graphql.service-mutations/service}
                                                       service/removeStar                                           #:com.wsscode.pathom.connect{:sym com.wsscode.pathom.connect.graphql.service-mutations/service}
                                                       service/requestReviews                                       #:com.wsscode.pathom.connect{:sym com.wsscode.pathom.connect.graphql.service-mutations/service}}
    :com.wsscode.pathom.connect/index-oir             {:service.Customer/cpf               {#{:service.Customer/id} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Customer/creditCardAccount {#{:service.Customer/id} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Customer/feed              {#{:service.Customer/id} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Customer/id                {#{:service.Customer/id} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Customer/name              {#{:service.Customer/id} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Customer/preferredName     {#{:service.Customer/id} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Customer/savingsAccount    {#{:service.Customer/id} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Repository/id              {#{:service.Customer/name
                                                                                              :service.Repository/name} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service.Repository/name            {#{:service.Customer/name
                                                                                              :service.Repository/name} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service/banks                      {#{} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service/creditCardAccount          {#{} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service/customer                   {#{} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service/repository                 {#{} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service/savingsAccount             {#{} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}
                                                       :service/viewer                     {#{} #{com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}}
    :com.wsscode.pathom.connect/index-resolvers       #:com.wsscode.pathom.connect.graphql2-test{graphql-sampler-resolver {:com.wsscode.pathom.connect.graphql2/graphql? true
                                                                                                                           :com.wsscode.pathom.connect/cache?            false
                                                                                                                           :com.wsscode.pathom.connect/dynamic-resolver? true
                                                                                                                           :com.wsscode.pathom.connect/sym               com.wsscode.pathom.connect.graphql2-test/graphql-sampler-resolver}}})

(def schema-test-config
  {::p.gql/prefix    prefix
   ::p.gql/schema    (p.gql/index-schema-types schema)
   ::p.gql/ident-map {"customer"       {"customerId" :service.Customer/id}
                      "savingsAccount" {"customerId" :service.Customer/id}
                      "repository"     {"owner" :service.Customer/name
                                        "name"  :service.Repository/name}}
   ::p.gql/resolver  `graphql-sampler-resolver})

(deftest index-schema-io-test
  (is (= (p.gql/index-schema-io schema-test-config)
         {#{:service.types/CreditCardBalances}               #:service.CreditCardBalances{:available {},
                                                                                          :due       {},
                                                                                          :future    {},
                                                                                          :open      {},
                                                                                          :prepaid   {}},
          #{:service.types/Customer}                         #:service.Customer{:id                {},
                                                                                :cpf               {},
                                                                                :creditCardAccount #:service.types{:CreditCardAccount {}},
                                                                                :feed              #:service.interfaces{:FeedEvent {}},
                                                                                :name              {},
                                                                                :preferredName     {},
                                                                                :savingsAccount    #:service.types{:SavingsAccount {}}},
          #{:service.types/OnboardingEvent}                  {:service.OnboardingEvent/detail   {},
                                                              :service.OnboardingEvent/id       {},
                                                              :service.OnboardingEvent/postDate {},
                                                              :service.OnboardingEvent/title    {},
                                                              :service.interfaces/FeedEvent     {}},
          #{:service.types/Repository}                       #:service.Repository{:id {}, :name {}},
          #{:service.types/Bank}                             #:service.Bank{:id {}, :name {}},
          #{:service.types/SavingsAccount}                   #:service.SavingsAccount{:id {}, :number {}},
          #{}                                                #:service{:banks             #:service.types{:Bank {}},
                                                                       :creditCardAccount #:service.types{:CreditCardAccount {}},
                                                                       :viewer            #:service.types{:Customer {}}},
          #{:service.Customer/id}                            #:service.types{:Customer {}, :SavingsAccount {}},
          #{:service.Repository/name :service.Customer/name} #:service.types{:Repository {}},
          #{:service.interfaces/FeedEvent}                   #:service.FeedEvent{:detail   {},
                                                                                 :id       {},
                                                                                 :postDate {},
                                                                                 :title    {}},
          #{:service.types/CreditCardAccount}                #:service.CreditCardAccount{:id {}, :number {}},
          #{:service.types/Mutation}                         #:service.Mutation{:addStar        #:service.types{:Customer {}},
                                                                                :removeStar     {},
                                                                                :requestReviews {}}})))

(deftest index-aux-resolvers-test
  (is (= (->> (p.gql/index-aux-resolvers schema-test-config)
              (mapv pco/operation-config))
         '[#:com.wsscode.pathom3.connect.operation{:input        [],
                                                   :provides     #:service{:banks #:service.Bank{:id   {},
                                                                                                 :name {}}},
                                                   :op-name      service.root-entry/banks,
                                                   :dynamic-name com.wsscode.pathom3.graphql-test/graphql-sampler-resolver,
                                                   :output       [#:service{:banks [:service.Bank/id
                                                                                    :service.Bank/name]}],
                                                   :requires     {}}
           #:com.wsscode.pathom3.connect.operation{:input        [],
                                                   :provides     #:service{:creditCardAccount #:service.CreditCardAccount{:id     {},
                                                                                                                          :number {}}},
                                                   :op-name      service.root-entry/creditCardAccount,
                                                   :dynamic-name com.wsscode.pathom3.graphql-test/graphql-sampler-resolver,
                                                   :output       [#:service{:creditCardAccount [:service.CreditCardAccount/id
                                                                                                :service.CreditCardAccount/number]}],
                                                   :requires     {}}
           #:com.wsscode.pathom3.connect.operation{:input        [:service.Customer/id],
                                                   :provides     #:service.Customer{:id                {},
                                                                                    :cpf               {},
                                                                                    :creditCardAccount {},
                                                                                    :feed              {},
                                                                                    :name              {},
                                                                                    :preferredName     {},
                                                                                    :savingsAccount    {}},
                                                   :op-name      service.root-entry/customer,
                                                   :dynamic-name com.wsscode.pathom3.graphql-test/graphql-sampler-resolver,
                                                   :output       [:service.Customer/id
                                                                  :service.Customer/cpf
                                                                  :service.Customer/creditCardAccount
                                                                  :service.Customer/feed
                                                                  :service.Customer/name
                                                                  :service.Customer/preferredName
                                                                  :service.Customer/savingsAccount],
                                                   :requires     #:service.Customer{:id {}}}
           #:com.wsscode.pathom3.connect.operation{:input        [:service.Customer/name
                                                                  :service.Repository/name],
                                                   :provides     #:service.Repository{:id   {},
                                                                                      :name {}},
                                                   :op-name      service.root-entry/repository,
                                                   :dynamic-name com.wsscode.pathom3.graphql-test/graphql-sampler-resolver,
                                                   :output       [:service.Repository/id
                                                                  :service.Repository/name],
                                                   :requires     {:service.Customer/name   {},
                                                                  :service.Repository/name {}}}
           #:com.wsscode.pathom3.connect.operation{:input        [:service.Customer/id],
                                                   :provides     #:service.SavingsAccount{:id     {},
                                                                                          :number {}},
                                                   :op-name      service.root-entry/savingsAccount,
                                                   :dynamic-name com.wsscode.pathom3.graphql-test/graphql-sampler-resolver,
                                                   :output       [:service.SavingsAccount/id
                                                                  :service.SavingsAccount/number],
                                                   :requires     #:service.Customer{:id {}}}
           #:com.wsscode.pathom3.connect.operation{:input        [],
                                                   :provides     #:service{:viewer #:service.Customer{:id                {},
                                                                                                      :cpf               {},
                                                                                                      :creditCardAccount {},
                                                                                                      :feed              {},
                                                                                                      :name              {},
                                                                                                      :preferredName     {},
                                                                                                      :savingsAccount    {}}},
                                                   :op-name      service.root-entry/viewer,
                                                   :dynamic-name com.wsscode.pathom3.graphql-test/graphql-sampler-resolver,
                                                   :output       [#:service{:viewer [:service.Customer/id
                                                                                     :service.Customer/cpf
                                                                                     :service.Customer/creditCardAccount
                                                                                     :service.Customer/feed
                                                                                     :service.Customer/name
                                                                                     :service.Customer/preferredName
                                                                                     :service.Customer/savingsAccount]}],
                                                   :requires     {}}])))

(comment
  (p.gql/index-schema-io schema-test-config)

  (let [env (p.gql/index-schema
              {::p.gql/prefix    prefix
               ::p.gql/schema    (p.gql/index-schema-types schema)
               ::p.gql/ident-map {"customer"       {"customerId" :service.Customer/id}
                                  "savingsAccount" {"customerId" :service.Customer/id}
                                  "repository"     {"owner" :service.Customer/name
                                                    "name"  :service.Repository/name}}
               ::p.gql/resolver  `graphql-sampler-resolver})]
    (meta (p.eql/process env
            {:service.Customer/id 123}
            [:service.Customer/name
             :service.SavingsAccount/number])))

  (p.gql/index-schema schema-test-config)

  (-> (p.gql/index-schema-types schema)
      :__schema)

  (p.gql/index-graphql-idents schema-test-config))

(deftest index-schema-test
  (let [{::pci/keys [index-resolvers]
         :as        indexes} (p.gql/index-schema schema-test-config)]
    (is (= (pco/operation-config (get index-resolvers `graphql-sampler-resolver))
           '{:com.wsscode.pathom3.connect.operation/cache?            false
             :com.wsscode.pathom3.connect.operation/dynamic-resolver? true
             :com.wsscode.pathom3.connect.operation/op-name           com.wsscode.pathom3.graphql-test/graphql-sampler-resolver
             :com.wsscode.pathom3.graphql/graphql?                    true
             ::p.gql/field->ident                                     {:service.Customer/preferredName     #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :customer/customerId},
                                                                       :service.SavingsAccount/id          #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :savingsAccount/customerId},
                                                                       :service.Customer/creditCardAccount #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :customer/customerId},
                                                                       :service.Repository/name            #:com.wsscode.pathom3.graphql{:entity-field [:service.Customer/name
                                                                                                                                                        :service.Repository/name],
                                                                                                                                         :ident-key    :repository/owner-and-name},
                                                                       :service.Customer/cpf               #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :customer/customerId},
                                                                       :service.Repository/id              #:com.wsscode.pathom3.graphql{:entity-field [:service.Customer/name
                                                                                                                                                        :service.Repository/name],
                                                                                                                                         :ident-key    :repository/owner-and-name},
                                                                       :service.Customer/name              #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :customer/customerId},
                                                                       :service.Customer/savingsAccount    #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :customer/customerId},
                                                                       :service.SavingsAccount/number      #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :savingsAccount/customerId},
                                                                       :service.Customer/feed              #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :customer/customerId},
                                                                       :service.Customer/id                #:com.wsscode.pathom3.graphql{:entity-field :service.Customer/id,
                                                                                                                                         :ident-key    :customer/customerId}}}))

    (is (= (get indexes :com.wsscode.pathom.viz.query-editor/autocomplete-ignore)
           #{:service.interfaces/FeedEvent
             :service.types/Bank
             :service.types/CreditCardAccount
             :service.types/CreditCardBalances
             :service.types/Customer
             :service.types/Mutation
             :service.types/OnboardingEvent
             :service.types/Repository
             :service.types/SavingsAccount}))))
