(ns br.com.souenzzo.resteql-test
  (:require [clojure.test :refer [deftest is testing]]
            [br.com.souenzzo.resteql :as resteql]
            [io.pedestal.http :as http]
            [io.pedestal.test :refer [response-for]]
            [br.com.souenzzo.eql-as.alpha :as eql-as]
            [clojure.java.io :as io]
            [datascript.core :as ds]
            [spec-coerce.core :as sc]
            [clojure.data.json :as json]
            [clojure.instant :as inst]
            [edn-query-language.core :as eql]
            [camel-snake-kebab.core :as csk]
            [com.wsscode.pathom.core :as p]
            [clojure.spec.alpha :as s]
            [com.wsscode.pathom.connect :as pc])
  (:import (java.util Date)))

(defmethod sc/sym->coercer `inst?
  [_]
  (fn [x]
    (try
      (inst/read-instant-date x)
      (catch Throwable _
        (sc/parse-inst x)))))

(defn query->http
  [service-fn {::resteql/keys [rest->eql open-api]} query]
  (let [op->input (resteql/index-by ::resteql/mutation-sym rest->eql)
        {:keys [dispatch-key params]} (eql/query->ast1 query)
        {::resteql/keys [operation-id
                         request->params
                         params-as returning-as]} (get op->input dispatch-key)
        {::keys [path method]
         :strs  [parameters]
         :as    spec} (first (for [[path spec] (get open-api "paths")
                                   [method spec] spec
                                   :when (= operation-id (get spec "operationId"))]
                               (assoc spec
                                 ::path path ::method method)))
        params (p/map-select
                 params
                 (eql-as/as-query {::eql-as/as-map params-as
                                   ::eql-as/as-key :pathom/as}))
        qualify-query (eql-as/as-query {::eql-as/as-key :pathom/as
                                        ::eql-as/as-map returning-as})]
    (-> (response-for service-fn (keyword method) path :headers {"Content-Type" "application/json"}
                      :body (json/write-str params))
        :body
        (json/read-str :key-fn keyword)
        (p/map-select qualify-query)
        (resteql/merge-in request->params)
        (sc/coerce-structure))))

(s/def :conduit.article/created-at inst?)
(s/def :conduit.article/updated-at inst?)


(def conduit-register
  [(pc/mutation `create-article
                {::pc/params [:conduit.article/body
                              :conduit.article/title
                              :conduit.article/description
                              :conduit.article/tag-list]
                 ::pc/output [:conduit.article/slug]}
                (fn [{::keys [conn]} {:conduit.article/keys [body title description tag-list]}]
                  (let [slug (csk/->kebab-case-string title)
                        {:keys [db-after]} @(ds/transact conn [{:conduit.article/body        body
                                                                :conduit.article/slug        slug
                                                                :conduit.article/title       title
                                                                :conduit.article/author      [:conduit.user/username "username"]
                                                                :conduit.article/updated-at  #inst"2019"
                                                                :conduit.article/created-at  #inst"2019"
                                                                :conduit.article/description description
                                                                :conduit.article/tag-list    tag-list}])]
                    {::db                  db-after
                     :conduit.article/slug slug})))
   (pc/mutation `create-user
                {::pc/params [:conduit.user/username
                              :conduit.user/password
                              :conduit.user/email]
                 ::pc/output [:conduit.user/username]}
                (fn [{::keys [conn]} {:conduit.user/keys [email username password]}]
                  (let [{:keys [db-after]} @(ds/transact conn [{:conduit.user/username username
                                                                :conduit.user/password password
                                                                :conduit.user/email    email}])]
                    {::db                   db-after
                     :conduit.user/username username})))

   (pc/constantly-resolver :conduit.article/favorites-count 0)
   (pc/constantly-resolver :conduit.article/favorited false)
   (pc/resolver `pull-article
                {::pc/output [:conduit.article/description
                              :conduit.article/slug
                              :conduit.article/updated-at
                              :conduit.article/created-at
                              :conduit.article/title
                              {:conduit.article/author [:conduit.user/username]}
                              :conduit.article/body
                              :conduit.article/tag-list]
                 ::pc/input  #{:conduit.article/slug
                               ::db}}
                (fn [_ {:conduit.article/keys [slug]
                        ::keys                [db]}]
                  (-> (ds/pull db
                               [:conduit.article/description
                                :conduit.article/slug
                                :conduit.article/updated-at
                                :conduit.article/created-at
                                :conduit.article/title
                                {:conduit.article/author [:conduit.user/username]}
                                :conduit.article/body
                                :conduit.article/tag-list]
                               [:conduit.article/slug slug])
                      (assoc-in [:conduit.article/author ::db] db)
                      (update :conduit.article/tag-list sort))))
   (pc/resolver `pull-profile
                {::pc/output [:conduit.user/bio
                              :conduit.user/email
                              :conduit.user/following
                              :conduit.user/image]
                 ::pc/input  #{:conduit.user/username
                               ::db}}
                (fn [_ {:conduit.user/keys [username]
                        ::keys             [db]}]
                  (ds/pull db
                           [:conduit.user/bio
                            :conduit.user/email
                            :conduit.user/following
                            :conduit.user/image]
                           [:conduit.user/username username])))])

(def conduit-schema
  {:conduit.article/author   {:db/valueType :db.type/ref}
   :conduit.user/username    {:db/unique :db.unique/identity}
   :conduit.article/slug     {:db/unique :db.unique/identity}
   :conduit.article/tag-list {:db/cardinality :db.cardinality/many}})


(def conduit-resteql
  [{::resteql/operation-id    "CreateArticle"
    ::resteql/mutation-sym    `create-article
    ::resteql/params-as       {:>/article [:article {:conduit.article/body        :body
                                                     :conduit.article/title       :title
                                                     :conduit.article/description :description
                                                     :conduit.article/tag-list    :tagList}]}
    ::resteql/request->params [[:>/article]]

    ::resteql/returning-as    {:article [:>/article {:author         [:conduit.article/author {:bio       :conduit.user/bio,
                                                                                               :following :conduit.user/following,
                                                                                               :image     :conduit.user/image,
                                                                                               :username  :conduit.user/username}]
                                                     :body           :conduit.article/body
                                                     :createdAt      :conduit.article/created-at
                                                     :description    :conduit.article/description
                                                     :favorited      :conduit.article/favorited
                                                     :favoritesCount :conduit.article/favorites-count
                                                     :slug           :conduit.article/slug
                                                     :tagList        :conduit.article/tag-list
                                                     :title          :conduit.article/title
                                                     :updatedAt      :conduit.article/updated-at}]}}
   {::resteql/operation-id    "CreateUser"
    ::resteql/mutation-sym    `create-user
    ::resteql/request->params [[:>/user]]
    ::resteql/params-as       {:>/user [:user {:conduit.user/username :username
                                               :conduit.user/password :password
                                               :conduit.user/email    :email}]}

    ::resteql/returning-as    {:user [:>/user {:username :conduit.user/username
                                               :email    :conduit.user/email}]}}])


(deftest resteql
  (let [open-api (-> "br/com/souenzzo/resteql/conduit_swagger.json"
                     io/resource
                     io/reader
                     json/read)
        conn (ds/create-conn conduit-schema)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/register conduit-register})
                                       p/elide-special-outputs-plugin]
                          ::p/mutate  pc/mutate
                          ::p/env     {::conn                   conn
                                       ::p/reader               [p/map-reader
                                                                 pc/reader2
                                                                 p/env-placeholder-reader]
                                       ::p/placeholder-prefixes #{">"}}})
        service-fn (-> {::http/routes (-> {::resteql/open-api  open-api
                                           ::resteql/parser    parser
                                           ::resteql/rest->eql conduit-resteql}
                                          resteql/->routes)}
                       http/default-interceptors
                       http/dev-interceptors
                       http/create-servlet
                       ::http/service-fn)
        parser! (partial query->http service-fn {::resteql/open-api  open-api
                                                 ::resteql/rest->eql conduit-resteql})]
    (is (= (parser! `[(create-user {:conduit.user/username "username"
                                    :conduit.user/password "password"
                                    :conduit.user/email    "email"})])
           {:conduit.user/email    "email"
            :conduit.user/username "username"}))
    (is (= (parser! `[(create-article {:conduit.article/body        "body"
                                       :conduit.article/title       "title"
                                       :conduit.article/description "description"
                                       :conduit.article/tag-list    ["tag" "list"]})])
           {:conduit.article/author          {:conduit.user/username  "username"}
            :conduit.article/body            "body"
            :conduit.article/created-at      #inst "2019-01-01"
            :conduit.article/description     "description"
            :conduit.article/favorited       false
            :conduit.article/favorites-count 0
            :conduit.article/slug            "title"
            :conduit.article/tag-list        ["list" "tag"]
            :conduit.article/title           "title"
            :conduit.article/updated-at      #inst "2019-01-01"}))))
