(ns br.com.souenzzo.resteql-test
  (:require [clojure.test :refer [deftest is testing]]
            [br.com.souenzzo.resteql :as resteql]
            [io.pedestal.http :as http]
            [io.pedestal.test :refer [response-for]]
            [br.com.souenzzo.eql-as.alpha :as eql-as]
            [clojure.java.io :as io]
            [spec-coerce.core :as sc]
            [clojure.data.json :as json]
            [edn-query-language.core :as eql]
            [com.wsscode.pathom.core :as p]
            [clojure.spec.alpha :as s]))

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


(deftest resteql
  (let [open-api (-> "br/com/souenzzo/resteql/conduit_swagger.json"
                     io/resource
                     io/reader
                     json/read)
        rest->eql [{::resteql/operation-id    "CreateArticle"
                    ::resteql/mutation-sym    `create-article
                    ::resteql/params-as       {:>/article [:article {:conduit.article/body        :body
                                                                     :conduit.article/title       :title
                                                                     :conduit.article/description :description
                                                                     :conduit.article/tag-list    :tagList}]}
                    ::resteql/request->params [[:>/article]]

                    ::resteql/returning-as    {:article [:>/article {:author         [:conduit.article/author {:bio       :conduit.profile/bio,
                                                                                                               :following :conduit.profile/following,
                                                                                                               :image     :conduit.profile/image,
                                                                                                               :username  :conduit.profile/username}]
                                                                     :body           :conduit.article/body
                                                                     :createdAt      :conduit.article/created-at
                                                                     :description    :conduit.article/description
                                                                     :favorited      :conduit.article/favorited
                                                                     :favoritesCount :conduit.article/favorites-count
                                                                     :slug           :conduit.article/slug
                                                                     :tagList        :conduit.article/tag-list
                                                                     :title          :conduit.article/title
                                                                     :updatedAt      :conduit.article/updated-at}]}}]
        service-fn (-> {::http/routes (-> {::resteql/open-api  open-api
                                           ::resteql/parser    (fn [env query]
                                                                 `{create-article {:article {:author         {:bio       "bio"
                                                                                                              :following "following"
                                                                                                              :image     "image"
                                                                                                              :username  "username"}
                                                                                             :body           "body"
                                                                                             :createdAt      #inst"2019"
                                                                                             :description    "description"
                                                                                             :favorited      false
                                                                                             :favoritesCount 0
                                                                                             :slug           "slug"
                                                                                             :tagList        ["tag" "list"]
                                                                                             :title          "title"
                                                                                             :updatedAt      #inst"2019"}}})
                                           ::resteql/rest->eql rest->eql}
                                          resteql/->routes
                                          (doto clojure.pprint/pprint))}
                       http/default-interceptors
                       http/dev-interceptors
                       http/create-servlet
                       ::http/service-fn)]
    (is (= (query->http service-fn {::resteql/open-api  open-api
                                    ::resteql/rest->eql rest->eql}
                        `[(create-article {:conduit.article/body        "body"
                                           :conduit.article/title       "title"
                                           :conduit.article/description "description"
                                           :conduit.article/tag-list    ["tag" "list"]})])
           {:conduit.article/author          {:conduit.profile/bio       "bio"
                                              :conduit.profile/following "following"
                                              :conduit.profile/image     "image"
                                              :conduit.profile/username  "username"}
            :conduit.article/body            "body"
            :conduit.article/created-at      #inst "2019-01-01"
            :conduit.article/description     "description"
            :conduit.article/favorited       false
            :conduit.article/favorites-count 0
            :conduit.article/slug            "slug"
            :conduit.article/tag-list        ["tag"
                                              "list"]
            :conduit.article/title           "title"
            :conduit.article/updated-at      #inst "2019-01-01"}))))
