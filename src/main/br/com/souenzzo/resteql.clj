(ns br.com.souenzzo.resteql
  (:require [clojure.string :as string]
            [br.com.souenzzo.eql-as.alpha :as eql-as]
            [com.wsscode.pathom.core :as p]
            [io.pedestal.http.body-params :as body-params]
            [clojure.data.json :as json]))

(defn index-by
  ([f]
   (map (juxt f identity)))
  ([f coll]
   (into {}
         (index-by f)
         coll)))

(defn merge-in
  [data paths]
  (reduce
    (fn [acc path]
      (merge acc
             (get-in data path)))
    {}
    paths))

(defn ->routes
  [{::keys [rest->eql
            parser
            open-api]}]
  (let [op->input (index-by ::operation-id rest->eql)]
    (-> (for [[path spec] (get open-api "paths")
              [method spec] spec
              :let [operation-id (get spec "operationId")
                    route-name (keyword (get spec "operationId" (gensym)))
                    {::keys [mutation-sym returning-as params-as request->params]
                     :as    input} (get op->input operation-id)]
              :when input]
          [(-> path
               (string/replace #"\}" "")
               (string/replace #"\{" ":"))
           (keyword method)
           [(body-params/body-params)
            (fn [request]
              (-> request keys prn)
              (let [params (p/map-select request
                                         (eql-as/ident-query {::eql-as/as-map params-as
                                                              ::eql-as/as-key :pathom/as}))
                    params (merge-in params request->params)
                    query `[{(~mutation-sym ~params) ~(eql-as/ident-query {::eql-as/as-map returning-as
                                                                           ::eql-as/as-key :pathom/as})}]
                    body (get (parser request query)
                              mutation-sym)]
                {:body   (json/write-str body :value-fn (fn [k v]
                                                          (if (inst? v)
                                                            (subs (pr-str v)
                                                                  7
                                                                  17)
                                                            v)))
                 :status 200}))]
           :route-name route-name])
        set)))
