(ns flowliner.data
  (:require [clojure.core.async :as async]
            [datomic.api :as d])
  (:import [java.nio ByteBuffer])
  (:use clojure.test))


;; Datomic

(defn ids-to-bytes [ids]
  (let [buffer (ByteBuffer/allocate (* Long/BYTES (count ids)))]
    (doseq [id ids]
      (.putLong buffer id))
    (.array buffer)))

(defn bytes-to-ids [bytes]
  (let [buffer (ByteBuffer/allocate (count bytes))]
    (doto buffer
      (.put bytes)
      (.flip))
    (loop [ids []]
      (if (> (.remaining buffer) 0)
        (recur (conj ids (.getLong buffer)))
        ids))))

(defn attribute [ident value-type cardinality]
  {:db/id (d/tempid :db.part/db)
   :db/ident ident
   :db/valueType value-type
   :db/cardinality cardinality
   :db.install/_attribute :db.part/db})

(defn set-text [node text]
  [:db/add
   node
   :flowliner.node/text
   text])

(defn set-children [parent children]
  [:db/add
   parent
   :flowliner.node/children
   (ids-to-bytes children)])

(defn get-children [parent-entity]
  (if-let [bytes (:flowliner.node/children parent-entity)]
    (->> (bytes-to-ids bytes)
         (map #(d/entity (.db parent-entity) %)))
    []))

(defn create-node [text conn]
  (let [node-tempid (d/tempid :db.part/user)
        {:keys [tempids db-after]} @(d/transact
                                     conn
                                     [(set-text node-tempid text)])]
    (d/resolve-tempid db-after tempids node-tempid)))

(def add-child-function
  (d/function '{:lang :clojure
                :params [db parent child]
                :requires [[outliner.core :as core]]
                :code [(core/set-children parent (-> (d/entity db parent)
                                                     (core/get-children)
                                                     (conj child)))] }))

(defn add-child [parent child]
  [:flowliner/add-child parent child])

(defn set-root [node]
  [:db/add
   node
   :flowliner.node/root
   true])

(defn get-root [db]
  (ffirst (d/q '[:find ?node
                 :where [?node :flowliner.node/root true]]
               db)))

(defn create-database []
  (let [db-uri "datomic:mem://flowliner"]
    (d/create-database db-uri)
    (let [conn (d/connect db-uri)]
      (d/transact
       conn
       [(attribute :flowliner.node/text
                   :db.type/string
                   :db.cardinality/one)
        (attribute :flowliner.node/children
                   :db.type/bytes
                   :db.cardinality/one)
        (attribute :flowliner.node/root
                   :db.type/boolean
                   :db.cardinality/one)])

      (d/transact
       conn
       [{:db/id (d/tempid :db.part/user)
         :db/ident :flowliner/add-child
         :db/fn add-child-function}])

      conn)))

(let [conn (create-database)]
  (let [parent (create-node "root" conn)
        child (create-node "child" conn)]

    @(d/transact
      conn
      [(set-root parent)])

    #_(d/invoke (d/db conn) :flowliner/add-child (d/db conn) parent child)
    (d/transact
     conn
     [[:flowliner/add-child parent child]])

    (let [db (d/db conn)]
      (->> (get-root db)
           (d/entity db)
           (get-children)
           (first)
           :flowliner.node/text))))
