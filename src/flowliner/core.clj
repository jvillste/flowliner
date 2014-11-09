(ns outliner.core
  (:require [clojure.core.async :as async]
            [datomic.api :as d]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            [flow-gl.debug :as debug])
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable]
           [java.nio ByteBuffer])
  (:use flow-gl.utils
        clojure.test))


(defn handle-new-text [state new-text]
  (println "changed" new-text)
  (when (:on-change state)
    (async/go (async/>! (:on-change state) new-text)))
  (assoc-in state [:text] new-text))

(defn handle-text-editor-event [state event]
  (cond
   (events/key-pressed? event :back-space)
   [(handle-new-text state (apply str (drop-last (:text state))))
    false]

   (and (:character event)
        (= (:type event)
           :key-pressed))
   [(handle-new-text state (str (:text state)
                                (:character event)))
    false]

   :default
   [state true]))


(gui/def-control text-editor
  ([view-context control-channel]
     {:text ""
      :handle-keyboard-event handle-text-editor-event
      :can-gain-focus true})

  ([view-context state]
     (l/box 10
            (drawable/->Rectangle 0
                                  0
                                  (cond
                                   (:has-focus state) [200 255 255 255]
                                   (:mouse-over state) [255 255 255 255]
                                   :default [255 200 255 255]))
            (drawable/->Text (:text state)
                             (font/create "LiberationSans-Regular.ttf" 15)
                             (if (:has-focus state)
                               [0.8 0.8 0.8 1]
                               [1 1 1 1])))))

(defn node-view [node]
  (text-editor {:text (:text node)}))

(defn node-tree [node]
  (l/vertically
   (node-view node)
   (when (:children node)
     (l/margin 0 0 0 10
               (l/vertically
                (for-all [node (:children node)]
                         (node-tree node)))))))

(gui/def-control outliner
  ([view-context control-channel]
     (let [db-uri "datomic:mem://flowliner"]
       (d/create-database db-uri)
       (conj {:conn (d/connect db-uri)
              :nodes [{:text "foo"
                       :children [{:text "bar"}
                                  {:text "baz"}]}]}
             gui/child-focus-handlers)))

  ([view-context {:keys [nodes]}]
     (layouts/->VerticalStack
      (for-all [node nodes]
               (node-tree node)))))

(defn start []
  (gui/start-view #'create-outliner #'outliner-view)

  #_(.start (Thread. (fn []
                       (gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view)))))

(gui/redraw-last-started-view)



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

(defn attribute [ident value-type cardinality ]
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
    (bytes-to-ids bytes)
    []))

(defn create-node [text conn]
  (let [node-tempid (d/tempid :db.part/user)
        {:keys [tempids db-after]} @(d/transact
                                     conn
                                     [(set-text node-tempid text)])]
    (d/resolve-tempid db-after tempids node-tempid)))

(def add-child
  (d/function '{:lang :clojure
                :params [db parent child]
                :requires [[outliner.core :as core]]
                :code [(core/set-children parent (-> (d/entity db parent)
                                                     (core/get-children)
                                                     (conj child)))] }))

(defn set-root [node]
  [:db/add
   node
   :flowliner.node/root
   true])

(defn get-root [db]
  (ffirst (d/q '[:find ?node
                 :where [?node :flowliner.node/root true]]
               db)))

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
       :db/fn add-child}])

    (let [node-1 (d/tempid :db.part/user)
          node-2 (d/tempid :db.part/user)
          {:keys [tempids db-after]} @(d/transact
                                       conn
                                       [(set-root node-1)
                                        (set-text node-1 "root")
                                        (set-text node-2 "child")])
          parent (d/resolve-tempid db-after tempids node-1)
          child (d/resolve-tempid db-after tempids node-2)]

      #_(d/invoke (d/db conn) :flowliner/add-child (d/db conn) parent child)
      (d/transact
       conn
       [#_[:db/add parent :flowliner.node/children (ids-to-bytes [child])]
        [:flowliner/add-child parent child]])

      (let [db (d/db conn)]
        (->> parent
             (d/entity db)
             (get-children)
             (first)
             (d/entity db)
             :flowliner.node/text)))))
