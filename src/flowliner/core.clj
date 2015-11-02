(ns flowliner.core
  (:require [flowliner.data :as data]
            [clojure.core.async :as async]
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

;; UI

(defn handle-new-text [state new-text]
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

(defn node-view [node change-channel]
  (text-editor {:text (:flowliner.node/text node)
                :on-change (let [channel (async/chan)]
                             (async/go-loop []
                               (when-let [new-text (async/<! channel)]
                                 (async/>! change-channel
                                           [(:db/id node) new-text])
                                 (recur)))
                             channel)}))

(defn node-tree [node change-channel]
  (l/vertically
   (node-view node change-channel)
   (when (:flowliner.node/children node)
     (l/margin 0 0 0 10
               (l/vertically
                (for-all [node (data/get-children node)]
                         (node-tree node change-channel)))))))

(gui/def-control outliner
  ([view-context control-channel]
     (let [change-channel (async/chan)
           conn (data/create-database)
           parent (data/create-node "root" conn)
           child (data/create-node "child" conn)]

       @(d/transact
         conn
         [(data/set-root parent)
          (data/add-child parent child)])

       (async/go-loop []
         (async/alt! control-channel ([_] (println "exiting change process"))
                     change-channel ([[id text]]
                                       (d/transact conn
                                                   [(data/set-text id text)])
                                       (recur))))
       
       (let [tx-report-queue (d/tx-report-queue conn)]
         (.start (Thread. (fn []
                            (loop [report (.take tx-report-queue)]
                              (println "queue" (d/q '[:find ?a ?b ?c ?d
                                                      :where [?a ?b ?c ?d]]
                                                    (:tx-data report)))
                              (recur (.take tx-report-queue)))))))


       (conj {:conn conn
              :change-channel change-channel}
             gui/child-focus-handlers)))

  ([view-context {:keys [conn change-channel]}]
     (let [db (d/db conn)
           root (data/get-root db)]
       (node-tree (d/entity db root)
                  change-channel))))

(defn start []
  (gui/start-view #'create-outliner #'outliner-view)

  #_(.start (Thread. (fn []
                       (gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view)))))

(gui/redraw-last-started-view)

(d/q '[:find [?a ...]
       :where [?a :likes _]]

     '[[sally :age 21]
       [fred :likes [pizza sushi]]
       [foo :likes [bar baz]]])
