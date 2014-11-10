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

(defn node-view [node]
  (println "node" node)
  (text-editor {:text (:flowliner.node/text node)}))

(defn node-tree [node]
  (println "tree" node)
  (l/vertically
   (node-view node)
   (when (:flowliner.node/children node)
     (l/margin 0 0 0 10
               (l/vertically
                (for-all [node (data/get-children node)]
                         (node-tree node)))))))

(gui/def-control outliner
  ([view-context control-channel]
     (let [conn (data/create-database)
           parent (data/create-node "root" conn)
           child (data/create-node "child" conn)]

       @(d/transact
         conn
         [(data/set-root parent)
          (data/add-child parent child)])

       (conj {:conn conn}
             gui/child-focus-handlers)))

  ([view-context {:keys [conn]}]
     (let [db (d/db conn)
           root (data/get-root db)]
       (node-tree (d/entity db root)))))

(defn start []
  (gui/start-view #'create-outliner #'outliner-view)

  #_(.start (Thread. (fn []
                       (gui/start-view #'create-photo-archive-browser #'photo-archive-browser-view)))))

(gui/redraw-last-started-view)
