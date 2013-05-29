(ns ^:shared todomvc-pedestal.behavior
    (:require [clojure.set :as set]
              [clojure.string :as string]
              [io.pedestal.app.util.log :as log]
              [io.pedestal.app.messages :as msg]))

;; While creating new behavior, write tests to confirm that it is
;; correct. For examples of various kinds of tests, see
;; test/todomvc_pedestal/test/behavior.clj.

;; You'll always receive a message with the type msg/init when your
;; app starts up. This message will include a :value key with the
;; value of the :init key from your dataflow.

(defn get-uuid
  "returns a type 4 random UUID: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx.
   @code from http://catamorphic.wordpress.com/2012/03/02/generating-a-random-uuid-in-clojurescript/"
  []
  (let [r (repeatedly 30 (fn [] (.toString (rand-int 16) 16)))]
    (apply str (concat (take 8 r) ["-"]
                       (take 4 (drop 8 r)) ["-4"]
                       (take 3 (drop 12 r)) ["-"]
                       [(.toString  (bit-or 0x8 (bit-and 0x3 (rand-int 15))) 16)]
                       (take 3 (drop 15 r)) ["-"]
                       (take 12 (drop 18 r))))))

(defn diff-by [f new old]
  (let [o (set (map f old))
        n (set (map f new))
        new-keys (set/difference n o)]
    (filter (comp new-keys f) new)))



(defn test-state []
  {:todos 
   [{:id 1 :title "test 1" :completed false}
    {:id 2 :title "test 2" :completed true}
    {:id 3 :title "test 3" :completed true}]})

(defn init-state []
  (let [state (.getItem js/localStorage local-storage-name)]
    (if (not state)
      (do
        {:todos []
         :stats (compute-stats [])})
      (let [todos (keywordize-keys (js->clj (.parse js/JSON state)))]
        {:todos todos
         :stats (compute-stats todos)}))))


(defn todo-transform [state message]
  (condp = (msg/type message)
    msg/init (do
               (.log js/console "init:" (:value message) state)
               (:value message))
    :add-todo (do
                (.log js/console "value:" (:value message) (pr-str state))
                (update-in state [:todos] conj 
                           {:id (get-uuid)
                            :title (:value message)
                            :completed false}))
    :del-todo (do
                (let [id (:value message)]
                  (.log js/console "del-todo:" id)
                  (update-in state [:todos] (fn [todos] (remove #(= id (:id  %)) todos)))))))


(defn new-deltas [value] 
  (log/debug :in :new-deltas :new-value (pr-str value))
  (vec (mapcat (fn [{:keys [id] :as todo}] 
                 (log/debug :in :inner-new-deltas :new-value id)
                 [[:node-create [:app :todos id] :map]
                  [:value [:app :todos id] todo]])
               value)))


(defn- delete-deltas [value]
  (log/debug :in :delete-deltas :value value)
  (vec (mapcat (fn [{:keys [id] :as todo}]
                 [[:node-destroy [:app :todos id]]])
               value)))

(defn new-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (log/debug :new-todos true :diff (diff-by :id new old))
    (diff-by :id new old)))


(defn deleted-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (log/debug :in :deleted-todos :diff (diff-by :id old new) (pr-str old) (pr-str new2))
    (diff-by :id old new)))

(defn todo-emit
  ([inputs] 
     (.log js/console "todo-emit 1st")
     initial-app-model)
  ([inputs changed-inputs]
     (.log js/console "todo-emit")
     (reduce (fn 
               [a input-name]
               (let [new-value (:new (get inputs input-name))]
                 (log/debug :in :reduce :input input-name :new-value new-value)
                 (concat a (case input-name
                            :new-todos (new-deltas new-value)
                            :deleted-todos (delete-deltas new-value)
                            :updated-todos (update-deltas new-value)
                            []))))
             []
             changed-inputs)))

(def ^:private initial-app-model
  [{:app
    {:todos {}}}])


(def todo-app
  {:transform {:todo {:init nil :fn todo-transform}}
   :combine {:new-todos {:fn new-todos :input #{:todo}}
             :deleted-todos {:fn deleted-todos :input #{:todo}}}
   :emit {:emit {:fn todo-emit :input #{:new-todos :deleted-todos}}}})
