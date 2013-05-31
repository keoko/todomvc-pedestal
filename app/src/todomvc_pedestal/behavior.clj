(ns ^:shared todomvc-pedestal.behavior
    (:require [clojure.set :as set]
              [clojure.string :as string]
              [io.pedestal.app.util.log :as log]
              [io.pedestal.app.messages :as msg]))

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
      {:todos []}
      (let [todos (keywordize-keys (js->clj (.parse js/JSON state)))]
        {:todos todos}))))


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
                  (update-in state [:todos] (fn [todos] (remove #(= id (:id  %)) todos)))))
    :upd-todo (do
                (let [{:keys [id title completed]} message]
                  (.log js/console "upd-todo:" id title completed)
                  (update-in state [:todos] (fn [todos] (map #(if ( = id (:id %)) (assoc % :title title :completed completed) %) todos)))))))


(defn new-deltas [value] 
  (comment log/debug :in :new-deltas :new-value (pr-str value))
  (.log js/console "new-deltas")
  (vec (mapcat (fn [{:keys [id] :as todo}] 
                 (log/debug :in :inner-new-deltas :new-value id)
                 [[:node-create [:app :todos id] :map]
                  [:value [:app :todos id] todo]])
               value)))


(defn- delete-deltas [value]
  (comment log/debug :in :delete-deltas :value value)
  (vec (mapcat (fn [{:keys [id] :as todo}]
                 [[:node-destroy [:app :todos id]]])
               value)))

(defn- update-deltas [value]
  (comment log/debug :in :update-deltas :value value)
  (vec (mapcat (fn [{:keys [id] :as todo}]
                 [[:value [:app :todos id] todo]])
               value)))


(defn new-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (.log js/console "new-todos"  (diff-by :id new old))
    (comment log/debug :new-todos true :diff)
    (diff-by :id new old)))


(defn deleted-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (comment log/debug :in :deleted-todos :diff (diff-by :id old new) (pr-str old) (pr-str new2))
    (diff-by :id old new)))

(defn updated-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (comment log/debug :in :updated-todos :diff (diff-by :id old new) (pr-str old) (pr-str new2))
    (for [n new 
          o old
          :when (and (= (:id n) (:id o))
                     (or (not= (:title n) (:title o))
                         (not= (:completed n) (:completed o))))]
      n)))

(defn todo-emit
  ([inputs]
     (let [changed-inputs (assoc-in inputs [:new-todos :old] [])]
       (.log js/console "todo-emit-1" (pr-str changed-inputs))
       (concat  (todo-emit inputs changed-inputs)
                [[:node-create [:app] :map] [:node-create [:app :todos] :map]])))
  ([inputs changed-inputs]
     (.log js/console "todo-emit" (pr-str changed-inputs))
     (reduce (fn 
               [a input-name]
               (let [new-value (:new (get inputs input-name))]
                 (comment log/debug :in :reduce :input input-name :new-value new-value)
                 (.log js/console "in reduce")
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
  {:transform {:todo {:init (test-state) :fn todo-transform}}
   :combine {:new-todos {:fn new-todos :input #{:todo}}
             :deleted-todos {:fn deleted-todos :input #{:todo}}
             :updated-todos {:fn updated-todos :input #{:todo}}}
   :emit {:emit {:fn todo-emit :input #{:new-todos :deleted-todos :updated-todos}}}})
