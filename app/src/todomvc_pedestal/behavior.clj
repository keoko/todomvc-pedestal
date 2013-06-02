(ns ^:shared todomvc-pedestal.behavior
    (:require [clojure.set :as set]
              [clojure.string :as string]
              [io.pedestal.app.util.log :as log]
              [io.pedestal.app.messages :as msg]
              [todomvc-pedestal.data :as data]))


(defn diff-by [f new old]
  (let [o (set (map f old))
        n (set (map f new))
        new-keys (set/difference n o)]
    (filter (comp new-keys f) new)))


(defn todo-transform [state message]
  (let [new-state (condp = (msg/type message)
                    msg/init (:value message)
                    :add-todo (data/add-todo state (:value message))
                    :del-todo (data/delete-todo state (:value message))
                    :upd-todo (let [{:keys [id title completed]} message]
                                (data/update-todo state id title completed))
                    :clear-completed (data/clear-todo-completed state)
                    :toggle-all (data/toggle-all-todo state (:completed message)))]
    (.log js/console "data:" (data/update-stats new-state))
    (data/update-stats new-state)))


(defn new-deltas [value] 
  (vec (mapcat (fn [{:keys [id] :as todo}] 
                 [[:node-create [:app :todos id] :map]
                  [:value [:app :todos id] todo]])
               value)))


(defn- delete-deltas [value]
  (vec (mapcat (fn [{:keys [id] :as todo}]
                 [[:node-destroy [:app :todos id]]])
               value)))

(defn- update-deltas [value]
  (vec (mapcat (fn [{:keys [id] :as todo}]
                 [[:value [:app :todos id] todo]])
               value)))


(defn- create-stats-deltas [value]
  (when value
    [[:node-create [:app :stats] :map]
     [:value [:app :stats] value]]))

(defn- destroy-stats-deltas [value]
  (when value
    [[:node-destroy [:app :stats]]]))

(defn- update-stats-deltas [value]
  (when  value
    [[:value [:app :stats] value]]))


(defn new-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (diff-by :id new old)))


(defn deleted-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (diff-by :id old new)))

(defn updated-todos [state input-name old-input new-input]
  (let [old (:todos old-input)
        new (:todos new-input)]
    (for [n new 
          o old
          :when (and (= (:id n) (:id o))
                     (or (not= (:title n) (:title o))
                         (not= (:completed n) (:completed o))))]
      n)))


(defn create-stats [state input-name old-input new-input]
  (let [old (:stats old-input)
        new (:stats new-input)]
    (when (and (= 0 (:total-todo old))
               (< 0 (:total-todo new)))
      new)))


(defn destroy-stats [state input-name old-input new-input]
  (let [old (or (:stats old-input) {:total-todo 0})
        new (:stats new-input)]
    (when (and (= 0 (:total-todo new))
               (< 0 (:total-todo old)))
      new)))


(defn update-stats [state input-name old-input new-input]
  (let [old (:stats old-input)
        new (:stats new-input)]
    (when (and (< 0 (:total-todo new))
               (< 0 (:total-todo old))
               (not= (:todo-left new)
                     (:todo-left old)))
      new)))


(defn todo-emit
  ([inputs] initial-app-model)
  ([inputs changed-inputs]
     (log/debug :in :todo-emit :changed-inputs (pr-str changed-inputs))
     (reduce (fn 
               [a input-name]
               (let [new-value (:new (get inputs input-name))]
                 (concat a (case input-name
                            :new-todos (new-deltas new-value)
                            :deleted-todos (delete-deltas new-value)
                            :updated-todos (update-deltas new-value)
                            :create-stats (create-stats-deltas new-value)
                            :destroy-stats (destroy-stats-deltas new-value)
                            :update-stats (update-stats-deltas new-value)
                            []))))
             []
             changed-inputs)))

(def ^:private initial-app-model
  [{:app
    {:todos {}}}])


(def todo-app
  {:transform {:todo {:init {:todos []} :fn todo-transform}}
   :combine {:new-todos {:fn new-todos :input #{:todo}}
             :deleted-todos {:fn deleted-todos :input #{:todo}}
             :updated-todos {:fn updated-todos :input #{:todo}}
             :create-stats {:fn create-stats :input #{:todo}}
             :destroy-stats {:fn destroy-stats :input #{:todo}}
             :update-stats {:fn update-stats :input #{:todo}}}
   :emit {:emit {:fn todo-emit :input #{:new-todos :deleted-todos :updated-todos 
                                        :create-stats :destroy-stats :update-stats}}}})
