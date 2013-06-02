(ns todomvc-pedestal.data
  (:use [clojure.walk :only [keywordize-keys]]))

(def local-storage-name "todo-list")

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

(defn save-todos [todos]
  (let [todos (.stringify js/JSON (clj->js (get-in todos [:todos])))]
    (.setItem js/localStorage local-storage-name todos)))

(defn add-todo [state title]
  (update-in state [:todos] conj 
             {:id (get-uuid)
              :title title
              :completed false}))


(defn delete-todo [state id]
  (update-in state [:todos] (fn [todos] (remove #(= id (:id  %)) todos))))


(defn update-todo [state id title completed]
  (update-in state [:todos] (fn [todos] 
                              (map #(if ( = id (:id %))
                                      (assoc % :title title :completed completed)
                                      %) todos))))

(defn clear-todo-completed [state]
  (update-in state [:todos] #(remove :completed %)))

(defn toggle-all-todo [state completed]
  (update-in state [:todos] (fn [todos] (map #(assoc-in % [:completed] completed) todos))))


(defn compute-stats [todos]
  (let [total (count todos)
        completed (count (filter :completed todos))]
    {:total-todo total
     :todo-left (- total completed)
     :todo-completed completed}))


(defn update-stats [state]
  (update-in state [:stats] #(compute-stats (:todos state))))
