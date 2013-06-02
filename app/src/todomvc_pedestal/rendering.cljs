(ns todomvc-pedestal.rendering
  (:require [domina :as dom]
            [domina.events :as dom-events]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.render.events :as events]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [dommy.core :as dommy])
  (:use-macros [dommy.macros :only [node]]))


(def enter-key-code 13)


(defn new-todo-keypress-handler [e]
  (let [text-node (dom/by-id "new-todo")
        text (.-value text-node)
        evt (.-evt e)
        char (or (== (.-charCode evt) 0) 
                 (.fromCharCode js/String (.-charCode evt)))]
    (set! (.-value text-node) (str text char))
    (when (= enter-key-code (.-keyCode evt))
      (set! (.-value text-node) "")
      [{msg/topic :todo msg/type :add-todo :value text}])))


(defn render-page-todo [r [_ _ old-value new-value] input-queue]
  (let [btn (dom/by-id "new-todo")]
    (.focus (dom/by-id "new-todo"))
    (events/send-on :keypress btn input-queue new-todo-keypress-handler)))


(defn toggle-todo-change-handler [e]
  (let [evt (.-evt e)
        checkbox (.-target evt)
        completed (.-checked checkbox)
        id (dom/attr checkbox "data-todo-id")
        text (.-value (dom/by-id (str "input_" id)))]
    [{msg/topic :todo msg/type :upd-todo :id id :title text :completed completed}]))


(defn toggle-all-change-handler [e]
  (let [evt (.-evt e)
        target (.-target evt)
        completed (.-checked target)]
    [{msg/topic :todo msg/type :toggle-all :completed completed}]))


(defn update-todo-keypress-handler [e]
  (let [evt (.-evt e)
        node (.-target evt)
        text (.-value node)
        id (dom/attr node "data-todo-id")
        completed (.-checked (dom/by-id (str "chb_" id)))
        char (or (== (.-charCode evt) 0) 
                 (.fromCharCode js/String (.-charCode evt)))]
                        (set! (.-value node) (str text char))
                        (when (= enter-key-code (.-keyCode evt))
                          (set! (.-value node) "")                          
                          (dom/remove-class! (dom/by-id id) "editing")
                          [{msg/topic :todo msg/type :upd-todo :id id :title text :completed completed}])))

(defn create-todo-node [r [_ path] input-queue]
  (let [id (name (last path))
        container (dom/by-id "todo-list")]
    (dom/append! container  
                 (node [:li {:id id} 
                        [:div.view {:data-todo-id id}
                         [:input.toggle {:id (str "chb_" id)
                                         :data-todo-id id 
                                         :type "checkbox"}]
                         [:label {:id (str "lbl_" id)} "new-todo"]
                         [:button.destroy {:id (str "del_" id)}]]
                        [:input.edit {:id (str "input_" id)
                                      :data-todo-id id}]]))
    ;; delete-todo event
    (events/send-on :click 
                    (dom/by-id (str "del_" id)) 
                    input-queue
                    (fn [e]
                      [{msg/topic :todo msg/type :del-todo :value id}]))
    
    (dom-events/listen! (dom/by-id (str "lbl_" id))
                   :click
                   (fn [e] 
                     (dom/add-class! (dom/by-id id) "editing")
                     (.focus (dom/by-id (str "input_" id)))))
    ;; checkbox
    (events/send-on :change
                    (dom/by-id (str "chb_" id))
                    input-queue
                    toggle-todo-change-handler)
    ;; toggle all
    (events/send-on :change
                    (dom/by-id "toggle-all")
                    input-queue
                    toggle-all-change-handler)
    ;; update todo
    (events/send-on :keypress
                    (dom/by-id (str "input_" id))
                    input-queue
                    update-todo-keypress-handler)))

(defn update-todo [r [_ path o n] d]
  (let [id (name (last path))
        div-node (dom/by-id id)
        checkbox-node (dom/by-id (str "chb_" id))]
    (dom/set-text! (dom/by-id (str "lbl_" id)) (:title n))
    (dom/set-value! (dom/by-id (str "input_" id)) (:title n))
    (if (:completed n)
      (do
        (dom/set-attr! checkbox-node "checked" true)
        (dom/add-class! div-node "completed"))
      (do (dom/set-attr! checkbox-node "checked" false) 
          (dom/remove-attr! checkbox-node "checked")
          (dom/remove-class! div-node "completed")))))


(defn delete-todo-node [r [_ path o n] d]
  (let [id (name (last path))]
    (dom/destroy! (dom/by-id id))))



;; stats functions
(defn draw-todo-clear [input-queue]
  (let [button (node [:button#clear-completed "Clear completed"])]
    ;; clear completed
    (dom/append! (dom/by-id "footer") button)
    (events/send-on :click
                    button
                    input-queue
                    (fn [e]
                      [{msg/topic :todo msg/type :clear-completed}]))))

(defn draw-todo-count [input-queue]
  (let [remaining (node [:span#todo-count [:strong 0] " items"])]
1    (dom/append! (dom/by-id "footer") remaining)))

(defn create-stats-node [r [_ path] input-queue]
  (draw-todo-count input-queue)
  (draw-todo-clear input-queue)
  (dom/set-styles! (dom/by-id "footer") {:display "block"}))

(defn destroy-stats-node [r n input-queue]
  (dom/destroy-children! (dom/by-id "footer"))
  (dom/set-styles! (dom/by-id "footer") {:display "none"}))

(defn update-stats  [r [_ path o n] d]
  (let [completed-node (dom/by-id "clear-completed")]
    ;; stats bar display
    (when (< 0 (:total-todos n))
      (dom/set-style! (dom/by-id "footer") {:display "block"}))
    ;; clear completed button
    (if (< 0 (:todo-completed n))
      (do
        (dom/set-styles! completed-node {:display "block"}) 
        (dom/set-text! completed-node (str "Clear completed (" (:todo-completed n) ")")))
      (dom/set-styles! completed-node {:display "none"}))
    ;; # items left
    (when (<= 0 (:todo-left n))
      (dom/set-text! (dom/by-id "todo-count") 
                     (str (or (:todo-left n) 0) 
                          (if (= 1 (:todo-left n)) " item" " items") 
                          " left")))))


(defn render-config []
  [;; todo list
   [:node-create  [:app :todos] render-page-todo]
   ;; todo items
   [:node-create  [:app :todos :*] create-todo-node]
   [:node-destroy  [:app :todos :*] delete-todo-node]
   [:value [:app :todos :*] update-todo]
   ;; stats
   [:node-create [:app :stats] create-stats-node]
   [:node-destroy [:app :stats] destroy-stats-node]
   [:value [:app :stats] update-stats]])
