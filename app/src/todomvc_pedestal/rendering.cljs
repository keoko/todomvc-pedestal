(ns todomvc-pedestal.rendering
  (:require [domina :as dom]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.render.events :as events]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [dommy.core :as dommy])
  (:require-macros [todomvc-pedestal.html-templates :as html-templates])
  (:use-macros [dommy.macros :only [sel sel1 node]]))

;; Load templates.

(def enter-key-code 13)

(def templates (html-templates/todomvc-pedestal-templates))

;; The way rendering is handled below is the result of using the
;; renderer provided in `io.pedestal.app.render`. The only requirement
;; for a renderer is that it must implement the Renderer protocol.
;;
;; This renderer dispatches to rendering functions based on the
;; requested change. See the render-config table below. Each render
;; function takes three arguments: renderer, render operation and a
;; a transmitter which is used to send data back to the application's
;; behavior. This example does not use the transmitter.

(defn render-page [renderer [_ path] transmitter]
  (let [;; The renderer that we are using here helps us map changes to
        ;; the UI tree to the DOM. It keeps a mapping of paths to DOM
        ;; ids. The `get-parent-id` function will return the DOM id of
        ;; the parent of the node at path. If the path is [:a :b :c]
        ;; then this will find the id associated with [:a :b]. The
        ;; root node [] is configured when we created the renderer.
        parent (render/get-parent-id renderer path)
        ;; Use the `new-id!` function to associate a new id to the
        ;; given path. With two arguments, this function will generate
        ;; a random unique id. With three arguments, the given id will
        ;; be associated with the given path.
        id (render/new-id! renderer path)
        ;; Get the dynamic template named :todomvc-pedestal-page
        ;; from the templates map. The `add-template` function will
        ;; associate this template with the node at
        ;; path. `add-template` returns a function that can be called
        ;; to generate the initial HTML.
        html (templates/add-template renderer path (:todomvc-pedestal-page templates))]
    ;; Call the `html` function, passing the initial values for the
    ;; template. This returns an HTML string which is then added to
    ;; the DOM using Domina.
    (dom/append! (dom/by-id parent) (html {:id id :message ""}))))

(defn render-message [renderer [_ path _ new-value] transmitter]
  ;; This function responds to a :value event. It uses the
  ;; `update-t` function to update the template at `path` with the new
  ;; values in the passed map.
  (templates/update-t renderer path {:message new-value}))

;; The data structure below is used to map rendering data to functions
;; which handle rendering for that specific change. This function is
;; referenced in config/config.clj and must be a function in order to
;; be used from the tool's "render" view.

(defn render-page [renderer [_ path] transmitter]
  (let [;; The renderer that we are using here helps us map changes to
        ;; the UI tree to the DOM. It keeps a mapping of paths to DOM
        ;; ids. The `get-parent-id` function will return the DOM id of
        ;; the parent of the node at path. If the path is [:a :b :c]
        ;; then this will find the id associated with [:a :b]. The
        ;; root node [] is configured when we created the renderer.
        parent (render/get-parent-id renderer path)
        ;; Use the `new-id!` function to associate a new id to the
        ;; given path. With two arguments, this function will generate
        ;; a random unique id. With three arguments, the given id will
        ;; be associated with the given path.
        id (render/new-id! renderer path)
        ;; Get the dynamic template named :todomvc-pedestal-page
        ;; from the templates map. The `add-template` function will
        ;; associate this template with the node at
        ;; path. `add-template` returns a function that can be called
        ;; to generate the initial HTML.
        html (templates/add-template renderer path (:todomvc-pedestal-page templates))]
    ;; Call the `html` function, passing the initial values for the
    ;; template. This returns an HTML string which is then added to
    ;; the DOM using Domina.
    (dom/append! (dom/by-id parent) (html {:id id :message ""}))))

(comment defn render-todos [r [_ _ old-value new-value] input-queue]
  (let [container (dom/by-id "todo-list")]
    (dom/destroy-children! container)
    (doseq [new-todo new-value]
      (dom/append! container
                   (str "<li>" new-todo "</li>")))))

(defn render-todos [r [_ _ old-value new-value] input-queue]
  (let [container (sel1 :#todo-list)]
    (dom/destroy-children! container)
    (doseq [new-todo new-value]
      (dommy/append! container
                     (node [:li [:div.view {:data-todo-id 1}
                                 [:input.toggle {:type "checkbox"}]
                                 [:label new-todo]
                                 [:button.destroy]
                                 [:input.edit {:id (str "input_" 1)}]]])))))

(comment defn render-page-todo [r [_ _ old-value new-value] input-queue]
  (let [form (dom/by-id "todo-form")
        btn (dom/by-id "todo-add-button")]
    (.focus (dom/by-id "todo-entry"))
    (events/send-on :click
                    btn
                    input-queue
                    (fn [e]
                      (let [text-node (dom/by-id "todo-entry")
                            text (.-value text-node)]
                        [{msg/topic :todo msg/type :add :value text}])))))

(defn render-page-todo [r [_ _ old-value new-value] input-queue]
  (let [btn (dom/by-id "new-todo")]
    (.focus (dom/by-id "new-todo"))
    (events/send-on :keypress
                    btn
                    input-queue
                    (fn [e]
                      (let [text-node (dom/by-id "new-todo")
                            text (.-value text-node)
                            evt (.-evt e)
                            char (or (== (.-charCode evt) 0) 
                                     (.fromCharCode js/String (.-charCode evt)))]
                        (set! (.-value text-node) (str text char))
                        (when (= enter-key-code (.-keyCode evt))
                          (.log js/console text e char)
                          (set! (.-value text-node) "")
                          [{msg/topic :todo msg/type :add :value text}]))))))

(defn render-config []
  [;; All :node-create deltas for the node at
   ;; :io.pedestal.app/view-example-transform will be rendered by the
   ;; `render-page` function. The node name
   ;; :io.pedestal.app/view-example-transform is a default name that is used
   ;; when we don't provide our own combines and emits. To name your
   ;; own nodes, create a custom combine or emit in the application's
   ;; behavior.
   [:node-create  [:io.pedestal.app/view-todo-transform] render-page]
   [:node-create  [:app :todos] render-page-todo]
   ;; All :node-destroy deltas for this path will be handled by the
   ;; library function `d/default-exit`.
   [:node-destroy   [:io.pedestal.app/view-todo-transform] d/default-exit]
   ;; All :value deltas for this path will be handled by the
   ;; function `render-message`.
   [:value [:io.pedestal.app/view-todo-transform] render-message]
   [:value [:app :todos] render-todos]])

;; In render-config, paths can use wildcard keywords :* and :**. :*
;; means exactly one segment with any value. :** means 0 or more
;; elements.
