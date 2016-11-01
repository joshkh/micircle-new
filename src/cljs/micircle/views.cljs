(ns micircle.views
  (:require [re-frame.core :as re-frame]
            [micircle.views.core :as main]
            [json-html.core :as json-html]
            [micircle.views.drag :as drag]
            ))


;; home

(defn home-panel []
  (let [name   (re-frame/subscribe [:name])
        app-db (re-frame/subscribe [:app-db])]
    (fn []
      [:div
       ;[drag/main]
       [main/main]
       #_[:div (json-html/edn->hiccup
               ;@app-db
               ;(select-keys @app-db [:views :lengths :feature-map :link-views])
               (select-keys @app-db [:model :views :features])
               )]
       ])))


;; about

(defn about-panel []
  (fn []
    [:div "This is the About Page."
     [:div [:a {:href "#/"} "go to Home Page"]]]))


;; main

(defmulti panels identity)
(defmethod panels :home-panel [] [home-panel])
(defmethod panels :about-panel [] [about-panel])
(defmethod panels :default [] [:div])

(defn show-panel
  [panel-name]
  [panels panel-name])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [:active-panel])]
    (fn []
      [show-panel @active-panel])))
