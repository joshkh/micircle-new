(ns micircle.views.core
  (:require [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [micircle.views.entities :as entities]
            [micircle.views.links :as links]
            [micircle.math :as math]))


(defn controls []
  (let [options (subscribe [:options])]
    (fn []
      [:div.pane
       [:form.pure-form.pure-form-stacked
        [:fieldset
         [:label (str "Pinch Depth (" (:pinch-depth @options) ")")]
         [:input {:type      "range"
                  :on-change (fn [e] (dispatch [:set-pinch-depth (.. e -target -value)]))}]]
        [:fieldset
         [:label (str "Pinch Percent (" (:pinch-percent @options) ")")]
         [:input {:type      "range"
                  :min       0
                  :max       0.5
                  :step      0.01
                  :on-change (fn [e] (dispatch [:set-pinch-percent (.. e -target -value)]))}]]]])))

(defn center-dot []
  [:circle.center {:cx 250 :cy 250 :r 3}])

(defn svg []
  (let [views      (subscribe [:views])
        link-views (subscribe [:link-views])
        options    (subscribe [:options])
        features (subscribe [:features])]
    (fn []
      [:svg.micircle {:width "500" :height "500"}
       ;[center-dot]
       (into [:g.links] (map (fn [link] [links/link (assoc link :radius 200) @options]) @link-views))
       (into [:g.entities] (map (fn [entity] [entities/protein entity]) @views))
       (into [:g.features] (map (fn [feature] [entities/feature feature]) @features))])))

(defn main []
  (fn []
    [:div

     [:h4 "EBI-9082861"]
     [svg]
     [controls]]))
