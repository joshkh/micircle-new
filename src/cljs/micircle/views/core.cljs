(ns micircle.views.core
  (:require [re-frame.core :as re-frame :refer [subscribe]]
            [micircle.views.entities :as entities]
            [micircle.views.links :as links]
            [micircle.math :as math]))


(defn center-dot []
  [:circle.center {:cx 200 :cy 200 :r 5}])

(defn svg []
  (let [views (subscribe [:views])
        link-views (subscribe [:link-views])]
    (fn []
      (println @link-views)
      [:svg.micircle {:width "400" :height "400"}
       [center-dot]
       (into [:g.entities] (map (fn [entity] [entities/protein entity]) @views))
       (into [:g.links] (map (fn [link] [links/link (assoc link :radius 150)]) @link-views))
       #_[links/link-new {:radius        150
                        :start-angle-1 90
                        :start-angle-2 140
                        :end-angle-1   230
                        :end-angle-2   260}]])))

(defn inspector []
  (let [participants (subscribe [:participants])]
    (fn []
      [:div (str "participants: " @participants)])))

(defn main []
  (fn []
    [:div
     ;[inspector]
     [svg]]))
