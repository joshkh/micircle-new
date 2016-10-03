(ns micircle.views.links
  (:require [re-frame.core :as re-frame]
            [micircle.math :as math]))

(defn link []
  (fn [{:keys [radius start-angle-1 start-angle-2 end-angle-1 end-angle-2]}]
    [:path.link {:d (math/describe-link {:center-x      200
                                         :center-y      200
                                         :radius        radius
                                         :start-angle-1 start-angle-1
                                         :start-angle-2 start-angle-2
                                         :end-angle-1   end-angle-1
                                         :end-angle-2   end-angle-2})}]))

(defn link-new []
  (fn [{:keys [radius start-angle-1 start-angle-2 end-angle-1 end-angle-2]}]
    [:path.link {:d (math/describe-link-new {:center-x      200
                                         :center-y      200
                                         :radius        radius
                                         :start-angle-1 start-angle-1
                                         :start-angle-2 start-angle-2
                                         :end-angle-1   end-angle-1
                                         :end-angle-2   end-angle-2})}]))