(ns micircle.views.entities
  (:require [re-frame.core :as re-frame]
            [micircle.math :as math]))

(defn protein []
  (fn [{:keys [start-angle end-angle]}]
    [:path.arc {:d (math/describe-arc 200 200 150 start-angle end-angle 15)}]))