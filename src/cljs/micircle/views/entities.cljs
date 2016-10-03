(ns micircle.views.entities
  (:require [re-frame.core :as re-frame :refer [dispatch subscribe]]
            [micircle.math :as math]))



(defn question-box []
  (fn [{:keys [start-angle end-angle]}]
    [:path.unknown {:d (math/describe-arc 200 200 150 (- start-angle 5) start-angle 15)}]))

(defn protein []
  (let [flags (subscribe [:flags])]
    (fn [{:keys [id start-angle end-angle]}]
      [:g
       {:class          (if-let [visible-nodes (:visible-nodes @flags)]
                          (if (nil? (some #{id} visible-nodes))
                            "mute"))
        :on-mouse-enter (fn [e] (dispatch [:highlight-entity id]))
        :on-mouse-leave (fn [e] (dispatch [:highlight-entity nil]))}
       [:g.unknown
        [:path.unknown {:d (math/describe-arc 250 250 200 (- start-angle 5) start-angle 20)}]]
       [:g.arc
        [:path.arc {:d (math/describe-arc 250 250 200 start-angle end-angle 20)}]
        #_(into [:g.features (fn [f]
                               [:path.feature
                                {:d (math/describe-arc 250 250 200 start-angle end-angle 20)}])])]
       (into [:g.ticks]
             (map (fn [interval]
                    [:path.tick {:d (math/describe-tick 250 250 215 interval)}])
                  (range (+ start-angle 5) end-angle 5)))])))

(defn feature []
  (let [flags (subscribe [:flags])]
    (fn [{:keys [participant-id start-angle end-angle]}]
      [:g
       {:class (if-let [visible-nodes (:visible-nodes @flags)]
                 (if (nil? (some #{participant-id} visible-nodes))
                   "mute"))}
       [:path.feature {:d (math/describe-arc 250 250 200 start-angle end-angle 10)}]])))