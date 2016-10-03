(ns micircle.views.links
  (:require [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [micircle.math :as math]))


(defn link []
  (let [flags (subscribe [:flags])]
    (fn [{:keys [from to uid radius start-angle-1 start-angle-2 end-angle-1 end-angle-2]} & [options]]
      [:path.link
       {:on-mouse-enter (fn [e] (dispatch [:highlight-link uid from to]))
        :on-mouse-leave (fn [e] (dispatch [:highlight-link nil]))
        ;:style {:filter "url(#spotlight)"}
        :class (if-let [visible-links (:visible-links @flags)]
                 (if (nil? (some #{uid} visible-links)) "mute"))
        :d     (math/describe-link-new {:center-x      250
                                        :center-y      250
                                        :radius        radius
                                        :start-angle-1 start-angle-1
                                        :start-angle-2 start-angle-2
                                        :end-angle-1   end-angle-1
                                        :end-angle-2   end-angle-2} options)}])))
