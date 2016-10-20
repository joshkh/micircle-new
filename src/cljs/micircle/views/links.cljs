(ns micircle.views.links
  (:require [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [micircle.math :as math]))


(defn linkold []
  (let [flags (subscribe [:flags])]
    (fn [{:keys [color from to uid radius start-angle-1 start-angle-2 end-angle-1 end-angle-2]} & [options]]
      [:path.link
       {:on-mouse-enter (fn [e] (dispatch [:highlight-link uid from to]))
        :on-mouse-leave (fn [e] (dispatch [:highlight-link nil]))
        ;:style {:filter "url(#spotlight)"}
        :fill           color
        :class          (if-let [visible-links (:visible-links @flags)]
                          (if (nil? (some #{uid} visible-links)) "mute"))
        :d              (math/describe-link-new {:center-x      250
                                                 :center-y      250
                                                 :radius        radius
                                                 :start-angle-1 start-angle-1
                                                 :start-angle-2 start-angle-2
                                                 :end-angle-1   end-angle-1
                                                 :end-angle-2   end-angle-2} options)}])))

(defn link []
  (fn [{:keys [from-feature to-features] :as all}]
    (if from-feature
      (let [control-point-pos (math/place-at-radius
                               0 0 100
                               (math/center-angle-of-participants from-feature)
                               (math/center-angle-of-participants from-feature))]
       [:g
        [:circle.control-point {:r 5 :cx (:x control-point-pos) :cy (:y control-point-pos)}]
        [:path.link
         {:d (math/build-link-path 0 0 190 all)}]])
      [:g])))

(defn feature-group []
  (fn [participant all-features]
    (into [:g]
          (->>
            (:features participant)
            (map (fn [[feature-id feature]]
                   [link {:from-feature (get all-features feature-id)
                          :to-features  (vals
                                          (select-keys all-features
                                                       (get feature :linkedFeatures)))}]))))))

(defn participant-links []
  (fn [[from-participant-id from-participant] all-participants feature-map]
    (into [:g.link-group]
          (map (fn [[feature-id feature]]
                 [link {:from-feature
                        feature
                        :to-features
                        ; Select the participants
                        (select-keys all-participants
                                     ; that have the features
                                     (vals (select-keys feature-map
                                                        ; found in this participant's linked features
                                                        (get-in feature [:linkedFeatures]))))}])
               (:features from-participant)))))
