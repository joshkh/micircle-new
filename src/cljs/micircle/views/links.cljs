(ns micircle.views.links
  (:require [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [micircle.math :as math]
            [reagent.core :as reagent]
            [svge.components :as components]))


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



(defn parse-matrix
  "Parses a string representing a matrix transform:
  matrix(1 0 0 1 0 0) => (1 0 0 1 0 0"
  [matrix-string]
  (into [] (map int (map first (partition 2 (drop 7 matrix-string))))))

(defn select-element [state-atom evt]
  (swap! state-atom assoc
         :selected? true
         :current-x (.. evt -clientX)
         :current-y (.. evt -clientY)))

(defn deselect-element [state-atom evt]
  (swap! state-atom assoc :selected? false))

(defn move-element [state-atom evt]
  (if (:selected? @state-atom)
    (let [{:keys [matrix current-x current-y]} @state-atom
          client-x (.. evt -clientX)
          client-y (.. evt -clientY)
          dx (- client-x current-x)
          dy (- client-y current-y)]
      (swap! state-atom assoc
             :current-x client-x
             :current-y client-y
             :matrix (-> matrix (update 4 + dx) (update 5 + dy))))))

(defn control-point []
  (let [state (reagent/atom {:matrix [1 0 0 1 0 0]})]
    (fn []
      [:circle.control-point
       {:r             10
        :transform     (str "matrix(" (clojure.string/join " " (:matrix @state)) ")")
        :on-mouse-down (partial select-element state)
        :on-mouse-up   (partial deselect-element state)
        :on-mouse-move (partial move-element state)
        :cx            0
        :cy            0}])))

(defn link []
  (fn [{:keys [from-feature to-features] :as all}]
    (if from-feature
      (let [control-point-pos (math/place-at-radius
                                0 0 100
                                (math/center-angle-of-participants from-feature)
                                (math/center-angle-of-participants from-feature))]
        [:g
                  ;[:circle.control-point {:r 5 :cx (:x control-point-pos) :cy (:y control-point-pos)}]
         (.log js/console "doing" (math/build-link-path 0 0 190 all))
         [:path.link
          {:d (math/build-link-path 0 0 190 all)}]
         (let [{cx :x cy :y} (math/centroid 0 0 190 all)]
           [:circle.control-point {:r 3 :cx cx :cy cy}])
         ])
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
