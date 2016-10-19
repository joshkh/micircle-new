(ns micircle.views.entities
  (:require [re-frame.core :as re-frame :refer [dispatch subscribe]]
            [micircle.math :as math]))



(defn question-box []
  (fn [{:keys [start-angle end-angle]}]
    [:path.unknown {:d (math/describe-arc 200 200 150 (- start-angle 5) start-angle 15)}]))

(defn protein-old []
  (let [flags (subscribe [:flags])]
    (fn [{:keys [label id interactorRef start-angle end-angle]}]
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
                    [:path.tick {:d (math/describe-tick 250 250 200 interval)}])
                  (range (+ start-angle 5) end-angle 5)))
       [:g.label
        [:text.label {:text-anchor "middle"}
         [:textPath {:startOffset "50%"
                     :xlinkHref   (str "#entitytextpath" (name id))}
          label]]]])))

(defn translate-str [{:keys [x y]}]
  (str "translate(" x "," y ")"))

(defn adjust-binding-area
  ; Determine which part of a view is the binding region.
  ; We do this because some views, such as a protein, may have a binding region
  ; section as well as a "unknown" region at the beginning or end.
  [type-kw position-kw angle]
  (case type-kw
    :protein (case position-kw
               :start (+ angle 2)
               :end (- angle 2))
    angle))


(defn pack
  [element component items]
  (into element
        (map (fn [i]
               [component i]) items)))


(defn radial-features [feature]
  (into [:g]
        (map (fn [{:keys [start-angle end-angle pos]}]
               ; Only show features that have locations
               (if (not= pos "?-?")
                 [:path.feature {:d (math/describe-arc
                                      0 0 200
                                      (adjust-binding-area :protein :start start-angle)
                                      (adjust-binding-area :protein :end end-angle)
                                      10)}])) (:sequenceData feature))))


(defn protein []
  (let [flags (subscribe [:flags])]
    (fn [{:keys [label id interactorRef start-angle end-angle] :as participant}]
      [:g
       [:g.arc
        [:path.arc {:d (math/describe-arc
                         0 0 200
                         (adjust-binding-area :protein :start start-angle)
                         (adjust-binding-area :protein :end end-angle)
                         20)}]]
       (pack [:g.features] radial-features (:features participant))

       (into [:g.ticks]
             (map (fn [interval]
                    [:path.tick {:d (math/describe-tick 0 0 215 interval)}])
                  (range (+ 5 (adjust-binding-area :protein :start start-angle))
                         (adjust-binding-area :protein :end end-angle)
                         5)))
       [:g.label
        [:text.label {:text-anchor "middle"}
         [:textPath {:startOffset "50%"
                     :xlinkHref   (str "#entitytextpath" (name id))}
          "Test Label"]]]])))


(defn small-molecule []
  (fn [{:keys [label id interactorRef start-angle end-angle]}]
    (let [xy (math/place-at-radius 0 0 210 start-angle end-angle)]
      [:g {:transform (translate-str xy)}
       [:circle.small-molecule {:r 10 :cx 0 :cy 0}]
       ;[:path {:d (math/triangle-path)}]
       ])))


(defn feature []
  (let [flags   (subscribe [:flags])
        options (subscribe [:options])]
    (fn [{:keys [color participant-id start-angle end-angle]}]
      [:g
       {:class (if-let [visible-nodes (:visible-nodes @flags)]
                 (if (nil? (some #{participant-id} visible-nodes))
                   "mute"))}
       [:path.feature {:d (math/describe-arc 250
                                             250
                                             (if (:inline-features @options)
                                               210
                                               225) start-angle end-angle 10)}]])))

(defn superfam-feature []
  (let [flags   (subscribe [:flags])
        options (subscribe [:options])]
    (fn [{:keys [color participant-id start-angle end-angle]}]
      [:g
       {:class (if-let [visible-nodes (:visible-nodes @flags)]
                 (if (nil? (some #{participant-id} visible-nodes))
                   "mute"))}
       [:path.superfam {:d (math/describe-arc 250
                                              250
                                              (if (:inline-features @options)
                                                220
                                                225) start-angle end-angle 5)}]])))