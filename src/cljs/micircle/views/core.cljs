(ns micircle.views.core
  (:require [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [micircle.views.entities :as entities]
            [micircle.views.links :as links]
            [micircle.math :as math]
            [inkspot.color-chart :as cc]
            [svge.components :as c]
            [svge.factory :as f]))


(defn controls []
  (let [options (subscribe [:options])]
    (fn []
      [:div.pane
       [:h3 "Configuration"]
       [:form.pure-form.pure-form-stacked
        [:fieldset
         [:label (str "Pinch Depth (" (:pinch-depth @options) ")")]
         [:input {:type      "range"
                  :value     (:pinch-depth @options)
                  :on-change (fn [e] (dispatch [:set-pinch-depth (.. e -target -value)]))}]]
        [:fieldset
         [:label (str "Pinch Percent (" (:pinch-percent @options) ")")]
         [:input {:type      "range"
                  :min       0
                  :max       0.5
                  :step      0.01
                  :value     (:pinch-percent @options)
                  :on-change (fn [e] (dispatch [:set-pinch-percent (.. e -target -value)]))}]]
        [:fieldset
         [:label (str "Flare (" (:flare @options) ")")]
         [:input {:type      "range"
                  :min       2
                  :max       30
                  :step      1
                  :value     (:flare @options)
                  :on-change (fn [e] (dispatch [:set-flare (.. e -target -value)]))}]]
        [:fieldset
         [:label
          [:input {:type      "checkbox"
                   :on-change (fn [] (dispatch [:toggle-inline-features]))
                   :checked   (:inline-features @options)}]
          (str "Inline features? (" (:inline-features @options) ")")]]]])))

(defn center-dot []
  [:circle.center {:cx 250 :cy 250 :r 3}])

(defn def [d]
  (let [options (subscribe [:options])]
    [:path {:id (str "entitytextpath" (name (:id d)))
            :d  (math/describe-arc-text-path 0 0
                                             (if (:inline-features @options)
                                               230
                                               240) (:start-angle d) (:end-angle d))}]))

(defn links []
  (let [all-features (subscribe [:all-features])
        participants (subscribe [:participant-views])]
    (into [:g.links]
          (map (fn [participant]
                 [links/feature-group participant @all-features])
               (take 1 (vals @participants))))))


(defn svg []
  (let [views             (subscribe [:views])
        link-views        (subscribe [:link-views])
        options           (subscribe [:options])
        features          (subscribe [:features])
        participants      (subscribe [:participants])
        superfam          (subscribe [:superfamily-features])
        participant-views (subscribe [:participant-views])]
    (fn []
      ;(.log js/console "link" @link)
      (let [pallete (cc/gradient :red :blue (count @link-views))]
        [:svg.micircle {:width "500" :height "500"}
         [:g {:transform "translate(250,250)"}
          (into [:g.entities]
                (map (fn [{:keys [type] :as entity}]
                       (case type
                         "protein" [entities/protein entity]
                         "small molecule" [entities/small-molecule entity]
                         nil)) (vals @participant-views)))
          [links]

          #_[:path.link {:d (c/make-path
                              [:move-to 0 10]
                              [:line-to 30 20]
                              [:line-to 0 40]
                              [:circle-arc-to 30 1 1 -50 -30])}]

          #_(let [p [
                   [:move-to {:x 0 :y 0}]
                   [:line-to {:x 20 :y 10}]
                   [:line-to {:x 40 :y 30}]
                   [:line-to {:x 60 :y -20}]
                   [:line-to {:x 80 :y 5}]

                   ]]
            [:g
             [:path.link {:d (apply c/make-path p)}]
             [:g (apply c/annotate-path p)]])
          ]

         ;[radius radius n large-arc-flag p x y]

         ;(c/make-path
         ;  (f/move-to 0 10)
         ;  (f/line-to 30 20)
         ;  (f/line-to 0 40)
         ;  (f/close))

         (into [:defs] (map (fn [d] [def d]) (vals @views)))


         #_(into [:g.links] (map-indexed (fn [idx link]
                                           [links/link (assoc link :radius 200
                                                                   :color (nth pallete idx)) @options]) @link-views))

         #_#_#_(into [:g.entities] (map (fn [entity]
                                          [entities/protein entity]) (vals @participant-views)))
             (into [:g.features] (map-indexed (fn [idx feature]
                                                [entities/feature feature]) @features))
             (into [:g.superfam] (map-indexed (fn [idx feature]
                                                [entities/superfam-feature feature]) @superfam))]))))

(defn main []
  (let [complex-id (subscribe [:complex-id])]
    (fn []
      [:div.container
       [:h4 @complex-id]
       [:div.row
        [:div.col-sm-8
         [svg]]
        [:div.col-sm-4
         [controls]]]])))



;(let [p [
;         ;[:move-to {:x 0 :y 0}]
;         ;[:circle-arc-to {:pos :relative
;         ;                 :x              -40
;         ;                 :y              -20
;         ;                 :radius         1
;         ;                 :large-arc-flag 1
;         ;                 :sweep-flag     0}]
;         ;[:radiate-out {:distance 10}]
;         ;[:circle-arc-to {:pos :relative
;         ;                 :x              50
;         ;                 :y              0
;         ;                 :radius         10
;         ;                 :large-arc-flag 1
;         ;                 :sweep-flag     1}]
;         [:move-to {:x 0 :y 0}]
;
;         ]]
;  [:g
;   [:path.link {:d (apply c/make-path p)}]
;   [:g (apply c/annotate-path p)]])
