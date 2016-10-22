(ns micircle.views.drag
  (:require [reagent.core :as r]
            [micircle.views.components :as c]
            [micircle.views.geometry :as g]))


(defonce points (r/atom {:c (g/point 250 250)}))

(defn get-bcr [svg-root]
  (-> svg-root
      r/dom-node
      .getBoundingClientRect))

(defn move-point [svg-root p]
  (fn [x y]
    (let [bcr (get-bcr svg-root)]
      (swap! points assoc p (g/point (- x (.-left bcr)) (- y (.-top bcr)))))))

(defn root [svg-root]
  (let [{:keys [p1 p2 p3 p c]} @points]
    [:g
     [c/point {:on-drag (move-point svg-root :c)} c]]))

(defn main [{:keys [width height]}]
  [:svg
   {:width  (or width 800)
    :height (or height 600)
    :style  {:border "1px solid black"}}
   [:text {:style {:-webkit-user-select "none"
                   :-moz-user-select    "none"}
           :x     20 :y 20 :font-size 20}
    "The points are draggable and the slider controls history"]
   [root (r/current-component)]])

(defn by-id [id]
  (.getElementById js/document id))









;
;
;
;(defn drag-move-fn [on-drag]
;  (fn [evt]
;    (on-drag (.-clientX evt) (.-clientY evt))))
;
;(defn drag-end-fn [drag-move drag-end on-end]
;  (fn [evt]
;    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
;    (events/unlisten js/window EventType.MOUSEUP @drag-end)
;    (on-end)))
;
;(defn dragging
;  ([on-drag] (dragging on-drag (fn []) (fn [])))
;  ([on-drag on-start on-end]
;   (let [drag-move     (drag-move-fn on-drag)
;         drag-end-atom (atom nil)
;         drag-end      (drag-end-fn drag-move drag-end-atom on-end)]
;     (println "DRAGGING")
;     (on-start)
;     (reset! drag-end-atom drag-end)
;     (events/listen js/window EventType.MOUSEMOVE drag-move)
;     (events/listen js/window EventType.MOUSEUP drag-end))))
;
;(defn get-bcr [svg-root]
;  (.log js/console "RECT" (-> svg-root
;                              r/dom-node
;                              .getBoundingClientRect))
;  (-> svg-root
;      r/dom-node
;      .getBoundingClientRect))
;
;(defn move-point [state-atom svg-root p]
;  ;(println "MOVING" p)
;  (fn [x y]
;    (let [bcr (get-bcr svg-root)]
;      (.log js/console "bcr" bcr)
;      (swap! state-atom assoc :x (- x (.-left bcr)) :y (- y (.-top bcr))))))
;
;(defn control-point []
;  (let [state (reagent/atom {:x      0
;                             :y      0
;                             :matrix [1 0 0 1 0 0]})]
;    (fn []
;      ;(.log js/console "STATE" @state)
;      [:circle.control-point
;       {:r        10
;        :on-start (fn [x] (println "starting"))
;        :on-mouse-move  (move-point state (r/current-component) :c)
;        :cx       0
;        :cy       0}])))