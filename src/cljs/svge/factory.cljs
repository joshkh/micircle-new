(ns svge.factory
  (:require [svge.math :as math]))




(defn arc-to [{:keys [radius large-arc-flag sweep-flag x y pos] :or {x 0 y 0}} stack]
  [(if (= pos :relative) "a" "A") radius radius 0 large-arc-flag sweep-flag x y])

(defn move-to [{:keys [x y]} stack]
  ["M" x y])

(defn line-to [{:keys [x y]} stack]
  ["L" x y])

(defn quad-curve-to [{ctrl-x :x ctrl-y :y} {:keys [x y] :as end} stack]
  ["Q" ctrl-x ctrl-y x y])

(defn close []
  ["Z"])

(defn radiate-out [{:keys [distance angle]} stack]
  (if-not angle
    (let [[current-1 current-2] (take 2 (reverse stack))
          derived-angle (math/angle-between-points
                          (select-keys (second (get current-2 :instruction)) [:x :y])
                          (select-keys (second (get current-1 :instruction)) [:x :y]))
          {x :x y :y} (math/place-at-radius (* -1 distance) (- derived-angle 90))]
      ["l" x y])
    (let [{x :x y :y} (math/place-at-radius (* -1 distance) angle)]
      ["l" x y])))