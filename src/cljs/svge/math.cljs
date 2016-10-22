(ns svge.math)

(def pi (.-PI js/Math))

(defn polar->cartesian
  "Convert polar coordinates to cartesian coordinates."
  [center-x center-y radius angle-in-degrees]
  (let [angle-in-radians (* (- angle-in-degrees 90) (/ pi 180.0))]
    {:x (+ center-x (* radius (.cos js/Math angle-in-radians)))
     :y (+ center-y (* radius (.sin js/Math angle-in-radians)))}))

(defn cartesian->polar
  "Convert cartesian coords to polar coordinates."
  [x y]
  (* (.atan2 js/Math x y) (/ 180 pi)))

(defn place-at-radius
  ([radius angle center-x center-y]
   (println "running with center" radius angle center-x center-y)
   (polar->cartesian center-x center-y radius angle))
  ([radius angle]
   (polar->cartesian 0 0 radius angle)))

(defn angle-between-points
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (cartesian->polar (- y2 y1) (- x2 x1)))
