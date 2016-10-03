(ns micircle.math)

(def pi (.-PI js/Math))

(defn polar-to-cartesian
  "Convert polar coordinates to cartesian coordinates.
  TODO: confirm that x and y should not be reversed."
  [center-x center-y radius angle-in-degrees]
  (let [angle-in-radians (* (- angle-in-degrees 90) (/ pi 180.0))]
    {:x (+ center-x (* radius (.cos js/Math angle-in-radians)))
     :y (+ center-y (* radius (.sin js/Math angle-in-radians)))}))


(defn describe-link
  [{:keys [center-x center-y radius start-angle-1 start-angle-2 end-angle-1 end-angle-2]}]
  (let [start-1        (polar-to-cartesian center-x center-y radius start-angle-1)
        start-2        (polar-to-cartesian center-x center-y radius start-angle-2)
        end-1          (polar-to-cartesian center-x center-y radius end-angle-1)
        end-2          (polar-to-cartesian center-x center-y radius end-angle-2)
        ;end-inner            (polar-to-cartesian center-x center-y radius start-angle)
        large-arc-flag (if (<= (- end-angle-1 start-angle-1) 180) 0 1)]
    (clojure.string/join " " ["M" (:x start-1) (:y start-1)
                              ;"L" 100 100
                              "A" radius radius 0 large-arc-flag 1 (:x start-2) (:y start-2)
                              "Q" 200 200 (:x end-1) (:y end-1)
                              "A" radius radius 0 large-arc-flag 1 (:x end-2) (:y end-2)
                              "Q" 200 200 (:x start-1) (:y start-1)


                              ;"A" radius radius 0 large-arc-flag 0 (:x end-inner) (:y end-inner)
                              ])))


(defn describe-link-new
  [{:keys [center-x center-y radius start-angle-1 start-angle-2 end-angle-1 end-angle-2]}]
  (let [start-1        (polar-to-cartesian center-x center-y radius start-angle-1)
        start-2        (polar-to-cartesian center-x center-y radius start-angle-2)
        end-1          (polar-to-cartesian center-x center-y radius end-angle-1)
        end-2          (polar-to-cartesian center-x center-y radius end-angle-2)
        anchor-1       (polar-to-cartesian center-x center-y (- radius 50) (- start-angle-1 10))
        anchor-2       (polar-to-cartesian center-x center-y (- radius 50) (- start-angle-2 10))
        ;end-inner            (polar-to-cartesian center-x center-y radius start-angle)
        large-arc-flag (if (<= (- end-angle-1 start-angle-1) 180) 0 1)]
    (clojure.string/join " " ["M" (:x start-1) (:y start-1)
                              ;"L" 100 100
                              "A" radius radius 0 large-arc-flag 1 (:x start-2) (:y start-2)
                              "C" (:x anchor-2) (:y anchor-2) (:x anchor-2) (:y anchor-2) (:x anchor-2) (:y anchor-2)
                              "Q" 150 150 (:x end-1) (:y end-1)
                              "A" radius radius 0 large-arc-flag 1 (:x end-2) (:y end-2)
                              "Q" 150 150 (:x start-1) (:y start-1)


                              ;"A" radius radius 0 large-arc-flag 0 (:x end-inner) (:y end-inner)
                              ])))

(defn describe-arc
  [x y radius start-angle end-angle & [thickness]]
  (let [thickness      (if thickness thickness 10)
        start-inner    (polar-to-cartesian x y radius end-angle)
        end-inner      (polar-to-cartesian x y radius start-angle)
        start-outer    (polar-to-cartesian x y (+ thickness radius) end-angle)
        end-outer      (polar-to-cartesian x y (+ thickness radius) start-angle)
        large-arc-flag (if (<= (- end-angle start-angle) 180) 0 1)]
    (clojure.string/join " " ["M" (:x start-inner) (:y start-inner)
                              "A" radius radius 0 large-arc-flag 0 (:x end-inner) (:y end-inner)
                              "L" (:x end-outer) (:y end-outer)
                              "A" (+ thickness radius) (+ thickness radius) 0 large-arc-flag 1 (:x start-outer) (:y start-outer)
                              "Z"])))



#_(defn describe-arc
    [x y radius start-angle end-angle]
    (let [start          (polar-to-cartesian x y radius end-angle)
          end            (polar-to-cartesian x y radius start-angle)
          large-arc-flag (if (<= (- end-angle start-angle) 180) 0 1)]
      (clojure.string/join " " ["M" (:x start) (:y start)
                                "A" radius radius 0 large-arc-flag 0 (:x end) (:y end)
                                "L" (:x (polar-to-cartesian x y (+ radius 10) start-angle)) (:y (polar-to-cartesian x y (+ radius 10) start-angle))
                                "A" radius radius 0 large-arc-flag 0 (:x end) (:y end)
                                ])))

