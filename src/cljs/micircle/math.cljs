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
  [{:keys [center-x center-y radius start-angle-1 start-angle-2 end-angle-1 end-angle-2]} & [options]]
  (let [
        pinch-percent        (js/parseFloat (:pinch-percent options))
        pinch-depth          (js/parseInt (:pinch-depth options))
        ptc                  (partial polar-to-cartesian center-x center-y)
        flare                (js/parseInt (:flare options))

        start-1              (ptc radius start-angle-1)
        start-2              (ptc radius start-angle-2)
        end-1                (ptc radius end-angle-1)
        end-2                (ptc radius end-angle-2)

        anchor-start-2       (ptc (- radius pinch-depth) (- start-angle-2 (* pinch-percent (- start-angle-2 start-angle-1))))
        outer-handle-start-2 (ptc (- radius (/ pinch-depth flare)) (- start-angle-2 0))
        inner-handle-start-2 (ptc (- radius (/ pinch-depth flare)) (- start-angle-2 (* pinch-percent (- start-angle-2 start-angle-1))))

        anchor-end-1         (ptc (- radius pinch-depth) (+ end-angle-1 (* pinch-percent (- end-angle-2 end-angle-1))))
        outer-handle-end-1   (ptc (- radius (/ pinch-depth flare)) (+ end-angle-1 0))
        inner-handle-end-1   (ptc (- radius (/ pinch-depth flare)) (+ end-angle-1 (* pinch-percent (- end-angle-2 end-angle-1))))

        anchor-end-2         (ptc (- radius pinch-depth) (- end-angle-2 (* pinch-percent (- end-angle-2 end-angle-1))))
        outer-handle-end-2   (ptc (- radius (/ pinch-depth flare)) (- end-angle-2 0))
        inner-handle-end-2   (ptc (- radius (/ pinch-depth flare)) (- end-angle-2 (* pinch-percent (- end-angle-2 end-angle-1))))

        anchor-start-1       (ptc (- radius pinch-depth) (+ start-angle-1 (* pinch-percent (- start-angle-2 start-angle-1))))
        outer-handle-start-1 (ptc (- radius (/ pinch-depth flare)) (+ start-angle-1 0))
        inner-handle-start-1 (ptc (- radius (/ pinch-depth flare)) (+ start-angle-1 (* pinch-percent (- start-angle-2 start-angle-1))))



        large-arc-flag       (if (<= (- end-angle-1 start-angle-1) 180) 0 1)]
    (clojure.string/join " " ["M" (:x start-1) (:y start-1)
                              "A" radius radius 0 large-arc-flag 1 (:x start-2) (:y start-2)
                              "C" (:x outer-handle-start-2) (:y outer-handle-start-2) (:x inner-handle-start-2) (:y inner-handle-start-2) (:x anchor-start-2) (:y anchor-start-2)
                              "Q" 250 250 (:x anchor-end-1) (:y anchor-end-1)
                              "C" (:x inner-handle-end-1) (:y inner-handle-end-1) (:x outer-handle-end-1) (:y outer-handle-end-1) (:x end-1) (:y end-1)
                              "A" radius radius 0 large-arc-flag 1 (:x end-2) (:y end-2)
                              "C" (:x outer-handle-end-2) (:y outer-handle-end-2) (:x inner-handle-end-2) (:y inner-handle-end-2) (:x anchor-end-2) (:y anchor-end-2)
                              "Q" 250 250 (:x anchor-start-1) (:y anchor-start-1)
                              "C" (:x inner-handle-start-1) (:y inner-handle-start-1) (:x outer-handle-start-1) (:y outer-handle-start-1) (:x start-1) (:y start-1)
                              "Z"])))

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

(defn describe-arc-text-path
  [x y radius start-angle end-angle & [thickness]]
  (let [thickness      (if thickness thickness 10)
        start-inner    (polar-to-cartesian x y radius start-angle)
        end-inner      (polar-to-cartesian x y radius end-angle)
        large-arc-flag (if (<= (- end-angle start-angle) 180) 0 1)]
    (clojure.string/join " " ["M" (:x start-inner) (:y start-inner)
                              "A" radius radius 0 large-arc-flag 1 (:x end-inner) (:y end-inner)])))

(defn describe-tick
  [x y radius angle & [thickness]]
  (let [thickness (if thickness thickness 10)
        start     (polar-to-cartesian x y radius angle)
        end       (polar-to-cartesian x y (+ radius 5) angle)]
    (clojure.string/join " " ["M" (:x start) (:y start)
                              "L" (:x end) (:y end)])))

(defn place-at-radius [center-x center-y radius angle]
  (polar-to-cartesian center-x center-y radius angle))


(defn describe-triangle [center-x center-y radius start-angle end-angle & [thickness]]

  (clojure.string/join " " ["M" 12 0
                            "L" 24 24
                            "L" 0 24
                            "Z"])
  (let [thickness (if thickness thickness 20)
        {x :x y :y} (polar-to-cartesian center-x center-y (+ thickness radius) (- end-angle start-angle))]
    (println "thickness" thickness)
    (clojure.string/join " " ["M" 3 -6
                              "L" 12 12
                              "L" -6 12
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

