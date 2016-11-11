(ns micircle.math
  (:require [com.rpl.specter :as s]))

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

(defn svg-arc [radius start end]
  (let [large-arc-flag (if (<= (- (get-in end [:end-angle]) (get-in start [:start-angle])) 180) 0 1)]
    ["A" radius radius 0 large-arc-flag 1 (:x end) (:y end)]))

(defn svg-move-to [{:keys [x y]}]
  ["M" x y])

(defn svg-line-to [{:keys [x y]}]
  ["L" x y])

(defn svg-quad-curve [{ctrl-x :x ctrl-y :y} {:keys [x y] :as end}]
  ["Q" ctrl-x ctrl-y x y])

(defn svg-close []
  ["Z"])


(def min-max (juxt (partial apply min) (partial apply max)))

(defn center-angle-of-participants [feature]
  (let [angles (s/select [:sequenceData s/ALL (s/multi-path :start-angle :end-angle)] feature)]
    (/ (apply + (min-max angles)) 2)))

(defn pinchold [{:keys [angle]} radius]
  (let [pc      (partial polar-to-cartesian 0 0)
        pinched (pc (- radius 10) (- angle 5))
        bow-out (pc (- radius 20) (- angle 1))]
    ["Q" (:x pinched) (:y pinched) (:x bow-out) (:y bow-out)]))




(defn build-link-path-old [center-x center-y radius {:keys [from-feature to-features] :as all}]

  (let [starting-locations (sort-by :start-angle (map #(select-keys % [:start-angle :end-angle]) (get from-feature :sequenceData)))
        ending-locations   (sort-by :start-angle (map #(select-keys % [:start-angle :end-angle]) (get (first to-features) :sequenceData)))]


    (let [p->c                             (partial polar-to-cartesian center-x center-y radius)
          pc                               (partial polar-to-cartesian center-x center-y)
          starting-locations               (map (fn [{:keys [start-angle end-angle] :as m}]
                                                  (assoc {}
                                                    :start (assoc (p->c start-angle) :angle start-angle)
                                                    :end (assoc (p->c end-angle) :angle end-angle))) starting-locations)
          ending-locations                 (map (fn [{:keys [start-angle end-angle] :as m}]
                                                  (assoc {}
                                                    :start (assoc (p->c start-angle) :angle start-angle)
                                                    :end (assoc (p->c end-angle) :angle end-angle))) ending-locations)
          center-angle                     (center-angle-of-participants from-feature)
          all-starting-locations-angles    (flatten (map (fn [n]
                                                           [(get-in n [:start :angle])
                                                            (get-in n [:end :angle])]) starting-locations))

          all-ending-locations-angles      (flatten (map (fn [n]
                                                           [(get-in n [:start :angle])
                                                            (get-in n [:end :angle])]) ending-locations))

          starting-locations-average-angle (/ (apply + all-starting-locations-angles) (count all-starting-locations-angles))
          ending-locations-average-angle   (/ (apply + all-ending-locations-angles) (count all-ending-locations-angles))
          center-point-between-all-angles  (/ (+ starting-locations-average-angle ending-locations-average-angle) 2)]

      ;(.log js/console "starting-locations" starting-locations)
      ;(.log js/console "ending-locations" ending-locations)
      ;(.log js/console "done" center-point-between-all-angles)



      (clojure.string/join
        " "
        (flatten [; Start at the very beginning of the first binding region
                  "M" (:x (:start (first starting-locations))) (:y (:start (first starting-locations)))
                  ; Arc to the end of the binding region
                  "A" radius radius 0 0 1 (:x (:end (first starting-locations))) (:y (:end (first starting-locations)))
                  ; Squeeze the ribbon near end for a tapered effect

                  ;(pinch (:end (first starting-locations)) radius)


                  ;"Q" (pc (- radius 30) (:y (:end (first starting-locations))))
                  ; If we have another segment in this interaction then we need to repeatedly "sawtooth"
                  ])))))


(defn pinch [{:keys [angle]} radius]
  (let [ptc           (partial polar-to-cartesian 0 0)
        pinch-depth   75
        pinch-percent 0.3
        flare         30

        pinch-at-1    (ptc (- radius 40) (- angle 5))
        pinch-at-2    (ptc (- radius 40) (- angle 5))
        end-at        (ptc (- radius 100) (+ angle 5))]
    ["C" (:x pinch-at-1) (:y pinch-at-1) (:x pinch-at-2) (:y pinch-at-2) (:x end-at) (:y end-at)]))


(defn build-link-path [center-x center-y radius {:keys [from-feature to-features] :as all}]

  (let [starting-locations (sort-by :start-angle (map #(select-keys % [:start-angle :end-angle]) (get from-feature :sequenceData)))
        ending-locations   (sort-by :start-angle (map #(select-keys % [:start-angle :end-angle]) (get (first to-features) :sequenceData)))]


    (let [p->c                             (partial polar-to-cartesian center-x center-y radius)
          pc                               (partial polar-to-cartesian center-x center-y)
          starting-locations               (map (fn [{:keys [start-angle end-angle] :as m}]
                                                  (assoc {}
                                                    :start (assoc (p->c start-angle) :angle start-angle)
                                                    :middle (assoc (p->c (/ (+ start-angle end-angle) 2)) :angle (/ (+ start-angle end-angle) 2))
                                                    :end (assoc (p->c end-angle) :angle end-angle))) starting-locations)
          ending-locations                 (map (fn [{:keys [start-angle end-angle] :as m}]
                                                  (assoc {}
                                                    :start (assoc (p->c start-angle) :angle start-angle)
                                                    :middle (assoc (p->c (/ (+ start-angle end-angle) 2)) :angle (/ (+ start-angle end-angle) 2))
                                                    :end (assoc (p->c end-angle) :angle end-angle))) ending-locations)
          center-angle                     (center-angle-of-participants from-feature)
          all-starting-locations-angles    (flatten (map (fn [n]
                                                           [(get-in n [:start :angle])
                                                            (get-in n [:end :angle])]) starting-locations))

          all-ending-locations-angles      (flatten (map (fn [n]
                                                           [(get-in n [:start :angle])
                                                            (get-in n [:end :angle])]) ending-locations))
          centroid                         (let [all-x      (map (comp :x :middle) (concat starting-locations ending-locations))
                                                 all-y      (map (comp :y :middle) (concat starting-locations ending-locations))
                                                 all-angles (map (comp :angle :middle) (concat starting-locations ending-locations))]
                                             {:x     (/ (apply + all-x) (count all-x))
                                              :y     (/ (apply + all-y) (count all-x))
                                              :angle (/ (apply + all-angles) (count all-angles))})

          half-centroid                    (pc 100 (:angle centroid))

          starting-locations-average-angle (/ (apply + all-starting-locations-angles) (count all-starting-locations-angles))
          ending-locations-average-angle   (/ (apply + all-ending-locations-angles) (count all-ending-locations-angles))
          center-point-between-all-angles  (/ (+ starting-locations-average-angle ending-locations-average-angle) 2)]

      (println "HALF" half-centroid)

      ;(.log js/console "starting-locations" starting-locations)
      ;(.log js/console "ending-locations" ending-locations)
      ;;(.log js/console "done" center-point-between-all-angles)
      ;
      ;(.log js/console "REDUCTION"
      ;      (reduce (fn [total [current next]]
      ;                (apply conj total
      ;                       [])) [] (take-while not-empty (iterate rest starting-locations))))
      ;
      ;(.log js/console "centroid" centroid)



      (clojure.string/join
        " "
        (flatten [; Start at the very beginning of the first binding region
                  "M" (:x (:start (first starting-locations))) (:y (:start (first starting-locations)))

                  ; Arc to the end of the binding region
                  "A" radius radius 0 0 1 (:x (:end (first starting-locations))) (:y (:end (first starting-locations)))


                  ; If we have another starting location then arc back to its beginning
                  (if (not-empty (rest starting-locations))
                    (reduce (fn [total [current next]]
                              (apply conj total
                                     ["A" radius radius 0 0 1 (:x (:start current)) (:y (:start current))
                                      "A" radius radius 0 0 1 (:x (:end current)) (:y (:end current))]))
                            ["A" radius radius 0 0 0 (:x half-centroid) (:y half-centroid)]
                            (take-while not-empty (iterate rest (rest starting-locations)))))

                  ;"A" radius radius 0 0 0 (:x (:start (first ending-locations))) (:y (:start (first ending-locations)))

                  "Q" (:x half-centroid) (:y half-centroid) (:x (:start (first ending-locations))) (:y (:start (first ending-locations)))

                  ; Arc to the end of the binding region
                  "A" radius radius 0 0 1 (:x (:end (first ending-locations))) (:y (:end (first ending-locations)))

                  ;"Q" (:x centroid) (:y centroid) (:x (:start (first starting-locations))) (:y (:start (first starting-locations)))

                  "Q" 0 0 (:x (:start (first starting-locations))) (:y (:start (first starting-locations)))

                  ;["A" radius radius 0 0 0 (:x centroid) (:y centroid)]
                  ; Squeeze the ribbon near end for a tapered effect

                  ;(pinch (:end (first starting-locations)) radius)


                  ;"Q" (pc (- radius 30) (:y (:end (first starting-locations))))
                  ; If we have another segment in this interaction then we need to repeatedly "sawtooth"
                  ]))

      )))



(defn centroid [center-x center-y radius {:keys [from-feature to-features] :as all}]

  (let [starting-locations (sort-by :start-angle (map #(select-keys % [:start-angle :end-angle]) (get from-feature :sequenceData)))
        ending-locations   (sort-by :start-angle (map #(select-keys % [:start-angle :end-angle]) (get (first to-features) :sequenceData)))]


    (let [p->c                             (partial polar-to-cartesian center-x center-y radius)
          pc                               (partial polar-to-cartesian center-x center-y)
          starting-locations               (map (fn [{:keys [start-angle end-angle] :as m}]
                                                  (assoc {}
                                                    :start (assoc (p->c start-angle) :angle start-angle)
                                                    :middle (assoc (p->c (/ (+ start-angle end-angle) 2)) :angle (/ (+ start-angle end-angle) 2))
                                                    :end (assoc (p->c end-angle) :angle end-angle))) starting-locations)
          ending-locations                 (map (fn [{:keys [start-angle end-angle] :as m}]
                                                  (assoc {}
                                                    :start (assoc (p->c start-angle) :angle start-angle)
                                                    :middle (assoc (p->c (/ (+ start-angle end-angle) 2)) :angle (/ (+ start-angle end-angle) 2))
                                                    :end (assoc (p->c end-angle) :angle end-angle))) ending-locations)
          center-angle                     (center-angle-of-participants from-feature)
          all-starting-locations-angles    (flatten (map (fn [n]
                                                           [(get-in n [:start :angle])
                                                            (get-in n [:end :angle])]) starting-locations))

          all-ending-locations-angles      (flatten (map (fn [n]
                                                           [(get-in n [:start :angle])
                                                            (get-in n [:end :angle])]) ending-locations))
          centroid                         (let [all-x      (map (comp :x :middle) (concat starting-locations ending-locations))
                                                 all-y      (map (comp :y :middle) (concat starting-locations ending-locations))
                                                 all-angles (map (comp :angle :middle) (concat starting-locations ending-locations))]
                                             {:x     (/ (apply + all-x) (count all-x))
                                              :y     (/ (apply + all-y) (count all-x))
                                              :angle (/ (apply + all-angles) (count all-angles))})

          starting-locations-average-angle (/ (apply + all-starting-locations-angles) (count all-starting-locations-angles))
          ending-locations-average-angle   (/ (apply + all-ending-locations-angles) (count all-ending-locations-angles))
          center-point-between-all-angles  (/ (+ starting-locations-average-angle ending-locations-average-angle) 2)]
      centroid)))


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

(defn shortest-distance [d1 d2]
  (let [angle (mod (.abs js/Math (- d1 d2)) 360)]
    (if (> angle 180)
      (- 360 angle)
      angle)))

(defn triangle-path []
  (clojure.string/join " " ["M" 12 0
                            "L" 24 24
                            "L" 0 24
                            "Z"]))

(defn place-at-radius [center-x center-y radius start-angle end-angle]
  (polar-to-cartesian center-x center-y radius (/ (+ start-angle end-angle) 2)))


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




#_(let [sections [(svg-move-to (:start (first starting-locations)))
                  (svg-arc radius
                           (:start (first starting-locations))
                           (:end (first starting-locations)))
                  ;[(svg-line-to control-point)]
                  [(svg-quad-curve control-point-left control-point)]
                  (if-not (empty? (rest starting-locations))
                    (map (fn [[next remaining]]
                           [(svg-quad-curve control-point-left (:start next))
                            ;(svg-line-to (:start next))
                            (svg-arc radius
                                     (:start next)
                                     (:end next))])
                         (take-while not-empty (iterate rest (rest starting-locations)))))
                  ;(svg-line-to (:start (first ending-locations)))
                  (svg-quad-curve control-point-left (:start (first ending-locations)))
                  (svg-arc radius
                           (:start (first ending-locations))
                           (:end (first ending-locations)))

                  #_(svg-arc radius
                             (:start (first starting-locations))
                             (:end (first starting-locations)))
                  (svg-line-to control-point)
                  ]]

    (.log js/console "sections" sections)

    (clojure.string/join " " (flatten sections)))
