(ns svge.components
  (:require
    [svge.factory :as f]))

(def draw-dictionary
  {:circle-arc-to f/arc-to
   :radiate-out   f/radiate-out
   :move-to       f/move-to
   :line-to       f/line-to
   :close         f/close})

(defn path->str [path-vec]
  (clojure.string/join " " (flatten (map :result path-vec))))

(defn make-path [& segments]
  (->
    (reduce (fn [total next-operation]
              (if (keyword? next-operation)
                (conj total {:instruction next-operation
                             :result      ((get draw-dictionary next-operation))})
                (let [[op args] next-operation]
                  (conj total {:instruction next-operation
                               :result      ((get draw-dictionary op) args total)})))) [] segments)
    path->str))

(defn annotate-path [& segments]
  (println "GOT"
           (reduce (fn [total next-operation]
                     (if (keyword? next-operation)
                       (conj total nil)
                       (let [[op args] next-operation]
                         (conj total [:circle {:r 2 :cx (:x args) :cy (:y args)}])))) [] segments))
  (reduce (fn [total next-operation]
            (if (keyword? next-operation)
              (conj total nil)
              (let [[op args] next-operation]
                (conj total [:circle {:r 2 :cx (:x args) :cy (:y args)}])))) [:g] segments))


