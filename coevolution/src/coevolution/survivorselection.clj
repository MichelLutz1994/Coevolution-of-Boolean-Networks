(ns coevolution.survivorselection
  (:require [taoensso.timbre :as timbre]))

;returns the fitness inverted (1-fit) als relative verteilung
(defn inverted-rel-fitness
  [sortedHammingDistances]
  (let [invDist (map #(vector (key %) (- 1 (val %))) sortedHammingDistances)
        sumFit (reduce #(+ %1 (nth %2 1)) 0 invDist)]
    ;(comment (let [relFit (map #(vector (nth % 0) (/ (nth % 1) sumFit)) invDist)] (drop-last (reduce #(conj %1 (vector (nth %2 0) (+ (nth (first %1) 1) (nth %2 1)))) '([nil 0]) relFit))))
    (map #(vector (nth % 0) (/ (nth % 1) sumFit)) invDist)
    ))

(defn states->treemap
  "Builds a navigable map of `partial sum -> state`"
  [states]
  (->> states
       (reduce (fn [[entries sum] [k v]]
                 (let [sum' (+ sum v)]
                   [(conj entries [sum' k]) sum']))
               [[] 0])
       first
       (into {})
       (java.util.TreeMap.)))

(defn random-state [^java.util.NavigableMap nm]
  (.getValue
    (.ceilingEntry nm (rand))))


;manages the survivor selection
;modus can be:
; "fittest" takes just the fittest individuals
; "elitism" takes the 20% fittest and the rest random
; "roulette"
(defn survivor-selection
  [time-point sortedHammingDistances selection-modus]
  (case selection-modus
    "fittest" (take (-> time-point :specifications :population-size) sortedHammingDistances)
    ;roulette
    "roulette" (let [newDistances (into (hash-map) sortedHammingDistances)
                     nm (states->treemap (inverted-rel-fitness sortedHammingDistances))]
                    (let [selection (take (-> time-point :specifications :population-size) (repeatedly #(random-state nm)))]
                        (reduce #(conj %1 (vector %2 (newDistances %2))) (list) selection )))
    "elitism" (let [numbest10percent (int (* (-> time-point :specifications :population-size) 0.8))]
               (let [numrest (- (-> time-point :specifications :population-size) numbest10percent)]
                 (do
                   ;(timbre/info "popSIze"  (-> time-point :specifications :population-size) "num10best" numbest10percent "numrest" numrest )
                   ;(timbre/info (concat (doall (take numbest10percent sortedHammingDistances)) (doall (take numrest (shuffle (drop numrest sortedHammingDistances))))) )
                   (concat (doall (take numbest10percent sortedHammingDistances)) (doall (take numrest (shuffle (drop numrest sortedHammingDistances))))))))
    (timbre/error "No correct selection-modus, use fittest elitism or roulette")))

