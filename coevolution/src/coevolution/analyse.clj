(ns coevolution.analyse
  (:require [timeseriesevolution.initialstates :as iss]
            [commonbusiness.networkevaluation :as ne]
            [coevolution.utility :as ceu]
            [coevolution.initialstateexchange :as ceis]
            [commonbusiness.fitnessfunctionimpl :as coffi]))

;Contain functions for the analysis of networks


;this function returns the attractors reached from the inital-conditions in the track list
(defn get_attractors_from_initial_conditions-track
  [tribe individual-number]
  (let [state-list (-> tribe :TribeAnalytics :inital-condition-track)]
    (let [initial-states (reduce (fn [new-map timepoint-list]
                                   (conj new-map {(key timepoint-list)
                                                  (mapv (fn [state] (reduce #(conj %1 (iss/create-initial-states (:inital-condition %2)
                                                                                                                 (-> tribe :gene-list) 1 rand-int)) [] state))
                                                        (reduce #(conj %1 {:inital-condition %2} ) [] (val timepoint-list) ))}))
                                 {} state-list)]
      (let [network (nth (-> tribe :population-list) individual-number)]
        (reduce (fn [new-map timepoint] (conj new-map {(key timepoint)
                                                       (mapv (fn [state] (reduce #(conj %1 (ne/find-state-attractor
                                                                                             (-> network :networkTrees)
                                                                                             (-> tribe :gene-list) %2
                                                                                             (-> tribe :specifications :path-size))) [] state)) (apply concat (val timepoint)))}))
                {} initial-states)))))

;returns the attractor reached from the given state
;state as bool vector [Gene1 Gene2 Gene3 ...]
(defn get_attractors_from_state
  [tribe individual-number state]
  (let [network (nth (-> tribe :population-list) individual-number)]
    (ne/find-state-attractor (-> network :networkTrees) (-> tribe :gene-list) state  (-> tribe :specifications :path-size)))
  )

;returns the attractor reached from the given state
;state as bool vector [Gene1 Gene2 Gene3 ...]
(defn get_sequence_from_state
  [tribe individual-number state]
  (let [network (nth (-> tribe :population-list) individual-number)]
    (ne/generate-sequence (-> network :networkTrees) (-> tribe :gene-list) state  (-> tribe :specifications :path-size)))
  )



;makes the true false attractor representation to a 01 representation
(defn attractor-list-boolvec->binstr
  [attractor-list]
    (reduce (fn [new-map timepoint] (conj new-map {(key timepoint ) (reduce (fn [new-vec attractor] (conj new-vec (map ceu/boolvec->binstr attractor))) [] (val timepoint))})) {} attractor-list)
  )


;makes the true false attractor representation to a 01 representation
(defn attractor-list-boolvec->int
  [attractor-list]
  (reduce (fn [new-map timepoint] (conj new-map {(key timepoint ) (reduce (fn [new-vec attractor] (conj new-vec (map ceu/boolvec->int attractor))) [] (val timepoint))})) {} attractor-list)
  )


;names the attractors with the state within the attractor named as int
(defn attractor-list-boolvec->attractorname
  [attractor-list]
  (reduce (fn [new-map timepoint] (conj new-map {(key timepoint ) (reduce (fn [new-vec cond] (reduce #(conj new-vec (conj %1 (first (sort (map ceu/boolvec->int %2))))) [] cond)) [] (val timepoint))})) {} attractor-list)
  )


;get the fitness of the individual starting from state
;state as boolan vector [true true ... false]
(defn get-fitness-of-state
  [tribe individual-number state]
  (float (coffi/calculate-hamming-ditstance-attractor-dP (get_attractors_from_state tribe individual-number state) (-> tribe :specifications :states))))


;returns the attractor for each entry in the initial-conditions-track, based on the current fittest tribe network
;attractors are named with integers
(defn attractor-names-from-initial-conditions-track
  [tribe no-threads]
    (let [fittest-individual-num (ceis/get-fittest-individual tribe no-threads)]
      (let [attractor-list (get_attractors_from_initial_conditions-track tribe fittest-individual-num)]
       (into (sorted-map) (attractor-list-boolvec->attractorname attractor-list)))))

;returns the attractor-map based on the share_mode
(defn get-attractor-map
  [world tribe-num share_mode max-states no-threads]
  (case share_mode
    "overwrite" (vals (attractor-names-from-initial-conditions-track (ceu/get-tribe world tribe-num) no-threads))
    "append" (if (empty? max-states)
               ((attractor-names-from-initial-conditions-track (ceu/get-tribe world tribe-num) no-threads)
                    (last (sort (keys (-> (ceu/get-tribe world tribe-num) :TribeAnalytics :inital-condition-track)))))
               (attractor-names-from-initial-conditions-track (ceu/get-tribe world tribe-num) no-threads))))