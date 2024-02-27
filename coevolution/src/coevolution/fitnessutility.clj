(ns coevolution.fitnessutility
  (:require [commonbusiness.fitnessfunctionimpl :as coffi]
            [commonbusiness.networkevaluation :as ne]
            [com.climate.claypoole :as cp]
            [flatland.ordered.map :as om]
            [coevolution.utility :as ceu]
            [flatland.ordered.set :as os]))

;contain functions for fitness-calculations, !CAVE the fitness functions from module timeseriesevolution will not work here
;because of not updating already calculated fitness values

;find a attractor from the initial-states, contains wildcards
(defn find-attractor
  [tribe network initial-states cond-num ]
  (mapv #(ne/find-state-attractor (-> network :networkTrees) (-> tribe :gene-list) (vals %) (-> tribe :specifications :path-size))
        (nth initial-states cond-num)))

;generate die indexes of states that can be deleted because of wildcards
(defn get-index-to-delete
  [tribe filt-states cond-num]
  (let [indexes (filter #(not (contains? (nth (nth filt-states cond-num) 0) (nth (mapv keyword (-> tribe :gene-list)) %))) (range (count (-> tribe :gene-list))))]
    indexes))

(defn find-attractor-without-wildcards
  [tribe network initial-states filt-states cond-num]
  (let [indexes (get-index-to-delete tribe filt-states cond-num)]
    (if (empty? indexes)
      (find-attractor tribe network initial-states cond-num)
      (mapv (fn [att] (mapv (fn [state] (ceu/dissoc-idx state indexes))  att)) (find-attractor tribe network initial-states cond-num)))))

(defn extract-define-state
  [tribe]
  (let [defined-states (mapv #(set (reduce concat %)) (mapv (fn [att] (mapv (fn [state] (reduce #(conj %1 (key %2)) (os/ordered-set) state)) att))
                                                            (-> tribe :specifications :states)))]
    (mapv (fn [att-num] (filterv #(contains? (nth defined-states att-num) %) (map keyword (-> tribe :gene-list)))) (range (count defined-states)))))

;returns the values of the genes that are necessary for the fitness calculation, just the ones that are also defined
;in the attractor specs
(defn filter-initial-states
  [tribe defined-states initial-states]
  (mapv (fn [att-num] (mapv (fn [state-num] (reduce  #(conj %1 {%2 ((nth (nth initial-states att-num) state-num) %2)}) (om/ordered-map)
                                                     (nth defined-states att-num))) (range (count (nth initial-states att-num)))))
        (range (count initial-states))))

; makes fitness calculation correct, the firstPaths argument is the target state
; cave I think raghdas ne/find-stata-attractor works not correct
;this function is used if all goal-states are defined
(defn handleTribeFitnessCalculation-add-wilds
  [goal-states network tribe initial-states]
  (let [fitness (map (fn [cond-num] (coffi/calculateHammingDitstancePerAttractorsDP (nth goal-states cond-num)
                                          (find-attractor tribe network initial-states cond-num))) (range (count goal-states)))]
    (/ (reduce + fitness) (count goal-states))))

; makes fitness calculation correct, the firstPaths argument is the target state
; cave I think raghdas ne/find-stata-attractor works not correct
(defn handleTribeFitnessCalculation
  [goal-states network tribe initial-states filt-states]
  (let [fitness (map (fn [cond-num] (coffi/calculateHammingDitstancePerAttractorsDP (nth goal-states cond-num)
                                    (find-attractor-without-wildcards tribe network initial-states filt-states cond-num))) (range (count goal-states)))]
    (/ (reduce + fitness) (count goal-states))))

;creates inital-state-poo-size much initial-states with all occurring genes in the tribe,
;chooses random values for wildcard genes
(defn create-initial-states
  [tribe]
  (mapv (fn [initial-condition] (into [] (take (-> tribe :specifications :initial-state-pool-size)
                     (apply concat (repeatedly (-> tribe :specifications :initial-state-pool-size) (fn [] (map (fn [init-states]
                      (conj (ceu/create-rand-state tribe) init-states ))
                                initial-condition))))))) (-> tribe :specifications :inital-condition)))


(defn create-rand-state-filtered
  [tribe initial-states define-state cond-num]
  (into (om/ordered-map) (map vector (nth define-state cond-num)
                              (repeatedly (count (-> tribe :gene-list)) #(nth [true false] (rand-int 2))))))

;takes the :states and returns a vector with the states
;fills also wildcards
(defn create-goal-states-add-wildcards
  [tribe initial-states]
  (reduce (fn [goal-states cond-num]
            (conj goal-states (into [] (take (count (nth initial-states cond-num)))
                                    ;fill wildcards
                                    (repeat (mapv #(conj (ceu/create-rand-state tribe) %) (nth (-> tribe :specifications :states) cond-num))))))
          [] (range 0 (count (-> tribe :specifications :inital-condition)))))

(defn create-goal-states
  [tribe filtered-states define-state]
  (reduce (fn [goal-states cond-num]
            (conj goal-states (into [] (take (count (nth filtered-states cond-num)))
                                    ;fill wildcards
                                    (repeat (mapv #(conj (create-rand-state-filtered tribe filtered-states define-state cond-num) %) (nth (-> tribe :specifications :states) cond-num))))))
          [] (range 0 (count (-> tribe :specifications :inital-condition)))))

;takes the ordered-map of goal-states and returns just the values as bool vec.
(defn goal-states->boolvec
  [goal-states]
  (reduce (fn [cond-vec cond] (conj cond-vec (reduce (fn [att-vec att] (conj att-vec (mapv (fn [state] (reduce #(conj %1 (val %2)) [] state)) att))) [] cond))) [] goal-states))

;this mode compares all genes for the fitness
;it adds goal states if they are not defined
(defn tribe-fitness-calculation-add-goal-wilds
  [networks-score tribe no-threads]
  (let [initial-states (create-initial-states tribe)]
           (let [goal-states (goal-states->boolvec (create-goal-states-add-wildcards tribe initial-states))]
               (let [population-scores (cp/pmap no-threads #(handleTribeFitnessCalculation-add-wilds goal-states % tribe initial-states) (-> tribe :population-list))]
                  (let [population-scores (map float population-scores)]
                     (if (or (nil? networks-score) (= 0 (count networks-score)))
                        ;; if there is no networks-score; in this case from a previous specification; it returns the calculated scores.
                        (doall (into [ ] population-scores))
                        ;; if there is calculated scores, it averages both scores.
                        (mapv #(/ (+ %1 %2) 2) networks-score population-scores)))))))

;compares only the genes that are defined in the attractor,
;
(defn tribe-fitness-calculation
  [networks-score tribe no-threads]
  (let [initial-states (create-initial-states tribe)]
    (let [define-state (extract-define-state tribe)]
      (let [filt-states (filter-initial-states tribe define-state initial-states)]
        (let [goal-states (goal-states->boolvec (create-goal-states tribe filt-states define-state))]
      (let [population-scores (cp/pmap no-threads #(handleTribeFitnessCalculation goal-states % tribe initial-states filt-states) (-> tribe :population-list))]
        (let [population-scores (map float population-scores)]
          (if (or (nil? networks-score) (= 0 (count networks-score)))
            ;; if there is no networks-score; in this case from a previous specification; it returns the calculated scores.
            (doall (into [ ] population-scores))
            ;; if there is calculated scores, it averages both scores.
            (mapv #(/ (+ %1 %2) 2) networks-score population-scores)))))))))

;mode = ignore-wildcards -> compares only the genes that are defined in the attractor
;mode = add-wildcards -> add all wildcards in the attractor definition randomly
(defn compare-tribe-to-specification
  [tribe no-threads mode]
  (if (= mode "ignore-wildcards")
    (let [fitness-list (tribe-fitness-calculation nil tribe no-threads)]
      (zipmap (into [] (range (count fitness-list))) fitness-list))
    (let [fitness-list (tribe-fitness-calculation-add-goal-wilds nil tribe no-threads)]
      (zipmap (into [] (range (count fitness-list))) fitness-list))
    ))













