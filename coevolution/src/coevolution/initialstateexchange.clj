(ns coevolution.initialstateexchange
  (:require [timeseriesevolution.initialstates :as iss]
            [commonbusiness.networkevaluation :as ne]
            [coevolution.fitnessutility :as cef]
            [com.climate.claypoole :as cp]
            [clojure.set :as set]
            [taoensso.timbre :refer [error]]
            [coevolution.utility :as ceu]))

;contains all functions for initial state exchange. These are designed for experiment1 and 2


;returns a list with the intersecting genes in the world
(defn get_possible_shared_genes
  [world]
  (sort (into () (reduce set/intersection (map #(into (sorted-set) %) (reduce #(conj %1 (:gene-list %2)) #{} (-> world :tribe-list)))))))

;return n random world_shared_genes
(defn get_rand_shard_genes
  [world n]
  (let [intersection-genes-list (get_possible_shared_genes world)]
    (take n (shuffle intersection-genes-list) )))

;returns an attractor for each inital-condition
;cond-num describes witch initial-condition attractor pair should be used
(defn get_attractors_from_initial_conditions
  [tribe individual-number cond-num]
  (let [initial-states (cef/create-initial-states tribe)]
    ;(initial-states 0) is necessary due to double vector, if you want more attractors out of one inital condition change
    ; die 3. parameter in create-initial-states
    (let [network (nth (-> tribe :population-list) individual-number)]
      (mapv #(ne/find-state-attractor (-> network :networkTrees) (-> tribe :gene-list) % (-> tribe :specifications :path-size)) (nth initial-states cond-num)))))

;returns the position of the gene in the state context
(defn get_gene_position
  [gene-list gene]
  (.indexOf gene-list gene))

;returns the number of the fittest individual in the tribe
(defn get-fittest-individual
  [tribe no-threads]
  (nth (last (ceu/sort-map-by-value-desc (cef/compare-tribe-to-specification tribe no-threads "ignore-wildcards"))) 0))

;returns the attractor that could be reached from an individual from the initial-conditions
;cond-num describes witch initial-condition attractor pair should be used
(defn get-attractor-with-best-fitness-in-tribe
  [tribe cond-num no-threads]
  (let [num-best-individual (get-fittest-individual tribe no-threads)]
    (get_attractors_from_initial_conditions tribe num-best-individual cond-num)))

;get an addition for new initial_conditions from a tribe, based on the most occurring values for the
;shared_genes in the attractors reached by the individuals of the tribe
;cond-num describes witch initial-condition attractor pair should be used
;shared_genes can be
; 1. a list with genes (uses all with modifier = 1, or each with prob =modifier for modifier <1)
; 2. "all" uses all possible genes , modifier will be ignored
; 3. "rand" choose from all possible-shared-genes in the world
; (For modifier as Integer chooses exactly modifier much randomly, for modifier between 0-1 each possible gene will be choosen with probability=modifier
; if modifier is between 0 and 1 the genes will be chosen with probability = modifier
(defn get_new_inital_conditions
  [world tribe cond-num shared_genes modifier no-threads]
  (let [shared_genes (case shared_genes
                       "all" (get_possible_shared_genes world)
                       "rand" (if (= Long (class modifier))
                                (get_rand_shard_genes world modifier)
                                (if (and (>= modifier 0) (< modifier 1))
                                  (random-sample modifier (get_possible_shared_genes world))
                                  (error "For shard genes rand the modifier has to be an Integer to choose exactly modifier many random, or between 0 and 1 to choose each possible one with probability modifier "))
                                )
                       (if (or (= clojure.lang.PersistentList (class shared_genes)) (= clojure.lang.LazySeq (class shared_genes)))
                         (if (= modifier 1)
                           shared_genes
                           (if (and (>= modifier 0) (<= modifier 1))
                             (random-sample modifier shared_genes)
                             (error "For a given list with shared genes the modifier has to be between 0-1!"))
                           )
                         (error "Select a correct List with shared genes or all or rand ")
                         )
                       )
        attractor (get-attractor-with-best-fitness-in-tribe tribe cond-num no-threads)]
    ;take a random state from the attractor
    (let [attractor_state (nth (take 1 (shuffle (nth attractor 0))) 0)]
      (into {} (sort (reduce (fn [list gene] (conj list {(keyword gene) (nth attractor_state (get_gene_position (-> tribe :gene-list) gene))})) {} shared_genes))))
    ))

