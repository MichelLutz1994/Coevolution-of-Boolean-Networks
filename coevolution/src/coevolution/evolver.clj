(ns coevolution.evolver
  (:require [com.climate.claypoole :as cp]
            [commonbusiness.NetworkUtility :as cnutl]
            [coevolution.survivorselection :as cess]
            [coevolution.fitnessutility :as cef]
            [coevolution.creator :as cec]
            [taoensso.timbre :refer [trace debug info warn]]
            [timeseriesevolution.fitnessfunctionimpl :as ffi]
            [timeseriesevolution.initialstates :as iss]
            [timeseriesevolution.timeseriesevolution :as tse]
            [commonbusiness.Utility :as cutl]))

;contains all functions for the evolution of the tribes and the whole world



;; the function is to evolve a generation, the evolution is either through mutation or multiple combination methods, the percentage of which method is followed
;; is determined by muation-percentage variable.
(defn evolve-generation
  [generation geneList numOfMutations isRandom random-function parents-num mutation-percentage no-threads]
  (let [mutation-size (int (/ (* mutation-percentage (count generation)) 100))]
    (let [individuals-multiple-combination (into [] (take (* numOfMutations (- 100 mutation-size)) (repeat (cnutl/evolve-network-mulitple-combination generation (count geneList) random-function parents-num))))]
      (let [individuals-mutated-ids (take mutation-size (shuffle (range (count generation))))]
        (loop [individuals-mutated [] current 0]
          (if (= current (count individuals-mutated-ids))
            (cnutl/combining-evoluation-outputs individuals-mutated individuals-multiple-combination mutation-percentage)
            (recur (conj individuals-mutated (cec/mutate-network (nth generation (nth individuals-mutated-ids current)) numOfMutations geneList isRandom random-function)) (inc current))))))))


;computes the current tribe fitness as average over the population
(defn evaluate-tribe-fitness
  [tribe]
  ;(/ (reduce #(+ (:fitnessValue %1) (:fitnessValue %2)) {:fitnessValue 0} (:population-list tribe)) (-> tribe :specifications :population-size))
  (float (/ (loop [tot (count (-> tribe :population-list))
                   i 0
                   sum 0]
              (if (>= i tot)
                sum
                (recur tot (inc i) (+ sum (:fitnessValue ((:population-list tribe) i)))))
              ) (-> tribe :specifications :population-size))))

;evaluate the individual-fitness of each individual of the tribe
;wildcard-mode = ignore-wildcards -> compares only the genes that are defined in the attractor
;wildcard-mode = add-wildcards -> add all wildcards in the attractor definition randomly
(defn update-tribe-fitness
  [tribe wildcard-mode no-threads]
  ;; first sort the list of hamming distances
    (let [sortedHammingDistances (sort-by last (cef/compare-tribe-to-specification tribe no-threads wildcard-mode))]
      ;; sets the fitness value into the included networks
      (let [includedNetworks (mapv #(cnutl/set-fitness (-> tribe :population-list) %) sortedHammingDistances)]
        (let [tribe (assoc tribe :population-list includedNetworks)]
          (let [tribe (assoc
                        tribe
                        :TribeAnalytics
                        (assoc (-> tribe :TribeAnalytics)
                          :tribeFitness
                          (conj (-> tribe :TribeAnalytics :tribeFitness) {(-> tribe :TribeAnalytics :age) (evaluate-tribe-fitness tribe)})))]
            (debug "Set initial fitness " (-> tribe :folderPath) "AVG: " (vals (first (-> tribe :TribeAnalytics :tribeFitness))) " " (mapv #(:fitnessValue %) includedNetworks))
            tribe)))))

;update the fitness of all tribes in parallel
;mode = ignore-wildcards -> compares only the genes that are defined in the attractor
;mode = add-wildcards -> add all wildcards in the attractor definition randomly
(defn update-world-fitness
  [world wildcard-mode no-threads]
  (let [tribes (cp/pmap no-threads #(update-tribe-fitness % wildcard-mode no-threads) (-> world :tribe-list))]
    (assoc world :tribe-list tribes)))



;evolves the tribe for generations-number times
;updates age to the total number of generations the tribe has already evolved
;for selection modus could be choose: elitism or roulette
;wildcard-mode = ignore-wildcards -> compares only the genes that are defined in the attractor
;wildcard-mode = add-wildcards -> add all wildcards in the attractor definition randomly
(defn evolve-tribe
  [tribe selection-modus wildcard-mode random-function no-threads]
    ;evolve the tribe generations-number times
    (loop [current 1 time-point tribe]
      (if (> current (-> time-point :specifications :generations-number))
        ;all generations evolved
        time-point
        ;evolve the population one time
        (let [defFileName (-> (first (-> time-point :population-list)) :fileName)]
          (let [generation (into [] (concat (-> time-point :population-list)
                                          (evolve-generation (-> time-point :population-list)
                                                                   (-> time-point :gene-list)
                                                                   (-> time-point :specifications :mutation-per-network)
                                                                   true
                                                                   random-function
                                                                   (-> time-point :specifications :parents-num)
                                                                   (-> time-point :specifications :mutation-percentage)
                                                                   no-threads)))]
          (let [generation (map #(assoc % :fileName (if (= (:fileName %) nil) defFileName (:fileName %))) generation)]
           (let [time-point (assoc time-point :population-list generation)]
            (let [sortedHammingDistances (sort-by last (cef/compare-tribe-to-specification time-point no-threads wildcard-mode))]
            ;;suvivor selection
            (let [includedHammingDistances (cess/survivor-selection time-point sortedHammingDistances selection-modus)]
              (let [includedNetworks (mapv #(cnutl/set-fitness generation %) includedHammingDistances)]
                (let [time-point (assoc time-point :population-list includedNetworks)]
                  ;;update Tribe fitness
                  (let [time-point (assoc time-point :TribeAnalytics (assoc (-> time-point :TribeAnalytics) :age (+ 1 (-> time-point :TribeAnalytics :age))))]
                    (let [time-point (assoc time-point :TribeAnalytics
                                                       (assoc (-> time-point :TribeAnalytics) :tribeFitness
                                                                                              (conj (-> time-point :TribeAnalytics :tribeFitness)
                                                                                                    {(-> time-point :TribeAnalytics :age) (evaluate-tribe-fitness time-point)})))]
                      (trace "Age " (-> time-point :TribeAnalytics :age) (-> time-point :folderPath) " AVG: " (vals (first (-> time-point :TribeAnalytics :tribeFitness))) "Indi:" (mapv #(:fitnessValue %) includedNetworks))
                      (recur (inc current) time-point)))
                  )))))))))))


;evolves the whole world one time in parallel, that means each tribe will be evolved generations-numbers times
;see the age parameter to see how old a tribe really is. This feature is to simulate different mutation rates
;in different tribes, you can specify almost each mutation parameter for each tribe by specifying there
;specification file,
(defn evolve-world
  [world selection-modus wildcard-mode random-function no-threads]
  (let [tribes (cp/pmap no-threads #(evolve-tribe % selection-modus wildcard-mode random-function no-threads) (-> world :tribe-list))]
    (assoc world :tribe-list tribes)))