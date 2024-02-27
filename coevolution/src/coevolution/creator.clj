(ns coevolution.creator
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [coevolution.specificationsutility :as ssu]
            [timeseriesevolution.timeseriesevolution :as tse]
            [coevolution.validators.validator :as cval]
            [coevolution.utility :as ceu]
            [coevolution.specificationfileparser :as sfp]
            [com.climate.claypoole :as cp]
            [clojure.core.reducers :as r]
            [flatland.ordered.set :as os]
            [flatland.ordered.map :as om]
            [commonbusiness.Utility :as cutl]
            [commonbusiness.NetworkUtility :as cnutl]
            [commonbusiness.XMLParser :as cxmlp]
            [taoensso.timbre :as timbre]
            [commonbusiness.Analytics :as anl]
            [commonbusiness.BNMutations :as bm]
            [random-seed.core :refer :all])
  (:import (coevolution.models TribeAnalytics World Tribe)))

;contains functionalities to create a world out of the given specification folders and files
;see also Kreator |,,|

;correct working mutate networks
(defn mutate-network
  [network numOfMutations gene-list isRandom random-function]
    (loop [i 1 mutnet network]
    (if (>= i numOfMutations)
      mutnet
      (let [mutinfos (bm/applyMutations (:networkTrees mutnet) (:analytics-list mutnet) gene-list isRandom random-function)]
        (let [mutnet (cnutl/updateNetwork mutnet mutinfos (anl/createAnalytics (:networkOutput mutinfos)))]
          (recur (inc i) mutnet))))))

;; function called when more individulas of population are needed. the new individulas are created by mutating the initial population.
(defn create-population-remainder
  [population-remainder population random-function input-population-size gene-list mutation-per-network no-threads]
  ;(println "population remainder")
  ;(timbre/report "Add individuums population remainder" population-remainder " inputpopsize " input-population-size "mutationpernetwork" mutation-per-network)
  (let [pool (cp/threadpool no-threads)]
    (let [futures (doall (take population-remainder (repeatedly #(cp/future pool (mutate-network (nth population (random-function input-population-size)) mutation-per-network
                                                                                            gene-list false random-function)))))]
      (cp/shutdown pool)
      ;(timbre/report "futer number " (count (flatten (map deref futures))))
      (into [] (flatten (map deref futures))))))

(defn create-initial-population
  [input-files-folder gene-list population-size random-function mutation-per-network no-threads]
  ;;(info "Lexical env: create-initial-population ")
  ;;(info "Lexical env: create-initial-population " (get-env))
  ;; if the number of the input files and hence input population is equal to the population size, the input population
  ;; is returned else for the difference between them, new networks are created and added to the list.
  (let [file-paths (cutl/get-files-in-folder input-files-folder)]
    (let [input-population-size (count file-paths)]
      (let [input-population (into [] (cp/pmap no-threads #(cnutl/createNetwork (str input-files-folder "/" %) (cxmlp/create-equation-list (str input-files-folder "/" %))) file-paths))]
        ;(println "input pops")
        (let [population-remainder (- population-size input-population-size)]
          (if (> population-remainder 0)
            (into [] (concat input-population (create-population-remainder population-remainder input-population random-function input-population-size gene-list mutation-per-network no-threads)))
            input-population))))))

;creates an inital-tribe for each specified tribe in the tribe-folder
;if there are specifications missing for a specific tribe, the default-specs can be used, they will be
;the default file can also be used to set values that are the same for all tribes
;does not set tribe fitness
;diversity-enforcer increase the initial mutationrate through multiplication with mutation value
(defn create-initial-tribe
  [tribe-folder default-specification random-function diversity-enforcer no-threads]
  (let [specs (first (ssu/get-specifications-for-file (str tribe-folder "/specs") 0 default-specification))
        gene-list (tse/create-gene-list (str tribe-folder "/input"))]
    (let [population (create-initial-population (str tribe-folder "/input")
                                                    gene-list
                                                    (:population-size specs)
                                                    random-function
                                                    (* (:mutation-per-network specs) diversity-enforcer)
                                                    no-threads)]
      ;;a new tribe has age=0 -> that means it has evolves 0 generations now
      (let [tribeAnalytics (TribeAnalytics. 0
                                            nil
                                            [0]
                                            {0 (:comparison-type specs)}
                                            {0 (:inital-condition specs)}
                                            {0 (:states specs)}
                                            {0 (:population-size specs)}
                                            {0 (:mutation-per-network specs)}
                                            {0 (:mutation-percentage specs)}
                                            {0 (:parents-num specs)}
                                            {0 (:fitness-thresold specs)}
                                            {0 (:initial-state-pool-size specs)}
                                            {0 (:generations-number specs)}
                                            {0 (:path-size specs)})]
        (Tribe. tribe-folder gene-list specs tribeAnalytics (into [] population))))))

;returns a list with each gene that occurs in the world
(defn get_world_gene_list
  [world]
  (into () (into (os/ordered-set) (r/flatten (reduce #(conj %1 (:gene-list %2)) (os/ordered-set) (-> world :tribe-list)))))
  )

;returns an inital-state with n genes names Gene1, Gene2 ...
;Format is a map with the gene name as keyword and true or false as value
(defn get-random-initial-state
  [n]
  (into (om/ordered-map) (zipmap (into [] (map #(conj (keyword (str %2 %1))) (range 1 (inc n)) (repeat n "Gene"))) (into [] (repeatedly n #(nth [true false] (rand-int 2))))))
  )

;parses all tribes and specs in the folder, ignores not valid tribes
(defn create-world
  [experiment-folder default-spec-file random-function no-threads]
  ;check if the folder is valid
  (do (set-random-seed! (rand-int Integer/MAX_VALUE))
   (if (cval/valid-experiment-folder? experiment-folder)
    (let [file-paths (ceu/get-files-in-folder experiment-folder)]
      ;just use correct tribe folders
      (let [tribe-paths (filter #(cval/correct-tribe-folder-structure? (str experiment-folder "/" %))
                                (filter #(not (empty? (ceu/get-files-in-folder (str experiment-folder "/" %)))) file-paths))]
        (let [default-specification (sfp/pares-default-spec-file default-spec-file)]
          (let [world (World. experiment-folder (cp/pmap no-threads #(create-initial-tribe (str experiment-folder "/" %) default-specification random-function 4 no-threads) tribe-paths) nil)]
            (assoc world :world-gene-list (get_world_gene_list world))))
        )))))