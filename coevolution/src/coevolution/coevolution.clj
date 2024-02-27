(ns coevolution.coevolution
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [coevolution.creator :as cec]
            [coevolution.evolver :as cee]
            [coevolution.initialstateexchange :as ceis]
            [coevolution.specificationsupdater :as cesu]
            [coevolution.utility :as ceu]
            [coevolution.fitnessutility :as cef]
            [coevolution.writer :as cew]
            [coevolution.experiments :as ceex]
            [coevolution.survivorselection :as cess]
            [coevolution.analyse :as cean]
            [timeseriesevolution.initialstates :as iss]
            [taoensso.timbre :as timbre]
            [commonbusiness.networkevaluation :as ne]
            [commonbusiness.fitnessfunctionimpl :as coffi]
            [commonbusiness.jsonreading :as jr]
            [commonbusiness.NetworkUtility :as cnutl]
            [com.climate.claypoole :as cp]
            [clojure.math.numeric-tower :as math]
            [commonbusiness.Utility :as cutl]
            [flatland.ordered.map :as om]
            [flatland.ordered.set :as os]
            [commonbusiness.BNMutations :as bm]
            [random-seed.core :refer :all]
            [commonbusiness.Analytics :as anl])
  (:import (timeseriesevolution.models specification)))


;this section contains some working examples and experiments as well as some snippets for analysis


;example code for the biology model
(comment
  (timbre/set-level! :trace)
  (def bioexp (ceu/try-times 100 (cec/create-world "CEFiles/biology" "CEFiles/biology/specs_0.txt" rand-int 12)))
  (def bioexp (cee/update-world-fitness bioexp "ignore-wildcards" 12))
  (def bioexp (cee/evolve-world bioexp "fittest" "ignore-wildcards" rand-int 12))

  (ceu/view-tribe-specs bioexp 0)
  (ceu/view-tribe-specs bioexp 1)
  (def tribe1 (ceu/get-tribe bioexp 1))
  (def tribe0 (ceu/get-tribe bioexp 0))


  (ceex/experiment_n_times_4_tribes_saveResults ceex/experiment_4
                                                "CEFiles/biology2"
                                                "CEFiles/output/biology"
                                                30
                                                20
                                                rand-int
                                                4
                                                0
                                                "fittest"
                                                "ignore-wildcards"
                                                "all"
                                                1
                                                "overwrite"
                                                nil)


  (ceex/experiment_n_times_2_tribes_saveResults ceex/experiment_bio
                                                "CEFiles/biology"
                                                "CEFiles/output/biology"
                                                20
                                                20
                                                rand-int
                                                12
                                                0
                                                "fittest"
                                                "ignore-wildcards"
                                                "all"
                                                1
                                                "overwrite"
                                                nil))

;example code for debugging
(comment
  (def no-threads 4)
  (def world (ceu/try-times 100 (cec/create-world "CEFiles/genericV2" "CEFiles/genericV2/specs_0.txt" rand-int 4)))
  (def world (cee/update-world-fitness world "ignore-wildcards" 4))
  (def world (cee/evolve-world world "elitism" rand-int 12))
  (ceu/view-tribe-specs world 0)
  (def time-point (ceu/get-tribe world 1))
  (def pop-list (-> tribe :population-list))

  (def tribe (ceu/get-tribe world 0))


  (def initial-states (cef/create-initial-states tribe))
  (def define-state (cef/extract-define-state tribe))
  (def filt-states (cef/filter-initial-states tribe define-state initial-states))
  (def goal-states (cef/goal-states->boolvec (cef/create-goal-states tribe filt-states define-state)))

  (def network (nth (-> tribe :population-list) 0))

  (cef/handleTribeFitnessCalculation goal-states network tribe initial-states filt-states)

  (nth goal-states 0)
  (cef/find-attractor-without-wildcards tribe network initial-states filt-states 1)

  (cef/find-attractor tribe network initial-states 0)

  (def indexes (cef/get-index-to-delete tribe filt-states 0))




)

;example experiment calls
(comment
  (ceex/experiment_n_times_4_tribes ceex/experiment_1_crosslink
                                    "CEFiles/experiment1_14"
                                    "CEFiles/output"
                                    2
                                    10
                                    rand-int
                                    4
                                    0
                                    "fittest"
                                    "ignore-wildcards"
                                    "all"
                                    1
                                    "overwrite"
                                    nil)


  (ceex/experiment_n_times_4_tribes ceex/experiment_2
                                    "CEFiles/experiment2_14"
                                    "CEFiles/output"
                                    50
                                    80
                                    rand-int
                                    4
                                    0
                                    "fittest"
                                    "ignore-wildcards"
                                    "all"
                                    1
                                    "overwrite"
                                    nil)


  (ceex/experiment_n_times_4_tribes_saveResults ceex/experiment_1_crosslink
                                                "CEFiles/genericT4"
                                                "CEFiles/output/other"
                                                1
                                                10
                                                rand-int
                                                4
                                                0
                                                "fittest"
                                                "ignore-wildcards"
                                                "all"
                                                1
                                                "overwrite"
                                                nil)

  (ceex/experiment_n_times_2_tribes_saveResults ceex/experiment_1
                                                "CEFiles/generic"
                                                "CEFiles/output/other"
                                                1
                                                20
                                                rand-int
                                                4
                                                0
                                                "roulette"
                                                "ignore-wildcards"
                                                "all"
                                                1
                                                "overwrite"
                                                nil))

;scripts for runtime analysis
(comment
  (def duration
    (jr/read-json-vector
      "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\output\\runtime\\durationmapV3.json"))
  (def dur (reduce #(conj %1 {(Integer/parseInt (name (key %2))) (Double/parseDouble  (format "%.2f" (/ (val %2) 10)))}) (sorted-map)
          (into {} (map (fn [timepoint] {timepoint (reduce #(+ %1 (timepoint (val %2))) 0 duration)}) (keys (-> duration :Run1))))))
  (vals dur)
  )

;scripts for attractor-map analysis
(comment
  (def attractor-map (jr/read-json-vector "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\output\\biology\\attmap_att3_large\\attractor-map.json"))

  (def attractor-map1 (attractor-map :tribe1))
  (def attractor-map2 (attractor-map :tribe2))
  (def attractor-map3 (attractor-map :tribe3))


  (vals attractor-map1)
  (frequencies (map count (map vals (map frequencies (vals attractor-map1)))))
  (frequencies (map count (map vals (map frequencies (vals attractor-map2))))) ;append
  (frequencies (map count (map frequencies (map vals (vals attractor-map3))))) ;append - 4




  )

