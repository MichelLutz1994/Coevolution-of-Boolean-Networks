(ns coevolution.experiments
  (:require [coevolution.utility :as ceu]
            [coevolution.creator :as cec]
            [coevolution.evolver :as cee]
            [taoensso.timbre :as timbre]
            [coevolution.initialstateexchange :as ceis]
            [coevolution.specificationsupdater :as cesu]
            [coevolution.fitnessutility :as cef]
            [coevolution.writer :as cew]
            [coevolution.analyse :as cean]
            [com.climate.claypoole :as cp]))

;contains all experiment setups


;sharing initial conditions
;tribe 0 shares states of his fitest attractor with tribe1 as new inital-condition.
;tribe0 -> tribe1
(defn experiment_1
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 1 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
          (let [new_inital_conditions (ceis/get_new_inital_conditions world (ceu/get-tribe world 0) cond-num shared_genes modifier no-threads)]
            (let [world (cesu/update-initial-states world 1 cond-num new_inital_conditions no-threads share_mode max-states)]
              (let [fit (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 1) no-threads wildcard-mode))]
                (let [world (cesu/substitute-the-last-tribeFitness world 1 (float (/ (reduce + fit) (count fit))))]
                  (recur (inc timepoint) world))))))))))

;sharing initial conditions
;tribe 0 shares states of his fitest attractor with tribe1 as new inital-condition.
;tribe0 -> tribe1  igf->crc
(defn experiment_bio
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 1 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
          (let [new_inital_conditions (ceis/get_new_inital_conditions world (ceu/get-tribe world 0) (rand-int 4) shared_genes modifier no-threads)]
            (let [world (cesu/update-initial-states world 1 0 new_inital_conditions no-threads share_mode max-states)]
              (let [fit (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 1) no-threads wildcard-mode))]
                (let [world (cesu/substitute-the-last-tribeFitness world 1 (float (/ (reduce + fit) (count fit))))]
                  (recur (inc timepoint) world))))))))))

;sharing initial conditions
;tribe 0 shares states of his fitest attractor with tribe1 as new inital-condition.
;tribe0 -> tribe1  igf->crc
(defn experiment_bio_no_interaction
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 1 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
                  (recur (inc timepoint) world))))))


;sharing initial conditions, false just for fun
;tribe 0 shares states of his fitest attractor with tribe1 as new inital-condition.
;tribe0 -> tribe1
(defn experiment_bio_crc->igf
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 1 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
          (let [new_inital_conditions (ceis/get_new_inital_conditions world (ceu/get-tribe world 1) 0 shared_genes modifier no-threads)]
            (let [world (cesu/update-initial-states world 0 0 new_inital_conditions no-threads share_mode max-states)]
              (let [world (cesu/update-initial-states world 0 1 new_inital_conditions no-threads share_mode max-states)]
                (let [world (cesu/update-initial-states world 0 2 new_inital_conditions no-threads share_mode max-states)]
                  (let [world (cesu/update-initial-states world 0 3 new_inital_conditions no-threads share_mode max-states)]
                    (let [fit (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 0) no-threads wildcard-mode))]
                      (let [world (cesu/substitute-the-last-tribeFitness world 0 (float (/ (reduce + fit) (count fit))))]
                        (recur (inc timepoint) world)))))))))))))

;test of sharing initial conditions
;one tribe shares one state of his fitest attractor with another tribe as new inital-condition.
;tribe0 -> tribe1
;tribe3 -> tribe2
(defn experiment_1_crosslink
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 1 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (timbre/info "Experiment timepoint " timepoint)
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
          (let [new_inital_conditions0 (ceis/get_new_inital_conditions world (ceu/get-tribe world 0) cond-num shared_genes modifier no-threads)
                new_inital_conditions3 (ceis/get_new_inital_conditions world (ceu/get-tribe world 3) cond-num shared_genes modifier no-threads)]
            (let [world (cesu/update-initial-states world 1 cond-num new_inital_conditions0 no-threads share_mode max-states)]
              (let [world (cesu/update-initial-states world 2 cond-num new_inital_conditions3 no-threads share_mode max-states)]
                (let [fit1 (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 1) no-threads wildcard-mode))
                      fit2 (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 2) no-threads wildcard-mode))]
                  ;update the tribeFitnessTrack
                  (let [world (cesu/substitute-the-last-tribeFitness world 1 (float (/ (reduce + fit1) (count fit1))))]
                    (let [world (cesu/substitute-the-last-tribeFitness world 2 (float (/ (reduce + fit2) (count fit2))))]
                      (recur (inc timepoint) world))))))))))))

;each step the fitness of tribe0 and tribe1 is compared, the fitter one shares one of his attractor states
(defn experiment_2
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 0 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
          (timbre/info timepoint "with old initial state" "AVG_Tribe0" (ceu/get-tribe-fitness world 0)
                       "AVG_Tribe1" (ceu/get-tribe-fitness world 1))
          (if (< (ceu/get-tribe-fitness world 0) (ceu/get-tribe-fitness world 1))
            ;tribe0 fitter than tribe1
            (let [new_inital_conditions (ceis/get_new_inital_conditions world (ceu/get-tribe world 0) cond-num shared_genes modifier no-threads)]
              (let [world (cesu/update-initial-states world 1 cond-num new_inital_conditions no-threads share_mode max-states)]
                (let [fit (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 1) no-threads wildcard-mode))]
                  (let [world (cesu/substitute-the-last-tribeFitness world 1 (float (/ (reduce + fit) (count fit))))]
                    (timbre/info timepoint "with new initial state" "AVG_Tribe0" (ceu/get-tribe-fitness world 0)
                                 "AVG_Tribe1" (ceu/get-tribe-fitness world 1) " :" fit)
                    (recur (inc timepoint) world)))))
            ;tribe1 fitter than tribe0
            (let [new_inital_conditions (ceis/get_new_inital_conditions world (ceu/get-tribe world 1) cond-num shared_genes modifier no-threads)]
              (let [world (cesu/update-initial-states world 0 cond-num new_inital_conditions no-threads share_mode max-states)]
                (let [fit (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 0) no-threads wildcard-mode))]
                  (let [world (cesu/substitute-the-last-tribeFitness world 0 (float (/ (reduce + fit) (count fit))))]
                    (timbre/info timepoint "with new initial state" "AVG_Tribe0" (ceu/get-tribe-fitness world 0)
                                 "AVG_Tribe1" (ceu/get-tribe-fitness world 1) " :" fit)
                    (recur (inc timepoint) world)))))))))))

;changes the mutation_rate, if a tribe is less fit the mutationrate will be increased
(defn experiment_3
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 0 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
          (timbre/info timepoint "AVG_Tribe0" (ceu/get-tribe-fitness world 0)
                       "AVG_Tribe1" (ceu/get-tribe-fitness world 1))
          (if (< (ceu/get-tribe-fitness world 0) (ceu/get-tribe-fitness world 1))
            ;tribe0 fitter than tribe1
            (let [new-mut-rate0  (inc (ceu/get-tribe-spec world 0 :mutation-per-network))
                  new-mut-rate1  (max 1 (dec (ceu/get-tribe-spec world 1 :mutation-per-network)))]
              (timbre/info "Tribe 0  :old " (ceu/get-tribe-spec world 0 :mutation-per-network) " new: " new-mut-rate0
                           "Tribe 1  :old " (ceu/get-tribe-spec world 1 :mutation-per-network) " new: " new-mut-rate1)
              (let [world (cesu/update-world world 0 :mutation-per-network new-mut-rate0 no-threads)]
                (let [world (cesu/update-world world 1 :mutation-per-network new-mut-rate1 no-threads)]
                  (recur (inc timepoint) world))))
            ;tribe1 fitter than tribe0
            (let [new-mut-rate0  (max 1 (dec (ceu/get-tribe-spec world 0 :mutation-per-network)))
                  new-mut-rate1  (inc (ceu/get-tribe-spec world 1 :mutation-per-network))]
              (timbre/info "Tribe 0  :old " (ceu/get-tribe-spec world 0 :mutation-per-network) " new: " new-mut-rate0
                           "Tribe 1  :old " (ceu/get-tribe-spec world 1 :mutation-per-network) " new: " new-mut-rate1)
              (let [world (cesu/update-world world 0 :mutation-per-network new-mut-rate0 no-threads)]
                (let [world (cesu/update-world world 1 :mutation-per-network new-mut-rate1 no-threads)]
                  (recur (inc timepoint) world))))))))))

;test the update-initial-states function with to append, overwrite and append 4 functions against each other
;all tribes are the same , 1,2,3 become the same inital states each run
(defn experiment_4
  [folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "/specs_0.txt") rand-int no-threads))]
    (loop [timepoint 0 world (cee/update-world-fitness world wildcard-mode no-threads)]
      (timbre/info "Experiment timepoint " timepoint)
      (if (> timepoint steps)
        world
        (let [world (cee/evolve-world world selection-mode wildcard-mode random-function no-threads)]
          (let [new_inital_conditions (ceis/get_new_inital_conditions world (ceu/get-tribe world 0) cond-num shared_genes modifier no-threads)]
            (let [world (cesu/update-initial-states world 1 cond-num new_inital_conditions no-threads "overwrite" nil)]
              (let [world (cesu/update-initial-states world 2 cond-num new_inital_conditions no-threads "append" nil)]
                (let [world (cesu/update-initial-states world 3 cond-num new_inital_conditions no-threads "append" [4])]
                  (let [fit1 (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 1) no-threads wildcard-mode))
                        fit2 (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 2) no-threads wildcard-mode))
                        fit3 (vals (cef/compare-tribe-to-specification (ceu/get-tribe world 3) no-threads wildcard-mode))]
                    ;update the tribeFitnessTrack
                    (let [world (cesu/substitute-the-last-tribeFitness world 1 (float (/ (reduce + fit1) (count fit1))))]
                      (let [world (cesu/substitute-the-last-tribeFitness world 2 (float (/ (reduce + fit2) (count fit2))))]
                        (let [world (cesu/substitute-the-last-tribeFitness world 3 (float (/ (reduce + fit3) (count fit3))))]
                          (recur (inc timepoint) world))))))))))))))

;repeats the experiment function n times and saves all results
;the parameters are defined as follows
;  experiment = name of the experiment function
;  folder = experiment folder
;  output-folder : folder that will contain the outputs
;  repeats : specifics how many times the experiment will be repeated
;  steps = number of evolve repeats (total generations each run = steps * generation-num)
;  random-function = use rand-int
;  no-threads : number of threads
;  selection-mode : could be "fittest" "elitism" or "roulette"
;  shared_genes : see get_new_inital_conditions function description ("all", "rand" ...)
;  modifier : see get_new_inital_conditions function description
;  share_mode : see update-initial-states function description ("overwrite", "append" , "append n"
;   max-states : for sharing mode append n ; if you want to append all choose nil
;OUTPUT:
;  1. tribe0-tribe3.json :contains the overall fitness curve of all runs (output folder)
;  2. attractormap comment out because low runtime bigger networks
;  3. writes each run the fittest network of each tribe to the tribe output folder. Names then output<run number>_<net-number>
(defn experiment_n_times_2_tribes
  [experiment folder output-folder repeats steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (loop [resultTribe0 (list)
         resultTribe1 (list)
         time 1]
    (if (> time repeats)
      ;experiment finished write results
      (do
        (cew/write-json output-folder resultTribe0 "tribe0")
        (cew/write-json output-folder resultTribe1 "tribe1"))
      ;execute experimental run
      (do (timbre/info "Experiment run " time)
          (let [exp (experiment folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states)]
              (recur
                (conj resultTribe0 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 0) :TribeAnalytics :tribeFitness))))
                (conj resultTribe1 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 1) :TribeAnalytics :tribeFitness))))
                (inc time)))))))

;repeats the experiment function n times and saves all results
;the parameters are defined as follows
;  experiment = name of the experiment function
;  folder = experiment folder
;  output-folder : folder that will contain the outputs
;  repeats : specifics how many times the experiment will be repeated
;  steps = number of evolve repeats (total generations each run = steps * generation-num)
;  random-function = use rand-int
;  no-threads : number of threads
;  selection-mode : could be "fittest" "elitism" or "roulette"
;  shared_genes : see get_new_inital_conditions function description ("all", "rand" ...)
;  modifier : see get_new_inital_conditions function description
;  share_mode : see update-initial-states function description ("overwrite", "append" , "append n"
;   max-states : for sharing mode append n ; if you want to append all choose nil
;OUTPUT:
;just the fitness
(defn experiment_n_times_3_tribes
  [experiment folder output-folder repeats steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (loop [resultTribe0 (list)
         resultTribe1 (list)
         resultTribe2 (list)
         time 1]
    (if (> time repeats)
      ;experiment finished write results
      (do
        (cew/write-json output-folder resultTribe0 "tribe0")
        (cew/write-json output-folder resultTribe1 "tribe1")
        (cew/write-json output-folder resultTribe2 "tribe2"))
      ;execute experimental run
      (do (timbre/report "Experiment run " time)
          (let [exp (experiment folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states)]
            (recur
              (conj resultTribe0 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 0) :TribeAnalytics :tribeFitness))))
              (conj resultTribe1 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 1) :TribeAnalytics :tribeFitness))))
              (conj resultTribe2 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 2) :TribeAnalytics :tribeFitness))))
              (inc time)))))))

;repeats the experiment function n times and saves all results
;the parameters are defined as follows
;  experiment = name of the experiment function
;  folder = experiment folder
;  output-folder : folder that will contain the outputs
;  repeats : specifics how many times the experiment will be repeated
;  steps = number of evolve repeats (total generations each run = steps * generation-num)
;  random-function = use rand-int
;  no-threads : number of threads
;  selection-mode : could be "fittest" "elitism" or "roulette"
;  shared_genes : see get_new_inital_conditions function description ("all", "rand" ...)
;  modifier : see get_new_inital_conditions function description
;  share_mode : see update-initial-states function description ("overwrite", "append" , "append n"
;   max-states : for sharing mode append n ; if you want to append all choose nil
;OUTPUT:
;just the fitness
(defn experiment_n_times_4_tribes
  [experiment folder output-folder repeats steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (loop [resultTribe0 (list)
         resultTribe1 (list)
         resultTribe2 (list)
         resultTribe3 (list)
         time 1]
    (if (> time repeats)
      ;experiment finished write results
      (do
        (cew/write-json output-folder resultTribe0 "tribe0")
        (cew/write-json output-folder resultTribe1 "tribe1")
        (cew/write-json output-folder resultTribe2 "tribe2")
        (cew/write-json output-folder resultTribe3 "tribe3"))
      ;execute experimental run
      (do (timbre/report "Experiment run " time)
          (let [exp (experiment folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states)]
              (recur
                (conj resultTribe0 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 0) :TribeAnalytics :tribeFitness))))
                (conj resultTribe1 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 1) :TribeAnalytics :tribeFitness))))
                (conj resultTribe2 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 2) :TribeAnalytics :tribeFitness))))
                (conj resultTribe3 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 3) :TribeAnalytics :tribeFitness))))
                (inc time)))))))

;repeats the experiment function n times and saves all results
;the parameters are defined as follows
;  experiment = name of the experiment function
;  folder = experiment folder
;  output-folder : folder that will contain the outputs
;  repeats : specifics how many times the experiment will be repeated
;  steps = number of evolve repeats (total generations each run = steps * generation-num)
;  random-function = use rand-int
;  no-threads : number of threads
;  selection-mode : could be "fittest" "elitism" or "roulette"
;  shared_genes : see get_new_inital_conditions function description ("all", "rand" ...)
;  modifier : see get_new_inital_conditions function description
;  share_mode : see update-initial-states function description ("overwrite", "append" , "append n"
;   max-states : for sharing mode append n ; if you want to append all choose nil
;OUTPUT:
;  1. tribe0-tribe3.json :contains the overall fitness curve of all runs (output folder)
;  2. attractormap comment out because low runtime bigger networks
;  3. writes each run the fittest network of each tribe to the tribe output folder. Names then output<run number>_<net-number>
(defn experiment_n_times_2_tribes_saveResults
  [experiment folder output-folder repeats steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
  (loop [resultTribe0 (list)
         resultTribe1 (list)
         ;attractor-map1 (sorted-map)
         time 1]
    (if (> time repeats)
      ;experiment finished write results
      (do
        ;fitness
        (cew/write-json output-folder resultTribe0 "tribe0")
        (cew/write-json output-folder resultTribe1 "tribe1")
        ;attractor map
        ;(coevolution.writer/write-json output-folder (sorted-map :tribe1 attractor-map1) "attractor-map")
        )
      ;execute experimental run
      (do (timbre/report "Experiment run " time)
          (let [exp (experiment folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states)]
            (do
              (doall (map #(cew/write-fittest-net exp % output-folder (str "net_tribe_" % "_" time) no-threads) '(0 1)))
              (recur
                (conj resultTribe0 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 0) :TribeAnalytics :tribeFitness))))
                (conj resultTribe1 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 1) :TribeAnalytics :tribeFitness))))
                ;(conj attractor-map1 (sorted-map (keyword (str "Run" time)) (cean/get-attractor-map exp 1 share_mode max-states no-threads)))
                (inc time))))))))

;repeats the experiment function n times and saves all results
;the parameters are defined as follows
;  experiment = name of the experiment function
;  folder = experiment folder
;  output-folder : folder that will contain the outputs
;  repeats : specifics how many times the experiment will be repeated
;  steps = number of evolve repeats (total generations each run = steps * generation-num)
;  random-function = use rand-int
;  no-threads : number of threads
;  selection-mode : could be "fittest" "elitism" or "roulette"
;  shared_genes : see get_new_inital_conditions function description ("all", "rand" ...)
;  modifier : see get_new_inital_conditions function description
;  share_mode : see update-initial-states function description ("overwrite", "append" , "append n"
;   max-states : for sharing mode append n ; if you want to append all choose nil
;OUTPUT:
;  1. tribe0-tribe3.json :contains the overall fitness curve of all runs (output folder)
;  2. attractormap comment out because low runtime bigger networks
;  3. writes each run the fittest network of each tribe to the tribe output folder. Names then output<run number>_<net-number>
(defn experiment_n_times_4_tribes_saveResults
  [experiment folder output-folder repeats steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states]
      (loop [resultTribe0 (list)
             resultTribe1 (list)
             resultTribe2 (list)
             resultTribe3 (list)
             attractor-map1 (sorted-map)
             attractor-map2 (sorted-map)
             attractor-map3 (sorted-map)
             time 1]
        (if (> time repeats)
          ;experiment finished write results
          (do
            ;fitness
            (cew/write-json output-folder resultTribe0 "tribe0")
            (cew/write-json output-folder resultTribe1 "tribe1")
            (cew/write-json output-folder resultTribe2 "tribe2")
            (cew/write-json output-folder resultTribe3 "tribe3")
            ;attractor map
            (coevolution.writer/write-json output-folder (sorted-map :tribe1 attractor-map1 :tribe2 attractor-map2 :tribe3 attractor-map3) "attractor-map")
            )
          ;execute experimental run
          (do (timbre/report "Experiment run " time)
            (let [exp (experiment folder steps random-function no-threads cond-num selection-mode wildcard-mode shared_genes modifier share_mode max-states)]
            (do
              (doall (map #(cew/write-fittest-net exp % output-folder (str "net_tribe_" % "_" time) no-threads) '(0 1 2 3)))
              (recur
               (conj resultTribe0 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 0) :TribeAnalytics :tribeFitness))))
               (conj resultTribe1 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 1) :TribeAnalytics :tribeFitness))))
               (conj resultTribe2 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 2) :TribeAnalytics :tribeFitness))))
               (conj resultTribe3 (vals (reduce conj (sorted-map) (-> (ceu/get-tribe exp 3) :TribeAnalytics :tribeFitness))))
               (conj attractor-map1 (sorted-map (keyword (str "Run" time)) (cean/get-attractor-map exp 1 "overwrite" max-states no-threads)))
               (conj attractor-map2 (sorted-map (keyword (str "Run" time)) (cean/get-attractor-map exp 2 "append" max-states no-threads)))
               (conj attractor-map3 (sorted-map (keyword (str "Run" time)) (cean/get-attractor-map exp 3 "append" [4] no-threads)))
               (inc time))))))))

;runtime experiment returns a duration map with the runtime dependent on the number of threads
(defn runtime-exp
  [folder output-folder]
  (let [thread-num-list (conj (into [] (range 1 41)) 45 64 128 256)]
  (loop [thread-num 0 durationmap (sorted-map)]
    (if (>= thread-num (count thread-num-list))
      durationmap
      (let [no-threads (nth thread-num-list thread-num)]
        (let [tstart (inst-ms (java.time.Instant/now))]
          (do (experiment_n_times_4_tribes experiment_1_crosslink folder output-folder 1 2 rand-int no-threads 0 "fittest" "ignore-wildcards" "all" 1 "overwrite" nil)
              (let [tend (inst-ms (java.time.Instant/now))]
                (let [duration (double (/ (- tend tstart) 1000))]
                  (timbre/info "Threads " no-threads "Duration" duration)
                  (recur (inc thread-num) (conj durationmap {no-threads duration})))))))))))
