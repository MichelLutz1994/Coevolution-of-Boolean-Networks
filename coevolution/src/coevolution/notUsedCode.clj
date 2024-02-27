(ns coevolution.notUsedCode)

;contains some not used code. Most things are correct but now useless functions


(comment
;updates the whole TribeAnalytic, writes a new value to the tracker lists, should just be used when a specification was changed
;and the tribe has grown older
(defn update-TribeAnalytics
  [tribe]
  ;first: update all values, except time-point-track
  (let [tribe (reduce (fn [tribe spec] (update-one-tribe-analytic-spec tribe (keyword (str (name spec) "-track")) (-> tribe :specifications spec)))
                      tribe (drop 3 (map key (-> tribe :specifications))))]
    ;update time point track
    (let [tribe (update-one-tribe-analytic-spec tribe :time-point-track (-> tribe :TribeAnalytics :age))]
      ;update the tribeFitness, just copy's the last value again to the vector, due to the fact, that the fitness
      ;will not change till the tribe evolves the next time
      (update-one-tribe-analytic-spec tribe :tribeFitness (last (-> tribe :TribeAnalytics :tribeFitness)))
      )))




;example code for debugging
(def tribe (cec/create-initial-tribe "CEFiles/tribe_0" (sfp/pares-default-spec-file "CEFiles/specs_0.txt") rand-int no-threads))
(def tribe2 (add-individuals-to-tribe tribe 1 no-threads))


(def initial-states (mapv #(iss/create-initial-states (:inital-condition %) (-> tribe :gene-list) (-> tribe :specifications :initial-state-pool-size) rand-int)
                          [(-> tribe :specifications)]))
(def sortedHammingDistances (sort-by last (ffi/compare-to-specifications [(-> tribe :specifications)]
                                                                         (-> tribe :population-list)
                                                                         initial-states
                                                                         (-> tribe :gene-list)
                                                                         (-> tribe :specifications :path-size)
                                                                         no-threads)))
(def includedNetworks (mapv #(cnutl/set-fitness (-> tribe :population-list) %) sortedHammingDistances))
(def tribe (assoc tribe :population-list includedNetworks))

(cee/evaluate-tribe-fitness tribe)


(reduce #(+ %1 %2) 0 '(1 2))


;returns the shard gene value that occurs the most in the attractors of all individuals
;if the number of occurring trues and false is equal a random ture or false is returned
(defn get_most_occurring_values
  [shard_gene_values]
  (reduce (fn [l gene] (conj l (let [sums (frequencies (map #(gene %) shard_gene_values))]
                                 ;there are true and false values
                                 (if (= 2 (count sums))
                                   (if (= (sums true) (sums false))
                                     (if (= 0 (rand-int 2))
                                       {gene true}
                                       {gene false})
                                     (if (or (= nil (sums true) (not (= nil (sums false)))) (> (sums true) (sums false)))
                                       {gene true}
                                       {gene false})
                                     )
                                   ;there is just ture or just false
                                   {gene (keys sums)}
                                   )
                                 ))) {} (keys (nth shard_gene_values 0))))

;like experiment 1 but tribe 1 gets each iteration a completely random initial-condition
(defn experiment_1_ref
  [folder steps random-function no-threads shared_genes modifier]
  (let [world (ceu/try-times 100 (cec/create-world folder (str folder "\\specs_0.txt") rand-int no-threads))]
    (loop [timepoint 0 world (cee/update-world-fitness world no-threads)]
      (if (> timepoint steps)
        world
        (if (= timepoint 1)
          (let [world (cee/evolve-world world random-function no-threads)]
            (let [new_inital_conditions (zipmap [:Gene1  :Gene2  :Gene3  :Gene4  :Gene5  :Gene6  :Gene7  :Gene8  :Gene9  :Gene10 ] (into []  (repeatedly 10 #(nth [true false] (rand-int 2)))))]
              (let [world (cesu/update-world world 1 :initial-state-pool-size 1 no-threads)]
                (do (timbre/info timepoint new_inital_conditions)
                    (recur (inc timepoint) (cesu/update-world world 1 :inital-condition new_inital_conditions no-threads))))))
          (do (timbre/info timepoint )
              (recur (inc timepoint) (cee/evolve-world world random-function no-threads)))
          )))))








)