(ns coevolution.specificationsupdater
  (:require [commonbusiness.evolution :as cev]
            [coevolution.evolver :as cee]
            [taoensso.timbre :refer [error warn]]
            [flatland.ordered.map :as om]))

;contains all functions for specification and analytic updating


;update a TribeAnalytics :time-point-track
(defn update-tribe-analytic-time-point-track
  [tribe]
  (assoc tribe :TribeAnalytics
               (assoc (-> tribe :TribeAnalytics) :time-point-track
                                                 (conj (-> tribe :TribeAnalytics :time-point-track) (-> tribe :TribeAnalytics :age)))))

;update a TribeAnalytics value
(defn update-tribe-analytic-spec
  [tribe analytic-to-change new-value]
  (let [analytic-to-change (keyword (str (name analytic-to-change) "-track"))]
    (let [tribe (assoc tribe :TribeAnalytics
                             (assoc (-> tribe :TribeAnalytics) analytic-to-change
                                                               (conj (-> tribe :TribeAnalytics analytic-to-change) {(-> tribe :TribeAnalytics :age) new-value})))]
      (update-tribe-analytic-time-point-track tribe))))

;adds count individuals to a tribe
;chances also the specifications and TribeAnalytics parameters
(defn add-individuals-to-tribe
  [tribe count no-threads]
  (let [new-population-list
        (into [] (concat (-> tribe :population-list)
              (cev/create-population-remainder count
                   (-> tribe :population-list)
                   rand-int
                   (-> tribe :specifications :population-size)
                   (-> tribe :gene-list)
                   no-threads)))]
    (let [tribe (assoc tribe :population-list new-population-list)]
      ;update specifications and TribeAnalytics
      (let [tribe (assoc tribe :specifications (assoc (-> tribe :specifications) :population-size (+ (-> tribe :specifications :population-size) count)))]
        (let [tribe (update-tribe-analytic-spec tribe :population-size (+ (-> tribe :specifications :population-size) count))]
          (cee/update-tribe-fitness tribe no-threads)
          )))))

;updates the specification of a tribe
;cannot be used for initial-state updating
;creates also a new TribeAnalytics entry
(defn update-tribe-specification
  [tribe spec-to-change new-value no-threads]
  ;update Specification
  (if (and (= spec-to-change :population-size) (> new-value (-> tribe :specifications :population-size)))
    (add-individuals-to-tribe tribe (- new-value (-> tribe :specifications :population-size)) no-threads)
    (if (= spec-to-change :inital-condition)
        (do
          (warn "You can not use this function for updating initial states: use update-initial-states instead")
        tribe)
        (let [tribe (assoc tribe :specifications (assoc (-> tribe :specifications) spec-to-change new-value))]
        ;update the TribeAnalytics tracker
        (update-tribe-analytic-spec tribe spec-to-change new-value)))))

;substitutes the last entry in tribeFitness with a new fitness value
(defn substitute-the-last-tribeFitness
  [world tribe-num new-value]
  (let [tribes (into [] (-> world :tribe-list))]
    (let [tribes
          (assoc tribes tribe-num (assoc (nth tribes tribe-num) :TribeAnalytics
                                         (assoc (-> (nth tribes tribe-num) :TribeAnalytics) :tribeFitness
                                                (conj (drop 1(-> (nth tribes tribe-num) :TribeAnalytics :tribeFitness)) {(-> (nth tribes tribe-num) :TribeAnalytics :age) new-value}))))]
      (assoc world :tribe-list tribes))))

;should not be called form outside the update-initial-states function
;appends the new initial-state and increments the initial-state-size-pool
(defn update-initial-states-append
  [tribe cond-num new-value no-threads]
  (let [tribe (assoc-in tribe [:specifications :inital-condition cond-num] (concat (nth (-> tribe :specifications :inital-condition) cond-num) (list (into (om/ordered-map) new-value))))]
    (let [tribe (update-tribe-analytic-spec tribe :inital-condition (-> tribe :specifications :inital-condition))]
      (if (< (-> tribe :specifications :initial-state-pool-size) (count (nth (-> tribe :specifications :inital-condition) cond-num)))
        (let [tribe (update-tribe-specification tribe :initial-state-pool-size (inc (-> tribe :specifications :initial-state-pool-size)) no-threads)]
          (update-tribe-analytic-spec tribe :initial-state-pool-size (-> tribe :specifications :initial-state-pool-size)))
        tribe))))

;should not be called form outside the update-initial-states function
;appends the new initial-state and increments the initial-state-size-pool
;appends the new state will max uses the last max-state-number of states
(defn update-initial-states-append-max-states
  [tribe cond-num new-value max-state-number no-threads]
  (let [tribe (assoc-in tribe [:specifications :inital-condition cond-num] (take-last max-state-number (concat (nth (-> tribe :specifications :inital-condition) cond-num) (list (into (om/ordered-map) new-value)))))]
    (let [tribe (update-tribe-analytic-spec tribe :inital-condition (-> tribe :specifications :inital-condition))]
      (if (< (-> tribe :specifications :initial-state-pool-size) (count (nth (-> tribe :specifications :inital-condition) cond-num)))
        (let [tribe (update-tribe-specification tribe :initial-state-pool-size (inc (-> tribe :specifications :initial-state-pool-size)) no-threads)]
          (update-tribe-analytic-spec tribe :initial-state-pool-size (-> tribe :specifications :initial-state-pool-size)))
        tribe))))

;should not be called form outside the update-initial-states function
;overwrites the last initial-state and increments the initial-state-size-pool
(defn update-initial-states-overwrite
  [tribe cond-num new-value]
  (let [tribe (assoc-in tribe [:specifications :inital-condition cond-num]
                        (apply vector (assoc (into [] (nth (-> tribe :specifications :inital-condition) cond-num))
                                        (dec (count (nth (-> tribe :specifications :inital-condition) cond-num)))
                                        (conj (last (nth (-> tribe :specifications :inital-condition) cond-num))
                                              (into (om/ordered-map) new-value)))))]
    (update-tribe-analytic-spec tribe :inital-condition (-> tribe :specifications :inital-condition))))

;updates the initial_states of tribe with tribe_num
;mode = "overwrite" -> just the now state will remain
;mode = "append" -> adds a new initial-state to the current ones, increments also the initial-states-pool
;mode = "append" with additional args list: args must contain one integer value. This defines how many initial states
;will remain. If there are more than this number, the oldest will be removed.
(defn update-initial-states
  [world tribe-num cond-num new-value no-threads mode max-states]
  (let [tribes (into [] (-> world :tribe-list))]
    ;check if the spec-to-change is valid
      (if (= mode "overwrite")
        (let [tribes (assoc tribes tribe-num (update-initial-states-overwrite (nth tribes tribe-num) cond-num new-value))]
          (assoc world :tribe-list tribes))
        (if (= mode "append")
          (if (empty? max-states)
            ;simple append mode
            (let [tribes (assoc tribes tribe-num (update-initial-states-append (nth tribes tribe-num) cond-num new-value no-threads))]
              (assoc world :tribe-list tribes))
            ;append and keep just (nth args 0) much
            (let [tribes (assoc tribes tribe-num (update-initial-states-append-max-states (nth tribes tribe-num) cond-num new-value (nth max-states 0) no-threads))]
              (assoc world :tribe-list tribes)))
          (error "no correct mode")))))

;updates one tribe in the world
;should not be used for initial-state updating, when so used the function will call
;the update-initial-states function in overwrite mode
;sets the spec-to-change specification to the new value
;updates the tribeAnalytics
(defn update-world
  [world tribe-num spec-to-change new-value no-threads]
  (if (= spec-to-change :inital-condition)
    (do (warn "update-world was used to update initial-condition, the overwrite mode was used for the first condition pair")
      (update-initial-states world tribe-num 0 new-value no-threads "overwrite" nil))
    (let [tribes (into [] (-> world :tribe-list))]
    ;check if the spec-to-change is valid
      (if (contains? (-> (nth tribes tribe-num) :specifications) spec-to-change)
        (let [tribes (assoc tribes tribe-num (update-tribe-specification (nth tribes tribe-num) spec-to-change new-value no-threads))]
          (assoc world :tribe-list tribes))
        (error spec-to-change " is not correct")))))
