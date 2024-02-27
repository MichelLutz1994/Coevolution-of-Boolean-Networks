(ns coevolution.specificationfileparser
  (:require [clojure.string :as str]
            [coevolution.models]
    ;; logging library
            [commonbusiness.Utility :as cutl]
            [coevolution.utility :as ceu]
            [taoensso.timbre :as timbre :refer [error]]
            [flatland.ordered.map :as om])
  (:import (coevolution.models specification)))


;; the function is used to transfer a line specifications into a list
;; of booleans. If the gene is mentioned that means its value is true, if it is mentioned with a ! in front of
;; it, that means that its value is false.
(defn transfer-state-boolean
  [line states]
  (let [initial-state (str/split line #" |,|;")]
    (loop [current 0 output (om/ordered-map)]
      (if (= current (count initial-state))
        (ceu/append-last states (into (om/ordered-map) output))
        (let [current-gene (nth initial-state current)]
          (if (str/starts-with? current-gene "!")
            (recur (inc current) (conj output [(keyword (subs current-gene 1)) false]))
            (recur (inc current) (conj output [(keyword current-gene) true]))))))))

;; the function is used to transfer a line into a map of a gene and a boolean state.
;; If the gene is mentioned that means its value is true, if it is mentioned with a ! in front of
;; it, that means that its value is false.
(defn transfer-initial-state-boolean
  [line]
  (let [initial-state (str/split line #" |,|;")]
    (loop [current 0 output (om/ordered-map)]
      (if (= current (count initial-state))
        output
        (let [current-gene (nth initial-state current)]
          (if (str/starts-with? current-gene "!")
            (recur (inc current) (conj output [(keyword (subs current-gene 1)) false]))
            (recur (inc current) (conj output [(keyword current-gene) true]))))))))

;checks if a set of specifications contains Attractor or Chain and inital-condition and states
(defn check-all-necessary-specs
  [specification]
  (if (:inital-condition specification)
    (if (:comparison-type specification)
     (if (:states specification)
       true)
     false)))

;parses a line with meta information, all meta infos are int,
; exceptions fitness-thresold= double
; exceptions random-function = string
(defn pars-meta-line
  [line specification]
  (let [line-list (str/split line #",|:|;|=| ")]
    (if (< 1 (count line-list))
        (let [parameter (nth line-list 0)
              value (nth line-list 1)]
          (try
            (case parameter
              "population-size" (assoc specification :population-size (Integer/parseInt value))
              "mutation-per-network" (assoc specification :mutation-per-network (Integer/parseInt value))
              "mutation-percentage" (assoc specification :mutation-percentage (Integer/parseInt value))
              "parents-num" (assoc specification :parents-num
                                                 (let [value (Integer/parseInt value)]
                                                   (if (>= value 2)
                                                     value
                                                     (throw (Exception. "parents-num have to be >=2")))))
              "fitness-thresold" (assoc specification :fitness-thresold
                                                      (let [value (Double/parseDouble value)]
                                                        (if (and (<= value 1) (>= value 0))
                                                          value
                                                          (throw (Exception. "fitness-thresold have to be in [0-1]")))))
              "generations-number" (assoc specification :generations-number (Integer/parseInt value))
              "initial-state-pool-size" (assoc specification :initial-state-pool-size (Integer/parseInt value))
              "path-size" (assoc specification :path-size (Integer/parseInt value))
              specification)
            (catch Exception e (error "Parameter have not the right format" (.getMessage e)) specification))
          )
      specification
      )))

;adds all specifications in a given file to the specifications,
;CAVE! Overwrite duplicates read default-values first
(defn parse-specs-and-add
  [spec-file specification]
  (let [lines (cutl/read-lines spec-file)]
    ;attractorsNum defines how many initial-conditions and attractor pairs are present
    (loop [current-spec nil specifications #{} specification specification current 0]
      (if (= current (count lines))
        ;reach end of specification file
            specification
        ;processes the current line
        (let [line (nth lines current)]
          (if (empty? line)
            (recur nil specifications specification (inc current))
            ;; if it starts with #, it means it is a comment and no need to check it.
            (if (str/starts-with? line "#")
              (recur current-spec specifications specification (inc current))

              (if (= line "Attractor")
                    (recur "Attractor" (conj specifications "Attractor") (assoc specification  :comparison-type "Attractor") (inc current))
                (if (= line "Chain")
                  (recur "Chain" (conj specifications "Chain") (assoc specification  :comparison-type "Chain") (inc current))
                  (if (= line "Initial condition")
                    (let [specification (assoc specification :inital-condition (conj (-> specification :inital-condition) []))]
                      (recur "Initial condition" (conj specifications "Initial condition") specification (inc current)))
                    (if (= line "State specifications")
                      (let [specification (assoc  specification  :states (conj (-> specification :states) []))]
                        (recur "State specifications" (conj specifications "State specifications") specification (inc current)))
                      ;update inital-conditions
                      (if (and (= current-spec "Initial condition") (not (empty? line)))
                        (let [initial-state (transfer-initial-state-boolean line)]
                          (recur "Initial condition" specifications (assoc specification :inital-condition (ceu/append-last (-> specification :inital-condition) initial-state)) (inc current)))
                        ;update state-specifications
                        (if (and (= current-spec "State specifications") (not (empty? line)))
                          (let [states (transfer-state-boolean line (:states  specification ))]
                            (recur "State specifications" specifications (assoc  specification  :states states) (inc current)))
                          ;check for meta-information's
                          (if (not (empty? line))
                              (let [specification (pars-meta-line line specification)]
                                (recur "Meta" specifications specification (inc current)))
                            nil))))))))))))))

(def initial-state (transfer-initial-state-boolean "Gene1 Gene2 Gene3 Gene4" ))
(ceu/append-last (ceu/append-last [[]] initial-state) (transfer-initial-state-boolean "!Gene1 !Gene2 !Gene3 !Gene4" ))

(ceu/append-last [[]] initial-state)

;parses the default-spec-file and returns a specification datarecord
(defn pares-default-spec-file
  [default-spec-file]
  (let [specification (specification. nil [] [] nil nil nil nil nil nil nil nil)]
    (if (not (nil? default-spec-file))
      (let [specification (parse-specs-and-add default-spec-file specification)]
        specification)
      specification
      )))

;pars tribe specific specification files
(defn parse-spec-file
  [tribe-spec-file default-specification]
  (if (not (nil? tribe-spec-file))
    (let [specification (parse-specs-and-add tribe-spec-file default-specification)]
      ;the initial condition and states my starting with a [[] ... ] when there is no default specification
      ;in this case removes the first empty vector, otherwise not
      (let [specification (assoc specification :inital-condition (into [] (filter #(not (= % [])) (-> specification :inital-condition))))]
        (let [specification (assoc specification :states (into [] (filter #(not (= % [])) (-> specification :states))))]
      (if (check-all-necessary-specs specification)
        [specification]
        (do
          (print specification)
          (timbre/error "Not all specifications available. Should at least include [Attractor/Chain, Inital condition, State specification]")
          nil)))))
    (if (check-all-necessary-specs default-specification)
      [default-specification]
      (do
        (print default-specification)
        (timbre/error "Not all specifications available. Should at least include [Attractor/Chain, Inital condition, State specification]")
        nil))
    ))
