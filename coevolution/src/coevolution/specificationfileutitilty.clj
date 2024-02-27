(ns coevolution.specificationfileutitilty
  (:require [commonbusiness.Utility :as cutl]
            [clojure.string :as str]))

;; a hash set of all the keywords that are required to exist in path specs flies
(def path-specs-keywords (hash-map :Chain 0, (keyword "Initial condition") 1,
                                   (keyword "State specifications") 2))

;; a hash set of all the keywords that are required to exist in attractors specs flies
(def attractor-specs-keywords (hash-map :Attractor 0, (keyword "Initial condition") 1,
                                        (keyword "State specifications") 2))

;; checks if a word is a key in the specification map
(defn spec-restricted-word
  [word specs-map]
  (contains? specs-map (keyword word)))

;; function to create a spec list from the specification file.
(defn identify-spec
  [lines current specs-map]
  (loop [spec-list [] current-line current]
    (if  (= (count lines) current-line)
      (if (= 0 (count spec-list))
        nil
        spec-list)
      (if (and (spec-restricted-word (nth lines current-line) specs-map) (= current-line current))
        nil
        (if (spec-restricted-word (nth lines current-line) specs-map)
          spec-list
          (recur (if (or (empty? (nth lines current-line)) (str/starts-with? (nth lines current-line) "#")) spec-list (conj spec-list (cutl/split-by-whitespace (nth lines current-line)))) (inc current-line)))))))
