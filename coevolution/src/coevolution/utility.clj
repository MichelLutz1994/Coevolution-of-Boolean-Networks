(ns coevolution.utility
  (:require [timeseriesevolution.fitnessfunctionimpl :as ffi]
            [flatland.ordered.map :as om]))

; returns a list with all files in the folder,
; does not return files in subfolders
(defn get-files-in-folder
  [path]
  (seq (.list (clojure.java.io/file path))))

;replaces the last value from a vector, or the first value of a list
(defn set-last [coll x]
  (conj (pop coll) x))

;macro to change an exception and try again
(defn try-times*
  "Executes thunk. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain."
  [n thunk]
  (loop [n n]
    (if-let [result (try
                      [(thunk)]
                      (catch Exception e
                        (when (zero? n)
                          (throw e))))]
      (result 0)
      (recur (dec n)))))
(defmacro try-times
  "Executes body. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain."
  [n & body]
  `(try-times* ~n (fn [] ~@body)))



;just for simplify the console view of a tribe
(defn view-tribe-specs
  [world tribe-number]
  (let [tribe (nth (-> world :tribe-list) tribe-number)]
    (map #(-> tribe %) '(:folderPath :specifications :TribeAnalytics))
    ))

;sorts a map by its values and returns the map in descent order
(defn sort-map-by-value-desc
  [coll]
  (into (sorted-map-by (fn [key1 key2]
                         (compare [(get coll key2) key2]
                                  [(get coll key1) key1]))) coll))

;returns the current tribe average fitness
(defn get-tribe-fitness
  [world tribe-num]
  (nth (vals (first (-> (nth (-> world :tribe-list) tribe-num) :TribeAnalytics :tribeFitness))) 0))

(defn get-tribe
  [world tribe-num]
  (nth (-> world :tribe-list) tribe-num))

;returns a current spec of the tribe
(defn get-tribe-spec
  [world tribe-num spec]
  (-> (nth (-> world :tribe-list) tribe-num) :specifications spec))

"[false false true] ->  001"
(defn boolvec->binstr [s]
  (reduce str (replace {false "0" true "1"} s)))

"[false false true -> 1"
(defn boolvec->int [v]
  (Long/parseLong (boolvec->binstr v) 2))

"integer -> 001
i: integer , n length of the vector / should be the number of genes"
(defn int->boolstr [i n]
  (clojure.string/replace (format (str "%" n "s") (Integer/toBinaryString i)) #" " "0"))

"001 -> [false false true]"
(defn binstr->boolvec [s]
  (replace {"0" false "1" true} (clojure.string/split s #"")))

;returns a state as boolean vector form the integer representaion
(defn attractorname->stat
  [attractorname tribe]
  (binstr->boolvec (int->boolstr attractorname (count (-> tribe :gene-list)))))

;appends the value to the last vector of a vector of vectors
(defn append-last
  [vec value]
  (conj (pop vec) (conj (last vec) value)))

(defn create-rand-state
  [tribe]
  (into (om/ordered-map) (map vector (map keyword (-> tribe :gene-list))
                              (repeatedly (count (-> tribe :gene-list)) #(nth [true false] (rand-int 2))))))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (if (= pos nil)
    coll
    (into (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn index-exclude [r ex]
  "Take all indices execpted ex"
  (filterv #(not (ex %)) (range r)))


(defn dissoc-idx [v ds]
  (mapv v (index-exclude (count v) (into #{} ds))))