(ns coevolution.writer
  (:require [commonbusiness.jsonwriter :as jsw]
            [com.climate.claypoole :as cp]
            [commonbusiness.XMLWriter :as cxmlw]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [coevolution.utility :as ceu]
            [coevolution.initialstateexchange :as ceis]
            [taoensso.timbre :as timbre]))









;write a network to sbml file
(defn write-network
  [tribe network-num path name]
    (cxmlw/writeXML (:fileName (nth (-> tribe :population-list) network-num))
                    (str path "/" name "_" network-num  ".sbml")
                    (:networkTrees (nth (-> tribe :population-list) network-num))))

;writes the fittest network of the tribe to the tribe output folder as sbml
(defn write-fittest-net
  [world tribe-num path name no-threads]
  (write-network (ceu/get-tribe world tribe-num) (ceis/get-fittest-individual (ceu/get-tribe world tribe-num) no-threads) path name))

;write a tribe population to the output folder
(defn write-tribe-networks
  [tribe output-num no-threads]
  (let [pool (cp/threadpool no-threads)]
    ;; the number of written files is the minimum between the user entered and the output generation as in the last
    ;; generation where only the fittest are written the number of available generation could be lower than the
    ;; entered number by the user.
    (let [writen-files-count (min output-num (count (-> tribe :population-list)))]
      (loop [writitng-current 0]
        (if (< writitng-current writen-files-count)
          (do
            (cp/future pool (cxmlw/writeXML (:fileName (nth (-> tribe :population-list) writitng-current))
                                             (str (-> tribe :folderPath) "/output/output_"  writitng-current ".sbml")
                                             (:networkTrees (nth (-> tribe :population-list) writitng-current))))
             ;(info "Best fitness populations; index " time-point writitng-current (nth generation writitng-current))
             (recur (inc writitng-current))))))
    (cp/shutdown pool)))

;writes the tribe mutation-tracker to the output-folder if mutation-percentage is 100
(defn write-tribe-mutation-tracker
  [tribe]
  (if (= (-> tribe :specifications :mutation-percentage) 100)
    ;; write the JSON objects of mutation tracked object into a file
    (let [mutation-trackers (mapv #(hash-map (keyword "file-path") (:fileName %) (keyword "fitness") (:fitnessValue %)
                                             (keyword "mutation-trackers") (:mutationTrackers %)) (-> tribe :population-list))]
      ;(println "mutation trackers " mutation-trackers)
      (jsw/write-json (str (-> tribe :folderPath) "/output") mutation-trackers)
      ;(-> tribe :population-list)
      )
    ;; the output generation
    ;(mapv #(assoc % :mutationTrackers nil) (-> tribe :population-list))
    ))

;writes for all tribes in the world each output network to the output folder
(defn write-world-networks
  [world output-num no-threads]
  (cp/pmap no-threads #(write-tribe-networks % output-num no-threads) (-> world :tribe-list)))

;writes for all tribes in the world each output network to the output folder
(defn write-world-mutation-tracker
  [world no-threads]
  (cp/pmap no-threads write-tribe-mutation-tracker (-> world :tribe-list)))


;
(defn write-json
  ;; the function writes a list of json objects into the file path.
  ([file-path str-write filename]
   ;(println "in write json " str-write)
   (with-open [wrtr (io/writer (str file-path "/" (str filename ".json")))]
     (.write wrtr (json/write-str str-write)))))