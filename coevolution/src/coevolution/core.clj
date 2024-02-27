(ns coevolution.core
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require
    ;; as the random-seed namespace is included, there are versions of this functions in it,
    ;; those are the right version to use, not the one from clojure
    [coevolution.coevolution]
    [coevolution.experiments :as ceex]
    [com.climate.claypoole :as cp]
    [random-seed.core :refer :all]
    [taoensso.timbre :as timbre])

  ;; gen-class is required to transfer the clj to .class and main is required to identify that
  ;; this class has a main function which is essential for the jar production to choose the file
  ;; that contains main.
  (:gen-class :main true))

; :info will show the progression on experiment level
; :trace will show the progression on evolution level
(timbre/set-level! :info)

;arg 0 -> choose experiment
;arg 1 -> experiment folder
;arg 2 -> output folder
;arg 3 -> no-threads
;arg 4 -> repeats (how often should the experiment performed)
;arg 5 -> steps (How often will the evolve-world be called => (number of generations = steps * generations-number (of the tribe))
(defn -main [& args]
  (if (empty? args)
    (println "Args empty - NightGoblin")
    (do
      (timbre/report "Experiment folder" (nth args 1))
      (timbre/report "Output folder    " (nth args 2))
      (case (nth args 0)
        "0" (ceex/experiment_n_times_4_tribes ceex/experiment_1_crosslink (nth args 1) (nth args 2) (Integer/parseInt (nth args 4)) (Integer/parseInt (nth args 5)) rand-int (Integer/parseInt (nth args 3)) 0 "fittest" "ignore-wildcards" "all" 1 "overwrite" nil)
        "1" (ceex/experiment_n_times_2_tribes_saveResults ceex/experiment_bio (nth args 1) (nth args 2) (Integer/parseInt (nth args 4)) (Integer/parseInt (nth args 5)) rand-int (Integer/parseInt (nth args 3)) 0 "fittest" "ignore-wildcards" "all" 1 "overwrite" nil)
        ;runtime test
        "2"   (let [durations (sorted-map)]
                (loop [repeat 1 durations durations]
                  (if (> repeat 10)
                    (coevolution.writer/write-json (nth args 2) durations "durationmap")
                    (recur (inc repeat) (conj durations {(keyword (str "Run" repeat)) (ceex/runtime-exp (nth args 1) (nth args 2))})))))
        "3"
        (timbre/report "No case matches")))))



(comment
  (timbre/set-level! :trace)
  (-main "0" "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\experiment1_14"
         "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\output\\other" "4" "1" "10")
  (-main "1" "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\biology"
         "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\output\\biology" "4" "4" "10")
  (-main "2" "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\exp_runtime"
         "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\output" "2")
  )


"C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\experiment4_1"
"C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\coevolution\\CEFiles\\experiment4_1\\output"

