(ns coevolution.validators.validator
  (:require [clojure.string :as str]
            [commonbusiness.validators.inputvalidator :as cvi]
            [coevolution.utility :as ceu]
            ;;logging libraries
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))


;tests if a folder contains a .txt file
(defn folder-contains-txt-file?
  [folder-path]
  (some #(= % true) (map (fn [s] (some #(= "txt" %) s)) (map #(str/split % #"\.") (seq (.list (clojure.java.io/file folder-path)))))))

;tests if the tribe folder structure is correct
;must contain following folders: input / output /specs
(defn correct-tribe-folder-structure?
  [folder-path]
  (let [files (seq (.list (clojure.java.io/file folder-path)))]
    (if (some #(= "output" %) files)
      (if (some #(= "input" %) files)
        (if (some #(= "specs" %) files)
          (if (folder-contains-txt-file? (str folder-path "/specs"))
            true)))
      (do (error "Warning tribe folder structure is not valid! Tribe will be ignored" folder-path )
        false))))

;a valid experiment-folder must contain
; 1. one or more folders with the tribe information's
; 2. a .txt file with default specification, if there is none, in each tribe folder there must be at last one spec file
; toDO: not finished
(defn valid-experiment-folder?
  [experiment-folder]
  (let [file-paths (ceu/get-files-in-folder experiment-folder)]
    (if (empty? file-paths)
      (error "The specified folder is empty!" experiment-folder)
      (if (and
            (for [file file-paths]
              (if (not (empty? (ceu/get-files-in-folder (str experiment-folder "/" file))))
                (correct-tribe-folder-structure? file))))
        true
        (error "Something is missing" experiment-folder))
      )))

