(ns coevolution.specificationsutility
  (:require [coevolution.specificationfileparser :as sfp]
            [coevolution.specificationfileutitilty :as sfu]
            [commonbusiness.Utility :as cutl]
            ;;logging libraries
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

;; returns the count of specification files inside the input folder
(defn get-specifications-count
  [specification-files-folder]
  (let [file-paths (cutl/get-files-in-folder specification-files-folder)]
    (count file-paths)))

;; return a vector of specifications files of the right order of the specifications files.
(defn get-specifications-for-file
  [specification-files-folder file-index default-specification]
  (let [file-paths (cutl/calculate-files-numbering? (mapv #(str specification-files-folder "/" %) (cutl/get-files-in-folder specification-files-folder)) false)]
    (sfp/parse-spec-file (nth file-paths file-index)  default-specification)))