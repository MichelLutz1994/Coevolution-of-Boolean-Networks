(defproject coevolution "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :aot
  [coevolution.core]
  ;; path to the local repository where commonbusiness jar should be stored
  :local-repo "C:\\Users\\Michel Lutz\\IdeaProjects\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\localRepo\\artifacts"
  ;:local-repo "C:\\Users\\miche\\Programming\\Java\\hopper.informatik.uni-ulm.decoevolutionofbooleannetworks\\localRepo\\artifacts"


  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.zip "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/data.xml "0.2.0-alpha6"]
                 ;;handling log files
                 [com.taoensso/timbre "5.1.2"]
                 [zip-visit "1.1.0"]
                 ;;handling parallel processing
                 [com.climate/claypoole "1.1.4"]
                 ; handling csv files
                 [org.clojure/data.csv "1.0.1"]
                 ; handling the input arguments for the main function.
                 [org.clojure/tools.cli "1.0.206"]
                 ;; handling json reading and writing
                 [org.clojure/data.json "2.4.0"]
                 ;; random seed handling
                 [random-seed "1.0.0"]
                 ;; the jar from the local repository
                 [commonbusiness "0.1.0-SNAPSHOT"]
                 [timeseriesevolution "0.1.0-SNAPSHOT"]
                 [org.flatland/ordered "1.15.10"]]

  ;;specifying the class with the main function
  :main coevolution.core

  )
