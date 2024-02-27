(ns coevolution.models)

;;analytic records for a tribe
;;contains a tracker for chaining parameters
;;time-point-track indicates the generation in witch a change occurs
;;age is the number of generations the tribe has already evolved
;;each record is a map with the age in witch the changes occur as key
(defrecord TribeAnalytics [age
                           tribeFitness
                           time-point-track
                           comparison-type-track
                           inital-condition-track
                           states-track
                           population-size-track
                           mutation-per-network-track
                           mutation-percentage-track
                           parents-num-track
                           fitness-thresold-track
                           initial-state-pool-size-track
                           generations-number-track
                           path-size-track
                           ])

;;a record that contains all information about a tribe
;;by now not final
(defrecord Tribe [folderPath gene-list specifications TribeAnalytics population-list])

;; a modelf for specification which contains the initial-condition for the
;; specification and the states, making a path or an attractor, defined within this specification,
;; and all information for mutation
(defrecord specification [comparison-type
                          inital-condition
                          states
                          population-size
                          mutation-per-network
                          mutation-percentage
                          parents-num
                          fitness-thresold
                          generations-number
                          initial-state-pool-size
                          path-size])

;record that contains all information of the world. All tribes and meta-information
;shared genes are genes that the tribes could share
(defrecord World [folderPath tribe-list world-gene-list])