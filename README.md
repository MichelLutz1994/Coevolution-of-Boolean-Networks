# Coevolution of Boolean networks in system biology

A Clojure library designed to evolve multiple populations, called tribes.
Each tribe optimizes its network structure against a specific target stored in the tribe's specification file.
The tribes can act in competition or cooperatively. For each tribe, the mutation rates and the generations until
they interact with each other again can be set separately. 
The coevolution parameters are stored in another specification file.

## Usage
There are two methods to run the project
1. REPL server: by right clicking on project.clj and run on repl. The order of the inputs is explained below.

2. Create a jar
- not yet implemented

Before creating the jar make sure that the following:
1. You created the common business jar and it is copied to the folder of the local repository with the right folder structure.
    The path of the local repository is written in the project.clj file.
    commonbusiness-> commonbusiness -> 0.1.0-SNAPSHOT -> commonbusiness-0.1.0-SNAPSHOT.jar.
2. You created the timeseriesevolution jar and it is copied to the folder of the local repository with the right folder structure.
   The path of the local repository is written in the project.clj file.
   timeseriesevolution-> timeseriesevolution -> 0.1.0-SNAPSHOT -> timeseriesevolution-0.1.0-SNAPSHOT.jar.

## Experiment setup
Initial condition
    An experiment setup must contain following files and folders
    1. Experiment spec.txt
        - number of tribes
        - mode of experiment (not clear what there will be possible)
    2. 1-n Tribe folders. folder should end with \_(index of stage) starting from zero (example tribe_0)
        - tribe folders must contain tree subfolders
        tribe_0
        |- input
        |- output
        |- specs 
        input and output are pretty much the same as in the timeseries module (consult the readme in this module for more information)
        specs - currently still the same as in timeseries. Maybe there will be an extra part for tribe specific informations like
            - population-size
            - mutation-per-network
            - fitness-thresold
            - mutation-percentage
            - parents-num
        
Starting an experiment
In the experiment folder are many concrete predefined experiments with certain interactions between the tribes.
You can use one of those or design with the functionalities within this framework your own experiments. There 
are also wrapper experiments function witch allows easy repetition of a experiment and different levels of output for
analysis. An Example is found below. All parameters are more detailed described within the concrete functions 
and in the thesis.

Example: 
(ceex/experiment_n_times_4_tribes_saveResults   
        ceex/experiment_4               ;concrete experiment function
        "CEFiles/biology2"              ;experiment folder
        "CEFiles/output/biology"        ;output folder
        30                              ;who many times should the experiment be performed
        20                              ;who many times will the evolutionary loop be called each experiment
        rand-int                        ;random function 
        4                               ;number of threads
        0                               ;cond-num usefull if there are more than one initial condition / attractor pair
        "fittest"                       ;selection mode ("elitism", "roulette")
        "ignore-wildcards"              ;how to handle wildcards within target states ("add-wildcards")
        "all"                           ; ("rand", or a list with concrete Genes)
        1                               ;modifier (0-1 for probabilistic mode, integer for concrete number)
        "overwrite"                     ;sharing mode ("append")
        nil                             ;max state number, just for apppend n
        )







More information are within the thesis Coevolution of Boolean Networks

## License

Copyright © 2017 Systems Biology Institute, Ulm university, Germany.
