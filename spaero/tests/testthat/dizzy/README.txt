
This directory contains model files and output for the running the
dizzy program to generate results to compare with spaero's
simulator. The program was obtained from

http://magnet.systemsbiology.net/software/Dizzy/

Below are the commands used.

    time ../bin/runmodel.sh -modelFile ./sis.cmdl -stopTime 1000 -simulator gillespie-direct -ensembleSize 40000 -outputFile out.csv -computeFluctuations
    time ../bin/runmodel.sh -modelFile ./sis-linear-beta-trend.cmdl -stopTime 50 -simulator gillespie-direct -ensembleSize 1000 -outputFile out-linear-trend.csv -computeFluctuations
    time ../bin/runmodel.sh -modelFile ./sis-multiple-moving-parameters.cmdl -stopTime 20 -simulator gillespie-direct -ensembleSize 100 -outputFile out-multiple-moving-parameters.csv -computeFluctuations
