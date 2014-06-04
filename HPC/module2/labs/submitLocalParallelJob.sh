#!/bin/bash
#BSUB -R rusage[mem=2048] # ask for memory
#BSUB -R span[hosts=1]    # ask for all the cores on a single machine
#BSUB -W 0:10             # not sure what this is doing
#BSUB -q short  # which queue we want to run in

module load R/3.0.1
R CMD BATCH /home/ngr67a/BiP/permTest_foreach.R