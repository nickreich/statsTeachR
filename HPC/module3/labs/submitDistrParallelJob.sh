#!/bin/bash
#BSUB -n 100
#BSUB -R rusage[mem=2048] # ask for memory
#BSUB -W 0:10
#BSUB -q parallel  # which queue we want to run in

module load R/3.0.1
R CMD BATCH /home/gjm43a/module3/permTest3_mclapply1.R
R CMD BATCH /home/gjm43a/module3/permTest3_mclapply2.R
