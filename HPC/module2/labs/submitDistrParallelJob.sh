#!/bin/bash
#BSUB -n 100
#BSUB -R rusage[mem=2048] # ask for memory
#BSUB -W 0:10
#BSUB -q parallel  # which queue we want to run in

module load R/3.0.1
R CMD BATCH /home/ngr67a/BiP/distrParallelPermTest.R /home/ngr67a/BiP/file1.Rout
R CMD BATCH /home/ngr67a/BiP/distrParallelPermTest.R /home/ngr67a/BiP/file2.Rout
R CMD BATCH /home/ngr67a/BiP/distrParallelPermTest.R /home/ngr67a/BiP/file3.Rout
R CMD BATCH /home/ngr67a/BiP/distrParallelPermTest.R /home/ngr67a/BiP/file4.Rout