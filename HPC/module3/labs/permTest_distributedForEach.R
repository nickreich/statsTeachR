## locally parallelized permutation test
## BiP HPC workshop 2014
## Nicholas Reich

## Made available under the Creative Commons Attribution-ShareAlike 3.0 Unported License: http://creativecommons.org/licenses/by-sa/3.0/deed.en\textunderscore US 

require(doMC)
require(foreach)
nSim <- 100 ## number of permutations
nCores <- 10

registerDoMC(nCores)

setwd("/Users/module2/labs/")
#setwd("/home/ngr67a/BiP/")
hts <- read.csv("heights.csv")

## if output file exists already, remove it
file.remove("coefsDistr.csv")

## run permutation loop, storing each time
tic <- Sys.time()
mat <- foreach(i=1:nSim, .combine=rbind) %dopar% {
        permDhts <- sample(hts$Dheight, replace=FALSE)
        mdl <- lm(permDhts ~ hts$Mheight)
        output <- c(i, coef(mdl))
        lineText <- paste(round(output,4), collapse=",")
        cat(c(lineText, "\n"), file="coefsDistr.csv", append=TRUE)
        output
}
toc <- Sys.time()
(toc-tic)
colnames(mat) <- c("iter", "b0", "b1")

## notes that lines in coefDistr.csv are not in order
## rows in mat object are
