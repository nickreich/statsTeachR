## locally parallelized permutation test
## BiP HPC workshop 2014
## Nicholas Reich

require(doMC)
require(foreach)
require(parallel)
nSim <- 100 ## number of permutations
nCores <- 10



setwd("/Users/gregorymatthews/Dropbox/BiPSandbox/code")
#setwd("/home/ngr67a/BiP/")
hts <- read.csv("heights.csv")

## if output file exists already, remove it
file.remove("coefsDistr.csv")

## Use mclapply
permTest<-function(x){
  permDhts <- sample(hts$Dheight, replace=FALSE)
  mdl <- lm(permDhts ~ hts$Mheight)
  output <- coef(mdl)
  lineText <- paste(round(output,4), collapse=",")
  #cat(c(lineText, "\n"), file="coefsDistr.csv", append=TRUE)
  output
}
out <- mclapply(as.list(c(1:100)),permTest,mc.cores=2)
mat <- do.call(rbind,out)
mat <- cbind(c(1:dim(mat)[1]),mat)
colnames(mat) <- c("iter", "b0", "b1")

