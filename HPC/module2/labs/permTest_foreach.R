## locally parallelized permutation test
## BiP HPC workshop 2014
## Nicholas Reich

require(doMC)
require(foreach)
nSim <- 100 ## number of permutations
nCores <- 10

registerDoMC(nCores)

########################################################
#### YOU WILL NEED TO CHANGE THIS WORKING DIRECTORY ####

#### Choose the directory where your heights.csv dataset is stored
setwd("/Users/nick/Documents/code_versioned/BiP/2013HPCwithR/modules/module2/labs/")
#setwd("/home/ngr67a/BiP/")
hts <- read.csv("heights.csv")

## fit initial model and create storage file
realDataModel <- lm(Dheight ~ Mheight, data=hts)
realData_beta1 <- coef(realDataModel)[2]

## run permutation loop
tic <- Sys.time()
mat <- foreach(i=1:nSim, .combine=rbind) %dopar% {
        permDhts <- sample(hts$Dheight, replace=FALSE)
        mdl <- lm(permDhts ~ hts$Mheight)
        c(i, coef(mdl))
}
toc <- Sys.time()
(toc-tic)

colnames(mat) <- c("iter", "b0", "b1")
(pval <- sum(abs(mat[,"b1"]) > realData_beta1)/nrow(mat))

require(parallel)
detectCores()
