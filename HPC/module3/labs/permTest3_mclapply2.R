## locally parallelized permutation test
## BiP HPC workshop 2014
## Nicholas Reich and Gregory J. Matthews

##Change to YOUR ACCOUNT NAME!
setwd("/home/gjm43a/module3")
heights <- read.csv("heights.csv")

#require(foreach)
nSim <- 100 ## number of permutations
nCores <- 10

tic <- Sys.time() ## start the clock!
require(parallel)

## fit the data to the real data
realDataModel <- lm(Dheight ~ Mheight, data=heights)
realData_beta1 <- coef(realDataModel)[2]

## Define function for mclapply. 
## This function needs an argument, but in this case, won't use it.
permTest<-function(x){
  permDhts <- sample(heights$Dheight, replace=FALSE)
  mdl <- lm(permDhts ~ heights$Mheight)
  coef(mdl)
}

## use the function above to run the test in parallel
start <- Sys.time()
out <- mclapply(as.list(c(1:nSim)), permTest, mc.cores=nCores)
end <- Sys.time()
end-start
mat <- do.call(rbind, out)
toc <- Sys.time() ## end the clock!
toc-tic
mat <- cbind(c(1:nrow(mat)), mat)
colnames(mat) <- c("iter", "b0", "b1")

write.csv(mat,"coefsMatrix2.csv")

#(pval <- sum(abs(mat[,"b1"]) > realData_beta1)/nrow(mat))

