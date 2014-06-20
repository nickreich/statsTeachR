## locally parallelized permutation test
## BiP HPC workshop 2014
## Nicholas Reich

require(alr3)
data(heights)

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
out <- mclapply(as.list(c(1:nSim)), permTest, mc.cores=nCores)
mat <- do.call(rbind, out)
toc <- Sys.time() ## end the clock!

mat <- cbind(c(1:nrow(mat)), mat)
colnames(mat) <- c("iter", "b0", "b1")

(pval <- sum(abs(mat[,"b1"]) > realData_beta1)/nrow(mat))

