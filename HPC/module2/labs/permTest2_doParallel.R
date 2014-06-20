## locally parallelized permutation test
## BiP HPC workshop 2014
## Nicholas Reich

require(alr3)
data(heights)

#require(foreach)
nSim <- 100 ## number of permutations
nCores <- 10

tic <- Sys.time() ## start the clock!
require(doParallel)
cl <- makeCluster(nCores)
registerDoParallel(cl)

## fit initial model and create storage file
realDataModel <- lm(Dheight ~ Mheight, data=heights)
realData_beta1 <- coef(realDataModel)[2]

## run permutation loop
mat <- foreach(i=1:nSim, .combine=rbind) %dopar% {
        permDhts <- sample(heights$Dheight, replace=FALSE)
        mdl <- lm(permDhts ~ heights$Mheight)
        c(i, coef(mdl))
}
toc <- Sys.time() ## end the clock!
(toc-tic)

stopCluster(cl)
colnames(mat) <- c("iter", "b0", "b1")
(pval <- sum(abs(mat[,"b1"]) > realData_beta1)/nrow(mat))

require(parallel)
detectCores()
