## MLR regression and diagnostics on the lung dataset
## Nicholas Reich

require(ggplot2)
library(visreg)
library(car)

dat <- read.table("~/Google Drive/teaching/methods2/datasets/lung/lungc.txt", header=TRUE)
head(dat)

################
## SLR models ##
################

layout(matrix(1:4, nrow=2))
slr1 <- lm(disease ~ crowding, data=dat)
summary(slr1)
plot(slr1)

slr2 <- lm(disease ~ education, data=dat)
plot(slr2)
## cook's distance measures the degree to which the parameter estimates change when an observation is left out

################
## MLR models ##
################

mlr1 <- lm(disease ~ crowding + education, data=dat)
mlr2 <- lm(disease ~ crowding + education + airqual, data=dat)
mlr3 <- lm(disease ~ crowding + factor(education) + airqual, data=dat)

summary(mlr1)
summary(mlr2)

## visualizing some models
layout(matrix(1:4, nrow=2))
plot(mlr2)
residualPlots(mlr2) ## with tests for curvature
residualPlots(mlr2, tests=FALSE)
residualPlots(mlr2, tests=FALSE, layout=c(1,4))
residualPlots(mlr3)

avPlot(mlr2, variable="airqual")
avPlot(mlr2, variable="education")

avPlot(mlr3, variable="education") ## get an error!
