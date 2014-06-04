
## plot 1
x2 <- rnorm(100)
y2 = -2-2*x2 + rnorm(100, 0, .5)
x2[1] <- -.2; y2[1] <- 4
x2[2] <- 3; y2[2] <- .2
x2[3] <- .1; y2[3] <- -6

qplot(x2, y2) + theme_bw() + geom_text(aes(label=LETTERS[1:3], x=x2[1:3]-.1, y=y2[1:3], color="red", fontface="bold")) + guides(color=FALSE)


## plot 2
x4 <- rnorm(100, mean=2, sd=.5)
y4 = -2+4*x4 + rnorm(100, 0, exp(x4))
qplot(x4, y4) + theme_bw()

