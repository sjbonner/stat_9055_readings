## Load packages
library(tidyverse)
library(tikzDevice)

## Define parameters
b1 <- 1/2
b0 <- -5*b1

## Compute curve
plotDF <- data.frame(x=seq(0,10,length=100))

plotDF <- mutate(plotDF,eta=b0 + b1 * x,p=(1 + exp(-eta))^-1)

## Draw plot
tikz(file="geometric_interpretation.tex",standAlone=FALSE,height=3)

qplot(data=plotDF,x=x,y=p,geom="line",ylim=c(0,1),ylab="$\\pi(x)$",xlab="$x$") +
    geom_abline(intercept=.5 + b0/4,slope=b1/4) +
    geom_vline(xintercept=-b0/b1+ c(-1/b1,1/b1),lty=2)

dev.off()
