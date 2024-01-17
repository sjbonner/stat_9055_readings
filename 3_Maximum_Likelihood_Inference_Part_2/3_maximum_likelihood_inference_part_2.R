## Load packages
library(tidyverse)
library(latex2exp)

## Set parameters
mu <- 1
theta <- mu - 1

n <- 25
xbar <- .95

## Compute LRT statistic

lrt <- function(mu, n, xbar,c = 0){
  - 2 * n * (log(xbar/mu) + xbar *(mu - xbar)/(mu * xbar)) - c
}

lrt_ci <- c(uniroot(lrt, c(.5, xbar), n = n, xbar = xbar, c= qchisq(.95,1)) $ root,
            uniroot(lrt, c(xbar, 1.5), n = n, xbar = xbar, c= qchisq(.95,1)) $ root)


## Plot LRT vs theta
mydat <- tibble(mu = seq(.5,1.5,length = 100)) %>%
  mutate(G = lrt(mu, n, xbar))

mydat %>%
  ggplot(aes(x = mu, y= G)) +
  geom_line() +
  geom_hline(yintercept = qchisq(.95,1), colour = "grey") +
  geom_vline(xintercept = xbar, lty = 2) + 
  ylim(c(NA, qchisq(.95,1) * 1.1)) +
  xlab(TeX("$\\mu$"))+
  ylab(TeX("$G^2$"))
