## Load packages
library(tidyverse)
library(emmeans)
library(broom)

## Define data
melanoma = data.frame(type=rep(c("HMF","SSM","No","Ind"),rep(3,4)),
                      site=rep(c("H&N","Tru","Extr"),4),
                      y=c(22,2,10,16,54,115,19,33,73,11,17,28))

## Fit model
glm1 = glm(y ~ type * site,family=poisson(),data=melanoma)

## Test effects
anova(glm1,test="LRT")

## Tabulate coefficients
tidy(glm1, conf.int = TRUE)

## Compute predicted values
glm1_means <- emmeans(glm1,c("site","type"),type="response") %>%
  as_tibble()

  ## Plot predicted values
glm1_means %>%
  mutate(x = as.numeric(site) + (as.numeric(type) - 2.5)/10) %>%
  ggplot(aes(x=x,y=rate,colour=type)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  scale_x_continuous(name = "Location", breaks=1:3,labels=levels(glm1_means$site)) +
  labs(colour="Type")
