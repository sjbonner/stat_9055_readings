library(tidyverse)
library(broom)

## Load data
ducks <- read_csv("ducks_on_lakes.csv")

## Exploratory Plot 1: Ducks per lake
ducks %>%
  ggplot(aes(x = Fish, y = Ducks))+
  geom_point() +
  xlim(c(0,NA)) +
  ylim(c(0,NA)) + 
  xlab("Size of Biggest Fish (kg)") + 
  ylab("Number of Ducks")

## Model 1: Poisson Model
glm1 <- glm(Ducks ~ Fish,family=poisson(),data=ducks)
summary(glm1)
tidy(glm1, conf.int = TRUE)

## Residual Plot
ducks1 <- augment(glm1)

ducks1 %>%
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  xlab("Fitted Value") +
  ylab("Standardized Residual") + 
  geom_hline(yintercept = 0)

## Exploratory Plot 2: Ducks per square kilometer
ducks %>%
  mutate(Density = Fish/Area) %>%
  ggplot(aes(x=Ducks,y = Density)) +
  geom_point() + 
  xlab("Size of Biggest Fish (kg)") + 
  ylab("Number of Ducks/km^2")

## Model 2: Poisson model with offset
glm2 <- glm(Ducks ~ Fish,family=poisson(),offset=log(Area),data=ducks)
tidy(glm2, conf.int = TRUE)

## Plot residuals
ducks2 <- augment(glm2)

ducks2 %>%
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  xlab("Fitted Value") +
  ylab("Standardized Residual") + 
  geom_hline(yintercept = 0)

