## Load packages
library(tidyverse)
library(broom)
library(DescTools)

## Parameters
n <- 500
beta <- c(-1,.2)

## Set seed for reproducibility
set.seed(777)

## Simulate data
sim_data <- tibble(id = as.factor(1:n),
                   x = runif(n, 0, 10),
                   eta = beta[1] + beta[2] * x,
                   p = (1 + exp(-eta))^-1,
                   y = 1 * (runif(n, 0, 1) < p))

## Fit logistic regression model
glm1 <- glm(y ~ x, family = binomial(), data = sim_data)

## Deviance goodness of fit test
pchisq(glm1$deviance, glm1$df.residual, lower.tail = FALSE)

## Residual plot
glm1_fitted <- augment(glm1)

glm1_fitted %>%
  ggplot(aes(x = x, y = .std.resid)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  ylab("Std. Residual")

## Pearson chi-square test
fitted <- fitted(glm1, type = "response")
chisq <- sum((sim_data$y - fitted)^2/(fitted * (1-fitted)))
pchisq(chisq, df = n - 2, lower.tail = TRUE)

## Hosmer-Lemeshow Test (10 groups)
HosmerLemeshowTest(fitted(glm1), sim_data$y)

## Hosmer-Lemeshow Test (100 groups)
HosmerLemeshowTest(fitted(glm1), sim_data$y, ngr = 100)

## le Cessie - van Houwelingen - Copas - Hosmer test
HosmerLemeshowTest(fitted(glm1), sim_data$y, X = cbind(sim_data$x))

## Grouped residual plot
ngroup <- 50
breaks <- seq(0,10, length = ngroup + 1)
values <- (breaks[-1] + breaks[-(ngroup+1)])/2

sim_data2 <- sim_data %>%
  mutate(Group = cut(x, breaks = breaks)) %>%
  group_by(Group) %>%
  dplyr::summarize(n = n(),
            y = sum(y)) %>%
  add_column(x2 = values)


glm1b <- glm(cbind(y,n-y) ~ x2, family = binomial(), data = sim_data2)

## Residual plot
glm1b_fitted <- augment(glm1b)

glm1b_fitted %>%
  ggplot(aes(x = x2, y = .std.resid)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  ylab("Std. Residual")

