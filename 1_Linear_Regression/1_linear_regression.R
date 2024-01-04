## Load packages
library(tidyverse)
library(GGally)
library(patchwork)
library(broom)

## Load mtcars package
data(mtcars)

## Initial manipulations
mycars <- mtcars |>
  as_tibble() |> # Convert data frame to tibble
  select(mpg, cyl, hp, wt, am) |> # Select variables of interest
  mutate(cyl = as.factor(cyl), am = as.factor(am)) # Convert cyl and am to factors

## Pairs plot
ggpairs(mycars,
        upper = list(continuous = "cor",
                     combo = "na",
                     discrete = "na"),
        lower = list(continuous = "points",
                     combo = "points",
                     discrete = "points"),
        diag = list(continuous = "barDiag", discrete = "barDiag"))

## Fit main effects model
lm1 <- lm(mpg ~ cyl + hp + wt + am, data = mycars)
summary(lm1)

confint(lm1)

## First interaction model
lm2 <- lm(mpg ~ cyl * am, data = mycars)
summary(lm2)
confint(lm2)

## Second interaction model
lm3 <- lm(mpg ~ wt * am, data = mycars)
summary(lm3)
confint(lm3)

## Third interaction model
lm4 <- lm(mpg ~ wt * hp, data = mycars)
summary(lm4)
confint(lm4)

## ANOVA for model including the number of cylinders, transmission type, and their interaction
anova(lm2)

## Compute AIC values for the models including cylinder, transmission type, and their interaction
lm2a <- lm(mpg ~ cyl + am, data = mycars)
lm2b <- lm(mpg ~ cyl, data = mycars)
lm2c <- lm(mpg ~ am, data = mycars)

AIC(lm2, lm2a, lm2b, lm2c)

## Compute BIC values for the models including cylinder, transmission type, and their interaction
BIC(lm2, lm2a, lm2b, lm2c)

## Perform stepwise selection
lm5 <- lm(mpg ~ (cyl + hp + wt + am)^2, data = mycars)
step(lm5)

## Compare models with the AIC
Model1 <- lm(mpg ~ cyl * hp, data = mycars)
Model2 <- lm(mpg ~ wt * am, data = mycars)
Model3 <- lm(mpg ~ cyl * am, data = mycars)
AIC(Model1, Model2, Model3)

## Add residuals to the original data
mycars <- lm2a |>
  augment()

## QQ-plot of residuals
plot1 <- mycars |>
  ggplot(aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Residuals")

## Plot of standardized residuals versus cyl
plot2 <- mycars |>
  ggplot(aes(x = cyl, y = .std.resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("Standardized Residuals")

## Plot of residuals versus cyl
plot3 <- mycars |>
  ggplot(aes(x = am, y = .std.resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("Standardized Residuals")

## Plot of residuals versus fitted values
plot4 <- mycars |>
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted") + 
  ylab("Standardized Residuals")

(plot1 + plot2) / (plot3 + plot4)

## Add residuals to the original data
mycars <- mycars |>
  rowid_to_column("Observation") |>
  mutate(DFfits = dffits(lm2a),
         DFbetas = dfbetas(lm2a))

## Leverage plot
plot1 <- mycars |>
  ggplot(aes(x= Observation, y = .hat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ylab("Leverage")

## Cook's Distance
plot2 <- mycars |>
  ggplot(aes(x= Observation, y = .cooksd)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ylab("Cook's Distance")

## DFfits
plot3 <- mycars |>
  ggplot(aes(x= Observation, y = DFfits)) +
  geom_point() +
  geom_hline(yintercept = 0)

## DFbetas
plot4 <- mycars |>
  ggplot(aes(x= Observation, y = DFbetas[,"(Intercept)"])) +
  geom_point() +
  geom_hline(yintercept = 0)

(plot1 + plot2) / (plot3 + plot4)

## Fitted values
augment(x = lm2a, interval = 'confidence') |>
  select(mpg, .fitted, .lower, .upper) |>
  print(n = 5)

augment(x = lm2a, interval = 'prediction') |>
  select(mpg, .fitted, .lower, .upper) |>
  print(n = 5)
