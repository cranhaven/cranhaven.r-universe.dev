## ----initial, echo = FALSE, cache = FALSE, results = 'hide'-------------------
library(knitr)
opts_chunk$set(
  warning = TRUE,
  message = TRUE,
  echo = TRUE,
  cache = TRUE,
  fig.width = 7,
  fig.height = 4,
  fig.align = 'centre',
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gratis)
library(feasts)
set.seed(5)

## ----marmodel-----------------------------------------------------------------
qmar <- mar_model(seasonal_periods = 4)
qmar

## ----marplot------------------------------------------------------------------
qmar %>%
  generate(nseries = 9, length = 20) %>%
  autoplot(value)

## ----hmarmodel----------------------------------------------------------------
hmar <- mar_model(seasonal_periods = c(24, 7*24))
hmar %>%
  generate(nseries = 1, length= 2*7*24) %>%
  autoplot(value)

## ----myfeatures---------------------------------------------------------------
library(tsfeatures)
my_features <- function(y) {
  c(stl_features(y)[c("trend", "seasonal_strength", "peak", "trough")])
}
y <- simulate_target(
  length = length(USAccDeaths),
  seasonal_periods = frequency(USAccDeaths),
  feature_function = my_features, target = my_features(USAccDeaths)
)
# Make new series same scale and frequency as USAccDeaths
y <- ts(scale(y) * sd(USAccDeaths) + mean(USAccDeaths))
tsp(y) <- tsp(USAccDeaths)
cbind(USAccDeaths, y) %>% autoplot()
cbind(my_features(USAccDeaths), my_features(y))

## -----------------------------------------------------------------------------
library(dplyr)
my_features <- function(y) {
  c(entropy(y), acf = acf(y, plot = FALSE)$acf[2:3, 1, 1])
}
df <- generate_target(
  length = 60, feature_function = my_features, target = c(0.5, 0.9, 0.8)
)
df %>%
 as_tibble() %>%
 group_by(key) %>%
 summarise(value = my_features(value),
           feature=c("entropy","acf1", "acf2"),
           .groups = "drop")
df %>% autoplot(value)

## ----arimamodel---------------------------------------------------------------
mod <- arima_model(frequency = 4)
mod

