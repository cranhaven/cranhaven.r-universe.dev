## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
ggplot2::theme_set(ggplot2::theme_bw())
set.seed(8675309)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(faux)

## ----plot-cars-orig, fig.cap="Original cars dataset"--------------------------
cars %>%
  ggplot(aes(speed, dist)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## ----plot-cars-sim, fig.cap="Simulated cars dataset"--------------------------
sim_df(cars, 500) %>%
  ggplot(aes(speed, dist)) + 
    geom_point() +
    geom_smooth(method = "lm", formula = "y~x")

## ----plot-mtcars-orig, fig.cap="Original mtcars dataset"----------------------
mtcars %>%
  mutate(transmission = factor(am, labels = c("automatic", "manual"))) %>%
  ggplot(aes(hp, wt, color = transmission)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## ----plot-iris-sim, fig.cap="Simulated iris dataset"--------------------------
sim_df(mtcars, 50 , between = "am") %>%
  mutate(transmission = factor(am, labels = c("automatic", "manual"))) %>%
  ggplot(aes(hp, wt, color = transmission)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## -----------------------------------------------------------------------------
exact_mtcars <- sim_df(mtcars, 50, between = "am", empirical = TRUE)

## ----plot-iris-sim-round, fig.cap="Simulated iris dataset (rounded)"----------
sim_df(mtcars, 50, between = "am") %>%
  mutate(hp = round(hp),
         transmission = factor(am, labels = c("automatic", "manual"))) %>%
  ggplot(aes(hp, wt, color = transmission)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## -----------------------------------------------------------------------------
data <- sim_design(2, 2, n = 10, plot = FALSE)
data$W1a[1:3] <- NA
data$W1b[1:6] <- NA
data

## -----------------------------------------------------------------------------
simdat <- sim_df(data, between = "B1", n = 1000, 
                 missing = TRUE)

## ---- echo = FALSE, results = 'asis'------------------------------------------
simdat %>%
  mutate(W1a = ifelse(is.na(W1a), "NA", "not NA"),
         W1b = ifelse(is.na(W1b), "NA", "not NA")) %>%
  count(B1, W1a, W1b) %>%
  group_by(B1) %>%
  mutate(n = round(n/sum(n), 2)) %>%
  knitr::kable()

