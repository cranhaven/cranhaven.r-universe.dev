## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7 / 1.61,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(oncomsm)

## ----weibull-hazards, echo=FALSE, warning=FALSE-------------------------------
h <- function(t, a, b) b / a * (t / a)^(b - 1)
shapes <- c(0.75, 1, 2, 3)
t <- pmax(0.1, seq(0, 12, length.out = 100))
tidyr::expand_grid(
  t = t,
  tibble(
    a = rep(6, length(shapes)) / log(2)^(1 / shapes),
    b = shapes
  )
) %>%
mutate(
  hazard = h(t, a, b),
  label = sprintf("median=%5.2f, b=%5.2f", a * log(2)^(1 / shapes), b)
) %>%
ggplot() +
  aes(t, hazard, color = label) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1))

## -----------------------------------------------------------------------------
res <- oncomsm:::get_mu_sigma(1, 36)
median_transition_time <- t
tibble(
  median_transition_time = median_transition_time,
  pdf = dlnorm(median_transition_time, res$mu, res$sigma)
) %>%
ggplot() +
  aes(median_transition_time, pdf) +
  geom_line()

## -----------------------------------------------------------------------------
res <- oncomsm:::get_mu_sigma(0.9, 2.5)
shape <- seq(0, 5, length.out = 100)
set.seed(251L)
tibble(
  b = rlnorm(100, res$mu, res$sigma),
  a = 6
) %>%
expand_grid(t = t) %>%
mutate(
  survival = 1 - pweibull(t, b, a),
  label = sprintf("a=%5.2f, b=%5.2f", a, b)
) %>%
ggplot() +
  aes(t, survival, group = label) +
  geom_line(alpha = .2)

## -----------------------------------------------------------------------------
tibble(shape = shape, pdf = dlnorm(shape, res$mu, res$sigma)) %>%
ggplot() +
  aes(shape, pdf) +
  geom_line()

## -----------------------------------------------------------------------------
set.seed(251L)
tibble(
  b = rlnorm(100, oncomsm:::get_mu_sigma(0.75, 3)$mu,
             oncomsm:::get_mu_sigma(0.75, 3)$sigma),
  a = purrr::map_dbl(b, function(x) {
      res <- oncomsm:::get_mu_sigma(2, 6)
      rlnorm(1, res$mu, res$sigma) / log(2)^(1 / x)
    }
  )
) %>%
expand_grid(t = t) %>%
mutate(
  survival = 1 - pweibull(t, b, a),
  label = sprintf("a=%5.2f, b=%5.2f", a, b)
) %>%
ggplot() +
  aes(t, survival, group = label) +
  geom_line(alpha = .2)

