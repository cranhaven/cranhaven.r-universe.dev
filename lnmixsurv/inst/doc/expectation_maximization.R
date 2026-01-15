## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  comment = "#>",
  eval = FALSE,
  include = TRUE
)

## ----eval=TRUE, include=TRUE--------------------------------------------------
library(lnmixsurv)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

set.seed(8)

data <- simulate_data(6000,
    mixture_components = 3, k = 2,
    percentage_censored = 0.3
  )$data |>
    rename(x = cat, y = t)

model_em <- survival_ln_mixture_em(Surv(y, delta) ~ x,
    data = data,
    iter = 200,
    starting_seed = 20,
    number_em_search = 0
  )

gg <- plot_fit_on_data(model_em, data)$ggplot

## ----fig.width=7, eval = TRUE-------------------------------------------------
plot(model_em)

## -----------------------------------------------------------------------------
#  plot_fit_on_data(model_em, data = data, type = "survival")$ggplot

## ----fig.width = 7, echo=FALSE, eval = TRUE-----------------------------------
gg

## ----eval = TRUE--------------------------------------------------------------
model_em <- survival_ln_mixture_em(Surv(y, delta) ~ x,
  data = data,
  iter = 200,
  starting_seed = 20,
  number_em_search = 200,
  show_progress = TRUE
)

## ----include=FALSE, eval = TRUE-----------------------------------------------
gg <- plot_fit_on_data(model_em, data)$ggplot

## -----------------------------------------------------------------------------
#  plot_fit_on_data(model_em, data = data, type = "survival")$ggplot

## ----fig.width = 7, echo = FALSE, eval = TRUE---------------------------------
gg

