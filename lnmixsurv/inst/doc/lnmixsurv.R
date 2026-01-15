## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  comment = "#>",
  eval = FALSE,
  include = TRUE,
  out.width = "100%"
)

## ----eval = TRUE, include = TRUE----------------------------------------------
library(lnmixsurv)
library(readr)

mod1 <- survival_ln_mixture(Surv(y, delta) ~ x,
                            sim_data$data,
                            starting_seed = 20)

mod1

## ----eval=TRUE, include=TRUE--------------------------------------------------
library(censored)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

mod_spec <- survival_reg() |>
  set_engine("survival_ln_mixture", starting_seed = 20) |>
  set_mode("censored regression")

mod2 <- mod_spec |>
  fit(Surv(y, delta) ~ x, sim_data$data)      

## ----eval = TRUE--------------------------------------------------------------
tidy(mod1)
tidy(mod2)

## ----prediction---------------------------------------------------------------
#  library(ggplot2)
#  library(dplyr)
#  library(tidyr)
#  library(purrr)
#  
#  models <- list(formula = mod1, tidymodels = mod2)
#  
#  new_data <- sim_data$data |> distinct(x)
#  pred_sob <- map(models, ~ predict(.x, new_data,
#                                    type = "survival",
#                                    eval_time = seq(120)
#  ))
#  
#  bind_rows(pred_sob, .id = "modelo") |>
#    group_by(modelo) |>
#    mutate(id = new_data$x) |>
#    ungroup() |>
#    unnest(cols = .pred) |>
#    ggplot(aes(x = .eval_time, y = .pred_survival, col = id)) +
#    geom_line() +
#    theme_bw() +
#    facet_wrap(~modelo)

## ----echo = FALSE, eval = TRUE, fig.width=7-----------------------------------
models <- list(formula = mod1, tidymodels = mod2)

new_data <- sim_data$data |> distinct(x)

pred_sob <- map(models, ~ predict(.x, new_data,
                                  type = "survival",
                                  eval_time = seq(120)
))

bind_rows(pred_sob, .id = "modelo") |>
  group_by(modelo) |>
  mutate(id = new_data$x) |>
  ungroup() |>
  unnest(cols = .pred) |>
  ggplot(aes(x = .eval_time, y = .pred_survival, col = id)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~modelo)

