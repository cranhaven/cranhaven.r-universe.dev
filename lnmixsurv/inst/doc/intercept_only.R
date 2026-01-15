## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  comment = "#>",
  eval = FALSE,
  include = TRUE
)

## ----eval = TRUE, include = TRUE----------------------------------------------
library(lnmixsurv)
library(dplyr)
library(tidyr)
library(readr)

mod1 <- survival_ln_mixture(Surv(y, delta) ~ NULL,
                            sim_data$data,
                            iter = 4000,
                            warmup = 2000,
                            intercept = TRUE,
                            starting_seed = 15,
                            em_iter = 50,
                            mixture_components = 3
)

chains <- bayesplot::mcmc_trace(mod1$posterior)

## -----------------------------------------------------------------------------
#  bayesplot::mcmc_trace(mod1$posterior)

## ----eval  = TRUE, echo = FALSE, fig.width = 7--------------------------------
chains

## ----eval = TRUE, fig.width=7-------------------------------------------------
km <- survival::survfit(
  Surv(y, delta) ~ NULL,
  sim_data$data
) |>
  broom::tidy() # Kaplan-Meier estimate

ggplot(km) +
  geom_step(aes(x = time, y = estimate),
            color = "darkslategrey"
  ) +
  labs(
    title = "Kaplan-Meier estimate",
    x = "t",
    y = "S(t)"
  ) +
  theme_bw()

## ----eval = TRUE, include = TRUE----------------------------------------------
predictions <- predict(mod1,
                       new_data = data.frame(val = NA),
                       type = "survival",
                       eval_time = seq(0, 300)
) |>
  tidyr::unnest(cols = .pred)

## ----fig.width=7, eval = TRUE-------------------------------------------------
ggplot() +
  geom_step(aes(x = time, y = estimate, linetype = "Kaplan-Meier"),
            color = "darkslategrey", data = km
  ) +
  geom_line(aes(x = .eval_time, y = .pred_survival, linetype = "Fitted"),
            color = "darkslategrey",
            data = predictions, alpha = 0.7
  ) +
  labs(
    title = "Fitted survival estimates",
    x = "t",
    y = "S(t)",
    linetype = "Type"
  ) +
  theme_bw()

