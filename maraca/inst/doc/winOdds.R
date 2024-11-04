## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
library(dplyr)
library(ggplot2)
library(maraca)

## ----maraca1, eval = TRUE-----------------------------------------------------
library(maraca)

data(hce_scenario_a)

## -----------------------------------------------------------------------------
maraca_dat <- maraca(
  data = hce_scenario_a,
  step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
  last_outcome = "Continuous outcome",
  fixed_followup_days = 3 * 365,
  column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
  arm_levels = c(active = "Active", control = "Control"),
  # Make sure to calculate the win odds
  compute_win_odds = TRUE
)

## ----fig.width=7, fig.height=6------------------------------------------------
component_plot(maraca_dat)

## ----fig.width=7, fig.height=6------------------------------------------------
library(hce)

Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
Rates_P <- c(2.47, 2.24, 2.9, 4, 6)

hce_dat <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
                  CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
                  seed = 31337)

component_plot(hce_dat)

## -----------------------------------------------------------------------------
maraca_dat <- maraca(
  data = hce_scenario_a,
  step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
  last_outcome = "Continuous outcome",
  fixed_followup_days = 3 * 365,
  column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
  arm_levels = c(active = "Active", control = "Control"),
  # Make sure to calculate the win odds
  compute_win_odds = TRUE
)

## ----fig.width=7, fig.height=6------------------------------------------------
cumulative_plot(maraca_dat)

## ----fig.width=7, fig.height=6------------------------------------------------
cumulative_plot(hce_dat)

## ----fig.width=7, fig.height=6------------------------------------------------
cumulative_plot(maraca_dat, include = "win odds")

## ----fig.width=7, fig.height=6------------------------------------------------
cumulative_plot(hce_dat, reverse = TRUE)

## ----fig.width=7, fig.height=6------------------------------------------------
component_plot(maraca_dat) +
  ggplot2::scale_fill_manual(values = c("seagreen", "red", "grey"), name = NULL)

## ----fig.width=7, fig.height=6------------------------------------------------
p <- cumulative_plot(maraca_dat)
# Accessing the first ggplot2 object and adding styling (bar plot)
p[[1]] <- p[[1]] +
  ggplot2::scale_fill_manual(values = c("seagreen", "red", "grey"), name = NULL)
p

## ----fig.width=7, fig.height=6------------------------------------------------
component_plot(maraca_dat, theme = "maraca")

## ----fig.width=7, fig.height=6------------------------------------------------
cumulative_plot(maraca_dat, theme = "color1")

## ----fig.width=7, fig.height=6------------------------------------------------
component_plot(maraca_dat, theme = "color2")

## ----fig.width=8, fig.height=6------------------------------------------------
cumulative_plot(maraca_dat, theme = "none")

