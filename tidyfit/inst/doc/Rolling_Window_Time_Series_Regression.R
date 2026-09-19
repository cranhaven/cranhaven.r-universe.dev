## ----include = FALSE----------------------------------------------------------
use_saved_results <- TRUE

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  eval = !use_saved_results,
  message = FALSE,
  warning = FALSE
)

if (use_saved_results) {
  results <- readRDS("vignette_rwtsr.rds")
  df_beta <- results$df_beta
}

## ----eval=TRUE----------------------------------------------------------------
library(dplyr); library(tidyr); library(purrr) # Data wrangling
library(ggplot2); library(stringr) # Plotting
library(lubridate)   # Date calculations
library(tidyfit)     # Model fitting

## ----eval=TRUE----------------------------------------------------------------
data <- Factor_Industry_Returns
data <- data |>
  mutate(Date = ym(Date)) |>         # Parse dates
  mutate(Return = Return - RF) |>    # Excess return
  select(-RF)
data

## ----eval=TRUE----------------------------------------------------------------
data <- data |>
  group_by(Industry)

## ----eval=TRUE----------------------------------------------------------------
mod_frame <- data |>
  regress(Return ~ CMA + HML + `Mkt-RF` + RMW + SMB, m("lm"))

mod_frame

## ----eval=TRUE----------------------------------------------------------------
mod_frame_hac <- data |>
  regress(Return ~ CMA + HML + `Mkt-RF` + RMW + SMB, m("lm", vcov. = "HAC"))

mod_frame_hac

## -----------------------------------------------------------------------------
# mod_frame_rolling <- data |>
#   regress(Return ~ CMA + HML + `Mkt-RF` + RMW + SMB,
#           m("lm", vcov. = "HAC"),
#           .cv = "sliding_index", .cv_args = list(lookback = years(5), step = 6, index = "Date"),
#           .force_cv = TRUE, .return_slices = TRUE)

## -----------------------------------------------------------------------------
# df_beta <- coef(mod_frame_rolling)

## ----eval=TRUE----------------------------------------------------------------
df_beta

## ----eval=TRUE----------------------------------------------------------------
df_beta <- df_beta |>
  unnest(model_info) |>
  mutate(upper = estimate + 2 * std.error, lower = estimate - 2 * std.error)

## ----eval=TRUE----------------------------------------------------------------
df_beta

## ----fig.width=7, fig.height=5, fig.align="center", eval=TRUE-----------------
df_beta |>
  mutate(slice_id = as.Date(slice_id)) |>
  filter(term == "Mkt-RF") |>
  ggplot(aes(slice_id)) +
  geom_hline(yintercept = 1) +
  facet_wrap("Industry", scales = "free") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.25) +
  geom_line(aes(y = estimate)) +
  theme_bw(8)

## ----fig.width=7, fig.height=5, fig.align="center", eval=TRUE-----------------
df_beta |>
  mutate(slice_id = as.Date(slice_id)) |>
  filter(term == "(Intercept)") |>
  ggplot(aes(slice_id)) +
  geom_hline(yintercept = 0) +
  facet_wrap("Industry", scales = "free") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.25) +
  geom_line(aes(y = estimate)) +
  theme_bw(8)

## ----fig.width=6, fig.height=4.25, fig.align="center", eval=TRUE--------------
df_beta |>
  mutate(slice_id = as.Date(slice_id)) |>
  filter(Industry == "HiTec") |>
  ggplot(aes(slice_id)) +
  geom_hline(yintercept = 0) +
  facet_wrap("term", scales = "free") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.25) +
  geom_line(aes(y = estimate)) +
  theme_bw(8)

