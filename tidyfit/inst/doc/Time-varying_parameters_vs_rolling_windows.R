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
  results <- readRDS("vignette_tvpvsrw.rds")
  beta <- results$beta
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
  filter(Industry == "HiTec") |>
  select(-Industry)

## -----------------------------------------------------------------------------
# mod_rolling <- data |>
#   regress(Return ~ CMA + HML + `Mkt-RF` + RMW + SMB,
#           m("lm", vcov. = "HAC"),
#           .cv = "sliding_index", .cv_args = list(lookback = years(5), step = 6, index = "Date"),
#           .force_cv = TRUE, .return_slices = TRUE)

## -----------------------------------------------------------------------------
# mod_tvp <- data |>
#   regress(Return ~ .,
#           m("tvp", sv = TRUE, niter = 1000, index_col = "Date"))

## -----------------------------------------------------------------------------
# mod_frame <- bind_rows(mod_rolling, mod_tvp)

## -----------------------------------------------------------------------------
# beta <- coef(mod_frame)

## -----------------------------------------------------------------------------
# beta <- beta |>
#   unnest(model_info) |>
#   mutate(lower = ifelse(is.na(lower), estimate - 2*std.error, lower)) |>
#   mutate(upper = ifelse(is.na(upper), estimate + 2*std.error, upper)) |>
#   mutate(date = coalesce(as.Date(index), as.Date(slice_id)))

## ----fig.width=7, fig.height=6, fig.align="center", eval=TRUE-----------------
beta |>
  ggplot(aes(date, estimate)) +
  facet_wrap("term", scales = "free", ncol = 2) +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = model), alpha = 0.25) +
  geom_line(aes(color = model)) +
  theme_bw(8)

