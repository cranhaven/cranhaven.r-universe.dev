## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(dplyr); library(tidyr); library(purrr) # Data wrangling
library(tidyfit) # Auto-ML modeling

## -----------------------------------------------------------------------------
data <- MASS::Boston
mod_frame <- data |> 
  regress(medv ~ ., m("hfr", kappa = c(0.25, 0.5, 0.75, 1))) |>
  unnest(settings)

# the tidyfit.models frame:
mod_frame

## -----------------------------------------------------------------------------
# the tidyFit object:
get_tidyFit(mod_frame, kappa == 1)

## ----fig.width=7, fig.height=6, fig.align='center'----------------------------
# the underlying model:
hfr_model <- get_model(mod_frame, kappa == 0.75)
plot(hfr_model, kappa = 0.75)

## ----fig.width=7, fig.height=6, fig.align='center'----------------------------
mod_frame |> 
  get_tidyFit(kappa == .25) |> 
  plot(kappa = .25)

## ----fig.width=7, fig.height=6, fig.align='center'----------------------------
# Store current par before editing
old_par <- par()

par(mfrow = c(2, 2))
par(family = "sans", cex = 0.7)
mod_frame |> 
  arrange(desc(kappa)) |> 
  select(model_object, kappa) |> 
  pwalk(~plot(.x, kappa = .y, 
              max_leaf_size = 2, 
              show_details = FALSE))

## -----------------------------------------------------------------------------
# Restore old par
par(old_par)

