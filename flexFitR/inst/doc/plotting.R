## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(flexFitR)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(purrr)
data(dt_potato)
head(dt_potato) |> kable()

## -----------------------------------------------------------------------------
plots <- 2:7
mod <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_logistic",
    parameters = c(a = 4, t0 = 40, k = 100),
    subset = plots
  )

## ----fig.width= 8, fig.height=5, fig.alt="plot derivatives"-------------------
# Raw data with fitted curves
plot(mod, type = 1, color = "blue", id = plots, title = "Fitted curves")

## ----fig.width= 8, fig.height=4, fig.alt="plot coef"--------------------------
# Model coefficients
plot(mod, type = 2, color = "blue", id = plots, label_size = 10)

## -----------------------------------------------------------------------------
# Fitted curves only
c <- plot(mod, type = 3, color = "blue", id = plots, title = "Fitted curves")

## -----------------------------------------------------------------------------
# Fitted curves with confidence intervals
d <- plot(mod, type = 4, n_points = 200, title = "Fitted curve (uid = 2)")

## -----------------------------------------------------------------------------
# First derivative with confidence intervals
e <- plot(mod, type = 5, n_points = 200, title = "1st Derivative (uid = 2)")

## ----fig.width= 10, fig.height=7, fig.alt="plot derivatives"------------------
# Second derivative with confidence intervals
f <- plot(mod, type = 6, n_points = 200, title = "2nd Derivative (uid = 2)")
ggarrange(c, d, e, f)

