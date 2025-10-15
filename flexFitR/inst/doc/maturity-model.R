## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(flexFitR)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(purrr)

## -----------------------------------------------------------------------------
data(dt_potato)
explorer <- explorer(dt_potato, x = DAP, y = c(GLI), id = Plot)

## ----fig.width= 8, fig.height=3, fig.alt="plot corr"--------------------------
p1 <- plot(explorer, type = "evolution", return_gg = TRUE, add_avg = TRUE)
p2 <- plot(explorer, type = "x_by_var", return_gg = TRUE)
ggarrange(p1, p2, nrow = 1)

## -----------------------------------------------------------------------------
kable(mutate_if(explorer$summ_vars, is.numeric, round, 2))

## ----fig.width= 8, fig.height=4, fig.alt="plot fn"----------------------------
plot_fn(
  fn = "fn_lin_pl_lin",
  params = c(t1 = 38.7, t2 = 62, t3 = 90, k = 0.32, beta = -0.01),
  interval = c(0, 108),
  color = "black",
  base_size = 15
)

## -----------------------------------------------------------------------------
# Define constraints and bounds for the model
lower_bounds <- c(t1 = 0, t2 = 0, dt = 0, k = 0, beta = -Inf)
upper_bounds <- c(t1 = Inf, t2 = Inf, dt = Inf, k = Inf, beta = 0)
# Initial values
initial_vals <- c(t1 = 38, t2 = 62, dt = 28, k = 0.32, beta = -0.01)

## ----warning=FALSE, message=FALSE---------------------------------------------
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = GLI,
    grp = Plot,
    fn = "fn_lin_pl_lin2",
    parameters = initial_vals,
    lower = lower_bounds,
    upper = upper_bounds,
    method = c("nlminb", "L-BFGS-B"),
    subset = c(195, 40)
  )

## -----------------------------------------------------------------------------
print(mod_1)

## ----fig.width= 8, fig.height=4, fig.alt="plot fit 1"-------------------------
plot(mod_1, id = c(195, 40))
kable(mod_1$param)

## -----------------------------------------------------------------------------
coef(mod_1, id = 40)

## -----------------------------------------------------------------------------
confint(mod_1, id = 40)

## -----------------------------------------------------------------------------
vcov(mod_1, id = 40)

## ----fig.width= 8, fig.height=4, fig.alt="plot coef"--------------------------
plot(mod_1, type = 2, id = c(195, 40), label_size = 8)

## ----fig.width= 8, fig.height=4, fig.alt="plot derivatives"-------------------
a <- plot(mod_1, type = 4, color = "black", title = "Fitted Curve + CIs & PIs")
b <- plot(mod_1, type = 5, color = "black")
ggarrange(a, b)

