## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(flexFitR)
library(dplyr)
library(ggpubr)

## ----warning=FALSE, message=FALSE, fig.alt = "Plot x,y"-----------------------
dt <- data.frame(X = 1:6, Y = c(12, 16, 44, 50, 95, 100))
plot(explorer(dt, X, Y), type = "xy")

## ----warning=FALSE, message=FALSE---------------------------------------------
fn_lm <- function(x, b, m) {
  y <- b + m * x
  return(y)
}

## ----fig.alt = "Plot function"------------------------------------------------
plot_fn(fn = "fn_lm", params = c(b = 10, m = 5))

## ----warning=FALSE, message=FALSE---------------------------------------------
mod <- dt |>
  modeler(
    x = X,
    y = Y,
    fn = "fn_lm",
    parameters = c(b = -5, m = 10)
  )
mod

## ----fig.alt = "Plot evolution", fig.width= 8, fig.height=4-------------------
a <- plot(mod, color = "blue", title = "Raw data")
b <- plot(mod, type = 4, n_points = 200, color = "black")
ggarrange(a, b)

## -----------------------------------------------------------------------------
coef(mod)

## -----------------------------------------------------------------------------
vcov(mod)

## -----------------------------------------------------------------------------
predict(mod, x = 4.5)

## ----warning=FALSE, message=FALSE---------------------------------------------
mo <- lm(Y ~ X, data = dt)

## -----------------------------------------------------------------------------
summary(mo)$coefficients

## -----------------------------------------------------------------------------
vcov(mo)
predict(mo, newdata = data.frame(X = 4.5), se.fit = TRUE)

## ----warning=FALSE, message=FALSE---------------------------------------------
fun <- function(t, t1 = 45, t2 = 80, k = 0.9) {
  ifelse(
    test = t < t1,
    yes = 0,
    no = ifelse(
      test = t >= t1 & t <= t2,
      yes = k / (t2 - t1) * (t - t1),
      no = k
    )
  )
}

## ----fig.alt = "Plot x,y"-----------------------------------------------------
dt <- data.frame(
  time = c(0, 29, 36, 42, 56, 76, 92, 100, 108),
  variable = c(0, 0, 0.67, 15.11, 77.38, 99.81, 99.81, 99.81, 99.81)
)
plot(explorer(dt, time, variable), type = "xy")

## ----fig.alt = "Plot function"------------------------------------------------
plot_fn(fn = "fun", params = c(t1 = 25, t2 = 70, k = 90))

## ----warning=FALSE, message=FALSE---------------------------------------------
mod_1 <- dt |>
  modeler(
    x = time,
    y = variable,
    fn = "fun",
    parameters = c(t1 = 40, t2 = 70, k = 100)
  )
mod_1

## ----fig.alt = "Plot evolution"-----------------------------------------------
plot(mod_1)

## -----------------------------------------------------------------------------
# Coefficients
coef(mod_1)

## -----------------------------------------------------------------------------
# Variance-Covariance Matrix
vcov(mod_1)

## -----------------------------------------------------------------------------
# Making predictions
predict(mod_1, x = 45)

## -----------------------------------------------------------------------------
mod_nls <- dt |>
  nls(
    formula = variable ~ fun(time, t1, t2, k),
    start = c(t1 = 40, t2 = 70, k = 100),
    algorithm = "default"
  )
summary(mod_nls)
coef(mod_nls)
vcov(mod_nls)
predict(mod_nls, newdata = data.frame(time = 45))

## ----warning=FALSE, message=FALSE---------------------------------------------
init <- data.frame(uid = 1, t1 = 20, t2 = 30, k = 0.8)

mod_2 <- dt |>
  modeler(
    x = time,
    y = variable,
    fn = "fun",
    parameters = init
  )
mod_2
coef(mod_2)

## ----warning=FALSE, message=FALSE, fig.alt = "Plot evolution"-----------------
fix <- data.frame(uid = 1, k = 98)

mod_3 <- dt |>
  modeler(
    x = time,
    y = variable,
    fn = "fun",
    parameters = c(t1 = 40, t2 = 70, k = 100),
    fixed_params = fix
  )
mod_3
coef(mod_3)
plot(mod_3)

## -----------------------------------------------------------------------------
performance(mod_1, mod_2, mod_3)

