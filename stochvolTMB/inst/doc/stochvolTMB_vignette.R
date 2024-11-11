## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3
)
library(stochvolTMB)

## -----------------------------------------------------------------------------
data(spy)
plot(spy$date, spy$log_return, type = "l", xlab = "", ylab = "", main = "Log-returns of S&P500")
plot(spy$date, spy$price, type = "l", xlab = "", ylab = "", main = "Price of S&P500")

## ----warning=FALSE------------------------------------------------------------
gaussian = estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)
t_dist = estimate_parameters(spy$log_return, model = "t", silent = TRUE)
skew_gaussian = estimate_parameters(spy$log_return, model = "skew_gaussian", silent = TRUE)
leverage = estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)

## -----------------------------------------------------------------------------
summary(t_dist, report = "transformed")

## -----------------------------------------------------------------------------
summary(skew_gaussian, report = "fixed")

## -----------------------------------------------------------------------------
summary(leverage, report = "transformed")

## -----------------------------------------------------------------------------

AIC(gaussian, 
    t_dist, 
    skew_gaussian, 
    leverage)

## -----------------------------------------------------------------------------
plot(leverage, include_ci = TRUE, plot_log = TRUE, dates = spy$date)
plot(leverage, include_ci = TRUE, plot_log = FALSE, dates = spy$date)

## -----------------------------------------------------------------------------

pred = predict(leverage, steps = 10, include_parameters = TRUE)
summary(pred)

# plot the forecast
plot(leverage, forecast = 50) + ggplot2::xlim(3200, nrow(spy) + 50)

## ----include=FALSE------------------------------------------------------------
stochvol_gauss <- readRDS("stochvol_gauss.rds")
stochvol_lev <- readRDS("stochvol_lev.rds")
stochvolTMB_gauss  <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE)
stochvolTMB_lev  <- estimate_parameters(spy$log_return, "leverage", silent = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library(stochvol)
#  
#  stochvol_gauss <- svsample(spy$log_return, quiet = T)
#  stochvolTMB_gauss  <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE)
#  
#  stochvol_lev <- svlsample(spy$log_return, quiet = T)
#  stochvolTMB_lev  <- estimate_parameters(spy$log_return, "leverage", silent = TRUE)

## -----------------------------------------------------------------------------

stochvol_gauss$para
summary(stochvolTMB_gauss, report = "transformed")
stochvol_lev$para
summary(stochvolTMB_lev, report = "transformed")


