## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  eval=rmarkdown::pandoc_available("1.12.3")
)

library(MBNMAtime)
library(rmarkdown)
library(knitr)
library(dplyr)
#load(system.file("extdata", "vignettedata.rda", package="MBNMAtime", mustWork = TRUE))

## ---- eval=FALSE--------------------------------------------------------------
#  tspline(type="bs", knots=3)
#  # ...is equivalent to
#  tspline(type="bs", knots=c(0.25,0.5,0.75))

## ---- results="hide"----------------------------------------------------------
# Prepare data using the alogliptin dataset
network.alog <- mb.network(alog_pcfb, reference = "placebo")

# Run a linear time-course MBNMA
mbnma <- mb.run(network.alog, fun=tpoly(degree=1, pool.1="rel", method.1="common"))

## -----------------------------------------------------------------------------
summary(mbnma)

## ---- results="hide"----------------------------------------------------------
# Run an Emax time-course MBNMA with two parameters
mbnma <- mb.run(network.alog, fun=temax(
  pool.emax = "rel", method.emax="common",
  pool.et50 = "abs", method.et50="common"
))

## -----------------------------------------------------------------------------
summary(mbnma)

## ---- eval=TRUE, results="hide"-----------------------------------------------
# Using the COPD dataset
network.copd <- mb.network(copd)

# Run an log-linear time-course MBNMA 
# that accounts for correlation between time points using variance adjustment
mbnma <- mb.run(network.copd, 
                fun=tloglin(pool.rate="rel", method.rate="random"),
                rho="dunif(0,1)", covar="varadj")

## ---- results="hide", message=FALSE, warning=FALSE----------------------------
# Create network object of gout dataset
network.gout <- mb.network(goutSUA_CFBcomb)

# Run a B-spline time-course MBNMA with a knot at 0.2 times the max follow-up
# Common class effect on beta.2, the 2nd spline coefficient
mbnma <- mb.run(network.gout, 
                fun=tspline(type="bs", knots=c(0.2),
                            pool.1 = "rel", method.1="common",
                            pool.2="rel", method.2="random"),
                class.effect = list(beta.2="common"))


## -----------------------------------------------------------------------------
summary(mbnma)

## ---- eval=FALSE--------------------------------------------------------------
#  mbnma <- mb.run(network.copd,
#                  fun=tloglin(pool.rate="rel", method.rate="random"),
#                  priors=list(rate="dnorm(0,2) T(0,)"))

## ---- results="hide"----------------------------------------------------------
# Define informative priors for spline parameters
spline.priors <- list(
  d.3 = c(
  Aclidinium="dnorm(-0.5, 100)", 
  Tiotropium="dnorm(0, 0.0001)"
  ),
  d.4 = c(
  Aclidinium="dnorm(0, 100)", 
  Tiotropium="dnorm(0, 0.0001)"
  ))

# Using the COPD dataset with a B-spline MBNMA
mbnma <- mb.run(network.copd, fun=tspline(degree=2, knots=c(0.1,0.5)),
                priors=spline.priors)

## -----------------------------------------------------------------------------
# Predict and plot time-course relative effect
pred <- predict(mbnma)
plot(pred)

