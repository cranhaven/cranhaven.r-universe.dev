## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lrstat)

## -----------------------------------------------------------------------------
lrsamplesize(beta = 0.2, kMax = 2, 
             informationRates = c(0.8, 1),
             alpha = 0.025, typeAlphaSpending = "sfOF", 
             accrualTime = seq(0, 9),
             accrualIntensity = c(26/9*seq(1, 9), 26),
             piecewiseSurvivalTime = c(0, 6),
             lambda2 = rep(log(2)/13, 2),
             lambda1 = c(log(2)/13, 0.58*log(2)/13),
             gamma1 = -log(1-0.05)/12, 
             gamma2 = -log(1-0.05)/12,
             accrualDuration = 22, followupTime = NA)$resultsUnderH1

## -----------------------------------------------------------------------------
lrsim(kMax = 2, criticalValues = c(2.250, 2.025), 
      accrualTime = seq(0, 9),
      accrualIntensity = c(26/9*seq(1, 9), 26),
      piecewiseSurvivalTime = c(0, 6),
      lambda2 = rep(log(2)/13, 2),
      lambda1 = c(log(2)/13, 0.58*log(2)/13),
      gamma1 = -log(1-0.05)/12, 
      gamma2 = -log(1-0.05)/12,
      accrualDuration = 22,
      plannedEvents = c(252, 315), 
      maxNumberOfIterations = 10000, seed = 314159)

