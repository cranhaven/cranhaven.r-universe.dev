## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lrstat)

## -----------------------------------------------------------------------------
caltime(nevents = 40, accrualDuration = 12, accrualIntensity = 200/12,
        lambda1 = -log(1-0.2)/12, lambda2 = -log(1-0.4)/12, 
        followupTime = 100)

## -----------------------------------------------------------------------------
lrpower(kMax = 1, criticalValues = 1.96, accrualDuration = 12, 
        accrualIntensity = 200/12, lambda1 = -log(1-0.2)/12, 
        lambda2 = -log(1-0.4)/12,  followupTime = 1.63)

## -----------------------------------------------------------------------------
hazardRatio = log(1-0.2)/log(1-0.4)
pnorm(abs(log(hazardRatio))*sqrt(40/4) - qnorm(0.975))

## -----------------------------------------------------------------------------
lrsim(kMax = 1, criticalValues = 1.96, 
      accrualIntensity = 200/12, 
      lambda1 = -log(1-0.2)/12, lambda2 = -log(1-0.4)/12,
      accrualDuration = 12, 
      plannedEvents = 40, 
      maxNumberOfIterations = 10000, seed = 314159)

