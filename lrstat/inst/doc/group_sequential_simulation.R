## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lrstat)

## -----------------------------------------------------------------------------
lrstat(time=c(20, 25, 30, 35, 38.5), allocationRatioPlanned = 3, 
       accrualIntensity = 5, 
       lambda2 = 0.95/12, lambda1 = 0.3*0.95/12, 
       gamma1 = -log(1-0.1)/24, gamma2 = -log(1-0.1)/24, 
       accrualDuration = 32, followupTime = 6.5, fixedFollowup = TRUE)

## -----------------------------------------------------------------------------
lrsim(kMax = 3, informationRates = c(0.5, 0.75, 1), 
      criticalValues = c(6, 2.34, 2.012), 
      futilityBounds = c(0.282, -6), 
      allocation1 = 3, allocation2 = 1,
      accrualTime = 0, accrualIntensity = 5, 
      piecewiseSurvivalTime = 0, 
      stratumFraction = 1, 
      lambda1 = 0.3*0.95/12, lambda2 = 0.95/12, 
      gamma1 = -log(1-0.1)/24, gamma2 = -log(1-0.1)/24, 
      accrualDuration = 32, followupTime = 6.5, 
      fixedFollowup = TRUE, 
      rho1 = 0, rho2 = 0, 
      plannedEvents = c(16, 24, 32), 
      maxNumberOfIterations = 1000, 
      maxNumberOfRawDatasetsPerStage = 0, 
      seed = 12345)

## -----------------------------------------------------------------------------
lrsim(kMax = 3, informationRates = c(0.5, 0.75, 1), 
      criticalValues = c(6, 2.34, 2.012), 
      futilityBounds = c(0.282, -6), 
      allocation1 = 3, allocation2 = 1,
      accrualTime = 0, accrualIntensity = 5, 
      piecewiseSurvivalTime = 0, 
      stratumFraction = 1, 
      lambda1 = 0.95/12, lambda2 = 0.95/12, 
      gamma1 = -log(1-0.1)/24, gamma2 = -log(1-0.1)/24, 
      accrualDuration = 32, followupTime = 6.5, 
      fixedFollowup = TRUE, 
      rho1 = 0, rho2 = 0, 
      plannedEvents = c(16, 24, 32), 
      maxNumberOfIterations = 1000, 
      maxNumberOfRawDatasetsPerStage = 0, 
      seed = 12345)

