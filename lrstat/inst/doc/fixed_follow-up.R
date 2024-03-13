## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lrstat)

## -----------------------------------------------------------------------------
lrsamplesize(beta = 0.1, kMax = 1, criticalValues = 1.96, 
             allocationRatioPlanned = 3, accrualIntensity = 5, 
             lambda2 = 0.95/12, lambda1 = 0.3*0.95/12, 
             gamma1 = -log(1-0.1)/24, gamma2 = -log(1-0.1)/24, 
             accrualDuration = NA, followupTime = 26/4, fixedFollowup = TRUE)

## -----------------------------------------------------------------------------
lrsim(kMax = 1, criticalValues = 1.96,  
      allocation1 = 3, allocation2 = 1,
      accrualIntensity = 5, 
      lambda2 = 0.95/12, lambda1 = 0.3*0.95/12, 
      gamma1 = -log(1-0.1)/24, gamma2 = -log(1-0.1)/24,
      accrualDuration = 39.2, followupTime = 6.5, 
      fixedFollowup = TRUE,  
      plannedEvents = 39, 
      maxNumberOfIterations = 1000, seed = 12345)

lrsim(kMax = 1, criticalValues = 1.96,  
      allocation1 = 3, allocation2 = 1,
      accrualIntensity = 5, 
      lambda2 = 0.95/12, lambda1 = 0.3*0.95/12, 
      gamma1 = -log(1-0.1)/24, gamma2 = -log(1-0.1)/24,
      accrualDuration = 25.2, followupTime = 6.5, 
      fixedFollowup = TRUE,  
      plannedEvents = 26, 
      maxNumberOfIterations = 1000, seed = 12345)

## -----------------------------------------------------------------------------
lrsim(kMax = 1, criticalValues = 1.96,  
      allocation1 = 3, allocation2 = 1,
      accrualIntensity = 5, 
      lambda2 = 0.95/12, lambda1 = 0.3*0.95/12, 
      gamma1 = -log(1-0.1)/24, gamma2 = -log(1-0.1)/24,
      accrualDuration = 31.2, followupTime = 6.5, 
      fixedFollowup = TRUE,  
      plannedEvents = 32, 
      maxNumberOfIterations = 1000, seed = 12345)

