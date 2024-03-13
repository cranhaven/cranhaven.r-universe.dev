## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lrstat)

## -----------------------------------------------------------------------------
p1 = c(0.28, 0.13, 0.25, 0.34)
p2 = c(0.28, 0.72)
p3 = c(0.43, 0.37, 0.2)
stratumFraction = p1 %x% p2 %x% p3

## -----------------------------------------------------------------------------
theta1 = c(1, 2.127, 0.528, 0.413)
theta2 = c(1, 0.438)
theta3 = c(1, 0.614, 0.159)

## -----------------------------------------------------------------------------
lambda2 = 0.009211*exp(log(theta1) %x% log(theta2) %x% log(theta3))

## -----------------------------------------------------------------------------
caltime(nevents = 66, accrualDuration = 24, accrualIntensity = 12,
        stratumFraction = stratumFraction, 
        lambda1 = 0.4466*lambda2, lambda2 = lambda2, 
        followupTime = 100)

## -----------------------------------------------------------------------------
lrpower(kMax = 3, 
        informationRates = c(0.333, 0.667, 1), 
        alpha = 0.025, typeAlphaSpending = "sfOF", 
        accrualIntensity = 12,
        stratumFraction = stratumFraction,
        lambda1 = 0.4466*lambda2, 
        lambda2 = lambda2, 
        accrualDuration = 24, 
        followupTime = 30.92)

## -----------------------------------------------------------------------------
lrsim(kMax = 3, 
      informationRates = c(0.333, 0.667, 1), 
      criticalValues = c(3.710, 2.511, 1.993), 
      accrualIntensity = 12,
      stratumFraction = stratumFraction,
      lambda1 = 0.4466*lambda2, 
      lambda2 = lambda2, 
      accrualDuration = 24, 
      followupTime = 30.92,
      plannedEvents = c(22, 44, 66),
      maxNumberOfIterations = 1000, 
      seed = 314159)

