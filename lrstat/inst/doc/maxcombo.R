## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lrstat)

## -----------------------------------------------------------------------------
(time = caltime(nevents=c(50, 99.9), accrualIntensity=25,
                piecewiseSurvivalTime=c(0, 1.5), 
                lambda1=c(0.25, 0.125), lambda2=c(0.25, 0.25), 
                accrualDuration=4, followupTime=60))

## -----------------------------------------------------------------------------
(lr00 = lrstat(time=c(5.363, 50.324), accrualIntensity=25,
               piecewiseSurvivalTime=c(0, 1.5), 
               lambda1=c(0.25, 0.125), lambda2=c(0.25, 0.25), 
               accrualDuration=4, followupTime=60, 
               rho1=0, rho2=0, numSubintervals=10000))

(lr01 = lrstat(time=c(5.363, 50.324), accrualIntensity=25,
               piecewiseSurvivalTime=c(0, 1.5), 
               lambda1=c(0.25, 0.125), lambda2=c(0.25, 0.25), 
               accrualDuration=4, followupTime=60, 
               rho1=0, rho2=1, numSubintervals=10000))

(lr0h = lrstat(time=c(5.363, 50.324), accrualIntensity=25,
               piecewiseSurvivalTime=c(0, 1.5), 
               lambda1=c(0.25, 0.125), lambda2=c(0.25, 0.25), 
               accrualDuration=4, followupTime=60, 
               rho1=0, rho2=0.5, numSubintervals=10000))

## -----------------------------------------------------------------------------
library(mvtnorm)
mu = c(0.900, 2.233, 2.661)
sigma = matrix(c(1, 0.748, 0.370, 0.748, 1, 0.860, 0.370, 0.860, 1), 3, 3)
u1 = 2.968
alpha = 0.025
f <- function(u2, u1, sigma, alpha) {
  1 - pmvnorm(upper=c(u1, u2, u2), corr=sigma, algorithm="Miwa") - alpha
}
(u2 = uniroot(f, c(1,3), u1, sigma, alpha)$root)

## -----------------------------------------------------------------------------
1 - pmvnorm(upper=c(u1, u2, u2), corr=sigma, mean=mu, algorithm="Miwa")

## -----------------------------------------------------------------------------
sim1 = lrsim(kMax = 2, informationRates = c(0.5, 1),
             criticalValues = c(6, 6),
             accrualIntensity = 25, 
             piecewiseSurvivalTime = c(0, 1.5),
             lambda1 = c(0.25, 0.125), lambda2 = c(0.25, 0.25),
             accrualDuration = 4,
             rho1 = 0, rho2 = 0,
             plannedEvents = c(50, 100), 
             maxNumberOfIterations = 1000,
             seed = 314159)

sim2 = lrsim(kMax = 2, informationRates = c(0.5, 1),
             criticalValues = c(6, 6),
             accrualIntensity = 25,
             piecewiseSurvivalTime = c(0, 1.5),
             lambda1 = c(0.25, 0.125), lambda2 = c(0.25, 0.25),
             accrualDuration = 4,
             rho1 = 0, rho2 = 1, 
             plannedEvents = c(50, 100),
             maxNumberOfIterations = 1000,
             seed = 314159)

w1max = subset(-sim1$sumdata$logRankStatistic, sim1$sumdata$stageNumber==1)

w2max = pmax(-sim1$sumdata$logRankStatistic, -sim2$sumdata$logRankStatistic) 
w2max = subset(w2max, sim1$sumdata$stageNumber==2)

mean((w1max > u1) | (w2max > u2))

