## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lrstat)

## ----updategraph--------------------------------------------------------------

alpha = 0.025
x = list(
  w = c(0.2, 0.8, 0, 0, 0, 0),
  G = matrix(c(0, 0.8, 0.2, 0, 0, 0, 
               0.5, 0, 0, 0.5, 0, 0,
               0, 0.8, 0, 0, 0.2, 0,
               0.72, 0, 0, 0, 0, 0.28,
               0, 1, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0),
             nrow=6, ncol=6, byrow = TRUE),
  I = seq(1, 6))

print(alpha*x$w, digits=4)
for (j in 1:6) {
  x = updateGraph(x$w, x$G, x$I, j)
  print(alpha*x$w, digits=4)
}


## ----sample size calculation for PFS A/C--------------------------------------
(lr1 <- lrsamplesize(
  beta = 0.03, kMax = 2, informationRates = c(0.75,1), 
  alpha = 0.005, typeAlphaSpending = "sfHSD", 
  parameterAlphaSpending = -4, 
  allocationRatioPlanned = 2, 
  accrualTime = c(0,8), accrualIntensity = c(10,28)*3/5, 
  lambda1 = log(2)/22, lambda2 = log(2)/12, 
  accrualDuration = 30.143, followupTime = NA, 
  typeOfComputation = "Schoenfeld")$resultsUnderH1)


## ----expected number of OS events at look 1 and look 2------------------------

(lr2a <- lrstat(
  time = lr1$byStageResults$analysisTime, 
  allocationRatioPlanned = 2,
  accrualTime = c(0,8), accrualIntensity = c(10,28)*3/5, 
  lambda1 = log(2)/50, lambda2 = log(2)/30,
  accrualDuration = 30.143, followupTime = 100,
  predictTarget = 1))


## ----required number of OS events at look 3 for 90% power---------------------

(d_os <- ceiling(uniroot(function(d) {
  lr <- lrsamplesize(
    beta = 0.1, kMax = 3, 
    informationRates = c(lr2a$nevents, d)/d,
    alpha = 0.020, typeAlphaSpending = "sfHSD", 
    parameterAlphaSpending = -4, 
    allocationRatioPlanned = 2, 
    accrualTime = c(0,8), 
    accrualIntensity = c(10,28)*3/5, 
    lambda1 = log(2)/50, lambda2 = log(2)/30, 
    accrualDuration = 30.143, followupTime = NA, 
    typeOfComputation = "Schoenfeld",
    rounding = 0)$resultsUnderH1
  lr$overallResults$numberOfEvents - d}, 
  c(138,300))$root))

(studyDuration = caltime(
  nevents = d_os, 
  allocationRatioPlanned = 2, 
  accrualTime = c(0,8), 
  accrualIntensity = c(10,28)*3/5, 
  lambda1 = log(2)/50, lambda2 = log(2)/30,
  accrualDuration = 30.143, followupTime = 1000))


## ----OS sample size-----------------------------------------------------------
(lr2 <- lrpower(
  kMax = 3, 
  informationRates = c(lr2a$nevents, d_os)/d_os, 
  alpha = 0.020, typeAlphaSpending = "sfHSD", 
  parameterAlphaSpending = -4, 
  allocationRatioPlanned = 2, 
  accrualTime = c(0,8), 
  accrualIntensity = c(10,28)*3/5, 
  lambda1 = log(2)/50, lambda2 = log(2)/30,
  accrualDuration = 30.143, followupTime = 25.586, 
  typeOfComputation = "Schoenfeld"))

## ----expected number of events for other comparisons--------------------------

(lr3 <- lrstat(
  time = lr2$byStageResults$analysisTime, 
  allocationRatioPlanned = 2,
  accrualTime = c(0,8), accrualIntensity = c(10,28)*3/5, 
  lambda1 = log(2)/17, lambda2 = log(2)/12,
  accrualDuration = 30.143, followupTime = 1000,
  predictTarget = 1))


(lr4 <- lrstat(
  time = lr2$byStageResults$analysisTime, 
  allocationRatioPlanned = 2,
  accrualTime = c(0,8), accrualIntensity = c(10,28)*3/5, 
  lambda1 = log(2)/40, lambda2 = log(2)/30,
  accrualDuration = 30.143, followupTime = 1000,
  predictTarget = 1))


(lr5 <- lrstat(
  time = lr2$byStageResults$analysisTime, 
  allocationRatioPlanned = 1,
  accrualTime = c(0,8), accrualIntensity = c(10,28)*4/5, 
  lambda1 = log(2)/22, lambda2 = log(2)/17,
  accrualDuration = 30.143, followupTime = 1000,
  predictTarget = 1))


(lr6 <- lrstat(
  time = lr2$byStageResults$analysisTime, 
  allocationRatioPlanned = 1,
  accrualTime = c(0,8), accrualIntensity = c(10,28)*4/5, 
  lambda1 = log(2)/50, lambda2 = log(2)/40,
  accrualDuration = 30.143, followupTime = 1000,
  predictTarget = 1))



## ----simulation---------------------------------------------------------------

sim1 = lrsim2e3a(
  kMax = 3, 
  kMaxe1 = 2,
  allocation1 = 2, 
  allocation2 = 2, 
  allocation3 = 1,
  accrualTime = c(0, 8), 
  accrualIntensity = c(10, 28),
  lambda1e1 = log(2)/22,
  lambda2e1 = log(2)/17,
  lambda3e1 = log(2)/12,
  lambda1e2 = log(2)/50, 
  lambda2e2 = log(2)/40,
  lambda3e2 = log(2)/30,
  accrualDuration = 30.143,
  plannedEvents = c(186, 248, 196),
  maxNumberOfIterations = 500,
  maxNumberOfRawDatasetsPerStage = 1,
  seed = 314159)

## ----simulated number of events and p-values----------------------------------
library(dplyr)
library(tidyr)

df <- sim1$sumdata %>% 
  mutate(events13 = events1 + events3,
         events23 = events2 + events3,
         events12 = events1 + events2,
         p13 = pnorm(logRankStatistic13), 
         p23 = pnorm(logRankStatistic23), 
         p12 = pnorm(logRankStatistic12)) %>% 
  select(iterationNumber, stageNumber, endpoint, 
         events13, events23, events12, 
         p13, p23, p12)

dfcomp <- tibble(comparison = c("13", "23", "12"),
                 order = c(1, 2, 3))



dfinfo <- df %>% 
  arrange(iterationNumber, endpoint) %>% 
  pivot_longer(
    c("events13", "events23", "events12"), 
    names_to = "c_events", 
    values_to = "events") %>% 
  mutate(comparison = substring(c_events, 7)) %>%
  left_join(dfcomp, by = c("comparison")) %>% 
  arrange(iterationNumber, order, endpoint, 
          stageNumber)


dfp <- df %>% 
  arrange(iterationNumber, endpoint) %>% 
  pivot_longer(
    c("p13", "p23", "p12"), 
    names_to = "c_p", 
    values_to = "p") %>% 
  mutate(comparison = substring(c_p, 2)) %>%
  left_join(dfcomp, by = c("comparison")) %>% 
  arrange(iterationNumber, order, endpoint, 
          stageNumber)


## ----adjusted power-----------------------------------------------------------
w = c(0.2, 0.8, 0, 0, 0, 0) 
G = matrix(c(0, 0.8, 0.2, 0, 0, 0, 
             0.5, 0, 0, 0.5, 0, 0,
             0, 0.8, 0, 0, 0.2, 0, 
             0.72, 0, 0, 0, 0, 0.28,
             0, 1, 0, 0, 0, 0, 
             1, 0, 0, 0, 0, 0), 
           nrow=6, ncol=6, byrow=TRUE)


rejStage = fseqbon(
  w = w, G = G, alpha = 0.025, kMax = 3,
  typeAlphaSpending = rep("sfHSD", 6),
  parameterAlphaSpending = rep(-4, 6),
  incidenceMatrix = matrix(
    c(1,1,0, 1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1), 
    nrow=6, ncol=3, byrow=TRUE),
  maxInformation = c(248, 196, 342, 216, 413, 249),
  p = matrix(dfp$p, 3000, 3, byrow=TRUE),
  information = matrix(dfinfo$events, 3000, 3, byrow=TRUE))

reject2a = matrix(rejStage>0, nrow=500, ncol=6, byrow=TRUE)
(power2 = apply(reject2a, 2, mean))



