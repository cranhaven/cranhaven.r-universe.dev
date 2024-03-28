## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BayesianMCPMod)
library(clinDR)
library(dplyr)

set.seed(7015)

## ----Historical Data----------------------------------------------------------
data("metaData")
testdata    <- as.data.frame(metaData)
dataset     <- filter(testdata, bname == "BRINTELLIX")
histcontrol <- filter(dataset, dose == 0, primtime == 8, indication == "MAJOR DEPRESSIVE DISORDER")

hist_data <- data.frame(
  trial = histcontrol$nctno,
  est   = histcontrol$rslt,
  se    = histcontrol$se,
  sd    = histcontrol$sd,
  n     = histcontrol$sampsize)

sd_tot <- with(hist_data, sum(sd * n) / sum(n))

## ----Setting Prior without execution, eval = FALSE----------------------------
#  dose_levels <- c(0, 2.5, 5, 10, 20)
#  
#  prior_list  <- getPriorList(
#    hist_data     = hist_data,
#    dose_levels   = dose_levels,
#    robust_weight = 0.3)

## ----Setting Prior, echo = FALSE----------------------------------------------
dose_levels <- c(0, 2.5, 5, 10, 20)

prior_list  <- list(
  Ctr  = RBesT::mixnorm(
    comp1  = c(w = 0.446213, m = -12.774661, s = 1.393130),
    comp1  = c(w = 0.253787, m = 3.148116,   s = 3.148116),
    robust = c(w = 0.3,      m = 9.425139,   s = 9.425139),
    sigma = sd_tot),
  DG_1 = RBesT::mixnorm(
    comp1 = c(w = 1, m = -12.816875, n = 1),
    sigma = sd_tot,
    param = "mn"),
  DG_2 = RBesT::mixnorm(
    comp1 = c(w = 1, m = -12.816875, n = 1),
    sigma = sd_tot,
    param = "mn"),
  DG_3 = RBesT::mixnorm(
    comp1 = c(w = 1, m = -12.816875, n = 1),
    sigma = sd_tot,
    param = "mn"),
  DG_4 = RBesT::mixnorm(
    comp1 = c(w = 1, m = -12.816875, n = 1),
    sigma = sd_tot,
    param = "mn")
)

## -----------------------------------------------------------------------------
exp     <- DoseFinding::guesst(
  d     = 5,
  p     = c(0.2),
  model = "exponential",
  Maxd  = max(dose_levels))

emax    <- DoseFinding::guesst(
  d     = 2.5,
  p     = c(0.9),
  model = "emax")

sigemax <- DoseFinding::guesst(
  d     = c(2.5, 5),
  p     = c(0.1, 0.6),
  model = "sigEmax")

sigemax2 <- DoseFinding::guesst(
  d     = c(2, 4),
  p     = c(0.3, 0.8),
  model = "sigEmax")

mods <- DoseFinding::Mods(
  linear      = NULL,
  emax        = emax,
  exponential = exp,
  sigEmax     = rbind(sigemax, sigemax2),
  doses       = dose_levels,
  maxEff      = -3,
  placEff     = -12.8)

n_patients <- c(60, 80, 80, 80, 80)

## -----------------------------------------------------------------------------
success_probabilities <- assessDesign(
  n_patients  = n_patients,
  mods        = mods,
  prior_list  = prior_list,
  sd          = sd_tot,
  n_sim       = 100) # speed up example run-time

success_probabilities

## -----------------------------------------------------------------------------
success_probabilities_uneq <- assessDesign(
  n_patients  = c(80, 60, 60, 60, 120),
  mods        = mods,
  prior_list  = prior_list,
  sd          = sd_tot,
  n_sim       = 100) # speed up example run-time
success_probabilities_uneq

## -----------------------------------------------------------------------------
success_probabilities <- assessDesign(
  n_patients  = c(60, 80, 80, 80, 80),
  mods        = mods,
  prior_list  = prior_list,
  sd          = sd_tot,
  dr_means    = c(-12, -14, -15, -16, -17),
  n_sim       = 100) # speed up example run-time
success_probabilities

