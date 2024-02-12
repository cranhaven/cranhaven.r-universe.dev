## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
load("vignette-sims.rda")

## ----setup--------------------------------------------------------------------
library(goldilocks)

## ----example_power, eval=FALSE------------------------------------------------
#  hc <- prop_to_haz(0.7, endtime = 12)
#  ht <- prop_to_haz(0.5, endtime = 12)
#  
#  out_power <- sim_trials(
#    hazard_treatment = ht,
#    hazard_control = hc,
#    cutpoint = 0,
#    N_total = 300,
#    lambda = 5,
#    lambda_time = 0,
#    interim_look = seq(100, 275, 25),
#    end_of_study = 12,
#    prior = c(0.1, 0.1),
#    block = 2,
#    rand_ratio = c(1, 1),
#    prop_loss = 0,
#    alternative = "two.sided",
#    Fn = rep(0.10, 8),
#    Sn = c(1, rep(0.9, 7)),
#    prob_ha = 0.95,
#    N_impute = 100,
#    N_trials = 500,
#    method = "logrank",
#    ncores = 8)

## ----example_type1, eval=FALSE------------------------------------------------
#  out_t1error <- update(out_power, hazard_treatment = hc)

## ----summarise_sims-----------------------------------------------------------
summarise_sims(list(out_power$sims, out_t1error$sims))

## ----example_p0.04, eval=FALSE------------------------------------------------
#  out_power2 <- update(out_power, prob_ha = 0.96)
#  out_t1error2 <- update(out_power2, hazard_treatment = hc)

## ----summarise_sims_p0.04-----------------------------------------------------
summarise_sims(list(out_power2$sims, out_t1error2$sims))

