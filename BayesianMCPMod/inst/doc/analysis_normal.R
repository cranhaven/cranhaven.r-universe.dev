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

## ----Historical Data for Control Arm------------------------------------------
data("metaData")
dataset     <- filter(as.data.frame(metaData), bname == "BRINTELLIX")
histcontrol <- filter(
  dataset,
  dose       == 0,
  primtime   == 8,
  indication == "MAJOR DEPRESSIVE DISORDER",
  protid     != 5)

hist_data   <- data.frame(
  trial = histcontrol$nctno,
  est   = histcontrol$rslt,
  se    = histcontrol$se,
  sd    = histcontrol$sd,
  n     = histcontrol$sampsize)

## ----Defining MAP prior function----------------------------------------------
getPriorList <- function (
  
  hist_data,
  dose_levels,
  dose_names    = NULL,
  robust_weight = 0.5
  
) {
  
  sd_tot <- with(hist_data, sum(sd * n) / sum(n))
  
  gmap <- RBesT::gMAP(
    formula    = cbind(est, se) ~ 1 | trial,
    weights    = hist_data$n,
    data       = hist_data,
    family     = gaussian,
    beta.prior = cbind(0, 100 * sd_tot),
    tau.dist   = "HalfNormal",
    tau.prior  = cbind(0, sd_tot / 4))
  
  prior_ctr <- RBesT::automixfit(gmap)
  
  if (!is.null(robust_weight)) {
    
    prior_ctr <- suppressMessages(RBesT::robustify(
      priormix = prior_ctr,
      weight   = robust_weight,
      sigma    = sd_tot))
    
  }
  
  prior_trt <- RBesT::mixnorm(
    comp1 = c(w = 1, m = summary(prior_ctr)[1], n = 1),
    sigma = sd_tot,
    param = "mn")
  
  prior_list <- c(list(prior_ctr),
                  rep(x     = list(prior_trt),
                      times = length(dose_levels[-1])))
  
  if (is.null(dose_names)) {
    
    dose_names <- c("Ctr", paste0("DG_", seq_along(dose_levels[-1])))
    
  }
  
  names(prior_list) <- dose_names
  
  return (prior_list)
  
}

## ----Getting the MAP prior----------------------------------------------------
dose_levels <- c(0, 2.5, 5, 10)

prior_list  <- getPriorList(
  hist_data     = hist_data,
  dose_levels   = dose_levels,
  robust_weight = 0.3)

getESS(prior_list)

## ----Pre-Specification of candidate models------------------------------------
exp_guesst  <- DoseFinding::guesst(
  d     = 5,
  p     = c(0.2),
  model = "exponential",
  Maxd  = max(dose_levels))

emax_guesst <- DoseFinding::guesst(
  d     = 2.5,
  p     = c(0.9),
  model = "emax")

mods <- DoseFinding::Mods(
  linear      = NULL,
  emax        = emax_guesst,
  exponential = exp_guesst,
  doses       = dose_levels,
  maxEff      = -1,
  placEff     = -12.8)

## ----new trial----------------------------------------------------------------
new_trial  <- filter(
  dataset,
  primtime   == 8,
  indication == "MAJOR DEPRESSIVE DISORDER",
  protid     == 5)

n_patients <- c(128, 124, 129, 122)

## ----Trial results------------------------------------------------------------
posterior <- getPosterior(
  prior    = prior_list,
  mu_hat   = new_trial$rslt,
  se_hat   = new_trial$se,
  calc_ess = TRUE)

summary(posterior)

## ----Preparation of input for Bayesian MCPMod Test step-----------------------
crit_pval <- getCritProb(
  mods           = mods,
  dose_levels    = dose_levels,
  se_new_trial   = new_trial$se,
  alpha_crit_val = 0.05)

contr_mat <- getContr(
  mods         = mods,
  dose_levels  = dose_levels,
  sd_posterior = summary(posterior)[, 2])

## ----eval = FALSE-------------------------------------------------------------
#  # i) the frequentist contrast
#  contr_mat_prior <- getContr(
#    mods           = mods,
#    dose_levels    = dose_levels,
#    dose_weights   = n_patients,
#    prior_list     = prior_list)
#  # ii) re-estimated frequentist contrasts
#  contr_mat_prior <- getContr(
#    mods           = mods,
#    dose_levels    = dose_levels,
#    se_new_trial   = new_trial$se)
#  # iii)  Bayesian approach using number of patients for new trial and prior distribution
#  contr_mat_prior <- getContr(
#    mods           = mods,
#    dose_levels    = dose_levels,
#    dose_weights   = n_patients,
#    prior_list     = prior_list)

## ----Execution of Bayesian MCPMod Test step-----------------------------------
BMCP_result <- performBayesianMCP(
  posterior_list = posterior,
  contr          = contr_mat, 
  crit_prob_adj  = crit_pval)

BMCP_result

## ----Model fitting------------------------------------------------------------
# Option a) Simplified approach by using approximated posterior distribution
fit_simple <- getModelFits(
  models      = names(mods),
  dose_levels = dose_levels,
  posterior   = posterior,
  simple      = TRUE)

# Option b) Making use of the complete posterior distribution
fit <- getModelFits(
  models      = names(mods),
  dose_levels = dose_levels,
  posterior   = posterior,
  simple      = FALSE)

## ----Predict------------------------------------------------------------------
predict(fit, doses = c(0, 2.5, 4, 5, 7, 10))

## ----Plot simple vs fit-------------------------------------------------------
plot(fit_simple)
plot(fit)

## ----Plot with bootstrap------------------------------------------------------
plot(fit, cr_bands = TRUE)

## ----Bootstrap----------------------------------------------------------------
getBootstrapQuantiles(
  model_fits = fit,
  quantiles  = c(0.025, 0.5, 0.975),
  doses      = c(0, 2.5, 4, 5, 7, 10),
  n_samples  = 6)

## ----eval = FALSE-------------------------------------------------------------
#  performBayesianMCPMod(
#        posterior_list   = posterior,
#        contr            = contr_mat,
#        crit_prob_adj    = crit_pval,
#        simple           = FALSE)

