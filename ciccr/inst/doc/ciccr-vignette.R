## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ciccr)
library(MASS)

## -----------------------------------------------------------------------------
  y = ACS_CC$topincome
  t = ACS_CC$baplus
  x = ACS_CC$age

## -----------------------------------------------------------------------------
  x = splines::bs(x, df = 6)

## -----------------------------------------------------------------------------
  results_case = avg_RR_logit(y, t, x, 'case')
  results_case$est
  results_case$se

## -----------------------------------------------------------------------------
  results_control = avg_RR_logit(y, t, x, 'control')
  results_control$est
  results_control$se

## -----------------------------------------------------------------------------
results = cicc_RR(y, t, x, 'cc', 0.95)

## -----------------------------------------------------------------------------
 # point estimates
 results$est
 # standard errors
 results$se
 # confidence intervals 
 results$ci

## -----------------------------------------------------------------------------
cicc_plot(results)

## -----------------------------------------------------------------------------
logit = stats::glm(y~t+x, family=stats::binomial("logit"))
est_logit = stats::coef(logit)
ci_logit = stats::confint(logit, level = 0.9)
# point estimate
exp(est_logit)
# confidence interval
exp(ci_logit)


## -----------------------------------------------------------------------------
results_AR = cicc_AR(y, t, x, sampling = 'cc', no_boot = 100)

## -----------------------------------------------------------------------------
cicc_plot(results_AR, parameter = 'AR')

## -----------------------------------------------------------------------------
  y = ACS_CP$topincome
  t = ACS_CP$baplus
  x = ACS_CP$age

## -----------------------------------------------------------------------------
 print(head(y))

## -----------------------------------------------------------------------------
  y = as.integer(is.na(y)==FALSE)

## -----------------------------------------------------------------------------
  results_control = avg_RR_logit(y, t, x, 'control')
  results_control$est
  results_control$se

## -----------------------------------------------------------------------------
results = cicc_RR(y, t, x, 'cp', 0.95)
cicc_plot(results)

## -----------------------------------------------------------------------------
results_AR = cicc_AR(y, t, x, sampling = 'cp', no_boot = 100)

## -----------------------------------------------------------------------------
cicc_plot(results_AR, parameter = 'AR')

