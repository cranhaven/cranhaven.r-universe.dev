## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, message = FALSE, warning = FALSE------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(rbmi)
library(beeca)
library(rbmiUtils)

## ----seed---------------------------------------------------------------------
set.seed(1974)

## ----load-data----------------------------------------------------------------
data("ADEFF")

ADEFF <- ADEFF %>%
  mutate(
    TRT = factor(TRT01P, levels = c("Placebo", "Drug A")),
    USUBJID = factor(USUBJID),
    AVISIT = factor(AVISIT)
  )

## ----define-vars--------------------------------------------------------------
vars <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")
)

## ----define-method------------------------------------------------------------
method <- method_bayes(
  n_samples = 100,
  control = control_bayes(warmup = 200, thin = 2)
)

## ----impute-------------------------------------------------------------------
dat <- ADEFF %>%
  select(USUBJID, STRATA, REGION, REGIONC, TRT, BASE, CHG, AVISIT)

draws_obj <- draws(data = dat, vars = vars, method = method)

impute_obj <- impute(draws_obj, references = c("Placebo" = "Placebo", "Drug A" = "Placebo"))

ADMI <- get_imputed_data(impute_obj)

## ----derive-responder---------------------------------------------------------
ADMI <- ADMI %>%
  mutate(
    CRIT1FLN = ifelse(CHG > 3, 1, 0),
    CRIT1FL = ifelse(CRIT1FLN == 1, "Y", "N"),
    CRIT = "CHG > 3"
  )

## ----analyse-chg--------------------------------------------------------------
ana_obj_ancova <- analyse_mi_data(
  data = ADMI,
  vars = vars,
  method = method,
  fun = ancova
)

## ----pool-chg-----------------------------------------------------------------
pool_obj_ancova <- pool(ana_obj_ancova)
print(pool_obj_ancova)

## ----tidy-chg-----------------------------------------------------------------
tidy_pool_obj(pool_obj_ancova)

## ----gcomp-fun----------------------------------------------------------------
gcomp_responder <- function(data, ...) {
  model <- glm(CRIT1FLN ~ TRT + BASE + STRATA + REGION, data = data, family = binomial)

  marginal_fit <- get_marginal_effect(
    model,
    trt = "TRT",
    method = "Ge",
    type = "HC0",
    contrast = "diff",
    reference = "Placebo"
  )

  res <- marginal_fit$marginal_results
  list(
    trt = list(
      est = res[res$STAT == "diff", "STATVAL"][[1]],
      se = res[res$STAT == "diff_se", "STATVAL"][[1]],
      df = NA
    )
  )
}

## ----vars-binary--------------------------------------------------------------
vars_binary <- set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CRIT1FLN",
  covariates = c("BASE", "STRATA", "REGION")
)

## ----analyse-binary-----------------------------------------------------------
ana_obj_prop <- analyse_mi_data(
  data = ADMI,
  vars = vars_binary,
  method = method,
  fun = gcomp_responder
)


## ----pool-binary--------------------------------------------------------------
pool_obj_prop <- pool(ana_obj_prop)
print(pool_obj_prop)

