## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)
library(SimtablR)
data(epitabl)
has_survival <- requireNamespace("survival", quietly = TRUE)
has_logistf <- requireNamespace("logistf", quietly = TRUE)
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

## ----design-resolution--------------------------------------------------------
# Declaring cohort design in bivariate analysis resolves to Risk Ratio (RR)
tb_cohort <- tb(
  epitabl,
  renal_impairment,
  adjudicated_acs,
  flags = "row",
  design = "cohort",
  ref = "No"
)
tb_cohort

## ----logistic-regtab----------------------------------------------------------
fit_logistic <- regtab(
  epitabl,
  outcomes = "adjudicated_acs",
  predictors = ~ age + sex + smoking + hypertension + diabetes + renal_impairment,
  family = stats::binomial("logit"),
  robust = TRUE,
  predictor_labels = c(
    smokingCurrent = "Current Smoker",
    hypertensionYes = "Hypertension",
    diabetesYes = "Diabetes Mellitus",
    renal_impairmentYes = "Renal Impairment"
  )
)
fit_logistic

## ----multi-outcome-regtab-----------------------------------------------------
fit_multi <- regtab(
  epitabl,
  outcomes = c("adjudicated_acs", "rehospitalized"),
  predictors = ~ age + sex + renal_impairment,
  family = stats::binomial("logit"),
  robust = TRUE,
  labels = c(
    adjudicated_acs = "Acute Coronary Syndrome",
    rehospitalized = "1-Year Readmission"
  )
)
fit_multi

## ----model-extract------------------------------------------------------------
# Broom-style tidy coefficients
head(generics::tidy(fit_logistic), 5)

# Model-level statistics including Generalized Variance Inflation Factors (GVIF)
generics::glance(fit_logistic, vif = TRUE)

## ----firth-regtab, eval=has_logistf-------------------------------------------
fit_firth <- regtab(
  epitabl,
  outcomes = "adjudicated_acs",
  predictors = ~ age + sex + dialysis,
  family = stats::binomial("logit"),
  method = "firth"
)
fit_firth

## ----survtab-cox, eval=has_survival-------------------------------------------
fit_cox <- survtab(
  epitabl,
  time = mace_time_days,
  event = mace_event,
  predictors = ~ age + sex + renal_impairment + hypertension + smoking,
  design = "cohort"
)
fit_cox

## ----cox-advise, eval=has_survival--------------------------------------------
# Inspect model fit and Schoenfeld residual diagnostics
generics::glance(fit_cox)

# Review methodological audit rules
advise(fit_cox, audit = TRUE)

## ----cox-plot, eval=has_survival && has_ggplot2, fig.alt="Forest plot displaying adjusted hazard ratios and 95% confidence intervals"----
ggplot2::autoplot(fit_cox) +
  ggplot2::labs(
    title = "Adjusted Hazard Ratios for 365-Day MACE",
    subtitle = "ESTROBE-ACS Prospective Cohort"
  )

## ----cox-methods, eval=has_survival-------------------------------------------
cat(as_methods(fit_cox))

