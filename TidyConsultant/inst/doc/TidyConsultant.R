## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(Ckmeans.1d.dp)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(TidyConsultant)

## -----------------------------------------------------------------------------
data(insurance)

## -----------------------------------------------------------------------------
insurance %>% 
  diagnose()

## -----------------------------------------------------------------------------
insurance %>% 
  diagnose_numeric()

## -----------------------------------------------------------------------------
insurance %>% 
  diagnose_category(everything(),  max_distinct = 7) %>% 
  print(width = Inf)

## -----------------------------------------------------------------------------
insurance %>% 
  determine_distinct(everything())

## -----------------------------------------------------------------------------
insurance %>% 
  auto_cor(sparse = TRUE) -> cors

cors

## -----------------------------------------------------------------------------
insurance %>% 
  auto_anova(everything(), baseline = "first_level") -> anovas1

anovas1 %>% 
  print(n = 50)

## -----------------------------------------------------------------------------

insurance %>% 
  auto_anova(everything(), baseline = "first_level", sparse = T, pval_thresh = .1) -> anovas2

anovas2 %>% 
  print(n = 50)


## -----------------------------------------------------------------------------
insurance %>% 
  create_dummies(remove_most_frequent_dummy = T) -> insurance1

## -----------------------------------------------------------------------------
insurance1 %>% 
  tidy_formula(target = charges) -> charges_form

charges_form

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  insurance1 %>%
#    auto_variable_contributions(formula = charges_form)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  insurance1 %>%
#    auto_model_accuracy(formula = charges_form, include_linear = T)

## -----------------------------------------------------------------------------
insurance1 %>% 
  bin_cols(charges) -> insurance_bins

insurance_bins

## -----------------------------------------------------------------------------
insurance_bins %>% 
  bin_summary()

## -----------------------------------------------------------------------------
insurance %>% 
  set_fct(smoker, first_level = "yes") -> insurance

insurance %>% 
  create_dummies(where(is.character), remove_first_dummy = T) -> insurance_dummies

insurance_dummies %>% 
  diagnose


## -----------------------------------------------------------------------------
insurance_dummies %>% 
  tidy_formula(target = smoker) -> smoker_form

smoker_form

## -----------------------------------------------------------------------------
insurance_dummies %>% 
  tidy_xgboost(formula = smoker_form, 
              mtry = .5,
              trees = 100L,
              loss_reduction = 1,
              alpha = .1,
              sample_size = .7) -> smoker_xgb_classif



## -----------------------------------------------------------------------------
smoker_xgb_classif %>% 
  tidy_predict(newdata = insurance_dummies, form = smoker_form) -> insurance_fit


## -----------------------------------------------------------------------------
names(insurance_fit)[length(names(insurance_fit)) - 1] -> prob_preds

insurance_fit %>% 
  bin_cols(prob_preds, n_bins = 5) -> insurance_fit1

insurance_fit1 %>% 
  bin_summary()

## -----------------------------------------------------------------------------
insurance_fit1 %>% 
  eval_preds()

## -----------------------------------------------------------------------------
names(insurance_fit)[length(names(insurance_fit))] -> class_preds

insurance_fit1 %>% 
  yardstick::conf_mat(truth = smoker, estimate = class_preds) -> conf_mat_sm

conf_mat_sm

