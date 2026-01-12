## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rlang_trace_top_env = rlang::current_env())

## ----setup, message=FALSE, warning=FALSE, results='hide'----------------------
library(autostats)
library(workflows)
library(dplyr)
library(tune)
library(rsample)
library(hardhat)
library(broom.mixed)
library(Ckmeans.1d.dp)
library(igraph)

## -----------------------------------------------------------------------------
set.seed(34)

 iris %>%
  dplyr::as_tibble() %>% 
  framecleaner::create_dummies(remove_first_dummy  = TRUE) -> iris1

 iris1 %>%
 tidy_formula(target = Petal.Length) -> petal_form
 
 petal_form


## -----------------------------------------------------------------------------
iris1 %>%
  rsample::initial_split() -> iris_split

iris_split %>%
  rsample::analysis() -> iris_train

iris_split %>%
  rsample::assessment() -> iris_val

iris_split

## ----eval=T-------------------------------------------------------------------
iris_train %>%
  auto_tune_xgboost(formula = petal_form, n_iter = 5L,trees = 20L, loss_reduction = 2, mtry = 3, tune_method = "grid", parallel = FALSE, counts = TRUE) -> xgb_tuned_grid

xgb_tuned_grid %>%
  parsnip::fit(iris_train) %>% 
  parsnip::extract_fit_engine() -> xgb_tuned_fit_grid


xgb_tuned_fit_grid %>% 
  visualize_model()

## -----------------------------------------------------------------------------
iris_train %>%
  tidy_xgboost(formula = petal_form) -> xgb_base


## -----------------------------------------------------------------------------
iris_train %>% 
  tidy_xgboost(petal_form, 
               trees = 250L, 
               tree_depth = 3L, 
               sample_size = .5,
               mtry = .5,
               min_n = 2) -> xgb_opt



## -----------------------------------------------------------------------------

xgb_base %>%
  tidy_predict(newdata = iris_val, form = petal_form) -> iris_val2

xgb_opt %>% 
  tidy_predict(newdata = iris_val2, petal_form) -> iris_val3


iris_val3 %>% 
  names()

## -----------------------------------------------------------------------------
iris_val3 %>% 
  eval_preds() 

## -----------------------------------------------------------------------------
xgb_base %>% 
  tidy_shap(newdata = iris_val, form = petal_form) -> shap_list


## -----------------------------------------------------------------------------
shap_list$shap_tbl

## -----------------------------------------------------------------------------
shap_list$shap_summary

## -----------------------------------------------------------------------------
shap_list$swarmplot

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  shap_list$scatterplots

## -----------------------------------------------------------------------------
 xgb_base %>% 
  xgboost::xgb.plot.deepness()

## -----------------------------------------------------------------------------
 xgb_base %>% 
  xgboost::xgb.plot.deepness()

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  xgb_base %>%
#    xgboost::xgb.plot.tree(model = ., trees = 1)

