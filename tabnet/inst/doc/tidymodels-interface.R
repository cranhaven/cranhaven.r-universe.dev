## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(tabnet)
#  library(tidymodels)
#  library(modeldata)

## -----------------------------------------------------------------------------
#  set.seed(123)
#  data("lending_club", package = "modeldata")
#  split <- initial_split(lending_club, strata = Class)
#  train <- training(split)
#  test  <- testing(split)

## -----------------------------------------------------------------------------
#  rec <- recipe(Class ~ ., train) %>%
#    step_normalize(all_numeric())

## -----------------------------------------------------------------------------
#  mod <- tabnet(epochs = 50, batch_size = 128) %>%
#    set_engine("torch", verbose = TRUE) %>%
#    set_mode("classification")

## -----------------------------------------------------------------------------
#  wf <- workflow() %>%
#    add_model(mod) %>%
#    add_recipe(rec)

## -----------------------------------------------------------------------------
#  folds <- vfold_cv(train, v = 5)

## -----------------------------------------------------------------------------
#  fit_rs <- wf %>%
#    fit_resamples(folds)

## -----------------------------------------------------------------------------
#  collect_metrics(fit_rs)

## -----------------------------------------------------------------------------
#  model <- wf %>% fit(train)
#  test %>%
#    bind_cols(
#      predict(model, test, type = "prob")
#    ) %>%
#    roc_auc(Class, .pred_bad)

