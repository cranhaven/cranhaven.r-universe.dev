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
#  library(ggplot2)

## -----------------------------------------------------------------------------
#  set.seed(123)
#  data("lending_club", package = "modeldata")
#  split <- initial_split(lending_club, strata = Class, prop = 9/10)
#  unlabeled <- training(split) %>% mutate(Class=factor(NA))
#  labeled  <- testing(split)

## -----------------------------------------------------------------------------
#  set.seed(123)
#  labeled_split <- initial_split(labeled, strata = Class)
#  train <- training(labeled_split)
#  test  <- testing(labeled_split)

## -----------------------------------------------------------------------------
#  rec <- recipe(Class ~ ., lending_club) %>%
#    step_normalize(all_numeric())
#  unlabeled_baked_df <- rec %>% prep %>% bake(new_data=unlabeled)

## -----------------------------------------------------------------------------
#  mod <- tabnet_pretrain(rec, unlabeled, epochs = 50, valid_split = 0.2, batch_size = 5000, verbose = TRUE)

## -----------------------------------------------------------------------------
#  autoplot(mod)

## -----------------------------------------------------------------------------
#  model_fit <- tabnet_fit(rec, train , tabnet_model = mod, from_epoch=40, valid_split = 0.2, epochs = 50, verbose=TRUE)

## -----------------------------------------------------------------------------
#  autoplot(model_fit)

## -----------------------------------------------------------------------------
#  model_fit <- tabnet_fit(rec, train , tabnet_model = model_fit, from_epoch=50, epochs = 4, valid_split = 0.2, verbose=TRUE)

## -----------------------------------------------------------------------------
#  test %>%
#    bind_cols(
#      predict(model_fit, test, type = "prob")
#    ) %>%
#    roc_auc(Class, .pred_bad)

## -----------------------------------------------------------------------------
#  vanilla_model_fit <- tabnet_fit(rec, train , valid_split = 0.2, epochs = 50, verbose=TRUE)
#  

## -----------------------------------------------------------------------------
#  autoplot(vanilla_model_fit)

## -----------------------------------------------------------------------------
#  vanilla_model_fit <- tabnet_fit(rec, train , tabnet_model= vanilla_model_fit, from_epoch=20, valid_split = 0.2, epochs = 1, verbose=TRUE)
#  test %>%
#    bind_cols(
#      predict(vanilla_model_fit, test, type = "prob")
#    ) %>%
#    roc_auc(Class, .pred_good)
#  

