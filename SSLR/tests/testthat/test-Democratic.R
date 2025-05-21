library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing Democratic")

source("wine.R")

rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest")


bt <-  boost_tree(trees = 100, mode = "classification") %>%
  set_engine("C5.0")


m <- democratic(learners = list(rf,bt))

test_that(
  desc = "Democratic model",
  code = {

    expect_is(m,"model_sslr")

  }
)


model <- m %>% fit(Wine ~ ., data = wine$train)

test_that(
  desc = "Democratic fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "Democratic predictions data frame",
  code = {

    predictions_frame <- predict(model,wine$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "Democratic predictions factor",
  code = {

    predictions_factor <- predict(model,wine$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(wine$test))

  }
)



