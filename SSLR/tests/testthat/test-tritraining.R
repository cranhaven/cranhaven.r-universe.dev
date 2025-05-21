library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing triTraining")

source("wine.R")

rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest")


m <- triTraining(learner = rf)

test_that(
  desc = "triTraining model",
  code = {

    expect_is(m,"model_sslr")

  }
)


model <- m %>% fit(Wine ~ ., data = wine$train)

test_that(
  desc = "triTraining fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "triTraining predictions data frame",
  code = {

    predictions_frame <- predict(model,wine$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "triTraining predictions factor",
  code = {

    predictions_factor <- predict(model,wine$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(wine$test))

  }
)



