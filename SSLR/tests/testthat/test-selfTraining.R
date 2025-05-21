library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing SelfTraining")

source("wine.R")

rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest")


m <- selfTraining(learner = rf,max.iter = 50,
                  perc.full = 0.7,
                  thr.conf = 0.5)

test_that(
  desc = "selfTraining model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "selfTraining args",
  code = {

    expect_equal(m$args$max.iter,50)
    expect_equal(m$args$perc.full,0.7)

  }
)

model <- m %>% fit(Wine ~ ., data = wine$train)

test_that(
  desc = "selfTraining fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "selfTraining predictions data frame",
  code = {

    predictions_frame <- predict(model,wine$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "selfTraining predictions factor",
  code = {

    predictions_factor <- predict(model,wine$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(wine$test))

  }
)



