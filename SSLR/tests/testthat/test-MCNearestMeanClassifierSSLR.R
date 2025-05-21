library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing MCNearestMeanClassifierSSLR")

source("breast.R")
source("helpers.R")


m <- MCNearestMeanClassifierSSLR()

test_that(
  desc = "MCNearestMeanClassifierSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "MCNearestMeanClassifierSSLR args",
  code = {

    expect_equal(m$args$prior,NULL)
    expect_equal(m$args$x_center,FALSE)
    expect_equal(m$args$update_sigma,FALSE)
    expect_equal(m$args$scale,FALSE)


  }
)

model <- m %>% fit(Class ~ ., data = breast$train)

test_that(
  desc = "MCNearestMeanClassifierSSLR fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "MCNearestMeanClassifierSSLR predictions data frame",
  code = {

    predictions_frame <- predict(model,breast$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "MCNearestMeanClassifierSSLR predictions factor",
  code = {

    predictions_factor <- predict(model,breast$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(breast$test))

  }
)



