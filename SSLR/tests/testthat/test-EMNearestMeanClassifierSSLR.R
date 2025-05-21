library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing EMNearestMeanClassifierSSLR")

source("breast.R")
source("helpers.R")


m <- EMNearestMeanClassifierSSLR()

test_that(
  desc = "EMNearestMeanClassifierSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "EMNearestMeanClassifierSSLR args",
  code = {

    expect_equal(m$args$method,"EM")
    expect_equal(m$args$scale,FALSE)
    expect_equal(m$args$eps,1e-04)
  }
)

model <- m %>% fit(Class ~ ., data = breast$train)

test_that(
  desc = "EMNearestMeanClassifierSSLR fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "EMNearestMeanClassifierSSLR predictions data frame",
  code = {

    predictions_frame <- predict(model,breast$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "EMNearestMeanClassifierSSLR predictions factor",
  code = {

    predictions_factor <- predict(model,breast$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(breast$test))

  }
)



