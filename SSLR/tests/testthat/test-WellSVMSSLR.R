library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing WellSVMSSLR")

source("breast.R")
source("helpers.R")


m <- WellSVMSSLR()

test_that(
  desc = "WellSVMSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "WellSVMSSLR args",
  code = {

    expect_equal(m$args$C1,1)
    expect_equal(m$args$gamma,1)
    expect_equal(m$args$C1,1)
    expect_equal(m$args$x_center,TRUE)
    expect_equal(m$args$scale,FALSE)
    expect_equal(m$args$use_Xu_for_scaling,FALSE)
    expect_equal(m$args$max_iter,20)


  }
)

model <- m %>% fit(Class ~ ., data = breast$train)

test_that(
  desc = "WellSVMSSLR fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "WellSVMSSLR predictions data frame",
  code = {

    predictions_frame <- predict(model,breast$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "WellSVMSSLR predictions factor",
  code = {

    predictions_factor <- predict(model,breast$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(breast$test))

  }
)



