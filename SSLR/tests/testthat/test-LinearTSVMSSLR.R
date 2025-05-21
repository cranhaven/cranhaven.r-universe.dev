library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing LinearTSVMSSLR")

source("breast.R")
source("helpers.R")

m <- LinearTSVMSSLR()

test_that(
  desc = "LinearTSVMSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "LinearTSVMSSLR args",
  code = {

    expect_equal(m$args$C,1)
    expect_equal(m$args$Cstar,0.1)
    expect_equal(m$args$s,0)
    expect_equal(m$args$x_center,FALSE)
    expect_equal(m$args$scale,FALSE)
    expect_equal(m$args$eps,1e-06)
    expect_equal(m$args$verbose,FALSE)
    expect_equal(m$args$init,NULL)

  }
)

model <- m %>% fit(Class ~ ., data = breast$train)

test_that(
  desc = "LinearTSVMSSLR fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "LinearTSVMSSLR predictions data frame",
  code = {

    predictions_frame <- predict(model,breast$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "LinearTSVMSSLR predictions factor",
  code = {

    predictions_factor <- predict(model,breast$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(breast$test))

  }
)



