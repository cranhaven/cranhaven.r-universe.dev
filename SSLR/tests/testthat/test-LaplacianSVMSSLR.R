library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing LaplacianSVMSSLR")

source("breast.R")
source("helpers.R")

m <- LaplacianSVMSSLR(kernel=kernlab::vanilladot())

test_that(
  desc = "LaplacianSVMSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "LaplacianSVMSSLR args",
  code = {

    expect_equal(m$args$lambda,1)
    expect_equal(m$args$gamma,1)
    expect_equal(m$args$scale,TRUE)
    expect_equal(m$args$adjacency_distance,"euclidean")
    expect_equal(m$args$adjacency_k,6)
    expect_equal(m$args$normalized_laplacian,FALSE)
    expect_equal(m$args$eps,1e-09)
  }
)

model <- m %>% fit(Class ~ ., data = breast$train)

test_that(
  desc = "LaplacianSVMSSLR fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "LaplacianSVMSSLR predictions data frame",
  code = {

    predictions_frame <- predict(model,breast$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "LaplacianSVMSSLR predictions factor",
  code = {

    predictions_factor <- predict(model,breast$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(breast$test))

  }
)



