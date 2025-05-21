library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing EntropyRegularizedLogisticRegressionSSLR")

source("breast.R")
source("helpers.R")

m <- EntropyRegularizedLogisticRegressionSSLR()

test_that(
  desc = "EntropyRegularizedLogisticRegressionSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "EntropyRegularizedLogisticRegressionSSLR args",
  code = {

    expect_equal(m$args$lambda,0)
    expect_equal(m$args$lambda_entropy,1)
    expect_equal(m$args$intercept,TRUE)
    expect_equal(m$args$init,NA)
    expect_equal(m$args$scale,FALSE)
    expect_equal(m$args$x_center,FALSE)
  }
)



