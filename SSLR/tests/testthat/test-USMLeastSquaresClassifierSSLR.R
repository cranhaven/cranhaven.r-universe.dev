library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing USMLeastSquaresClassifierSSLR")

source("breast.R")
source("helpers.R")


m <- USMLeastSquaresClassifierSSLR()

test_that(
  desc = "USMLeastSquaresClassifierSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "USMLeastSquaresClassifierSSLR args",
  code = {

    expect_equal(m$args$lambda,0)
    expect_equal(m$args$intercept,TRUE)
    expect_equal(m$args$x_center,FALSE)
    expect_equal(m$args$scale,FALSE)
    expect_equal(m$args$y_scale,FALSE)
    expect_equal(m$args$use_Xu_for_scaling,TRUE)
  }
)



