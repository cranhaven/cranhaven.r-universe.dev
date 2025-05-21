library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing coBC")

source("wine.R")

rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest")


m <- coBC(learner = rf,N = 3,
          perc.full = 0.7,
          u = 100,
          max.iter = 50)

test_that(
  desc = "coBC model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "coBC args",
  code = {

    expect_equal(m$args$N,3)
    expect_equal(m$args$max.iter,50)
    expect_equal(m$args$perc.full,0.7)
    expect_equal(m$args$u,100)

  }
)

model <- m %>% fit(Wine ~ ., data = wine$train)

test_that(
  desc = "coBC fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "coBC predictions data frame",
  code = {

    predictions_frame <- predict(model,wine$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "coBC predictions factor",
  code = {

    predictions_factor <- predict(model,wine$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(wine$test))

  }
)



