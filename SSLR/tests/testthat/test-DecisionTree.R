library(tidyverse)
library(tidymodels)
library(SSLR)


context("Testing DecisionTree")

source("wine.R")

m <- SSLRDecisionTree()

test_that(
  desc = "DecisionTree model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "DecisionTree args",
  code = {

    expect_equal(m$args$max_depth,30)
    expect_equal(m$args$w,0.5)
    expect_equal(m$args$min_samples_split,20)
    expect_equal(m$args$min_samples_leaf,7)

  }
)

model <- m %>% fit(Wine ~ ., data = wine$train)

test_that(
  desc = "DecisionTree fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "DecisionTree predictions data frame",
  code = {

    predictions_frame <- predict(model,wine$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "DecisionTree predictions factor",
  code = {

    predictions_factor <- predict(model,wine$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(wine$test))

  }
)


test_that(
  desc = "DecisionTree predictions probabilities",
  code = {

    predictions_frame <- predict(model,wine$test,type = "prob")
    expect_is(predictions_frame,"data.frame")

  }
)



