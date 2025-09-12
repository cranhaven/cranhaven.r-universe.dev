testthat::test_that("ClusterPredictions: initialize function works", {

  testthat::expect_is(ClusterPredictions$new(class.values = c(0, 1),
                                             positive.class = 1),
                      "ClusterPredictions")
})

testthat::test_that("ClusterPredictions: initialize function checks parameter type", {

  testthat::expect_error(ClusterPredictions$new(class.values = c(0, 1),
                                                positive.class = 2),
                         "[ClusterPredictions][FATAL] Positive class not found. Should be 0 or 1. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusterPredictions: add function works",{
  testthat::skip_if_not_installed("ranger")

  clusterPrediction <- ClusterPredictions$new(class.values = c(1,0,1,1),
                                              positive.class = 1)

  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  testthat::expect_invisible(clusterPrediction$add(prediction = prediction))
})

testthat::test_that("ClusterPredictions: add function checks parameter type", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  prediction <- NULL

  testthat::expect_error(clusterPrediction$add(prediction = prediction),
                         "[ClusterPredictions][FATAL] Prediction parameter must be defined as 'Prediction' object. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusterPredictions: get function works",{
  testthat::skip_if_not_installed("ranger")
  clusterPrediction <- ClusterPredictions$new(class.values = c(1,0,1,1),
                                              positive.class = 1)

  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  testthat::expect_invisible(clusterPrediction$add(prediction = prediction))

  position <- 1

  testthat::expect_is(clusterPrediction$get(1), "Prediction")
})

testthat::test_that("ClusterPredictions: get function checks parameter type", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  position <- NULL

  testthat::expect_error(clusterPrediction$get(position = 1),
                         "[ClusterPredictions][FATAL] Position exceeds list size. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusterPredictions: getAll function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_is(clusterPrediction$getAll(), "list")
})

testthat::test_that("ClusterPredictions: size function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_equal(clusterPrediction$size(), 0)
})

testthat::test_that("ClusterPredictions: getPositiveClass function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_equal(clusterPrediction$getPositiveClass(), 1)
})

testthat::test_that("ClusterPredictions: getClassValues function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_equal(clusterPrediction$getClassValues(), c(1, 0, 1, 1))
})
