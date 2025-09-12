testthat::test_that("ClassWeightedVoting: initialize function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_is(ClassWeightedVoting$new(cutoff = cutoff,
                                              weights = weights),
                      "ClassWeightedVoting")
})

testthat::test_that("ClassWeightedVoting: getWeights function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_equal(ClassWeightedVoting$new(cutoff = cutoff,
                                                 weights = weights)$getWeights(),
                         weights)
})

testthat::test_that("ClassWeightedVoting: setWeights function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_silent(ClassWeightedVoting$new(cutoff = cutoff,
                                                  weights = weights)$setWeights(weights = weights))
})

testthat::test_that("ClassWeightedVoting: setWeights function checks parameter type", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_message(ClassWeightedVoting$new(cutoff = cutoff,
                                                   weights = weights)$setWeights(weights = NULL),
                           "[ClassWeightedVoting][WARNING] Weights values not changed due to inconsistency error",
                           fixed = TRUE)
})

testthat::test_that("ClassWeightedVoting: execute function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  voting <- ClassWeightedVoting$new(cutoff = cutoff,
                                    weights = weights)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  predictions$add(prediction = predictions$get(1))

  verbose <- TRUE

  testthat::expect_message(voting$execute(predictions = predictions,
                                          verbose = verbose),
                           "[ClassWeightedVoting][WARNING] Weight values are missing or incorrect. Assuming default model performance values",
                           fixed = TRUE)

  testthat::expect_message(voting$execute(predictions = predictions,
                                          verbose = verbose),
                           "[ClassWeightedVoting][INFO] Performing voting with '~0.5486, ~0.3824, ~0.3854, ~0.5486' weights and cutoff of 0.5",
                           fixed = TRUE)
})

testthat::test_that("ClassWeightedVoting: execute function checks parameter type", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  voting <- ClassWeightedVoting$new(cutoff = cutoff,
                                    weights = weights)

  testthat::expect_error(voting$execute(predictions = NULL,
                                        verbose = FALSE),
                         "[ClassWeightedVoting][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[ClassWeightedVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})
