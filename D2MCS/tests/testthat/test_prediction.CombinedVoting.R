testthat::test_that("CombinedVoting: initialize function works", {
  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- MinimizeFP$new()
  methodology <- ProbBasedMethodology$new()
  metrics <- c("MCC", "PPV")
  testthat::expect_is(CombinedVoting$new(voting.schemes = voting.schemes,
                                         combined.metrics = combined.metrics,
                                         methodology = methodology,
                                         metrics = c("MCC", "PPV")),
                      "CombinedVoting")
})

testthat::test_that("CombinedVoting: initialize function checks parameter type", {

  voting.schemes <- NULL
  combined.metrics <- MinimizeFP$new()
  methodology <- ProbBasedMethodology$new()
  metrics <- c("MCC")
  testthat::expect_error(CombinedVoting$new(voting.schemes = voting.schemes,
                                            combined.metrics = combined.metrics,
                                            methodology = methodology,
                                            metrics = metrics),
                         "[CombinedVoting][FATAL] Voting.schemes parameter must be defined as 'SimpleVoting' type. Aborting...",
                         fixed = TRUE)

  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- NULL
  methodology <- ProbBasedMethodology$new()
  metrics <- c("MCC")
  testthat::expect_error(CombinedVoting$new(voting.schemes = voting.schemes,
                                            combined.metrics = combined.metrics,
                                            methodology = methodology,
                                            metrics = metrics),
                         "[CombinedVoting][FATAL] Combined.metrics parameter must be defined as 'CombinedMetrics' type. Aborting...",
                         fixed = TRUE)

  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- MinimizeFP$new()
  methodology <- NULL
  metrics <- c("MCC")
  testthat::expect_error(CombinedVoting$new(voting.schemes = voting.schemes,
                                            combined.metrics = combined.metrics,
                                            methodology = methodology,
                                            metrics = metrics),
                         "[CombinedVoting][FATAL] Methodology parameter must be defined as 'Methodology' type. Aborting...",
                         fixed = TRUE)

  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- MinimizeFP$new()
  methodology <- ProbBasedMethodology$new()
  metrics <- NULL
  testthat::expect_error(CombinedVoting$new(voting.schemes = voting.schemes,
                                            combined.metrics = combined.metrics,
                                            methodology = methodology,
                                            metrics = metrics),
                         "[CombinedVoting][FATAL] Invalid values of metrics. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("CombinedVoting: getCombinedMetrics function works", {
  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- MinimizeFP$new()
  methodology <- ProbBasedMethodology$new()
  metrics <- c("MCC", "PPV")

  voting <- CombinedVoting$new(voting.schemes = voting.schemes,
                               combined.metrics = combined.metrics,
                               methodology = methodology,
                               metrics = c("MCC", "PPV"))

  testthat::expect_equal(voting$getCombinedMetrics(), combined.metrics)
})

testthat::test_that("CombinedVoting: getCombinedMetrics function works", {
  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- MinimizeFP$new()
  methodology <- ProbBasedMethodology$new()
  metrics <- c("MCC", "PPV")

  voting <- CombinedVoting$new(voting.schemes = voting.schemes,
                               combined.metrics = combined.metrics,
                               methodology = methodology,
                               metrics = c("MCC", "PPV"))

  testthat::expect_equal(voting$getMethodology(), methodology)
})

testthat::test_that("CombinedVoting: getFinalPred function works", {

  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- MinimizeFP$new()
  methodology <- ProbBasedMethodology$new()
  metrics <- c("MCC", "PPV")
  voting <- CombinedVoting$new(voting.schemes = voting.schemes,
                               combined.metrics = combined.metrics,
                               methodology = methodology,
                               metrics = metrics)
  type <- NULL
  target <- NULL
  filter <- NULL
  testthat::expect_is(voting$getFinalPred(type = type,
                                          target = target,
                                          filter = filter),
                      "FinalPred")

  type <- "raw"
  target <- NULL
  filter <- NULL
  testthat::expect_null(voting$getFinalPred(type = type,
                                          target = target,
                                          filter = filter))

  type <- "raw"
  target <- NULL
  filter <- TRUE
  testthat::expect_null(voting$getFinalPred(type = type,
                                            target = target,
                                            filter = filter))

  type <- "prob"
  target <- NULL
  filter <- NULL
  testthat::expect_null(voting$getFinalPred(type = type,
                                          target = target,
                                          filter = filter))

  type <- "prob"
  target <- NULL
  filter <- FALSE
  testthat::expect_null(voting$getFinalPred(type = type,
                                            target = target,
                                            filter = filter))

  type <- "prob"
  target <- NULL
  filter <- NULL
  testthat::expect_null(voting$getFinalPred(type = type,
                                            target = target,
                                            filter = filter))

  type <- "prob"
  target <- NULL
  filter <- TRUE
  testthat::expect_null(voting$getFinalPred(type = type,
                                            target = target,
                                            filter = filter))
})

testthat::test_that("CombinedVoting: execute function checks parameter type", {
  testthat::skip_if_not_installed("ranger")
  voting.schemes <- ClassWeightedVoting$new(cutoff = 0.7)
  combined.metrics <- MinimizeFP$new()
  methodology <- ProbBasedMethodology$new()
  metrics <- c("MCC", "PPV")
  voting <- CombinedVoting$new(voting.schemes = voting.schemes,
                               combined.metrics = combined.metrics,
                               methodology = methodology,
                               metrics = metrics)
  predictions <- NULL
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[CombinedVoting][FATAL] Predictions parameter must be a list comprised of 'ClusterPredictions' objects. Aborting...",
                         fixed = TRUE)

  predictions <- list(ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                             positive.class = 1))
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[CombinedVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)

  voting <- CombinedVoting$new(voting.schemes = voting.schemes,
                               combined.metrics = combined.metrics,
                               methodology = methodology,
                               metrics = c("WRONG1", "WRONG2"))

  clusterPrediction <- ClusterPredictions$new(class.values = c(1,0,1,1),
                                              positive.class = 1)

  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  clusterPrediction$add(prediction = prediction)

  predictions <- list(clusterPrediction, clusterPrediction)
  names(predictions) <- c("MCC", "PPV")

  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[CombinedVoting][FATAL] Metrics are incorrect. Must be: [MCC, PPV]. Aborting...",
                         fixed = TRUE)

})
