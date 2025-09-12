testthat::test_that("ProbAverageVoting: initialize function works", {

  cutoff <- 0.5
  class.tie.character <- "Positive"
  majority.class <- "Positive"

  testthat::expect_is(ProbAverageVoting$new(cutoff = cutoff,
                                            class.tie = class.tie.character,
                                            majority.class = majority.class),
                      "ProbAverageVoting")

  class.tie.numeric <- 1

  testthat::expect_is(ProbAverageVoting$new(cutoff = cutoff,
                                            class.tie = class.tie.numeric,
                                            majority.class = majority.class),
                      "ProbAverageVoting")

  class.tie.null <- NULL

  testthat::expect_is(ProbAverageVoting$new(cutoff = cutoff,
                                            class.tie = class.tie.null,
                                            majority.class = majority.class),
                      "ProbAverageVoting")
})

testthat::test_that("ProbAverageVoting: initialize function checks parameter type", {

  cutoff <- 0.5
  class.tie <- list()
  majority.class <- "Positive"

  testthat::expect_error(ProbAverageVoting$new(cutoff = cutoff,
                                               class.tie = class.tie,
                                               majority.class = majority.class),
                         "[ProbAverageVoting][FATAL] Invalid class tie value. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ProbAverageVoting: getMajorityClass function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ProbAverageVoting$new(cutoff = cutoff,
                                               class.tie = class.tie,
                                               majority.class = majority.class)$getMajorityClass(),
                         "Positive")
})

testthat::test_that("ProbAverageVoting: getClassTie function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ProbAverageVoting$new(cutoff = cutoff,
                                               class.tie = class.tie,
                                               majority.class = majority.class)$getClassTie(),
                         "Positive")
})

testthat::test_that("ProbAverageVoting: execute function works", {

  cutoff <- 0.5
  class.tie <- "1"
  majority.class <- "1"

  voting <- ProbAverageVoting$new(cutoff = cutoff,
                                  class.tie = class.tie,
                                  majority.class = majority.class)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  verbose <- TRUE
  testthat::expect_message(voting$execute(predictions = predictions,
                                          verbose = verbose),
                           "[ProbAverageVoting][INFO] Performing voting using '1' as tie solving",
                           fixed = TRUE)
})

testthat::test_that("ProbAverageVoting: execute function works (tie)", {

  cutoff <- 0.5
  class.tie <- "1"
  majority.class <- "1"

  voting <- ProbAverageVoting$new(cutoff = cutoff,
                                  class.tie = class.tie,
                                  majority.class = majority.class)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  pred <- predictions$.__enclos_env__$private$pred[1]
  pred[[1]]$.__enclos_env__$private$results$prob[1, ] <- c(0.5, 0.5)
  predictions$.__enclos_env__$private$pred <- pred

  verbose <- TRUE
  testthat::expect_message(voting$execute(predictions = predictions,
                                          verbose = verbose),
                           "[ProbAverageVoting][INFO] Tie solver found. Resolving tie using '1'.",
                           fixed = TRUE)

  cutoff <- 0.5
  class.tie <- NULL
  majority.class <- "1"

  voting <- ProbAverageVoting$new(cutoff = cutoff,
                                  class.tie = class.tie,
                                  majority.class = majority.class)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  pred <- predictions$.__enclos_env__$private$pred[1]
  pred[[1]]$.__enclos_env__$private$results$prob[1, ] <- c(0.5, 0.5)
  predictions$.__enclos_env__$private$pred <- pred

  verbose <- TRUE
  testthat::expect_message(voting$execute(predictions = predictions,
                                          verbose = verbose),
                           "[ProbAverageVoting][INFO] Tie solver not found. Resolving tie using first occurrence.",
                           fixed = TRUE)
})

testthat::test_that("ProbAverageVoting: execute function checks parameter type", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  voting <- ProbAverageVoting$new(cutoff = cutoff,
                                  class.tie = class.tie,
                                  majority.class = majority.class)

  testthat::expect_error(voting$execute(predictions = NULL,
                                        verbose = FALSE),
                         "[ProbAverageVoting][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[ProbAverageVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})
