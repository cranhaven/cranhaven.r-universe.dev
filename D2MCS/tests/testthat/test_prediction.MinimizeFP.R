testthat::test_that("MinimizeFP: initialize function works", {

  testthat::expect_is(MinimizeFP$new(required.metrics = c("MCC", "PPV")),
                      "MinimizeFP")
})

testthat::test_that("MinimizeFP: initialize function checks parameter type", {

  testthat::expect_error(MinimizeFP$new(required.metrics = NULL),
                         "[MinimizeFP][FATAL] Invalid values of required.metrics. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("MinimizeFP: getFinalPrediction function works", {

  minFP <- MinimizeFP$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_true(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                 prob.pred = prob.pred,
                                                 positive.class = positive.class,
                                                 negative.class = negative.class))

  raw.pred <- list("Positive", "Negative")
  names(raw.pred) <- c("MCC", "PPV")

  testthat::expect_false(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class))
})

testthat::test_that("MinimizeFP: getFinalPrediction function checks parameter type", {

  minFP <- MinimizeFP$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- NULL
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFP][FATAL] Raw.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "xxx")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFP][FATAL] Raw.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- NULL
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFP][FATAL] Prob.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "xxx")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFP][FATAL] Prob.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- NULL
  negative.class <- "Negative"

  testthat::expect_error(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFP][FATAL] Positive class parameter must be defined. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- NULL

  testthat::expect_error(minFP$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFP][FATAL] Negative class parameter must be defined. Aborting...",
                         fixed = TRUE)

})
