testthat::test_that("MinimizeFN: initialize function works", {

  testthat::expect_is(MinimizeFN$new(required.metrics = c("MCC", "PPV")),
                      "MinimizeFN")
})

testthat::test_that("MinimizeFN: initialize function checks parameter type", {

  testthat::expect_error(MinimizeFN$new(required.metrics = NULL),
                         "[MinimizeFN][FATAL] Invalid values of required.metrics. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("MinimizeFN: getFinalPrediction function works", {

  maxFN <- MinimizeFN$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- list("Positive", "Negative")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_true(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                 prob.pred = prob.pred,
                                                 positive.class = positive.class,
                                                 negative.class = negative.class))

  raw.pred <- list("Negative", "Negative")
  names(raw.pred) <- c("MCC", "PPV")

  testthat::expect_false(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class))
})

testthat::test_that("MinimizeFN: getFinalPrediction function checks parameter type", {

  maxFN <- MinimizeFN$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- NULL
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFN][FATAL] Raw.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "xxx")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFN][FATAL] Raw.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- NULL
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFN][FATAL] Prob.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "xxx")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFN][FATAL] Prob.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- NULL
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFN][FATAL] Positive class parameter must be defined. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- NULL

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MinimizeFN][FATAL] Negative class parameter must be defined. Aborting...",
                         fixed = TRUE)

})
