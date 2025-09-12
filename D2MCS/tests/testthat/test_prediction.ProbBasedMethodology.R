testthat::test_that("ProbBasedMethodology: initialize function works", {

  testthat::expect_is(ProbBasedMethodology$new(required.metrics = c("MCC", "PPV")),
                      "ProbBasedMethodology")
})

testthat::test_that("ProbBasedMethodology: initialize function checks parameter type", {

  testthat::expect_error(ProbBasedMethodology$new(required.metrics = NULL),
                         "[ProbBasedMethodology][FATAL] Invalid values of required.metrics. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ProbBasedMethodology: compute function works", {

  probAV <- ProbBasedMethodology$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- list("Positive", "Negative")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_is(probAV$compute(raw.pred = raw.pred,
                                     prob.pred = prob.pred,
                                     positive.class = positive.class,
                                     negative.class = negative.class),
                      "numeric")

})

testthat::test_that("ProbBasedMethodology: compute function checks parameter type", {

  probAV <- ProbBasedMethodology$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- NULL
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(probAV$compute(raw.pred = raw.pred,
                                        prob.pred = prob.pred,
                                        positive.class = positive.class,
                                        negative.class = negative.class),
                         "[ProbBasedMethodology][FATAL] Raw.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "xxx")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(probAV$compute(raw.pred = raw.pred,
                                        prob.pred = prob.pred,
                                        positive.class = positive.class,
                                        negative.class = negative.class),
                         "[ProbBasedMethodology][FATAL] Raw.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- NULL
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(probAV$compute(raw.pred = raw.pred,
                                        prob.pred = prob.pred,
                                        positive.class = positive.class,
                                        negative.class = negative.class),
                         "[ProbBasedMethodology][FATAL] Prob.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "xxx")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(probAV$compute(raw.pred = raw.pred,
                                        prob.pred = prob.pred,
                                        positive.class = positive.class,
                                        negative.class = negative.class),
                         "[ProbBasedMethodology][FATAL] Prob.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- NULL
  negative.class <- "Negative"

  testthat::expect_error(probAV$compute(raw.pred = raw.pred,
                                        prob.pred = prob.pred,
                                        positive.class = positive.class,
                                        negative.class = negative.class),
                         "[ProbBasedMethodology][FATAL] Positive class parameter must be defined. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- NULL

  testthat::expect_error(probAV$compute(raw.pred = raw.pred,
                                        prob.pred = prob.pred,
                                        positive.class = positive.class,
                                        negative.class = negative.class),
                         "[ProbBasedMethodology][FATAL] Negative class parameter must be defined. Aborting...",
                         fixed = TRUE)
})
