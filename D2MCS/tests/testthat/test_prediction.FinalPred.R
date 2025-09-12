testthat::test_that("FinalPred: initialize function works", {

  testthat::expect_is(FinalPred$new(),
                      "FinalPred")
})

testthat::test_that("FinalPred: set function works", {

  prob <- data.frame(c(0.2, 0.5, 0.6), c(0.8, 0.5, 0.4))
  raw <- factor(c("Negative", "Positive", "Positive"))
  class.values <- c("Positive", "Negative")
  positive.class <- "Positive"

  testthat::expect_silent(FinalPred$new()$set(prob = prob,
                                              raw = raw,
                                              class.values = class.values,
                                              positive.class = positive.class))

  prob <- data.frame(c(0.2, 0.5, 0.6), c(0.8, 0.5, 0.4))
  raw <- c("Negative", "Positive", "Positive")
  class.values <- c("Positive", "Negative")
  positive.class <- "Positive"

  testthat::expect_silent(FinalPred$new()$set(prob = prob,
                                              raw = raw,
                                              class.values = class.values,
                                              positive.class = positive.class))

})

testthat::test_that("FinalPred: set function checks parameter type", {

  prob <- data.frame(c(0.2, 0.5, 0.6), c(0.8, 0.5, 0.4))
  raw <- factor(c("Negative", "Positive", "Positive"))
  class.values <- c("Positive", "Negative")
  positive.class <- "wrong"

  testthat::expect_error(FinalPred$new()$set(prob = prob,
                                             raw = raw,
                                             class.values = class.values,
                                             positive.class = positive.class),
                         "[FinalPred][FATAL] Positive class is invalid. Must be one of (Positive, Negative). Aborting...",
                         fixed = TRUE)

  prob <- NULL
  raw <- c("Negative", "Positive", "Positive")
  class.values <- c("Positive", "Negative")
  positive.class <- "Positive"

  testthat::expect_error(FinalPred$new()$set(prob = prob,
                                             raw = raw,
                                             class.values = class.values,
                                             positive.class = positive.class),
                         "[FinalPred][FATAL] Predictions were not computed. Aborting...",
                         fixed = TRUE)

  prob <- data.frame(c(0.2, 0.5, 0.6), c(0.8, 0.5, 0.4))
  raw <- data.frame(c("Negative", "Positive", "Positive"))
  class.values <- c("Positive", "Negative")
  positive.class <- "Positive"

  testthat::expect_error(FinalPred$new()$set(prob = prob,
                                             raw = raw,
                                             class.values = class.values,
                                             positive.class = positive.class),
                         "[FinalPred][FATAL] Class values contains NA's. Aborting...",
                         fixed = TRUE)

})

testthat::test_that("FinalPred: getProb function works", {

  testthat::expect_null(FinalPred$new()$getProb())
})

testthat::test_that("FinalPred: getRaw function works", {

  testthat::expect_null(FinalPred$new()$getRaw())
})

testthat::test_that("FinalPred: getClassValues function works", {

  testthat::expect_null(FinalPred$new()$getClassValues())
})

testthat::test_that("FinalPred: getPositiveClass function works", {

  testthat::expect_null(FinalPred$new()$getPositiveClass())
})

testthat::test_that("FinalPred: getNegativeClass function works", {

  testthat::expect_null(FinalPred$new()$getNegativeClass())
})
