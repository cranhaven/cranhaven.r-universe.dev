testthat::test_that("UseProbability: initialize function works", {
  useProbability <- UseProbability$new()
  testthat::expect_is(useProbability,
                      "UseProbability")

  testthat::expect_equal(useProbability$getMeasures(),
                         c("ROC", "Sens", "Spec", "Kappa", "Accuracy", "TCR_9", "MCC", "PPV"))
})

testthat::test_that("UseProbability: execute function works", {

  useProbability <- UseProbability$new()

  data <- data.frame(as.factor(c(1, 1, 0)), as.factor(c(0, 1, 0)), c(0.4, 0.7, 0.2), c(0.6, 0.3, 0.8))
  names(data) <- c("obs", "pred", "1", "0")
  levels(data$obs) <- c(1, 0)
  levels(data$pred) <- c(1, 0)
  lev <- as.factor(c(1, 0))

  result <- useProbability$execute(data = data,
                                   lev = lev,
                                   model = NULL)

  testthat::expect_length(result, 8)
  testthat::expect_equal(names(result), c("ROC", "Sens", "Spec", "Kappa", "Accuracy", "TCR_9", "MCC", "PPV"))
  testthat::expect_equal(class(result), "numeric")
})

testthat::test_that("UseProbability: execute function checks parameter", {

  useProbability <- UseProbability$new()

  data <- data.frame(c(1, 1, 0, 2), c(0, 1, 0, 2))
  names(data) <- c("obs", "pred")
  levels(data$obs) <- c(1, 0, 2)
  levels(data$pred) <- c(1, 0, 2)

  testthat::expect_error(useProbability$execute(data = data,
                                                lev = NULL,
                                                model = NULL),
                         "[UseProbability][FATAL] Your outcome has 3 levels. The 'UseProbability' function is not appropriate. Aborting...",
                         fixed = TRUE)

  data <- data.frame(c(1, 1, 0), c(0, 1, 2))
  names(data) <- c("obs", "pred")
  levels(data$obs) <- c(1, 0)
  levels(data$pred) <- c(1, 2)

  testthat::expect_error(useProbability$execute(data = data,
                                                lev = NULL,
                                                model = NULL),
                         "[UseProbability][FATAL] Levels of observed and predicted data do not match. Aborting...",
                         fixed = TRUE)
})
