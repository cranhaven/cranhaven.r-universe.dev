testthat::test_that("NoProbability: initialize function works", {

  noProbability <- NoProbability$new()
  testthat::expect_is(noProbability,
                      "NoProbability")

  testthat::expect_equal(noProbability$getMeasures(),
                         c("Kappa", "Accuracy", "TCR_9", "MCC", "PPV"))
})

testthat::test_that("NoProbability: execute function works", {

  noProbability <- NoProbability$new()

  data <- data.frame(c(1, 1, 0), c(0, 1, 0))
  names(data) <- c("obs", "pred")
  levels(data$obs) <- c(1,0)
  levels(data$pred) <- c(1,0)

  result <- noProbability$execute(data = data,
                                  lev = NULL,
                                  model = NULL)

  testthat::expect_length(result, 5)
  testthat::expect_equal(names(result), c("Kappa", "Accuracy", "TCR_9", "MCC", "PPV"))
  testthat::expect_equal(class(result), "numeric")
})

testthat::test_that("NoProbability: execute function checks parameter", {

  noProbability <- NoProbability$new()

  data <- data.frame(c(1, 1, 0, 2), c(0, 1, 0, 2))
  names(data) <- c("obs", "pred")
  levels(data$obs) <- c(1, 0, 2)
  levels(data$pred) <- c(1, 0, 2)

  testthat::expect_error(noProbability$execute(data = data,
                                               lev = NULL,
                                               model = NULL),
                         "[NoProbability][FATAL] Your outcome has 3 levels. The 'NoProbability' function is not appropriate. Aborting...",
                         fixed = TRUE)

  data <- data.frame(c(1, 1, 0), c(0, 1, 2))
  names(data) <- c("obs", "pred")
  levels(data$obs) <- c(1, 0)
  levels(data$pred) <- c(1, 2)

  testthat::expect_error(noProbability$execute(data = data,
                                               lev = NULL,
                                               model = NULL),
                         "[NoProbability][FATAL] Levels of observed and predicted data do not match. Aborting...",
                         fixed = TRUE)
})
