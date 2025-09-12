testthat::test_that("Precision: initialize function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- ConfMatrix$new(confMatrix = caret::confusionMatrix(xtab))

  testthat::expect_is(Precision$new(performance = confMatrix),
                      "Precision")
})

testthat::test_that("Accuracy: compute function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- ConfMatrix$new(confMatrix = caret::confusionMatrix(xtab))

  testthat::expect_is(Precision$new(performance = confMatrix)$compute(performance.output = NULL),
                      "numeric")

  testthat::expect_is(Precision$new(performance = NULL)$compute(performance.output = confMatrix),
                      "numeric")
})

testthat::test_that("Precision: compute function checks parameter type", {
  testthat::expect_error(Precision$new(performance = NULL)$compute(performance.output = NULL),
                         "[Precision][FATAL] Performance output parameter must be defined as 'MinResult' or 'ConfMatrix' type. Aborting...",
                         fixed = TRUE)
})
