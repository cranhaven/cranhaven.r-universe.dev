testthat::test_that("MCC: initialize function works", {

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

  testthat::expect_is(MCC$new(performance = confMatrix),
                      "MCC")
})

testthat::test_that("MCC: compute function works", {

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

  testthat::expect_is(MCC$new(performance = confMatrix)$compute(performance.output = NULL),
                      "numeric")

  testthat::expect_is(MCC$new(performance = NULL)$compute(performance.output = confMatrix),
                      "numeric")
})

testthat::test_that("MCC: compute function checks parameter type", {
  testthat::expect_error(MCC$new(performance = NULL)$compute(performance.output = NULL),
                         "[MCC][FATAL] Performance output parameter must be defined as 'MinResult' or 'ConfMatrix' type. Aborting...",
                         fixed = TRUE)
})
