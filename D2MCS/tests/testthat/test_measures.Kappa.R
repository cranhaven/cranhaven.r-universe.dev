testthat::test_that("Kappa: initialize function works", {

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

  testthat::expect_is(Kappa$new(performance = confMatrix),
                      "Kappa")
})

testthat::test_that("Kappa: compute function works", {

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

  testthat::expect_is(Kappa$new(performance = confMatrix)$compute(performance.output = NULL),
                      "numeric")

  testthat::expect_is(Kappa$new(performance = NULL)$compute(performance.output = confMatrix),
                      "numeric")
})

testthat::test_that("Kappa: compute function checks parameter type", {
  testthat::expect_error(Kappa$new(performance = NULL)$compute(performance.output = NULL),
                         "[Kappa][FATAL] Performance output parameter must be defined as 'MinResult' or 'ConfMatrix' type. Aborting...",
                         fixed = TRUE)
})
