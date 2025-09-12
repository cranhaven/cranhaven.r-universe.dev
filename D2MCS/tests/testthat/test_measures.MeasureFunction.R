testthat::test_that("MeasureFunction: initialize function works", {

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

  testthat::expect_is(MeasureFunction$new(performance = confMatrix),
                      "MeasureFunction")
})

testthat::test_that("MeasureFunction: initialize function checks parameter type", {

  testthat::expect_error(MeasureFunction$new(performance = list()),
                         "[MeasureFunction][FATAL] Performance parameter must be defined as 'MinResult' or 'ConfMatrix' type. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("MeasureFunction: compute function works", {

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

  testthat::expect_error(MeasureFunction$new(performance = confMatrix)$compute(),
                         "[MeasureFunction][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})
