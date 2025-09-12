testthat::test_that("FN: initialize function works", {

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

  testthat::expect_is(FN$new(performance = confMatrix),
                      "FN")
})

testthat::test_that("FN: compute function works", {

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

  testthat::expect_is(FN$new(performance = confMatrix)$compute(performance.output = NULL),
                      "character")

  testthat::expect_is(FN$new(performance = NULL)$compute(performance.output = confMatrix),
                      "character")
})

testthat::test_that("FN: compute function checks parameter type", {
  testthat::expect_error(FN$new(performance = NULL)$compute(performance.output = NULL),
                         "[FN][FATAL] Performance output parameter must be defined as 'MinResult' or 'ConfMatrix' type. Aborting...",
                         fixed = TRUE)
})
