testthat::test_that("ConfMatrix: initialize function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- caret::confusionMatrix(xtab)

  testthat::expect_is(ConfMatrix$new(confMatrix = confMatrix),
                      "ConfMatrix")
})

testthat::test_that("ConfMatrix: initialize function checks parameter type", {

  testthat::expect_error(ConfMatrix$new(confMatrix = NULL),
                         "[ConfMatrix][FATAL] ConfMatrix parameter must be defined as 'caret::confusionMatrix' type. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ConfMatrix: getConfusionMatrix function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- caret::confusionMatrix(xtab)

  testthat::expect_equal(ConfMatrix$new(confMatrix = confMatrix)$getConfusionMatrix(),
                         confMatrix)
})

testthat::test_that("ConfMatrix: getTP function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- caret::confusionMatrix(xtab)

  testthat::expect_equal(ConfMatrix$new(confMatrix = confMatrix)$getTP(),
                         231)
})

testthat::test_that("ConfMatrix: getTN function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- caret::confusionMatrix(xtab)

  testthat::expect_equal(ConfMatrix$new(confMatrix = confMatrix)$getTN(),
                         54)
})

testthat::test_that("ConfMatrix: getFN function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- caret::confusionMatrix(xtab)

  testthat::expect_equal(ConfMatrix$new(confMatrix = confMatrix)$getFN(),
                         27)
})

testthat::test_that("ConfMatrix: getFP function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- caret::confusionMatrix(xtab)

  testthat::expect_equal(ConfMatrix$new(confMatrix = confMatrix)$getFP(),
                         32)
})
