#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the print.BT function.
#
########################

testthat::test_that("Check the BT_Print function - Results", {
  skip_on_cran()

  # Create datasets.
  set.seed(4)
  n <- 10000 #100000

  Gender <- factor(sample(c("male", "female"), n, replace = TRUE))
  Age <- sample(c(18:65), n, replace = TRUE)
  Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
  Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

  lambda <- 0.1 * ifelse(Gender == "male", 1.1, 1)
  lambda <- lambda * (1 + 1 / (Age - 17) ^ 0.5)
  lambda <- lambda * ifelse(Sport == "yes", 1.15, 1)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR * lambda)
  Y_normalized <- Y / ExpoR
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)

  # Run a BT algo.
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 100,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  best.iter <- BT_perf(BT_algo, plot.it = F, method = "validation")
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  100 iterations were performed."
  expectedMessage_part3 <-
    paste("The best validation-set iteration was ", best.iter, ".", sep = "")
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part3, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(print(BT_algo), printFun())

  ####
  # With OOB only.
  ####
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 1,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  expect_message(best.iter <-
                   BT_perf(BT_algo, plot.it = F, method = "OOB"))
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  200 iterations were performed."
  expectedMessage_part3 <-
    paste("The best out-of-bag iteration was ", best.iter, ".", sep = "")
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part3, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(expect_message(print(BT_algo)), printFun())

  ####
  # With cv only.
  ####
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 1,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 3,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  best.iter <- BT_perf(BT_algo, plot.it = F, method = "cv")
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  200 iterations were performed."
  expectedMessage_part3 <-
    paste("The best cross-validation iteration was ", best.iter, ".", sep = "")
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part3, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(print(BT_algo), printFun())

  ####
  # No validation, oob nor cv.
  ####
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 1,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  best.iter <- 200
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  200 iterations were performed."
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(print(BT_algo), printFun())

  ####
  # With Validation and OOB.
  ####
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  best.iter <- BT_perf(BT_algo, plot.it = F, method = "validation")
  expect_message(best.iter.oob <-
                   BT_perf(BT_algo, plot.it = F, method = "OOB"))
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  200 iterations were performed."
  expectedMessage_part3 <-
    paste("The best out-of-bag iteration was ", best.iter.oob, ".", sep = "")
  expectedMessage_part3_bis <-
    paste("The best validation-set iteration was ", best.iter, ".", sep = "")
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part3, "\n")
    cat(expectedMessage_part3_bis, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(expect_message(print(BT_algo)), printFun())

  ####
  # With cv and OOB.
  ####
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 1,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 3,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  best.iter <- BT_perf(BT_algo, plot.it = F, method = "cv")
  expect_message(best.iter.oob <-
                   BT_perf(BT_algo, plot.it = F, method = "OOB"))
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  200 iterations were performed."
  expectedMessage_part3 <-
    paste("The best out-of-bag iteration was ", best.iter.oob, ".", sep = "")
  expectedMessage_part3_bis <-
    paste("The best cross-validation iteration was ", best.iter, ".", sep = "")
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part3, "\n")
    cat(expectedMessage_part3_bis, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(expect_message(print(BT_algo)), printFun())

  ####
  # With Validation and cv
  ####
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 3,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  best.iter <- BT_perf(BT_algo, plot.it = F, method = "validation")
  best.iter.cv <- BT_perf(BT_algo, plot.it = F, method = "cv")
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  200 iterations were performed."
  expectedMessage_part3 <-
    paste("The best cross-validation iteration was ",
          best.iter.cv,
          ".",
          sep = "")
  expectedMessage_part3_bis <-
    paste("The best validation-set iteration was ", best.iter, ".", sep = "")
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part3, "\n")
    cat(expectedMessage_part3_bis, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(print(BT_algo), printFun())

  ####
  # With validation, cv and OOB.
  ####
  set.seed(4)
  BT_algo <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 3,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  best.iter <- BT_perf(BT_algo, plot.it = F, method = "validation")
  best.iter.cv <- BT_perf(BT_algo, plot.it = F, method = "cv")
  expect_message(best.iter.oob <-
                   BT_perf(BT_algo, plot.it = F, method = "OOB"))
  ri <- .BT_relative_influence(BT_algo, best.iter)

  expectedMessage_part1 <- BT_algo$call
  expectedMessage_part2 <-
    "An adaptive boosting tree model with Tweedie parameter : 1  has been fitted.
  200 iterations were performed."
  expectedMessage_part3 <-
    paste("The best out-of-bag iteration was ", best.iter.oob, ".", sep = "")
  expectedMessage_part3_bis <-
    paste("The best cross-validation iteration was ",
          best.iter.cv,
          ".",
          sep = "")
  expectedMessage_part3_bis2 <-
    paste("The best validation-set iteration was ", best.iter, ".", sep = "")
  expectedMessage_part4 <-
    paste(
      "There were ",
      length(BT_algo$var.names),
      " predictors of which ",
      sum(ri > 0),
      " had non-zero influence.",
      sep = ""
    )

  printFun <- function() {
    print(expectedMessage_part1)
    cat(expectedMessage_part2, "\n")
    cat(expectedMessage_part3, "\n")
    cat(expectedMessage_part3_bis, "\n")
    cat(expectedMessage_part3_bis2, "\n")
    cat(expectedMessage_part4, "\n")
  }
  expect_output(expect_message(print(BT_algo)), printFun())

})
