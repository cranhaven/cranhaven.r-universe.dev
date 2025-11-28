#########################
# Author : Gireg Willame
# May 2022.
#
# Series of tests to check the outputs of the BT algorithm.
# Allows us to evaluate if the outputs are expected.
#
########################

testthat::test_that("Checks BT outputs",{

  # Create datasets.
  set.seed(4)
  # training set.
  n <- 10000#500000 # size of training set (number of observations)

  Gender <- factor(sample(c("male","female"),n,replace=TRUE))
  Age <- sample(c(18:65),n,replace=TRUE)
  Split <- factor(sample(c("yes","no"),n,replace=TRUE))
  Sport <- factor(sample(c("yes","no"),n,replace=TRUE))


  lambda <- 0.1*ifelse(Gender=="male",1.1,1)
  lambda <- lambda*(1+1/(Age-17)^0.5)
  lambda <- lambda*ifelse(Sport=="yes",1.15,1)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR*lambda)
  Y_normalized <- Y/ExpoR

  training.set <- data.frame(Y,Gender,Age,Split,Sport,ExpoR, Y_normalized)

  # validation set
  n.val <- 1500000 # size of validation set (number of observations)

  Gender <- factor(sample(c("male","female"),n.val,replace=TRUE))
  Age <- sample(c(18:65),n.val,replace=TRUE)
  Split <- factor(sample(c("yes","no"),n.val,replace=TRUE))
  Sport <- factor(sample(c("yes","no"),n.val,replace=TRUE))

  lambda <- 0.1*ifelse(Gender=="male",1.1,1)
  lambda <- lambda*(1+1/(Age-17)^0.5)
  lambda <- lambda*ifelse(Sport=="yes",1.15,1)

  ExpoR <- runif(n.val)

  Y <- rpois(n.val, ExpoR*lambda)
  Y_normalized <- Y/ExpoR

  test.set <- data.frame(Y,Gender,Age,Split,Sport,ExpoR,lambda, Y_normalized)

  # Additional parameters.
  tweedie.power <- 1
  respVar <- "Y_normalized"
  w <- "ExpoR"

  # First check.
  paramsList <- list(formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
                     data = training.set)

  BT_algo <- do.call(BT, paramsList)

  expect_equal(BT_algo$BTParams$ABT, T)
  expect_equal(BT_algo$BTParams$train.fraction, 1)
  expect_equal(BT_algo$BTParams$shrinkage, 1)
  expect_equal(BT_algo$BTParams$interaction.depth, 4)
  expect_equal(BT_algo$BTParams$bag.fraction, 1)
  expect_equal(BT_algo$BTParams$n.iter, 100)
  expect_equal(BT_algo$BTParams$colsample.bytree, NULL)
  expect_equal(BT_algo$BTParams$tree.control$minsplit, 2)
  expect_equal(BT_algo$BTParams$tree.control$cp, -Inf)
  expect_equal(BT_algo$BTParams$tree.control$xval, 0)
  expect_equal(BT_algo$BTParams$tree.control$maxdepth, 4)
  expect_equal(BT_algo$BTData$validation.set, NULL)
  expect_equal(BT_algo$distribution, 1)
  expect_equal(length(BT_algo$BTIndivFits), BT_algo$BTParams$n.iter)
  expect_equal(BT_algo$response, respVar)
  expect_equal(BT_algo$var.names, c("Gender", "Age", "Split", "Sport"))
  expect_equal(BT_algo$w, "w")
  expect_equal(BT_algo$keep.data, T)
  expect_equal(BT_algo$is.verbose, F)
  expect_equal(BT_algo$cv.folds, 1)

  expect_equal(length(BT_algo$fitted.values), nrow(training.set))
  expect_equal(length(BT_algo$BTInit$training.error), 1)
  expect_equal(BT_algo$BTInit$validation.error, NULL)

  expect_equal(BT_algo$BTErrors$validation.error, NULL)
  expect_equal(BT_algo$BTErrors$oob.improvement, NULL)
  expect_equal(length(BT_algo$BTErrors$training.error), BT_algo$BTParams$n.iter)

  # Second check.
  paramsList <- list(formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
                     data = training.set,
                     n.iter = 200,
                     train.fraction = 0.5,
                     interaction.depth = 2,
                     shrinkage = 0.01,
                     bag.fraction = 0.5,
                     colsample.bytree = 2,
                     keep.data = F,
                     is.verbose = T,
                     cv.folds = 1,
                     folds.id = NULL)

  BT_algo <- do.call(BT, paramsList)

  expect_equal(BT_algo$BTParams$ABT, T)
  expect_equal(BT_algo$BTParams$train.fraction, 0.5)
  expect_equal(BT_algo$BTParams$interaction.depth, 2)
  expect_equal(BT_algo$BTParams$shrinkage, 0.01)
  expect_equal(BT_algo$BTParams$bag.fraction, 0.5)
  expect_equal(BT_algo$BTParams$n.iter, 200)
  expect_equal(BT_algo$BTParams$colsample.bytree, 2)
  expect_equal(BT_algo$BTParams$tree.control$minsplit, 2)
  expect_equal(BT_algo$BTParams$tree.control$cp, -Inf)
  expect_equal(BT_algo$BTParams$tree.control$xval, 0)
  expect_equal(BT_algo$BTParams$tree.control$maxdepth, 2)
  expect_equal(BT_algo$BTData$validation.set, NULL)
  expect_equal(BT_algo$BTData$training.set, NULL)
  expect_equal(BT_algo$distribution, 1)
  expect_equal(length(BT_algo$BTIndivFits), BT_algo$BTParams$n.iter)
  expect_equal(BT_algo$response, respVar)
  expect_equal(BT_algo$var.names, c("Gender", "Age", "Split", "Sport"))
  expect_equal(BT_algo$w, "w")
  expect_equal(BT_algo$keep.data, F)
  expect_equal(BT_algo$is.verbose, T)
  expect_equal(BT_algo$cv.folds, 1)

  expect_equal(length(BT_algo$fitted.values), nrow(training.set)*BT_algo$BTParams$train.fraction)
  expect_equal(length(BT_algo$BTInit$training.error), 1)
  expect_equal(length(BT_algo$BTInit$validation.error), 1)

  expect_equal(length(BT_algo$BTErrors$oob.improvement), BT_algo$BTParams$n.iter)
  expect_equal(length(BT_algo$BTErrors$training.error), BT_algo$BTParams$n.iter)
  expect_equal(length(BT_algo$BTErrors$validation.error), BT_algo$BTParams$n.iter)

})


