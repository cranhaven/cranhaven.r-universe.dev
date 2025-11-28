#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_callInit function.
#
########################

testthat::test_that("Same values as with the offset approach", {
  # Create datasets.
  set.seed(4)
  # training set.
  n <- 10000 #500000 # size of training set (number of observations)

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

  training.set <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)

  # validation set
  n.val <- 1500000 # size of validation set (number of observations)

  Gender <- factor(sample(c("male", "female"), n.val, replace = TRUE))
  Age <- sample(c(18:65), n.val, replace = TRUE)
  Split <- factor(sample(c("yes", "no"), n.val, replace = TRUE))
  Sport <- factor(sample(c("yes", "no"), n.val, replace = TRUE))

  lambda <- 0.1 * ifelse(Gender == "male", 1.1, 1)
  lambda <- lambda * (1 + 1 / (Age - 17) ^ 0.5)
  lambda <- lambda * ifelse(Sport == "yes", 1.15, 1)

  ExpoR <- runif(n.val)

  Y <- rpois(n.val, ExpoR * lambda)
  Y_normalized <- Y / ExpoR

  validation.set <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, lambda, Y_normalized)

  # Additional parameters.
  tweedie.power <- 1
  respVar <- "Y_normalized"
  w <- "ExpoR"

  # comparison.
  resBT <-
    BT_callInit(training.set, validation.set, tweedie.power, respVar, w)

  # Offset approach
  formGLM <- as.formula('Y ~ 1 + offset(log(ExpoR))')
  resGLM <-
    glm(formula = formGLM,
        family = poisson(link = "log"),
        data = training.set)

  pred_training <-
    predict(resGLM, newdata = training.set, type = 'response')
  pred_validation <-
    predict(resGLM, newdata = validation.set, type = 'response')

  train_error <-
    sum(BT::BT_devTweedie(training.set$Y, pred_training, tweedieVal = 1)) /
    nrow(training.set)
  validation_error <-
    sum(BT::BT_devTweedie(validation.set$Y, pred_validation, tweedieVal = 1)) /
    nrow(validation.set)

  # Similar intercept expected.
  expect_equal(log(resBT$initFit), resGLM$coefficients[[1]])

  # Similar predictions.
  expect_equal(exp(resBT$currTrainScore) * training.set$ExpoR,
               unname(pred_training))
  expect_equal(exp(resBT$currValScore) * validation.set$ExpoR,
               unname(pred_validation))
  # Should be similar to the intercept.
  expect_equal(unname(unlist(resBT$currTrainScore)), rep(resGLM$coefficients[[1]], nrow(training.set)))
  expect_equal(unname(unlist(resBT$currValScore)), rep(resGLM$coefficients[[1]], nrow(validation.set)))

  # Similar deviance.
  expect_equal(train_error, resBT$trainingError)
  expect_equal(validation_error, resBT$validationError)

})
