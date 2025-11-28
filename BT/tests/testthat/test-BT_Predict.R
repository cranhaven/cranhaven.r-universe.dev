#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_Predict function.
#
########################

testthat::test_that("BT_Predict function checks - Check results", {
  skip_on_cran()

  # Create datasets.
  set.seed(4)
  # training set.
  n <- 10000#500000 # size of training set (number of observations)

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

  test.set <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, lambda, Y_normalized)

  # Additional parameters.
  tweedie.power <- 1
  respVar <- "Y_normalized"
  w <- "ExpoR"

  paramsList <-
    list(
      formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
      data = training.set,
      n.iter = 200,
      train.fraction = 0.5,
      interaction.depth = 2,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL
    )

  BT_algo <- do.call(BT, paramsList)

  trainSet <-
    training.set[seq(1, paramsList$train.fraction * nrow(training.set)), ]
  valSet <-
    training.set[setdiff(seq(1, nrow(training.set)), seq(1, paramsList$train.fraction *
                                                           nrow(training.set))), ]

  glmTrain <- rep(log(BT_algo$BTInit$initFit), nrow(trainSet))
  glmVal <- rep(log(BT_algo$BTInit$initFit), nrow(valSet))
  predTrain <- list()
  predVal <- list()
  for (iTree in seq(1, 200)) {
    predTrain_current <-
      log(predict(
        BT_algo$BTIndivFits[[iTree]],
        newdata = trainSet,
        type = 'vector'
      ))
    predVal_current <-
      log(predict(
        BT_algo$BTIndivFits[[iTree]],
        newdata = valSet,
        type = 'vector'
      ))
    if (iTree == 1) {
      predTrain[[iTree]] <-
        glmTrain + (BT_algo$BTParams$shrinkage * predTrain_current)
      predVal[[iTree]] <-
        glmVal + (BT_algo$BTParams$shrinkage * predVal_current)
    } else{
      predTrain[[iTree]] <-
        predTrain[[iTree - 1]] + (BT_algo$BTParams$shrinkage * predTrain_current)
      predVal[[iTree]] <-
        predVal[[iTree - 1]] + (BT_algo$BTParams$shrinkage * predVal_current)
    }
  }

  # Comparison with 20 iterations.
  expect_equal(predict(BT_algo, trainSet, n.iter = 20), unname(unlist(predTrain[[20]])))
  expect_equal(predict(BT_algo, valSet, n.iter = 20), unname(unlist(predVal[[20]])))

  # Comparison with 100 iterations.
  expect_equal(predict(BT_algo, trainSet, n.iter = 100), unname(unlist(predTrain[[100]])))
  expect_equal(predict(BT_algo, valSet, n.iter = 100), unname(unlist(predVal[[100]])))

  # Should be limited at the n.iter directly.
  expect_warning(pred_trainBT1000 <-
                   predict(BT_algo, trainSet, n.iter = 1000))
  expect_warning(pred_valBT1000 <-
                   predict(BT_algo, valSet, n.iter = 1000))
  expect_equal(pred_trainBT1000, unname(unlist(predTrain[[200]])))
  expect_equal(pred_valBT1000, unname(unlist(predVal[[200]])))

  # Expect equal if two predict requested.
  expect_equal(matrix(c(unname(
    unlist(predTrain[[20]])
  ), unname(
    unlist(predTrain[[100]])
  )), ncol = 2),
  predict(BT_algo, trainSet, c(20, 100)))
  expect_equal(matrix(c(unname(unlist(
    predVal[[20]]
  )), unname(unlist(
    predVal[[100]]
  ))), ncol = 2),
  predict(BT_algo, valSet, c(20, 100)))

  # Check if ok with descending order and multiple values.
  expect_warning(pred_trainBT_250n100n20 <-
                   predict(BT_algo, trainSet, c(250, 100, 20)))
  expect_warning(pred_valBT_250n100n20 <-
                   predict(BT_algo, valSet, c(250, 100, 20)))
  expect_equal(matrix(c(
    unname(unlist(predTrain[[200]])), unname(unlist(predTrain[[100]])),
    unname(unlist(predTrain[[20]]))
  ), ncol = 3), pred_trainBT_250n100n20)
  expect_equal(matrix(c(
    unname(unlist(predVal[[200]])), unname(unlist(predVal[[100]])),
    unname(unlist(predVal[[20]]))
  ), ncol = 3), pred_valBT_250n100n20)

  # Check for the individual fit.
  expect_equal(predict(BT_algo, trainSet, 20, single.iter = T), log(unname(unlist(
    predict(BT_algo$BTIndivFits[[20]], newdata = trainSet, type = 'vector')
  ))))
  expect_equal(predict(BT_algo, valSet, 20, single.iter = T), log(unname(unlist(
    predict(BT_algo$BTIndivFits[[20]], newdata = valSet, type = 'vector')
  ))))

  expect_warning(pred_trainBT_250n20_singleIter <-
                   predict(BT_algo, trainSet, c(250, 20), single.iter = T))
  expect_warning(pred_valBT_250n20_singleIter <-
                   predict(BT_algo, valSet, c(250, 20), single.iter = T))
  expect_equal(pred_trainBT_250n20_singleIter, matrix(c(log(unname(
    unlist(
      predict(BT_algo$BTIndivFits[[200]], newdata = trainSet, type = 'vector')
    )
  )),
  log(unname(
    unlist(
      predict(BT_algo$BTIndivFits[[20]], newdata = trainSet, type = 'vector')
    )
  ))), ncol = 2))
  expect_equal(pred_valBT_250n20_singleIter, matrix(c(log(unname(
    unlist(
      predict(BT_algo$BTIndivFits[[200]], newdata = valSet, type = 'vector')
    )
  )),
  log(unname(
    unlist(
      predict(BT_algo$BTIndivFits[[20]], newdata = valSet, type = 'vector')
    )
  ))), ncol = 2))



  ### Check with the type = 'response'.


  # Comparison with 20 iterations.
  expect_equal(predict(BT_algo, trainSet, n.iter = 20, type = 'response'),
               exp(unname(unlist(predTrain[[20]]))))
  expect_equal(predict(BT_algo, valSet, n.iter = 20, type = 'response'),
               exp(unname(unlist(predVal[[20]]))))

  # Comparison with 100 iterations.
  expect_equal(predict(BT_algo, trainSet, n.iter = 100, type = 'response'),
               exp(unname(unlist(predTrain[[100]]))))
  expect_equal(predict(BT_algo, valSet, n.iter = 100, type = 'response'),
               exp(unname(unlist(predVal[[100]]))))

  # Should be limited at the n.iter directly.
  expect_warning(pred_trainBT1000 <-
                   predict(BT_algo, trainSet, n.iter = 1000, type = 'response'))
  expect_warning(pred_valBT1000 <-
                   predict(BT_algo, valSet, n.iter = 1000, type = 'response'))
  expect_equal(pred_trainBT1000, exp(unname(unlist(predTrain[[200]]))))
  expect_equal(pred_valBT1000, exp(unname(unlist(predVal[[200]]))))

  # Expect equal if two predict requested.
  expect_equal(matrix(c(exp(unname(
    unlist(predTrain[[20]])
  )), exp(unname(
    unlist(predTrain[[100]])
  ))), ncol = 2),
  predict(BT_algo, trainSet, c(20, 100), type = 'response'))
  expect_equal(matrix(c(exp(unname(
    unlist(predVal[[20]])
  )), exp(unname(
    unlist(predVal[[100]])
  ))), ncol = 2),
  predict(BT_algo, valSet, c(20, 100), type = 'response'))

  # Check if ok with descending order and multiple values.
  expect_warning(pred_trainBT_250n100n20 <-
                   predict(BT_algo, trainSet, c(250, 100, 20), type = 'response'))
  expect_warning(pred_valBT_250n100n20 <-
                   predict(BT_algo, valSet, c(250, 100, 20), type = 'response'))
  expect_equal(matrix(c(
    exp(unname(unlist(predTrain[[200]]))), exp(unname(unlist(predTrain[[100]]))),
    exp(unname(unlist(predTrain[[20]])))
  ), ncol = 3), pred_trainBT_250n100n20)
  expect_equal(matrix(c(
    exp(unname(unlist(predVal[[200]]))), exp(unname(unlist(predVal[[100]]))),
    exp(unname(unlist(predVal[[20]])))
  ), ncol = 3), pred_valBT_250n100n20)

  # Check for the individual fit.
  expect_equal(predict(
    BT_algo,
    trainSet,
    20,
    single.iter = T,
    type = 'response'
  ),
  unname(unlist(
    predict(BT_algo$BTIndivFits[[20]], newdata = trainSet, type = 'vector')
  )))
  expect_equal(predict(BT_algo, valSet, 20, single.iter = T, type = 'response'),
               unname(unlist(
                 predict(BT_algo$BTIndivFits[[20]], newdata = valSet, type = 'vector')
               )))

  expect_warning(
    pred_trainBT_250n20_singleIter <-
      predict(
        BT_algo,
        trainSet,
        c(250, 20),
        single.iter = T,
        type = 'response'
      )
  )
  expect_warning(
    pred_valBT_250n20_singleIter <-
      predict(
        BT_algo,
        valSet,
        c(250, 20),
        single.iter = T,
        type = 'response'
      )
  )
  expect_equal(pred_trainBT_250n20_singleIter, matrix(c(unname(unlist(
    predict(BT_algo$BTIndivFits[[200]], newdata = trainSet, type = 'vector')
  )),
  unname(unlist(
    predict(BT_algo$BTIndivFits[[20]], newdata = trainSet, type = 'vector')
  ))), ncol = 2))
  expect_equal(pred_valBT_250n20_singleIter, matrix(c(unname(unlist(
    predict(BT_algo$BTIndivFits[[200]], newdata = valSet, type = 'vector')
  )),
  unname(unlist(
    predict(BT_algo$BTIndivFits[[20]], newdata = valSet, type = 'vector')
  ))), ncol = 2))


})

testthat::test_that("BT_Predict function checks - Wrong inputs", {
  skip_on_cran()

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

  test.set <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, lambda, Y_normalized)

  # Additional parameters.
  tweedie.power <- 1
  respVar <- "Y_normalized"
  w <- "ExpoR"

  paramsList <-
    list(
      formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
      data = training.set,
      n.iter = 200,
      train.fraction = 0.5,
      interaction.depth = 2,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL
    )

  BT_algo <- do.call(BT, paramsList)

  trainSet <-
    training.set[seq(1, paramsList$train.fraction * nrow(training.set)), ]
  valSet <-
    training.set[setdiff(seq(1, nrow(training.set)), seq(1, paramsList$train.fraction *
                                                           nrow(training.set))), ]

  # Check for errors.
  # Wrong n.iter argument.
  expect_error(predict(BT_algo, newdata = valSet))
  expect_error(predict(BT_algo, newdata = valSet, n.iter = 0))

  # Wrong type argument.
  expect_error(predict(BT_algo, newdata = valSet, type = "Test"))
  expect_error(predict(BT_algo, newdata = valSet, type = NA))
  expect_error(predict(BT_algo, newdata = valSet, type = NULL))

  # No dataset given with keep.data = F
  expect_error(predict(BT_algo, n.iter = 10))
  expect_error(predict(BT_algo, n.iter = 100, type = 'response'))

  # Wrong single.iter argument
  expect_error(predict(
    BT_algo,
    newdata = valSet,
    n.iter = 10,
    single.iter = NA
  ))
  expect_error(predict(
    BT_algo,
    newdata = valSet,
    n.iter = 10,
    single.iter = NULL
  ))
  expect_error(predict(
    BT_algo,
    newdata = valSet,
    n.iter = 10,
    single.iter = 2
  ))

})

testthat::test_that("BT_Predict function checks - keep.data set to TRUE", {
  skip_on_cran()

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

  test.set <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, lambda, Y_normalized)

  # Additional parameters.
  tweedie.power <- 1
  respVar <- "Y_normalized"
  w <- "ExpoR"

  paramsList <-
    list(
      formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
      data = training.set,
      n.iter = 200,
      train.fraction = 0.5,
      interaction.depth = 2,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = TRUE,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL
    )

  BT_algo <- do.call(BT, paramsList)

  trainSet <-
    training.set[seq(1, paramsList$train.fraction * nrow(training.set)), ]
  valSet <-
    training.set[setdiff(seq(1, nrow(training.set)), seq(1, paramsList$train.fraction *
                                                           nrow(training.set))), ]

  glmTrain <- rep(log(BT_algo$BTInit$initFit), nrow(trainSet))
  glmVal <- rep(log(BT_algo$BTInit$initFit), nrow(valSet))
  predTrain <- list()
  predVal <- list()
  for (iTree in seq(1, 200)) {
    predTrain_current <-
      log(predict(
        BT_algo$BTIndivFits[[iTree]],
        newdata = trainSet,
        type = 'vector'
      ))
    predVal_current <-
      log(predict(
        BT_algo$BTIndivFits[[iTree]],
        newdata = valSet,
        type = 'vector'
      ))
    if (iTree == 1) {
      predTrain[[iTree]] <-
        glmTrain + (BT_algo$BTParams$shrinkage * predTrain_current)
      predVal[[iTree]] <-
        glmVal + (BT_algo$BTParams$shrinkage * predVal_current)
    } else{
      predTrain[[iTree]] <-
        predTrain[[iTree - 1]] + (BT_algo$BTParams$shrinkage * predTrain_current)
      predVal[[iTree]] <-
        predVal[[iTree - 1]] + (BT_algo$BTParams$shrinkage * predVal_current)
    }
  }

  # Check for similar results.
  # Comparison with 20 iterations.
  expect_equal(predict(BT_algo, n.iter = 20), unname(unlist(predTrain[[20]])))

  # Comparison with 100 iterations.
  expect_equal(predict(BT_algo, n.iter = 100), unname(unlist(predTrain[[100]])))

  # Should be limited at the n.iter directly.
  expect_warning(pred_trainBT1000 <-
                   predict(BT_algo, n.iter = 1000))
  expect_equal(pred_trainBT1000, unname(unlist(predTrain[[200]])))

  # Expect equal if two predict requested.
  expect_equal(matrix(c(unname(
    unlist(predTrain[[20]])
  ), unname(
    unlist(predTrain[[100]])
  )), ncol = 2),
  predict(BT_algo, n.iter = c(20, 100)))

  # Check if ok with descending order and multiple values.
  expect_warning(pred_trainBT_250n100n20 <-
                   predict(BT_algo, n.iter = c(250, 100, 20)))
  expect_equal(matrix(c(
    unname(unlist(predTrain[[200]])), unname(unlist(predTrain[[100]])),
    unname(unlist(predTrain[[20]]))
  ), ncol = 3), pred_trainBT_250n100n20)

  # Check for the individual fit.
  expect_equal(predict(BT_algo, n.iter = 20, single.iter = T), log(unname(unlist(
    predict(BT_algo$BTIndivFits[[20]], newdata = trainSet, type = 'vector')
  ))))

  expect_warning(pred_trainBT_250n20_singleIter <-
                   predict(BT_algo, n.iter = c(250, 20), single.iter = T))
  expect_equal(pred_trainBT_250n20_singleIter, matrix(c(log(unname(
    unlist(
      predict(BT_algo$BTIndivFits[[200]], newdata = trainSet, type = 'vector')
    )
  )),
  log(unname(
    unlist(
      predict(BT_algo$BTIndivFits[[20]], newdata = trainSet, type = 'vector')
    )
  ))), ncol = 2))

})
