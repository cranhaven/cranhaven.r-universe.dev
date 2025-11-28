#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_CV_Errors and
#   BT_CV_Predict functions.
#
########################

testthat::test_that("Check the BT_CV_Errors and BT_CV_Predict functions", {
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

  # Create folds.
  trainFrac <- 0.6
  folds <-
    c(rep(1, n * trainFrac / 3),
      rep(2, n * trainFrac / 3),
      rep(3, n * trainFrac / 3))
  # Run BT algos to simulate the CV results.
  datasetReduce <- datasetFull[seq(1, trainFrac * n), ]
  firstOrder <-
    c(which(folds == 2), which(folds == 3), which(folds == 1))
  secondOrder <-
    c(which(folds == 1), which(folds == 3), which(folds == 2))
  thirdOrder <-
    c(which(folds == 1), which(folds == 2), which(folds == 3))

  BT_algo1 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetReduce[firstOrder, ],
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 2 / 3,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetReduce[firstOrder, "ExpoR"]
    )

  BT_algo2 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetReduce[secondOrder, ],
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 2 / 3,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetReduce[secondOrder, "ExpoR"]
    )

  BT_algo3 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetReduce[thirdOrder, ],
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 2 / 3,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetReduce[thirdOrder, "ExpoR"]
    )

  BT_CV_toCheck <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = trainFrac,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 1,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 3,
      folds.id = folds,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  cv_error_recomputed <-
    (1 / (n * trainFrac)) * (
      BT_algo1$BTErrors$validation.error * (n * trainFrac / 3) +
        BT_algo2$BTErrors$validation.error *
        (n * trainFrac / 3) +
        BT_algo3$BTErrors$validation.error *
        (n * trainFrac / 3)
    )
  min_cv_error_recomputed <- which.min(cv_error_recomputed)
  pred_BT_algo1 <-
    predict(BT_algo1,
            newdata = datasetReduce[which(folds == 1), ],
            n.iter = min_cv_error_recomputed,
            type = "link")
  pred_BT_algo2 <-
    predict(BT_algo2,
            newdata = datasetReduce[which(folds == 2), ],
            n.iter = min_cv_error_recomputed,
            type = "link")
  pred_BT_algo3 <-
    predict(BT_algo3,
            newdata = datasetReduce[which(folds == 3), ],
            n.iter = min_cv_error_recomputed,
            type = "link")
  pred_BT_recomputed <-
    c(pred_BT_algo1, pred_BT_algo2, pred_BT_algo3)

  expect_equal(cv_error_recomputed, BT_CV_toCheck$BTErrors$cv.error)
  expect_equal(pred_BT_recomputed, BT_CV_toCheck$cv.fitted)
})
