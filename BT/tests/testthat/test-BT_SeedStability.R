#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the results
#   stability using seed.
#
########################

testthat::test_that("Stability in results with seed - Without CV", {
  skip_on_cran()
  # Create dataset.
  set.seed(444)
  n <- 10000 #100000 # size of training set (number of observations)

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

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)

  # First BT algo run.
  set.seed(44)
  BT_algo1 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = ExpoR
    )

  # Second BT algo run.
  BT_algo2 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = ExpoR,
      seed = 44
    )

  # Third BT algo run - Different seed : Expect different results.
  BT_algo3 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = ExpoR,
      seed = 25
    )

  # Check results 1-2
  expect_equal(
    BT::BT_perf(BT_algo1, method = "OOB", plot.it = F),
    BT::BT_perf(BT_algo2, method = "OOB", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "validation", plot.it = F),
    BT::BT_perf(BT_algo2, method = "validation", plot.it = F)
  )

  expect_equal(BT_algo1$BTErrors$training.error,
               BT_algo2$BTErrors$training.error)
  expect_equal(BT_algo1$BTErrors$validation.error,
               BT_algo2$BTErrors$validation.error)
  expect_equal(BT_algo1$BTErrors$oob.improvement,
               BT_algo2$BTErrors$oob.improvement)

  expect_equal(BT_algo1$fitted.values, BT_algo2$fitted.values)

  # Check results 1-3 - Expect different.
  expect_false(isTRUE(
    all.equal(
      BT_algo1$BTErrors$training.error,
      BT_algo3$BTErrors$training.error
    )
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo1$BTErrors$validation.error,
      BT_algo3$BTErrors$validation.error
    )
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo1$BTErrors$oob.improvement,
      BT_algo3$BTErrors$oob.improvement
    )
  ))

  expect_false(isTRUE(all.equal(
    BT_algo1$fitted.values, BT_algo3$fitted.values
  )))

})

testthat::test_that("Stability in results with seed - With CV", {
  skip_on_cran()

  # Create dataset.
  set.seed(444)
  n <- 10000#100000 # size of training set (number of observations)

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

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)

  # First BT algo run.
  BT_algo1 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 4,
      folds.id = NULL,
      n.cores = 1,
      weights = ExpoR,
      seed = 4
    )

  # Second BT algo run.
  Sys.sleep(5)
  BT_algo2 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = T,
      is.verbose = F,
      cv.folds = 4,
      folds.id = NULL,
      n.cores = 1,
      weights = ExpoR,
      seed = 4
    )

  # Third BT algo run - multiple cores.
  Sys.sleep(5)
  BT_algo3 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = T,
      is.verbose = F,
      cv.folds = 4,
      folds.id = NULL,
      n.cores = 2,
      # 4 (originally) is working - but to use devtools::check(), CRAN allows max 2 cores...
      weights = ExpoR,
      seed = 4
    )

  # Fourth BT algo run - multiple cores.
  Sys.sleep(5)
  BT_algo4 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 4,
      folds.id = NULL,
      n.cores = 2,
      # 6 (originally) is working - but to use devtools::check(), CRAN allows max 2 cores...
      weights = ExpoR,
      seed = 4
    )

  # Fifth BT algo run - Seed defined outside -> should obtain same results except CV.
  Sys.sleep(5)
  set.seed(4)
  BT_algo5 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 4,
      folds.id = NULL,
      n.cores = 1,
      weights = ExpoR
    )

  # Sixth BT algo run - Different seed : Expect different results.
  Sys.sleep(5)
  BT_algo6 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = T,
      is.verbose = F,
      cv.folds = 4,
      folds.id = NULL,
      n.cores = 1,
      weights = ExpoR,
      seed = 895
    )

  # Seventh BT algo run - Should be similar to 6 !
  Sys.sleep(5)
  BT_algo7 <-
    BT(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 3,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = F,
      is.verbose = F,
      cv.folds = 4,
      folds.id = NULL,
      n.cores = 2,
      # 5 (originally) is working - but to use devtools::check(), CRAN allows max 2 cores...
      weights = ExpoR,
      seed = 895
    )

  # Check results 1-2
  expect_equal(BT_algo1$folds, BT_algo2$folds)
  expect_equal(
    BT::BT_perf(BT_algo1, method = "cv", plot.it = F),
    BT::BT_perf(BT_algo2, method = "cv", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "OOB", plot.it = F),
    BT::BT_perf(BT_algo2, method = "OOB", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "validation", plot.it = F),
    BT::BT_perf(BT_algo2, method = "validation", plot.it = F)
  )

  expect_equal(BT_algo1$BTErrors$training.error,
               BT_algo2$BTErrors$training.error)
  expect_equal(BT_algo1$BTErrors$validation.error,
               BT_algo2$BTErrors$validation.error)
  expect_equal(BT_algo1$BTErrors$cv.error, BT_algo2$BTErrors$cv.error)
  expect_equal(BT_algo1$BTErrors$oob.improvement,
               BT_algo2$BTErrors$oob.improvement)

  expect_equal(BT_algo1$cv.fitted, BT_algo2$cv.fitted)
  expect_equal(BT_algo1$fitted.values, BT_algo2$fitted.values)

  # Check results 1-3
  expect_equal(BT_algo1$folds, BT_algo3$folds)
  expect_equal(
    BT::BT_perf(BT_algo1, method = "cv", plot.it = F),
    BT::BT_perf(BT_algo3, method = "cv", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "OOB", plot.it = F),
    BT::BT_perf(BT_algo3, method = "OOB", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "validation", plot.it = F),
    BT::BT_perf(BT_algo3, method = "validation", plot.it = F)
  )

  expect_equal(BT_algo1$BTErrors$training.error,
               BT_algo3$BTErrors$training.error)
  expect_equal(BT_algo1$BTErrors$validation.error,
               BT_algo3$BTErrors$validation.error)
  expect_equal(BT_algo1$BTErrors$cv.error, BT_algo3$BTErrors$cv.error)
  expect_equal(BT_algo1$BTErrors$oob.improvement,
               BT_algo3$BTErrors$oob.improvement)

  expect_equal(BT_algo1$cv.fitted, BT_algo3$cv.fitted)
  expect_equal(BT_algo1$fitted.values, BT_algo3$fitted.values)

  # Check results 1-4
  expect_equal(BT_algo1$folds, BT_algo4$folds)
  expect_equal(
    BT::BT_perf(BT_algo1, method = "cv", plot.it = F),
    BT::BT_perf(BT_algo4, method = "cv", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "OOB", plot.it = F),
    BT::BT_perf(BT_algo4, method = "OOB", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "validation", plot.it = F),
    BT::BT_perf(BT_algo4, method = "validation", plot.it = F)
  )

  expect_equal(BT_algo1$BTErrors$training.error,
               BT_algo4$BTErrors$training.error)
  expect_equal(BT_algo1$BTErrors$validation.error,
               BT_algo4$BTErrors$validation.error)
  expect_equal(BT_algo1$BTErrors$cv.error, BT_algo4$BTErrors$cv.error)
  expect_equal(BT_algo1$BTErrors$oob.improvement,
               BT_algo4$BTErrors$oob.improvement)

  expect_equal(BT_algo1$cv.fitted, BT_algo4$cv.fitted)
  expect_equal(BT_algo1$fitted.values, BT_algo4$fitted.values)

  # Check results 1-5
  expect_equal(
    BT::BT_perf(BT_algo1, method = "OOB", plot.it = F),
    BT::BT_perf(BT_algo5, method = "OOB", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo1, method = "validation", plot.it = F),
    BT::BT_perf(BT_algo5, method = "validation", plot.it = F)
  )

  expect_equal(BT_algo1$BTErrors$training.error,
               BT_algo5$BTErrors$training.error)
  expect_equal(BT_algo1$BTErrors$validation.error,
               BT_algo5$BTErrors$validation.error)
  expect_equal(BT_algo1$BTErrors$oob.improvement,
               BT_algo5$BTErrors$oob.improvement)

  expect_equal(BT_algo1$fitted.values, BT_algo5$fitted.values)

  # Check results 1-6
  expect_false(isTRUE(all.equal(BT_algo1$folds, BT_algo6$folds)))

  expect_false(isTRUE(
    all.equal(
      BT_algo1$BTErrors$training.error,
      BT_algo6$BTErrors$training.error
    )
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo1$BTErrors$validation.error,
      BT_algo6$BTErrors$validation.error
    )
  ))
  expect_false(isTRUE(
    all.equal(BT_algo1$BTErrors$cv.error, BT_algo6$BTErrors$cv.error)
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo1$BTErrors$oob.improvement,
      BT_algo6$BTErrors$oob.improvement
    )
  ))

  expect_false(isTRUE(all.equal(
    BT_algo1$cv.fitted, BT_algo6$cv.fitted
  )))
  expect_false(isTRUE(all.equal(
    BT_algo1$fitted.values, BT_algo6$fitted.values
  )))

  # Check results 6-7
  expect_equal(BT_algo6$folds, BT_algo7$folds)
  expect_equal(
    BT::BT_perf(BT_algo6, method = "cv", plot.it = F),
    BT::BT_perf(BT_algo7, method = "cv", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo6, method = "OOB", plot.it = F),
    BT::BT_perf(BT_algo7, method = "OOB", plot.it = F)
  )
  expect_equal(
    BT::BT_perf(BT_algo6, method = "validation", plot.it = F),
    BT::BT_perf(BT_algo7, method = "validation", plot.it = F)
  )

  expect_equal(BT_algo6$BTErrors$training.error,
               BT_algo7$BTErrors$training.error)
  expect_equal(BT_algo6$BTErrors$validation.error,
               BT_algo7$BTErrors$validation.error)
  expect_equal(BT_algo6$BTErrors$cv.error, BT_algo7$BTErrors$cv.error)
  expect_equal(BT_algo6$BTErrors$oob.improvement,
               BT_algo7$BTErrors$oob.improvement)

  expect_equal(BT_algo6$cv.fitted, BT_algo7$cv.fitted)
  expect_equal(BT_algo6$fitted.values, BT_algo7$fitted.values)

})
