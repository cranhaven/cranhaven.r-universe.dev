#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_More function.
#
########################

testthat::test_that("BT_More checks - Inputs consistency", {
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
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 100,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  BT_algo <- do.call(BT, paramsBT)

  #BT_more <- function(BTFit_object, new.n.iter=100, is.verbose=FALSE)

  # Expect error if not BT_type object.
  expect_error(BT_more(BTFit_object = "Test"))
  expect_error(BT_more(BTFit_object = list("a" = 1, "b" = 2)))

  # Expect error if n.iter is not a natural number.
  n.iter <- 0
  expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <-
    c(3, 4)
  expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <-
    NULL
  expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <- NA
  expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <-
    "Text"
  expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <- F
  expect_error(BT_more(BT_algo, new.n.iter = n.iter))

  # Expect error if is.verbose is not a boolean
  is.verbose <-
    1
  expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <-
    0.4
  expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <-
    2.785
  expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <-
    c(3, 4)
  expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <-
    NULL
  expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <-
    NA
  expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <-
    "Text"
  expect_error(BT_more(BT_algo, is.verbose = is.verbose))

  # Expect error on non-allowed seed.
  seed <- NA
  expect_error(BT_more(BT_algo, seed = seed))
  seed <-
    "Text"
  expect_warning(expect_error(BT_more(BT_algo, seed = seed)))

  # Expect error if keep.data = F
  expect_error(BT_more(BT_algo))

  # Expect warning if cross-validation applied.
  paramsBT$keep.data <- T

  set.seed(4)
  paramsBT$cv.folds <- 3
  BT_algo <- do.call(BT, paramsBT)
  expect_warning(BT_more(BT_algo, new.n.iter = 100))

  set.seed(4)
  paramsBT$cv.folds <-
    3
  paramsBT$folds.id <-
    sample(seq(1, 3),
           size = nrow(datasetFull) * 0.8,
           replace = T)
  BT_algo <- do.call(BT, paramsBT)
  expect_warning(BT_more(BT_algo, new.n.iter = 100))

})

testthat::test_that("BT_More checks - Results", {
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
  paramsBT <-
    list(
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


  ########
  ### Full run up to 200 iterations.
  ########

  paramsBT_Full <- paramsBT
  paramsBT_Full$n.iter <- 200
  BT_algo_full <- do.call(BT, paramsBT_Full)


  ########
  ### Simple fit without cv from 1 to 100 and extend from 101 to 200 afterwards.
  ### Comparison with the full run directly from 1 to 200.
  ########

  BT_algo <- do.call(BT, paramsBT)
  # Extend initial run up to 200 iterations in total.
  BT_algo_contd <- BT_more(BT_algo, new.n.iter = 100)

  ### Tests if similar results.

  # Same lists of trees expected.
  expect_identical(class(BT_algo_contd$BTIndivFits),
                   class(BT_algo_full$BTIndivFits))
  for (iTree in seq(1, 200)) {
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$splits,
                 BT_algo_full$BTIndivFits[[iTree]]$splits)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$frame,
                 BT_algo_full$BTIndivFits[[iTree]]$frame)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$cptable,
                 BT_algo_full$BTIndivFits[[iTree]]$cptable)
  }

  # Same initial fit.
  expect_equal(class(BT_algo_contd$BTInit), class(BT_algo_full$BTInit))
  expect_equal(BT_algo_contd$BTInit$initFit, BT_algo_full$BTInit$initFit)
  expect_equal(BT_algo_contd$BTInit$training.error,
               BT_algo_full$BTInit$training.error)
  expect_equal(BT_algo_contd$BTInit$validation.error,
               BT_algo_full$BTInit$validation.error)

  # New number of iterations.
  expect_equal(BT_algo_contd$BTParams$n.iter, 200)

  # Same vectors of errors.
  expect_equal(BT_algo_contd$BTErrors$training.error,
               BT_algo_full$BTErrors$training.error)
  expect_equal(
    BT_algo_contd$BTErrors$validation.error,
    BT_algo_full$BTErrors$validation.error
  )

  # Same classes expected.
  expect_equal(class(BT_algo_contd), class(BT_algo_full))

  # Same Terms expected.
  expect_equal(BT_algo_contd$Terms, BT_algo_full$Terms)


  ########
  ### Should be similar even if we use cross-validation. The initial full fit is used only.
  ### Comparison with the full run directly from iteration 1 to 200.
  ########

  paramsBT$cv.folds <- 3
  BT_algo <- do.call(BT, paramsBT)
  expect_warning(BT_algo_contd_v2 <-
                   BT_more(BT_algo, new.n.iter = 100))

  ### Tests if similar results.

  # Same lists of trees expected.
  expect_identical(class(BT_algo_contd_v2$BTIndivFits),
                   class(BT_algo_full$BTIndivFits))
  for (iTree in seq(1, 200)) {
    expect_equal(BT_algo_contd_v2$BTIndivFits[[iTree]]$splits,
                 BT_algo_full$BTIndivFits[[iTree]]$splits)
    expect_equal(BT_algo_contd_v2$BTIndivFits[[iTree]]$frame,
                 BT_algo_full$BTIndivFits[[iTree]]$frame)
    expect_equal(BT_algo_contd_v2$BTIndivFits[[iTree]]$cptable,
                 BT_algo_full$BTIndivFits[[iTree]]$cptable)
  }

  # Same initial fit.
  expect_equal(class(BT_algo_contd_v2$BTInit),
               class(BT_algo_full$BTInit))
  expect_equal(BT_algo_contd_v2$BTInit$initFit,
               BT_algo_full$BTInit$initFit)
  expect_equal(BT_algo_contd_v2$BTInit$training.error,
               BT_algo_full$BTInit$training.error)
  expect_equal(
    BT_algo_contd_v2$BTInit$validation.error,
    BT_algo_full$BTInit$validation.error
  )

  # New number of iterations.
  expect_equal(BT_algo_contd_v2$BTParams$n.iter, 200)

  # Same vectors of errors.
  expect_equal(
    BT_algo_contd_v2$BTErrors$training.error,
    BT_algo_full$BTErrors$training.error
  )
  expect_equal(
    BT_algo_contd_v2$BTErrors$validation.error,
    BT_algo_full$BTErrors$validation.error
  )

  # Same classes expected.
  expect_equal(class(BT_algo_contd_v2), class(BT_algo_full))

  # Same Terms expected.
  expect_equal(BT_algo_contd_v2$Terms, BT_algo_full$Terms)

})

testthat::test_that("BT_More checks - Results - Seed stability", {
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
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 100,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = 2,
      keep.data = T,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR,
      seed = 4
    )

  BT_algo <- do.call(BT, paramsBT)

  ####
  # Expect all equal results if same seed.
  ####

  # Extend initial run up to 200 iterations in total.
  BT_algo_contd <- BT_more(BT_algo, new.n.iter = 100, seed = 25)
  # Should be equal to
  set.seed(25)
  BT_algo_contd_v2 <- BT_more(BT_algo, new.n.iter = 100)

  # Same lists of trees expected.
  expect_identical(class(BT_algo_contd$BTIndivFits),
                   class(BT_algo_contd_v2$BTIndivFits))
  for (iTree in seq(1, 200)) {
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$splits,
                 BT_algo_contd_v2$BTIndivFits[[iTree]]$splits)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$frame,
                 BT_algo_contd_v2$BTIndivFits[[iTree]]$frame)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$cptable,
                 BT_algo_contd_v2$BTIndivFits[[iTree]]$cptable)
  }

  # Same initial fit.
  expect_equal(class(BT_algo_contd$BTInit),
               class(BT_algo_contd_v2$BTInit))
  expect_equal(BT_algo_contd$BTInit$initFit,
               BT_algo_contd_v2$BTInit$initFit)
  expect_equal(BT_algo_contd$BTInit$training.error,
               BT_algo_contd_v2$BTInit$training.error)
  expect_equal(
    BT_algo_contd$BTInit$validation.error,
    BT_algo_contd_v2$BTInit$validation.error
  )

  # New number of iterations.
  expect_equal(BT_algo_contd$BTParams$n.iter, 200)

  # Same vectors of errors.
  expect_equal(
    BT_algo_contd$BTErrors$training.error,
    BT_algo_contd_v2$BTErrors$training.error
  )
  expect_equal(
    BT_algo_contd$BTErrors$validation.error,
    BT_algo_contd_v2$BTErrors$validation.error
  )
  expect_equal(
    BT_algo_contd$BTErrors$oob.improvement,
    BT_algo_contd_v2$BTErrors$oob.improvement
  )

  # Same classes expected.
  expect_equal(class(BT_algo_contd), class(BT_algo_contd_v2))

  # Same Terms expected.
  expect_equal(BT_algo_contd$Terms, BT_algo_contd_v2$Terms)

  # Expect seed value stored in initial call.
  expect_equal(BT_algo_contd$seed, 25)

  ####
  # Expect some different results if no seed set.
  ####

  BT_algo_contd_v3 <- BT_more(BT_algo, new.n.iter = 100)

  # Same list of tree up to 100.
  for (iTree in seq(1, 100)) {
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$splits,
                 BT_algo_contd_v3$BTIndivFits[[iTree]]$splits)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$frame,
                 BT_algo_contd_v3$BTIndivFits[[iTree]]$frame)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$cptable,
                 BT_algo_contd_v3$BTIndivFits[[iTree]]$cptable)
  }

  # Different list of tree from 101.
  for (iTree in seq(101, 200)) {
    expect_false(isTRUE(
      all.equal(
        BT_algo_contd$BTIndivFits[[iTree]]$splits,
        BT_algo_contd_v3$BTIndivFits[[iTree]]$splits
      )
    ))
    expect_false(isTRUE(
      all.equal(
        BT_algo_contd$BTIndivFits[[iTree]]$frame,
        BT_algo_contd_v3$BTIndivFits[[iTree]]$frame
      )
    ))
    expect_false(isTRUE(
      all.equal(
        BT_algo_contd$BTIndivFits[[iTree]]$cptable,
        BT_algo_contd_v3$BTIndivFits[[iTree]]$cptable
      )
    ))
  }

  # Same initial fit.
  expect_equal(class(BT_algo_contd$BTInit),
               class(BT_algo_contd_v3$BTInit))
  expect_equal(BT_algo_contd$BTInit$initFit,
               BT_algo_contd_v3$BTInit$initFit)
  expect_equal(BT_algo_contd$BTInit$training.error,
               BT_algo_contd_v3$BTInit$training.error)
  expect_equal(
    BT_algo_contd$BTInit$validation.error,
    BT_algo_contd_v3$BTInit$validation.error
  )

  # Different vectors of errors.
  expect_false(isTRUE(
    all.equal(
      BT_algo_contd$BTErrors$training.error,
      BT_algo_contd_v3$BTErrors$training.error
    )
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo_contd$BTErrors$validation.error,
      BT_algo_contd_v3$BTErrors$validation.error
    )
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo_contd$BTErrors$oob.improvement,
      BT_algo_contd_v3$BTErrors$oob.improvement
    )
  ))

  # Same classes expected.
  expect_equal(class(BT_algo_contd), class(BT_algo_contd_v3))

  # Same Terms expected.
  expect_equal(BT_algo_contd$Terms, BT_algo_contd_v3$Terms)

  # Expect null seed for BT_algo_contd_v3.
  expect_true(is.null(BT_algo_contd_v3$seed))

  ####
  # Expect some different results if different seed set.
  ####

  BT_algo_contd_v4 <- BT_more(BT_algo, new.n.iter = 100, seed = 92)

  # Same list of tree up to 100.
  for (iTree in seq(1, 100)) {
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$splits,
                 BT_algo_contd_v4$BTIndivFits[[iTree]]$splits)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$frame,
                 BT_algo_contd_v4$BTIndivFits[[iTree]]$frame)
    expect_equal(BT_algo_contd$BTIndivFits[[iTree]]$cptable,
                 BT_algo_contd_v4$BTIndivFits[[iTree]]$cptable)
  }

  # Different list of tree from 101.
  for (iTree in seq(101, 200)) {
    expect_false(isTRUE(
      all.equal(
        BT_algo_contd$BTIndivFits[[iTree]]$splits,
        BT_algo_contd_v4$BTIndivFits[[iTree]]$splits
      )
    ))
    expect_false(isTRUE(
      all.equal(
        BT_algo_contd$BTIndivFits[[iTree]]$frame,
        BT_algo_contd_v4$BTIndivFits[[iTree]]$frame
      )
    ))
    expect_false(isTRUE(
      all.equal(
        BT_algo_contd$BTIndivFits[[iTree]]$cptable,
        BT_algo_contd_v4$BTIndivFits[[iTree]]$cptable
      )
    ))
  }

  # Same initial fit.
  expect_equal(class(BT_algo_contd$BTInit),
               class(BT_algo_contd_v4$BTInit))
  expect_equal(BT_algo_contd$BTInit$initFit,
               BT_algo_contd_v4$BTInit$initFit)
  expect_equal(BT_algo_contd$BTInit$training.error,
               BT_algo_contd_v4$BTInit$training.error)
  expect_equal(
    BT_algo_contd$BTInit$validation.error,
    BT_algo_contd_v4$BTInit$validation.error
  )

  # Different vectors of errors.
  expect_false(isTRUE(
    all.equal(
      BT_algo_contd$BTErrors$training.error,
      BT_algo_contd_v4$BTErrors$training.error
    )
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo_contd$BTErrors$validation.error,
      BT_algo_contd_v4$BTErrors$validation.error
    )
  ))
  expect_false(isTRUE(
    all.equal(
      BT_algo_contd$BTErrors$oob.improvement,
      BT_algo_contd_v4$BTErrors$oob.improvement
    )
  ))

  # Same classes expected.
  expect_equal(class(BT_algo_contd), class(BT_algo_contd_v4))

  # Same Terms expected.
  expect_equal(BT_algo_contd$Terms, BT_algo_contd_v4$Terms)

  # Expect defined seed for BT_algo_contd_v4.
  expect_equal(BT_algo_contd_v4$seed, 92)

})
