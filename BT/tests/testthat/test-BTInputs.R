#########################
# Author : Gireg Willame
# May 2022.
#
# Series of tests to check the initial inputs of the BT algorithm.
#
########################

# Start tests input parameters.
testthat::test_that("Error thrown if no inputs defined at all", {
  expect_error(BT())
})

testthat::test_that("Invalid tweedie power checks", {
  tweedie.power <-
    0.5
  expect_error(.check_tweedie_power(tweedie.power))
  tweedie.power <-
    c(1, 2, 3)
  expect_error(.check_tweedie_power(tweedie.power))
  tweedie.power <-
    NULL
  expect_error(.check_tweedie_power(tweedie.power))
  tweedie.power <-
    NA
  expect_error(.check_tweedie_power(tweedie.power))
  tweedie.power <-
    "Text"
  expect_error(.check_tweedie_power(tweedie.power))

  tweedie.power <-
    1
  expect_silent(.check_tweedie_power(tweedie.power))
  tweedie.power <-
    0
  expect_silent(.check_tweedie_power(tweedie.power))
  tweedie.power <-
    1.5
  expect_silent(.check_tweedie_power(tweedie.power))
  tweedie.power <-
    2
  expect_silent(.check_tweedie_power(tweedie.power))

})

testthat::test_that("Invalid ABT checks", {
  ABT <- 1
  expect_error(.check_ABT(ABT))
  ABT <- 0.4
  expect_error(.check_ABT(ABT))
  ABT <- 2.785
  expect_error(.check_ABT(ABT))
  ABT <- c(3, 4)
  expect_error(.check_ABT(ABT))
  ABT <- NULL
  expect_error(.check_ABT(ABT))
  ABT <- NA
  expect_error(.check_ABT(ABT))
  ABT <- "Text"
  expect_error(.check_ABT(ABT))

  ABT <- T
  expect_silent(.check_ABT(ABT))
  ABT <- F
  expect_silent(.check_ABT(ABT))

})

testthat::test_that("Invalid n.iter checks", {
  n.iter <- 0
  expect_error(.check_n_iter(n.iter))
  n.iter <- c(3, 4)
  expect_error(.check_n_iter(n.iter))
  n.iter <- NULL
  expect_error(.check_n_iter(n.iter))
  n.iter <- NA
  expect_error(.check_n_iter(n.iter))
  n.iter <- "Text"
  expect_error(.check_n_iter(n.iter))
  n.iter <- F
  expect_error(.check_n_iter(n.iter))

  n.iter <-
    T
  expect_silent(.check_n_iter(n.iter)) # TRUE == 1 -> Check if this is working through the entire program.
  n.iter <- 20
  expect_silent(.check_n_iter(n.iter))
  n.iter <- 1000
  expect_silent(.check_n_iter(n.iter))

})

testthat::test_that("Invalid train.fraction checks", {
  train.fraction <-
    0
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    1.5
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    2
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    c(4, 5)
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    c(0.5, 0.8)
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    NULL
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    NA
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    "Text"
  expect_error(.check_train_fraction(train.fraction))
  train.fraction <-
    F
  expect_error(.check_train_fraction(train.fraction))

  train.fraction <-
    T
  expect_silent(.check_train_fraction(train.fraction)) # TRUE == 1 -> Check if this is working through the entire program.
  train.fraction <-
    1
  expect_silent(.check_train_fraction(train.fraction))
  train.fraction <-
    0.4
  expect_silent(.check_train_fraction(train.fraction))
  train.fraction <-
    0.7598
  expect_silent(.check_train_fraction(train.fraction))

})

testthat::test_that("Invalid interaction.depth checks", {
  interaction.depth <-
    0
  expect_error(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    c(1, 2.5)
  expect_error(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    1.5
  expect_error(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    2.5585
  expect_error(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    NA
  expect_error(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    "Text"
  expect_error(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    F
  expect_error(.check_interaction_depth(interaction.depth))

  interaction.depth <-
    NULL
  expect_silent(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    T
  expect_silent(.check_interaction_depth(interaction.depth)) # TRUE == 1 -> Check if this is working through the entire program.
  interaction.depth <-
    1
  expect_silent(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    20
  expect_silent(.check_interaction_depth(interaction.depth))
  interaction.depth <-
    200
  expect_silent(.check_interaction_depth(interaction.depth))

})

testthat::test_that("Invalid shrinkage checks", {
  shrinkage <- 0
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <- 1.5
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <- 2
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <-
    c(0.4, 0.82)
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <- c(2, 3)
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <- NULL
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <- NA
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <- "Text"
  expect_error(.check_shrinkage(shrinkage))
  shrinkage <- F
  expect_error(.check_shrinkage(shrinkage))

  shrinkage <-
    T
  expect_silent(.check_shrinkage(shrinkage)) # TRUE == 1 -> Check if this is working through the entire program.
  shrinkage <- 1
  expect_silent(.check_shrinkage(shrinkage))
  shrinkage <- 0.4
  expect_silent(.check_shrinkage(shrinkage))
  shrinkage <- 0.7598
  expect_silent(.check_shrinkage(shrinkage))

})

testthat::test_that("Invalid bag.fraction checks", {
  bag.fraction <- 0
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    1.5
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <- 2
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    c(0.4, 0.82)
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    c(2, 3)
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    NULL
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    NA
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    "Text"
  expect_error(.check_bag_fraction(bag.fraction))
  bag.fraction <- F
  expect_error(.check_bag_fraction(bag.fraction))

  bag.fraction <-
    T
  expect_silent(.check_bag_fraction(bag.fraction)) # TRUE == 1 -> Check if this is working through the entire program.
  bag.fraction <-
    1
  expect_silent(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    0.4
  expect_silent(.check_bag_fraction(bag.fraction))
  bag.fraction <-
    0.7598
  expect_silent(.check_bag_fraction(bag.fraction))

})

testthat::test_that("Invalid colsample.bytree checks", {
  colsample.bytree <-
    0
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    1.5
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    5.7595
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    c(2, 3)
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    c(1.4, 5.759)
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    NA
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    "Text"
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    F
  numExplVar = 10
  expect_error(.check_colsample_bytree(colsample.bytree, numExplVar))

  colsample.bytree <-
    NULL
  numExplVar = 10
  expect_silent(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    T
  numExplVar = 10
  expect_silent(.check_colsample_bytree(colsample.bytree, numExplVar)) # TRUE == 1 -> Check if this is working through the entire program.
  colsample.bytree <-
    1
  numExplVar = 10
  expect_silent(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    5
  numExplVar = 10
  expect_silent(.check_colsample_bytree(colsample.bytree, numExplVar))
  colsample.bytree <-
    10
  numExplVar = 10
  expect_silent(.check_colsample_bytree(colsample.bytree, numExplVar))

})

testthat::test_that("Invalid keep.data checks", {
  keep.data <- 1
  expect_error(.check_keep_data(keep.data))
  keep.data <- 0.4
  expect_error(.check_keep_data(keep.data))
  keep.data <- 2.785
  expect_error(.check_keep_data(keep.data))
  keep.data <- c(3, 4)
  expect_error(.check_keep_data(keep.data))
  keep.data <- NULL
  expect_error(.check_keep_data(keep.data))
  keep.data <- NA
  expect_error(.check_keep_data(keep.data))
  keep.data <- "Text"
  expect_error(.check_keep_data(keep.data))

  keep.data <- T
  expect_silent(.check_keep_data(keep.data))
  keep.data <- F
  expect_silent(.check_keep_data(keep.data))

})

testthat::test_that("Invalid is.verbose checks", {
  is.verbose <- 1
  expect_error(.check_is_verbose(is.verbose))
  is.verbose <- 0.4
  expect_error(.check_is_verbose(is.verbose))
  is.verbose <- 2.785
  expect_error(.check_is_verbose(is.verbose))
  is.verbose <- c(3, 4)
  expect_error(.check_is_verbose(is.verbose))
  is.verbose <- NULL
  expect_error(.check_is_verbose(is.verbose))
  is.verbose <- NA
  expect_error(.check_is_verbose(is.verbose))
  is.verbose <- "Text"
  expect_error(.check_is_verbose(is.verbose))

  is.verbose <- T
  expect_silent(.check_is_verbose(is.verbose))
  is.verbose <- F
  expect_silent(.check_is_verbose(is.verbose))

})

testthat::test_that("Invalid cv.folds checks", {
  cv.folds <- 0
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- 0.4
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- 2.785
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- c(2, 3)
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- c(1.4, 5.759)
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- NULL
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- NA
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- "Text"
  expect_error(.check_cv_folds(cv.folds))
  cv.folds <- F
  expect_error(.check_cv_folds(cv.folds))

  cv.folds <-
    T
  expect_silent(.check_cv_folds(cv.folds)) # TRUE == 1 -> Check if this is working through the entire program.
  cv.folds <- 1
  expect_silent(.check_cv_folds(cv.folds))
  cv.folds <- 10
  expect_silent(.check_cv_folds(cv.folds))

})

testthat::test_that("Invalid folds.id checks", {
  # There are tests in the BT_Data_Split/create_cv_folds function as well to check that vectors have the same length.
  # It's done later as it's based on the training.set that is built during the BT algorithm first steps.
  # Therefore the first .check_folds_id check should be a bit lighter.

  folds.id <- NA
  expect_error(.check_folds_id(folds.id))
  folds.id <-
    c(1, 2, 3, NA)
  expect_error(.check_folds_id(folds.id))

  folds.id <- NULL
  expect_silent(.check_folds_id(folds.id))
  folds.id <- 1
  expect_silent(.check_folds_id(folds.id))
  folds.id <- seq(1, 10)
  expect_silent(.check_folds_id(folds.id))
  folds.id <- seq(1, 5)
  expect_silent(.check_folds_id(folds.id))
  folds.id <- rep("A", 5)
  expect_silent(.check_folds_id(folds.id))
})

testthat::test_that("Invalid n.cores checks", {
  skip_on_cran()

  n.cores <- 0
  expect_error(.check_n_cores(n.cores))
  n.cores <- 0.4
  expect_error(.check_n_cores(n.cores))
  n.cores <- 2.785
  expect_error(.check_n_cores(n.cores))
  n.cores <- c(2, 3)
  expect_error(.check_n_cores(n.cores))
  n.cores <- c(1.4, 5.759)
  expect_error(.check_n_cores(n.cores))
  n.cores <- NULL
  expect_error(.check_n_cores(n.cores))
  n.cores <- NA
  expect_error(.check_n_cores(n.cores))
  n.cores <- "Text"
  expect_error(.check_n_cores(n.cores))
  n.cores <- F
  expect_error(.check_n_cores(n.cores))
  n.cores <- parallel::detectCores() + 1
  expect_error(.check_n_cores(n.cores))

  n.cores <- parallel::detectCores()
  expect_warning(
    .check_n_cores(n.cores),
    "n.cores is equal to maximum available cores. System might become unresponsive and crash in case of insufficient memory."
  )

  n.cores <-
    T
  expect_silent(.check_n_cores(n.cores)) # TRUE == 1 -> Check if this is working through the entire program.
  n.cores <- 1
  expect_silent(.check_n_cores(n.cores))

})

testthat::test_that("Invalid weights checks", {
  weights <- 0
  expect_error(.check_weights(weights))
  weights <- -1
  expect_error(.check_weights(weights))
  weights <- c(0, -1, -2)
  expect_error(.check_weights(weights))
  weights <- c(-0.5, -2.7894)
  expect_error(.check_weights(weights))
  weights <- c(1, 2, -2)
  expect_error(.check_weights(weights))
  weights <- "Text"
  expect_error(.check_weights(weights))
  # weights <- NA ; expect_error(.check_weights(weights))
  weights <- F
  expect_error(.check_weights(weights))
  weights <- T
  expect_error(.check_weights(weights))

  weights <- 1
  expect_silent(.check_weights(weights))
  weights <- c(1, 2, 3)
  expect_silent(.check_weights(weights))

})
