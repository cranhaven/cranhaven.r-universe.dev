#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_Data_Split functions.
#
########################

testthat::test_that("Create_validation_set function", {
  # Create datasets.
  set.seed(4)
  n <- 100000

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

  # Expect error if dataset is empty.
  expect_error(.create_validation_set(NULL, train.fraction = 0.1))

  # Expect NULL validation.set if train.fraction is 1.
  expect_equal(.create_validation_set(datasetFull, 1)$validation.set, NULL)
  expect_equal(.create_validation_set(datasetFull, 1)$training.set,
               datasetFull)

  # Check the results if everything is well defined.
  expect_equal(.create_validation_set(datasetFull, 0.5)$validation.set,
               datasetFull[seq(0.5 * nrow(datasetFull) + 1, nrow(datasetFull)), ])
  expect_equal(.create_validation_set(datasetFull, 0.5)$training.set,
               datasetFull[seq(1, 0.5 * nrow(datasetFull)), ])

  expect_equal(.create_validation_set(datasetFull, 0.25)$validation.set,
               datasetFull[seq(0.25 * nrow(datasetFull) + 1, nrow(datasetFull)), ])
  expect_equal(.create_validation_set(datasetFull, 0.25)$training.set,
               datasetFull[seq(1, 0.25 * nrow(datasetFull)), ])

  expect_equal(.create_validation_set(datasetFull, 0.891)$validation.set,
               datasetFull[seq(0.891 * nrow(datasetFull) + 1, nrow(datasetFull)), ])
  expect_equal(.create_validation_set(datasetFull, 0.891)$training.set,
               datasetFull[seq(1, 0.891 * nrow(datasetFull)), ])

})

testthat::test_that("create_cv_folds function", {
  # Create datasets.
  set.seed(4)
  n <- 100000

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

  # Note additionnal checks on the values of cv.folds/folds.id are performed in the test-BTInputs.R code.

  # Expect error if everything empty
  expect_error(.create_cv_folds(
    data = NULL,
    cv.folds = NULL,
    folds.id = NULL
  ))
  # Expect error if dataset is empty
  expect_error(.create_cv_folds(
    data = NULL,
    cv.folds = 2,
    folds.id = NULL
  ))
  # Expect error if cv.folds and folds.id are not provided -> should not be the case as cv.folds cannot be NULL
  expect_error(.create_cv_folds(
    data = datasetFull,
    cv.folds = NULL,
    folds.id = NULL
  ))
  # Expect error if cv.folds defined but not the good length.
  expect_error(.create_cv_folds(
    data = datasetFull,
    cv.folds = 3,
    folds.id = sample(
      seq(1, 3),
      size = nrow(datasetFull) / 2,
      replace = T
    )
  ))
  expect_error(.create_cv_folds(
    data = datasetFull,
    cv.folds = 3,
    folds.id = sample(
      seq(1, 3),
      size = nrow(datasetFull) * 0.8,
      replace = T
    )
  ))


  set.seed(4)
  foldsExample <-
    sample(seq(1, 3), size = nrow(datasetFull), replace = T)
  expect_equal(
    .create_cv_folds(
      data = datasetFull,
      cv.folds = 3,
      folds.id = foldsExample
    ),
    foldsExample
  )

  set.seed(4)
  foldsExample <-
    sample(seq(1, 5), size = nrow(datasetFull), replace = T)
  expect_equal(
    .create_cv_folds(
      data = datasetFull,
      cv.folds = 5,
      folds.id = foldsExample
    ),
    foldsExample
  )

  set.seed(4)
  # Should not be the case as it's transform to numeric. However, this function should work in this case as well.
  foldsExample <-
    sample(c("A", "B", "C"),
           size = nrow(datasetFull),
           replace = T)
  expect_equal(
    .create_cv_folds(
      data = datasetFull,
      cv.folds = 3,
      folds.id = foldsExample
    ),
    foldsExample
  )

  set.seed(4)
  # Should not be the case as it's transform to numeric. However, this function should work in this case as well.
  foldsExample <-
    as.numeric(as.factor(sample(
      c("A", "B", "C", "D"),
      size = nrow(datasetFull),
      replace = T
    )))
  expect_equal(
    .create_cv_folds(
      data = datasetFull,
      cv.folds = 4,
      folds.id = foldsExample
    ),
    foldsExample
  )

  set.seed(4)
  # Should not be the case as it's transform to numeric. However, this function should work in this case as well.
  foldsExample <-
    as.numeric(as.factor(sample(
      c("A", "B", "C", "D", 1, 2),
      size = nrow(datasetFull),
      replace = T
    )))
  expect_equal(
    .create_cv_folds(
      data = datasetFull,
      cv.folds = 6,
      folds.id = foldsExample
    ),
    foldsExample
  )

})
