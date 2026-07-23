# Tests for na_method = 'exclude' (anesrake-style NA handling)

# Helper: weighted proportion among non-NA cases for a variable level
wpct_nonNA <- function(weights, x, level) {
  keep <- !is.na(x) & x == level
  sum(weights[keep]) / sum(weights[!is.na(x)])
}

test_that("na_method = 'exclude' converges on data with no NAs", {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 200, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(c('young', 'old'), 200, replace = TRUE, prob = c(0.7, 0.3))
  )
  targets <- list(
    gender = c(M = 0.5, F = 0.5),
    age = c(young = 0.6, old = 0.4)
  )
  result <- rake(data, targets, na_method = 'exclude', cap = NULL)

  expect_true(result$converged)
  expect_equal(mean(result$weights), 1, tolerance = 1e-6)
  expect_equal(
    wpct_nonNA(result$weights, data$gender, 'M'),
    0.5,
    tolerance = 1e-4
  )
  expect_equal(
    wpct_nonNA(result$weights, data$age, 'young'),
    0.6,
    tolerance = 1e-4
  )
})

test_that("na_method = 'exclude' converges on data with no NAs", {
  set.seed(7)
  data <- data.frame(
    x = sample(c('a', 'b'), 100, replace = TRUE),
    y = sample(c('p', 'q'), 100, replace = TRUE)
  )
  targets <- list(x = c(a = 0.4, b = 0.6), y = c(p = 0.3, q = 0.7))

  result <- rake(data, targets, na_method = 'exclude', cap = NULL)

  expect_true(result$converged)
  expect_equal(result$na_method, 'exclude')
})

test_that("na_method = 'exclude' achieves target proportions among non-NA cases", {
  set.seed(1)
  n <- 300
  x <- sample(
    c('A', 'B', 'C', NA),
    n,
    replace = TRUE,
    prob = c(0.3, 0.3, 0.2, 0.2)
  )
  data <- data.frame(x = x)
  targets <- list(x = c(A = 1 / 3, B = 1 / 3, C = 1 / 3))

  result <- rake(data, targets, na_method = 'exclude', cap = NULL)

  expect_true(result$converged)
  w <- result$weights
  for (lvl in c('A', 'B', 'C')) {
    expect_equal(wpct_nonNA(w, data$x, lvl), 1 / 3, tolerance = 1e-4)
  }
})

test_that("na_method = 'exclude' does not update weights of NA cases for that variable", {
  # With a single variable and 'exclude', NA cases are never touched.
  # Their weights should remain at 1 (base weight) since only that one variable
  # exists and NA cases have no margin to be adjusted by.
  data <- data.frame(x = c('A', 'B', 'A', NA, 'B', NA, 'A', 'B'))
  targets <- list(x = c(A = 0.5, B = 0.5))

  result <- rake(data, targets, na_method = 'exclude', cap = NULL)

  na_idx <- which(is.na(data$x))
  expect_equal(result$weights[na_idx], c(1, 1), tolerance = 1e-8)
})

test_that("na_method = 'exclude' gives different results from 'bucket' when NAs present", {
  set.seed(99)
  n <- 500
  data <- data.frame(
    sex = sample(
      c('M', 'F', NA),
      n,
      replace = TRUE,
      prob = c(0.45, 0.45, 0.10)
    ),
    income = sample(
      c('lo', 'hi', NA),
      n,
      replace = TRUE,
      prob = c(0.4, 0.4, 0.20)
    )
  )
  targets <- list(
    sex = c(M = 0.5, F = 0.5),
    income = c(lo = 0.4, hi = 0.6)
  )

  r_bucket <- rake(data, targets, na_method = 'bucket', cap = NULL)
  r_exclude <- rake(data, targets, na_method = 'exclude', cap = NULL)

  # Weights should differ when there are NAs
  expect_false(isTRUE(all.equal(r_bucket$weights, r_exclude$weights)))
})

test_that("na_method = 'exclude' two-variable: non-NA proportions match targets", {
  set.seed(42)
  n <- 400
  data <- data.frame(
    sex = sample(
      c('M', 'F', NA),
      n,
      replace = TRUE,
      prob = c(0.44, 0.44, 0.12)
    ),
    region = sample(
      c('N', 'S', 'E', NA),
      n,
      replace = TRUE,
      prob = c(0.3, 0.3, 0.3, 0.1)
    )
  )
  targets <- list(
    sex = c(M = 0.48, F = 0.52),
    region = c(N = 1 / 3, S = 1 / 3, E = 1 / 3)
  )

  result <- rake(data, targets, na_method = 'exclude', cap = NULL)

  expect_true(result$converged)
  w <- result$weights

  expect_equal(wpct_nonNA(w, data$sex, 'M'), 0.48, tolerance = 1e-4)
  expect_equal(wpct_nonNA(w, data$sex, 'F'), 0.52, tolerance = 1e-4)
  for (lvl in c('N', 'S', 'E')) {
    expect_equal(wpct_nonNA(w, data$region, lvl), 1 / 3, tolerance = 1e-4)
  }
})

test_that("na_method stored correctly on result object", {
  data <- data.frame(x = c('a', 'b', NA, 'a', 'b'))
  targets <- list(x = c(a = 0.5, b = 0.5))
  result <- rake(data, targets, na_method = 'exclude')
  expect_equal(result$na_method, 'exclude')
})
