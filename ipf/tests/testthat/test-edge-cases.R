# Edge case tests
test_that('rake handles NA values in data', {
  set.seed(42)
  data <- data.frame(
    gender = c('M', 'F', 'M', NA, 'F', 'M', NA, 'F', 'M', 'F')
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  result <- rake(data, targets)

  expect_true(result$converged)
  # NAs are ignored; weighted proportions of non-NA should match
  w <- result$weights
  non_na <- !is.na(data$gender)
  wpct_m <- sum(w[data$gender == 'M' & non_na]) / sum(w[non_na])
  expect_equal(wpct_m, 0.5, tolerance = 1e-3)
})

test_that('rake can bucket NA values as an implicit missing category', {
  data <- data.frame(
    gender = c('M', 'F', 'M', NA, 'F', 'M', NA, 'F', 'M', 'F')
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))

  result <- rake(data, targets, na_method = 'bucket')

  expect_true(result$converged)

  w <- result$weights
  expect_equal(sum(w[is.na(data$gender)]) / sum(w), 0.2, tolerance = 1e-6)
  expect_equal(
    sum(w[!is.na(data$gender) & data$gender == 'M']) / sum(w),
    0.4,
    tolerance = 1e-6
  )
  expect_equal(
    sum(w[!is.na(data$gender) & data$gender == 'F']) / sum(w),
    0.4,
    tolerance = 1e-6
  )
})

test_that('rake handles many categories', {
  set.seed(42)
  k <- 10
  levels <- paste0('cat', seq_len(k))
  data <- data.frame(
    x = sample(levels, 500, replace = TRUE)
  )
  tgt <- rep(1 / k, k)
  names(tgt) <- levels
  targets <- list(x = tgt)

  result <- rake(data, targets)
  expect_true(result$converged)
})

test_that('rake preserves weight mean of 1', {
  set.seed(42)
  data <- data.frame(
    v1 = sample(c('a', 'b'), 100, replace = TRUE),
    v2 = sample(c('x', 'y'), 100, replace = TRUE)
  )
  targets <- list(
    v1 = c(a = 0.5, b = 0.5),
    v2 = c(x = 0.5, y = 0.5)
  )

  result <- rake(data, targets)
  expect_equal(mean(result$weights), 1, tolerance = 1e-6)
})

test_that('diagnostics_every captures per-iteration data', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.7, 0.3))
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))

  result <- rake(data, targets, diagnostics_every = 1L)
  expect_s3_class(result$diagnostics, 'tbl_df')
  expect_gt(nrow(result$diagnostics), 2) # At least baseline + 1 iteration
})
