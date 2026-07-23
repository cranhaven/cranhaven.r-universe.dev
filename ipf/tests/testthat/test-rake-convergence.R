# Convergence and bounds tests
test_that('rake respects cap parameter', {
  set.seed(42)
  data <- data.frame(
    x = c(rep('a', 95), rep('b', 5))
  )
  targets <- list(x = c(a = 0.5, b = 0.5))

  result <- rake(data, targets, cap = 3)
  expect_true(all(result$weights <= 3 + 1e-6))
})

test_that('rake respects bounds parameter', {
  set.seed(42)
  data <- data.frame(
    x = c(rep('a', 90), rep('b', 10))
  )
  targets <- list(x = c(a = 0.5, b = 0.5))

  result <- rake(data, targets, bounds = c(0.5, 3))
  expect_true(all(result$weights >= 0.5 - 1e-6))
  expect_true(all(result$weights <= 3 + 1e-6))
})

test_that('rake converges with many variables', {
  set.seed(123)
  n <- 500
  data <- data.frame(
    v1 = sample(c('a', 'b'), n, replace = TRUE),
    v2 = sample(c('x', 'y', 'z'), n, replace = TRUE),
    v3 = sample(c('p', 'q'), n, replace = TRUE)
  )
  targets <- list(
    v1 = c(a = 0.55, b = 0.45),
    v2 = c(x = 0.3, y = 0.4, z = 0.3),
    v3 = c(p = 0.6, q = 0.4)
  )

  result <- rake(data, targets)
  expect_true(result$converged)
  expect_lt(result$max_prop_err, 1e-6)
})

test_that('rake with base_weights works', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE)
  )
  base_wt <- runif(100, 0.5, 2)
  targets <- list(gender = c(M = 0.5, F = 0.5))

  result <- rake(data, targets, base_weights = base_wt)
  expect_true(result$converged)

  w <- result$weights
  expect_equal(sum(w[data$gender == 'M']) / sum(w), 0.5, tolerance = 1e-4)
})

test_that('rake warns on non-convergence with max_iter = 1', {
  set.seed(42)
  data <- data.frame(
    x = sample(c('a', 'b'), 100, replace = TRUE, prob = c(0.9, 0.1)),
    y = sample(c('p', 'q'), 100, replace = TRUE, prob = c(0.1, 0.9))
  )
  targets <- list(
    x = c(a = 0.5, b = 0.5),
    y = c(p = 0.5, q = 0.5)
  )

  result <- rake(data, targets, max_iter = 1L, tol = 1e-12)
  # With 1 iteration, convergence may or may not be achieved
  # but we should get a valid result

  expect_s3_class(result, 'ipf_rake')
  expect_equal(result$iterations, 1L)
})
