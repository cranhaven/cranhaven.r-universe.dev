# Basic raking tests
test_that('rake works with simple two-variable example', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 200, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(c('young', 'old'), 200, replace = TRUE, prob = c(0.7, 0.3))
  )
  targets <- list(
    gender = c(M = 0.5, F = 0.5),
    age = c(young = 0.6, old = 0.4)
  )

  result <- rake(data, targets)

  expect_s3_class(result, 'ipf_rake')
  expect_true(result$converged)
  expect_equal(length(result$weights), 200)
  expect_equal(mean(result$weights), 1, tolerance = 1e-6)

  # Check that weighted proportions match targets
  w <- result$weights
  wpct_m <- sum(w[data$gender == 'M']) / sum(w)
  wpct_f <- sum(w[data$gender == 'F']) / sum(w)
  expect_equal(wpct_m, 0.5, tolerance = 1e-4)
  expect_equal(wpct_f, 0.5, tolerance = 1e-4)

  wpct_young <- sum(w[data$age == 'young']) / sum(w)
  wpct_old <- sum(w[data$age == 'old']) / sum(w)
  expect_equal(wpct_young, 0.6, tolerance = 1e-4)
  expect_equal(wpct_old, 0.4, tolerance = 1e-4)
})

test_that('rake works with single variable', {
  set.seed(1)
  data <- data.frame(
    x = sample(c('a', 'b', 'c'), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )
  targets <- list(x = c(a = 1 / 3, b = 1 / 3, c = 1 / 3))
  result <- rake(data, targets)

  expect_true(result$converged)
  w <- result$weights
  for (lvl in c('a', 'b', 'c')) {
    expect_equal(sum(w[data$x == lvl]) / sum(w), 1 / 3, tolerance = 1e-4)
  }
})

test_that('rake works with factors', {
  data <- data.frame(
    color = factor(
      c('red', 'blue', 'green', 'red', 'blue', 'green', 'red', 'red'),
      levels = c('red', 'blue', 'green')
    )
  )
  targets <- list(color = c(red = 0.4, blue = 0.3, green = 0.3))
  result <- rake(data, targets)

  expect_true(result$converged)
  w <- result$weights
  expect_equal(sum(w[data$color == 'red']) / sum(w), 0.4, tolerance = 1e-4)
})

test_that('rake works with logical variables', {
  set.seed(10)
  data <- data.frame(
    voted = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.7, 0.3))
  )
  targets <- list(voted = c('TRUE' = 0.6, 'FALSE' = 0.4))
  result <- rake(data, targets)

  expect_true(result$converged)
  w <- result$weights
  expect_equal(sum(w[data$voted]) / sum(w), 0.6, tolerance = 1e-4)
})

test_that('rake works with numeric (integer) variables', {
  set.seed(5)
  data <- data.frame(
    region = sample(1:4, 100, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
  )
  targets <- list(region = c('1' = 0.25, '2' = 0.25, '3' = 0.25, '4' = 0.25))
  result <- rake(data, targets)

  expect_true(result$converged)
})
