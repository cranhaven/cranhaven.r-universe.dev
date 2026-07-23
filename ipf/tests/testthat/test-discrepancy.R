# Discrepancy computation tests
test_that('find_discrepant_vars returns named numeric vector', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.7, 0.3))
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  weights <- rep(1, 100)

  disc <- find_discrepant_vars(data, targets, weights)
  expect_type(disc, 'double')
  expect_named(disc, 'gender')
  expect_gt(disc[['gender']], 0)
})

test_that('find_discrepant_vars works with all choosemethods', {
  set.seed(42)
  data <- data.frame(
    x = sample(c('a', 'b', 'c'), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )
  targets <- list(x = c(a = 1 / 3, b = 1 / 3, c = 1 / 3))
  weights <- rep(1, 100)

  methods <- c(
    'total',
    'max',
    'average',
    'totalsquared',
    'maxsquared',
    'averagesquared'
  )
  for (m in methods) {
    disc <- find_discrepant_vars(data, targets, weights, choosemethod = m)
    expect_type(disc, 'double')
    expect_true(disc[['x']] >= 0)
  }
})

test_that('discrepancy is near zero after successful raking', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 200, replace = TRUE, prob = c(0.6, 0.4))
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))

  result <- rake(data, targets)
  disc <- find_discrepant_vars(data, targets, result$weights)
  expect_lt(disc[['gender']], 1e-4)
})
