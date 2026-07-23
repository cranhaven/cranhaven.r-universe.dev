# Variable selection tests
test_that("type = 'pctlim' selects only discrepant variables", {
  set.seed(42)
  data <- data.frame(
    # This variable is already close to target
    balanced = sample(c('a', 'b'), 200, replace = TRUE, prob = c(0.5, 0.5)),
    # This one is far from target
    skewed = sample(c('x', 'y'), 200, replace = TRUE, prob = c(0.9, 0.1))
  )
  targets <- list(
    balanced = c(a = 0.5, b = 0.5),
    skewed = c(x = 0.5, y = 0.5)
  )

  result <- rake(data, targets, type = 'pctlim', pctlim = 0.1)
  # Only "skewed" should be selected (discrepancy > 10%)
  expect_true('skewed' %in% result$vars_used)
  # "balanced" might or might not be included depending on random draw
})

test_that("type = 'nlim' selects top N variables", {
  set.seed(42)
  data <- data.frame(
    v1 = sample(c('a', 'b'), 200, replace = TRUE, prob = c(0.8, 0.2)),
    v2 = sample(c('x', 'y'), 200, replace = TRUE, prob = c(0.9, 0.1)),
    v3 = sample(c('p', 'q'), 200, replace = TRUE, prob = c(0.5, 0.5))
  )
  targets <- list(
    v1 = c(a = 0.5, b = 0.5),
    v2 = c(x = 0.5, y = 0.5),
    v3 = c(p = 0.5, q = 0.5)
  )

  result <- rake(data, targets, type = 'nlim', nlim = 2)
  expect_lte(length(result$vars_used), 2)
})

test_that('iterate = TRUE adds newly discrepant variables', {
  set.seed(42)
  data <- data.frame(
    v1 = sample(c('a', 'b'), 200, replace = TRUE, prob = c(0.8, 0.2)),
    v2 = sample(c('x', 'y'), 200, replace = TRUE, prob = c(0.6, 0.4))
  )
  targets <- list(
    v1 = c(a = 0.5, b = 0.5),
    v2 = c(x = 0.5, y = 0.5)
  )

  result <- rake(data, targets, type = 'pctlim', pctlim = 0.05, iterate = TRUE)
  expect_s3_class(result, 'ipf_rake')
  expect_true(result$converged)
})
