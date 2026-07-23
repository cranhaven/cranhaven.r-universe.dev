# S3 method tests
test_that('print.ipf_rake works without error', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE)
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  result <- rake(data, targets)

  # cli output goes to stderr/message; just confirm no error + invisible return

  expect_invisible(print(result))
})

test_that('summary.ipf_rake works without error', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE)
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  result <- rake(data, targets)

  out <- summary(result)
  expect_type(out, 'list')
  expect_true('convergence' %in% names(out))
  expect_true('assessment' %in% names(out))
  expect_true('design_effect' %in% names(out))
})

test_that('tidy.ipf_rake returns expected tibble', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE),
    age = sample(c('young', 'old'), 100, replace = TRUE)
  )
  targets <- list(
    gender = c(M = 0.5, F = 0.5),
    age = c(young = 0.6, old = 0.4)
  )
  result <- rake(data, targets)

  tidy_df <- tidy(result)
  expect_s3_class(tidy_df, 'tbl_df')
  expect_true(all(
    c('variable', 'level', 'target', 'weighted_pct', 'discrepancy') %in%
      names(tidy_df)
  ))
  expect_equal(nrow(tidy_df), 4) # 2 + 2 levels
})

test_that('glance.ipf_rake returns single-row tibble', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE)
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  result <- rake(data, targets)

  glance_df <- glance(result)
  expect_s3_class(glance_df, 'tbl_df')
  expect_equal(nrow(glance_df), 1)
  expect_true(all(
    c('converged', 'iterations', 'deff', 'n_eff') %in% names(glance_df)
  ))
})

test_that('rake stores the missing-data method on the result', {
  data <- data.frame(gender = c('M', 'F', NA, 'M'))
  targets <- list(gender = c(M = 0.5, F = 0.5))

  result <- rake(data, targets, na_method = 'bucket')

  expect_identical(result$na_method, 'bucket')
})

test_that('augment.ipf_rake appends .weight column', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE)
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  result <- rake(data, targets)

  aug <- augment(result)
  expect_s3_class(aug, 'tbl_df')
  expect_true('.weight' %in% names(aug))
  expect_equal(nrow(aug), 100)
  expect_equal(aug$.weight, result$weights)
})
