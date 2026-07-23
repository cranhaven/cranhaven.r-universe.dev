# Diagnostics tests
test_that('design_effect returns valid results', {
  weights <- rep(1, 100)
  de <- design_effect(weights)

  expect_equal(de$deff, 1, tolerance = 1e-6)
  expect_equal(de$n_eff, 100, tolerance = 1e-6)
})

test_that('design_effect increases with unequal weights', {
  weights_equal <- rep(1, 100)
  weights_unequal <- c(rep(0.5, 50), rep(1.5, 50))

  de_eq <- design_effect(weights_equal)
  de_uneq <- design_effect(weights_unequal)

  expect_gt(de_uneq$deff, de_eq$deff)
  expect_lt(de_uneq$n_eff, de_eq$n_eff)
})

test_that('weight_assess returns per-variable tibbles', {
  set.seed(42)
  data <- data.frame(
    gender = sample(c('M', 'F'), 100, replace = TRUE, prob = c(0.6, 0.4))
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  result <- rake(data, targets)

  assessment <- weight_assess(data, targets, result$weights)
  expect_type(assessment, 'list')
  expect_named(assessment, 'gender')

  tbl <- assessment[['gender']]
  expect_s3_class(tbl, 'tbl_df')
  expect_true('level' %in% names(tbl))
  expect_true('target' %in% names(tbl))
  expect_true('weighted_pct' %in% names(tbl))
  expect_true('residual_disc' %in% names(tbl))

  # Should include Total row

  expect_true('Total' %in% tbl$level)

  # Residual discrepancy should be near zero
  data_rows <- tbl[tbl$level != 'Total', ]
  expect_true(all(abs(data_rows$residual_disc) < 1e-4))
})

test_that('weight_assess includes a missing row with na_method = bucket', {
  data <- data.frame(
    gender = c('M', 'F', NA, 'M', 'F', NA)
  )
  targets <- list(gender = c(M = 0.5, F = 0.5))
  result <- rake(data, targets, na_method = 'bucket')

  assessment <- weight_assess(
    data,
    targets,
    result$weights,
    na_method = 'bucket'
  )

  expect_true('(Missing)' %in% assessment$gender$level)
})
