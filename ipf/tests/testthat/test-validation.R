# Input validation tests
test_that('rake errors on non-data.frame input', {
  expect_error(rake(list(a = 1), list(a = c(x = 0.5))), 'data frame')
})

test_that('rake errors on missing variable in data', {
  data <- data.frame(x = c('a', 'b'))
  targets <- list(y = c(a = 0.5, b = 0.5))
  expect_error(rake(data, targets), 'not found')
})

test_that('rake errors on unnamed targets', {
  data <- data.frame(x = c('a', 'b'))
  expect_error(rake(data, list(c(0.5, 0.5))), 'named list')
})

test_that('rake errors on negative target values', {
  data <- data.frame(x = c('a', 'b', 'a', 'b'))
  targets <- list(x = c(a = -0.5, b = 1.5))
  expect_error(rake(data, targets), 'negative')
})

test_that('rake errors on data levels not in targets', {
  data <- data.frame(x = c('a', 'b', 'c'))
  targets <- list(x = c(a = 0.5, b = 0.5))
  expect_error(rake(data, targets), 'not in targets')
})

test_that('rake warns on target levels with zero observations', {
  data <- data.frame(x = c('a', 'a', 'a'))
  targets <- list(x = c(a = 0.5, b = 0.5))
  expect_warning(rake(data, targets), 'zero-cell')
})

test_that('rake warns on targets not summing to 1', {
  data <- data.frame(x = c('a', 'b', 'a', 'b'))
  targets <- list(x = c(a = 50, b = 50))
  expect_warning(rake(data, targets), 'sum to')
})

test_that('encode_variable handles character vectors', {
  enc <- encode_variable(c('a', 'b', 'a'), c('a', 'b'), var_name = 'test')
  expect_equal(enc$codes, c(1L, 2L, 1L))
  expect_equal(enc$level_names, c('a', 'b'))
})

test_that('encode_variable handles NAs', {
  enc <- encode_variable(c('a', NA, 'b'), c('a', 'b'), var_name = 'test')
  expect_equal(enc$codes, c(1L, 0L, 2L))
})

test_that('encode_variable can bucket NAs', {
  enc <- encode_variable(
    c('a', NA, 'b'),
    c('a', 'b'),
    var_name = 'test',
    na_method = 'bucket'
  )

  expect_equal(enc$codes, c(1L, 3L, 2L))
  expect_equal(enc$level_names, c('a', 'b', '(Missing)'))
})

test_that('encode_variable handles logical vectors', {
  enc <- encode_variable(
    c(TRUE, FALSE, TRUE),
    c('FALSE', 'TRUE'),
    var_name = 'test'
  )
  expect_equal(enc$codes, c(2L, 1L, 2L))
  expect_equal(enc$level_names, c('FALSE', 'TRUE'))
})
