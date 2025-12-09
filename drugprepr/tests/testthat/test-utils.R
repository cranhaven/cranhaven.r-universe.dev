context('Utility functions')

test_that('get_mode finds the most common value, ignoring NAs', {
  get_mode <- drugprepr:::get_mode
  expect_equal(get_mode(c(1, 1, 1, 2, 3, -1, 3, 2)), 1)
  expect_equal(get_mode(c(2, 2, 2, NA)), 2)
  expect_equal(get_mode(c(NA, NA, NA, 1)), 1)
  expect_equal(get_mode(c(NA, NA, NA, 1), na.rm = FALSE), NA_real_)
  expect_equal(get_mode(c(2, 2, 2, NaN)), 2)
  expect_equal(get_mode(c(-Inf, Inf, Inf, NaN)), Inf)
  expect_equal(get_mode(c('a', 'a', 'b')), 'a')
  expect_equal(get_mode(c(NA, NA), na.rm = FALSE), NA)
  expect_equal(get_mode(c(NA, NA), na.rm = TRUE), NA) # bit of an edge case
  expect_null(get_mode(NULL))
})

test_that('If there are two most common values, return the first to appear', {
  get_mode <- drugprepr:::get_mode
  expect_equal(get_mode(c(3, 2, 1, 2, 1)), 2)
  expect_equal(get_mode(c(1, 1, 2, 2, 3)), 1)
  expect_equal(get_mode(c('c', 'b', 'b', 'a', 'a')), 'b')
})
