test_that("00: Error when vectors dont have same length", {
  expect_error(.check_params(c(1,2), c('y'), 3L, 3L, "", NULL, 10))
})

test_that("01: Error when vectors dont have same length", {
  expect_error(.check_params(c(1), c('y', 'k'), 3L, 3L, "", NULL, 10))
})

test_that("02: Error when vectors dont have same length", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3L, "", c('#FFFFFF'), 10))
})

test_that("03: Error when cp_max not is integer", {
  expect_error(.check_params(c(1,2), c('y','k'), 3, 3L, "", NULL, 10))
})

test_that("04: Error when cp_line_width is not integer", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3, "", NULL, 10))
})

test_that("05: Error when cp_title_size is not numeric / integer", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3L, "", NULL, 's'))
})

test_that("06: Error when cp_title_size is < 1", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3L, "", NULL, 0))
})

test_that("07: Error when cp_max is <= 0", {
  expect_error(.check_params(c(1,2), c('y','k'), 0L, 3L, "", NULL, 1))
})

test_that("08: Error when cp_vals is null", {
  expect_error(.check_params(NULL, c('y','k'), 1L, 3L, "", NULL, 1))
})

test_that("09: Error when cp_text is null", {
  expect_error(.check_params(c(1,2), NULL, 1L, 3L, "", NULL, 1))
})

test_that("10: Error when cp_vals is not numeric", {
  expect_error(.check_params(c('s','s'), c('y','k'), 1L, 3L, "", NULL, 1))
})

test_that("11: Error when cp_text is not character", {
  expect_error(.check_params(c(1,2), c(1,2), 1L, 3L, "", NULL, 1))
})

test_that("12: Error when cp_sort is not a string", {
  expect_error(.check_params(c(1,2), c('y','k'), 1L, 3L, "", NULL, 1, 1))
})

test_that("13: Error when wrong option used in cp_sort", {
  expect_error(.check_params(c(1,2), c('y','k'), 1L, 3L, "", NULL, 1, "asce"))
})

test_that("14: Error when cp_tight_spacing is not numeric", {
  expect_error(.check_params(c(1,2), c('y','k'), 1L, 3L, "", NULL, 1, 'none', 's'))
})

test_that("15: Error when cp_tight_spacing has wrong value", {
  expect_error(.check_params(c(1,2), c('y','k'), 1L, 3L, "", NULL, 1, 'none', 0.9))
})

test_that("16: Error when cp_tight_spacing has wrong value", {
  expect_error(.check_params(c(1,2), c('y','k'), 1L, 3L, "", NULL, 1, 'none', 2.01))
})

test_that("17: Error when cp_shape has wrong value", {
  expect_error(.check_params(c(1,2), c('y','k'), 1L, 3L, "", NULL, 1, 'none', 2.0, 'aa'))
})

test_that("18: Error when cp_shape is not character", {
  expect_error(.check_params(c(1,2), c('y','k'), 1L, 3L, "", NULL, 1, 'none', 2.0, 1))
})
