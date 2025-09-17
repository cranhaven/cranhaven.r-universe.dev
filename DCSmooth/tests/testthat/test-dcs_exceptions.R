################################################################################
#                                                                              #
#                             Test for dcs-errors                              #
#                                                                              #
################################################################################

### Test for incorrect inputs in dcs()
context("DCS input")

test_that("ellipsis checks for useful arguments", {
  Y = matrix(rnorm(100^2), 100, 100)
  X = 1:100/100
  T = 1:100
  expect_warning(dcs(Y, var_model = "sfarima_RSS"), "are unsupported and will")
  expect_warning(dcs(Y, model_order = 1), "are unsupported and will")
  expect_equal(class(dcs(Y, X = X, T = T)), "dcs")
})

test_that("Y has exception handling", {
  expect_error(dcs(1), "Y must be a numeric matrix.")
  Y = matrix(rnorm(4^2), 4, 4)
  expect_error(dcs(Y), "Y has to be at least of dimension 5 in each direction.")
  Y = matrix(rnorm(100), 10, 10)
  Y[2, 5] = "test"
  expect_error(dcs(Y), "Y must be a numeric matrix.")
  Y[2, 5] = NA
  expect_error(dcs(Y), "Y contains missing values")
})

test_that("dcs_options has exception handling", {
  Y = y.norm1 + matrix(rnorm(101^2), 101, 101)
  expect_error(dcs(Y, dcs_options = "test"),
               "Incorrect options specified, please use")
  dcs_options = set.options()
  dcs_options$type = "test"
  expect_error(dcs(Y, dcs_options), "Unsupported regression type.")
  dcs_options = set.options()
  dcs_options$test = 1
  expect_warning(dcs(Y, dcs_options), "unknown and will be ignored.")
  dcs_options = set.options()
  dcs_options$type = NULL
  expect_error(dcs(Y, dcs_options), "not specified.")
})

test_that("dcs_options default values are correct", {
  Y = y.norm1 + matrix(rnorm(101^2), 101, 101)
  dcs_options = set.options()
  expect_equal(dcs(Y)$dcs_options, dcs_options)
})

test_that("dcs_options is correctly used", {
  Y = y.norm1 + matrix(rnorm(101^2), 101, 101)
  dcs_options = set.options()
  expect_equal(dcs(Y, dcs_options)$dcs_options, dcs_options)
  dcs_options = set.options(type = "KR")
  expect_equal(dcs(Y, dcs_options)$dcs_options, dcs_options)
})

test_that("h has expection handling", {
  Y = y.norm1 + matrix(rnorm(101^2), 101, 101)
  expect_error(dcs(Y, h = 1), "h must be a numeric vector")
  expect_error(dcs(Y, h = c(1, "bla")), "h must be a numeric vector")
  expect_error(dcs(Y, h = c(2, -2)), "h must be positive")
  expect_true(is.numeric(dcs(Y, h = "auto")$h))
})

test_that("parallel has expection handling", {
  Y = y.norm1 + matrix(rnorm(101^2), 101, 101)
  expect_error(dcs(Y, parallel = 1), "Unsupported value in argument")
  expect_error(dcs(Y, parallel = "bla"), "Unsupported value in argument")
  expect_error(dcs(Y, parallel = c(FALSE, FALSE)),
               "Unsupported value in argument")
})