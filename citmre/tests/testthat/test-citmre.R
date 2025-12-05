# Output tests
test_that("Checking output type for all frequencies", {
  expect_type(rmre_data(frequency = 365), "double")
  expect_type(rmre_data(frequency = 12), "double")
  expect_type(rmre_data(frequency = 4), "double")
  expect_type(rmre_data(frequency = 2), "double")
})

test_that("Checking output type when log_return is TRUE", {
  expect_type(rmre_data(frequency = 365, log_return = TRUE), "double")
  expect_type(rmre_data(frequency = 12, log_return = TRUE), "double")
  expect_type(rmre_data(frequency = 4, log_return = TRUE), "double")
  expect_type(rmre_data(frequency = 2, log_return = TRUE), "double")
})

# Value tests
test_that("Checking output values", {
  expect_true(all(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 365) > 0))

  expect_true(all(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 12) > 0))

  expect_true(all(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 4) > 0))

  expect_true(all(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 2) > 0))
})


test_that("Checking output null values", {
  expect_true(all(!is.null(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 365))))
  expect_true(all(!is.null(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 12))))
  expect_true(all(!is.null(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 4))))
  expect_true(all(!is.null(rmre_data(start_date = "2010-01-05", end_date = "2023-12-28", frequency = 2))))
})
