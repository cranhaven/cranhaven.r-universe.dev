context("test get_measurements")


test_that("page_size controls number of records returned", {
  skip_on_cran()

  result_01 <- get_measurements(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-09-30T20:00:00Z",
    num_rows = 2
  )

  result_02 <- get_measurements(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-09-30T20:00:00Z",
    num_rows = 11
  )

  expect_equal(NROW(result_01), 2)
  expect_equal(NROW(result_02), 11)
})

test_that("when num_rows is NULL, return all records between two dates", {
  skip_on_cran()
  result_10 <- get_measurements(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-10-01T00:00:00Z", date_to = "2019-10-01T00:00:10Z"
  )
  expect_equal(NROW(result_10), 2)
})


test_that("parse_json = FALSE returns character string", {
  skip_on_cran()
  result_08 <- get_measurements(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-09-30T20:00:00Z",
    num_rows = 7, parse_json = FALSE
  )
  expect_true(inherits(result_08[[1]], "character"))
})
