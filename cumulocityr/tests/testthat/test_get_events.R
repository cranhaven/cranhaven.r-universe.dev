context("test get_events")

test_that("page_size controls number of records returned", {
  skip_on_cran()

  result_06 <- get_events(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-09-30T20:00:00Z",
    num_rows = 2
  )

  result_07 <- get_events(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-09-30T20:00:00Z",
    num_rows = 4
  )

  expect_equal(NROW(result_06), 2)
  expect_equal(NROW(result_07), 4)
})


test_that("parse_json = FALSE returns character string", {
  skip_on_cran()

  result_10 <- get_events(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-09-30T20:00:00Z",
    num_rows = 7, parse_json = FALSE
  )

  expect_true(inherits(result_10[[1]], "character"))
})


test_that("when num_rows is NULL, return all records between two dates", {
  skip_on_cran()
  result_11 <- get_events(
    device_id = .get_cumulocity_device_id(),
    date_from = "2019-09-30T00:00:00Z", date_to = "2019-10-01T04:40:10Z"
  )
  expect_equal(NROW(result_11), 1)
})
