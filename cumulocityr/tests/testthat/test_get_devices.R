context("test get_devices")


test_that("get_devices returns data.frame", {
  skip_on_cran()
  expect_equal(is.data.frame(get_devices(num_rows = 3)), TRUE)
})


test_that("parse_json = FALSE returns character string", {
  skip_on_cran()

  result_09 <- get_devices(num_rows = 3, parse_json = FALSE)

  expect_true(inherits(result_09[[1]], "character"))
})
