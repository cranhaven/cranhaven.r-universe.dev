test_that("list_forecast_series() works without network", {
  result <- list_forecast_series()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("series", "sheet", "description"))
  expect_equal(nrow(result), 10)
  expect_true("PSNB" %in% result$series)
  expect_true("CPI" %in% result$series)
})

test_that("get_forecasts() errors on invalid series", {
  expect_error(
    get_forecasts("NOT_A_SERIES"),
    regexp = "Unknown series"
  )
})

test_that("get_forecasts() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_forecasts("PSNB")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("series", "forecast_date", "fiscal_year", "value"))
  expect_type(result$series, "character")
  expect_type(result$forecast_date, "character")
  expect_type(result$fiscal_year, "character")
  expect_type(result$value, "double")
  expect_gt(nrow(result), 100)
  # All rows should have series = "PSNB"
  expect_true(all(result$series == "PSNB"))
})

test_that("get_forecasts() returns data for 2024-25", {
  skip_on_cran()
  skip_if_offline()

  result <- get_forecasts("PSNB")
  forecasts_2425 <- result[result$fiscal_year == "2024-25", ]

  expect_gt(nrow(forecasts_2425), 5)
  # All values should be positive (UK was borrowing)
  expect_true(all(forecasts_2425$value > 0))
})

test_that("get_forecasts() works for CPI series", {
  skip_on_cran()
  skip_if_offline()

  result <- get_forecasts("CPI")

  expect_s3_class(result, "data.frame")
  expect_true(all(result$series == "CPI"))
  expect_gt(nrow(result), 50)
})
