test_that("list_forecast_series() works without network", {
  result <- list_forecast_series()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("series", "sheet", "description"))
  expect_equal(nrow(result), 10)
  expect_true("PSNB" %in% result$series)
  expect_true("CPI"  %in% result$series)
})

test_that("get_forecasts() errors on invalid series", {
  expect_error(
    get_forecasts("NOT_A_SERIES"),
    regexp = "should be one of"
  )
})

# v0.4.0 schema for HFD output: forecast_date plus the standard tidy long cols.
hfd_cols <- c("forecast_date", "period", "period_type",
              "series", "metric_type", "value", "unit")

test_that("get_forecasts('PSNB') returns the v0.4.0 HFD schema", {
  skip_on_cran()
  skip_if_offline()

  result <- get_forecasts("PSNB")

  expect_s3_class(result, "data.frame")
  expect_named(result, hfd_cols)
  expect_type(result$series,        "character")
  expect_type(result$forecast_date, "character")
  expect_type(result$period,        "character")
  expect_type(result$value,         "double")
  expect_gt(nrow(result), 100)
  expect_true(all(result$series == "PSNB"))
  expect_true(all(result$period_type == "fiscal_year"))
  expect_true(all(result$metric_type == "level"))
  expect_true(all(result$unit == "gbp_bn"))
})

test_that("get_forecasts('PSNB') returns data for 2024-25", {
  skip_on_cran()
  skip_if_offline()

  result <- get_forecasts("PSNB")
  forecasts_2425 <- result[result$period == "2024-25", ]

  expect_gt(nrow(forecasts_2425), 5)
  expect_true(all(forecasts_2425$value > 0))
})

test_that("get_forecasts('CPI') is tagged as a yoy_pct rate", {
  skip_on_cran()
  skip_if_offline()

  result <- get_forecasts("CPI")

  expect_s3_class(result, "data.frame")
  expect_named(result, hfd_cols)
  expect_true(all(result$series == "CPI"))
  expect_gt(nrow(result), 50)
  expect_true(all(result$metric_type == "yoy_pct"))
  expect_true(all(result$unit == "pct"))
})

test_that("get_forecasts() returns obr_tbl with HFD provenance", {
  skip_on_cran()
  skip_if_offline()

  res  <- get_forecasts("PSNB")
  expect_s3_class(res, "obr_tbl")
  prov <- obr_provenance(res)
  expect_equal(prov$publication, "HFD")
  expect_match(prov$source_url, "historical-official-forecasts-database")
  expect_match(prov$vintage, "^[A-Z][a-z]+ [0-9]{4}$")
})
