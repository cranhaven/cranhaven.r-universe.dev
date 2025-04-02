test_that("get_ensemble_forecast works", {
  skip_if_offline()
  skip_on_cran()

  df <- get_ensemble_forecast(
    latitude = 37.30,
    longitude = -79.83,
    forecast_days = 7,
    past_days = 2,
    model = "gfs_seamless",
    variables = c("temperature_2m"))

  expect_s3_class(df, "data.frame")
})


test_that("get_ensemble_forecast works", {
  skip_if_offline()
  skip_on_cran()

  df <- get_ensemble_forecast(
    latitude = 37.30,
    longitude = -79.83,
    forecast_days = 7,
    past_days = 2,
    model = "gfs_seamless",
    variables = c("temperature_2m"))

  expect_s3_class(df, "data.frame")
})


test_that("get_historical_weather works", {
  skip_if_offline()
  skip_on_cran()

  df <- get_historical_weather(
    latitude = 37.30,
    longitude = -79.83,
    start_date = "2023-01-01",
    end_date = Sys.Date() - lubridate::days(1),
    variables = c("temperature_2m"))

  expect_s3_class(df, "data.frame")
})

test_that("get_seasonal_forecast works", {
  skip_if_offline()
  skip_on_cran()

  df <- get_seasonal_forecast(
    latitude = 37.30,
    longitude = -79.83,
    forecast_days = 274,
    past_days = 5,
    variables = c("temperature_2m"))

  expect_s3_class(df, "data.frame")
})

test_that("get_climate_projections works", {
  skip_if_offline()
  skip_on_cran()

  df <- get_climate_projections(
    latitude = 37.30,
    longitude = -79.83,
    start_date = Sys.Date(),
    end_date = Sys.Date() + lubridate::years(1),
    model = "EC_Earth3P_HR",
    variables = c("temperature_2m_mean"))

  expect_s3_class(df, "data.frame")
})

test_that("get_forecast works", {
  skip_if_offline()
  skip_on_cran()

  df <- get_forecast(latitude = 37.30,
                     longitude = -79.83,
                     forecast_days = 7,
                     past_days = 2,
                     model = "generic",
                     variables = c("temperature_2m"))

  expect_s3_class(df, "data.frame")
})



