test_that("adaptive_cycle_trend_analysis works with dates_type = 'date'", {
  dates <- as.Date("2020-01-01") + 1:50
  signal <- sin(2*pi*(1:50)/10) + rnorm(50)

  res <- adaptive_cycle_trend_analysis(
    signal = signal,
    dates = dates,
    dates_type = "date",
    trendmethod = "loess"
  )

  expect_equal(length(res$Trend), 50)
  expect_s3_class(res$Plot$Trend, "ggplot")
  expect_true(is.numeric(res$Trend))
  expect_true(is.list(res$CI))
})

test_that("adaptive_cycle_trend_analysis works with dates_type = 'posix'", {
  t <- as.POSIXct("2020-01-01 00:00:00") + seq(0, by = 60, length.out = 200)
  x <- sin(2*pi*(1:200)/50) + rnorm(200)

  res <- adaptive_cycle_trend_analysis(
    signal = x,
    dates = t,
    dates_type = "posix",
    trendmethod = "gam"
  )

  expect_equal(length(res$Trend), 200)
  expect_s3_class(res$Plot$Trend, "ggplot")
  expect_true(is.numeric(res$Trend))
})

test_that("adaptive_cycle_trend_analysis works with dates_type = 'numeric'", {
  time <- seq(0, 10, length.out = 300)
  y <- sin(2*pi*time*5) + rnorm(300)

  res <- adaptive_cycle_trend_analysis(
    signal = y,
    dates = time,
    dates_type = "numeric",
    trendmethod = "loess"
  )

  expect_equal(length(res$Trend), 300)
  expect_s3_class(res$Plot$Trend, "ggplot")
  expect_true(is.numeric(res$Trend))
})
