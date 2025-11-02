test_that("FFT method detects correct period for simple sine wave", {
  # Simple sine wave with period = 4
  time <- seq(0, 20, by = 1)
  ts_data <- sin(2 * pi * time / 4)
  
  result <- compute_period(ts_data, method = "fft")
  expect_equal(result["period"], c(period = 4), tolerance = 0.1)
})

test_that("Lomb method detects correct period for simple sine wave", {
  # Simple sine wave with period = 4
  time <- seq(0, 20, by = 1)
  ts_data <- sin(2 * pi * time / 4)
  
  result <- compute_period(ts_data, time, method = "lomb")
  expect_equal(result["period"], c(period = 4), tolerance = 0.1)
})

test_that("Unsupported method throws error", {
  time <- seq(0, 10, by = 1)
  ts_data <- sin(2 * pi * time / 2)
  
  expect_error(compute_period(ts_data, method = "invalid_method"))
})

test_that("Function returns expected named vector", {
  time <- seq(0, 10, by = 1)
  ts_data <- sin(2 * pi * time / 2)
  
  result <- compute_period(ts_data, method = "fft")
  expect_named(result, c("period", "power", "snr", "p.value"))
})