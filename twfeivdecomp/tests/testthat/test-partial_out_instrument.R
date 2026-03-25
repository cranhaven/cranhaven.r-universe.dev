# Tests for partial_out_instrument() function

test_that("partial_out_instrument returns residuals and variance", {
  data <- data.frame(
  id        = rep(1:5, each = 6),
  time      = rep(2000:2005, times = 5),
  instrument = c(0,0,1,1,1,1,  0,1,1,1,1,1,  0,0,0,0,0,1,  0,0,0,1,1,1, 0,1,1,1,1,1)
  )
  
  result <- partial_out_instrument(data)

  expected_instrument_residuals <- residuals(lm(instrument ~ factor(id) + factor(time), data = data)) 
  N <- nrow(data)
  expected_V_Z <- var(expected_instrument_residuals) * (N - 1) / N

  expect_type(result, "list")
  expect_true(all(c("data", "V_Z") %in% names(result)))

  expect_true("instrument_residuals" %in% names(result$data))

  expect_equal(unname(result$data$instrument_residuals), unname(expected_instrument_residuals), tolerance = 1e-12)
  expect_equal(result$V_Z, expected_V_Z, tolerance = 1e-12)
})