# Tests for partial_out_cohort_time_covariate() function

test_that("partial_out_cohort_time_covariate residualizes cohort-time average controls", {
  data <- data.frame(
    id = rep(1:3, each = 4),
    time = rep(2000:2003, times = 3),
    control1_cohort_time_average = c(1,2,3,4, 2,3,4,5, 3,4,5,6),
    control2_cohort_time_average = c(10,20,30,40, 15,25,35,45, 12,22,32,42)
  )

  control_vars <- c("control1", "control2")

  result <- partial_out_cohort_time_covariate(data, control_vars)

  expect_true("control1_cohort_time_average_residuals" %in% names(result))
  expect_true("control2_cohort_time_average_residuals" %in% names(result))

  expected_residuals_control1 <- residuals(lm(control1_cohort_time_average ~ factor(id) + factor(time), data = data))
  expected_residuals_control2 <- residuals(lm(control2_cohort_time_average ~ factor(id) + factor(time), data = data))

  expect_equal(unname(result$control1_cohort_time_average_residuals), unname(expected_residuals_control1), tolerance = 1e-12)
  expect_equal(unname(result$control2_cohort_time_average_residuals), unname(expected_residuals_control2), tolerance = 1e-12)
})

