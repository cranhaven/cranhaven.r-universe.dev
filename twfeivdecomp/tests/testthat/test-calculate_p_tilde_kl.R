# Tests for calculate_p_tilde_kl() function

test_that("calculate_p_tilde_kl projects instrument_residuals onto residualized controls", {
  data <- data.frame(
  instrument_residuals = c(-0.3,  0.5, -1.2,  0.8,  1.5, -1.3),
  control1_cohort_time_average_residuals = c( 0.2, -0.4,  0.1, -0.2,  0.3,  0.0),
  control2_cohort_time_average_residuals = c(-0.1,  0.3, -0.5,  0.4, -0.2,  0.1)
)

  control_vars <- c("control1", "control2")

  result <- calculate_p_tilde_kl(data, control_vars)

  expected_fit_p_tilde_kl <- lm(
    instrument_residuals ~ control1_cohort_time_average_residuals +
      control2_cohort_time_average_residuals - 1,
    data = data
  )

  expected_p_tilde_kl <- predict(expected_fit_p_tilde_kl)
  expected_r_squared <- summary(expected_fit_p_tilde_kl)$r.squared

  expect_type(result, "list")
  expect_true(all(c("data", "r_squared") %in% names(result)))
  expect_true("p_tilde_kl" %in% names(result$data))
  expect_equal(unname(result$data$p_tilde_kl), unname(expected_p_tilde_kl), tolerance = 1e-12)
  expect_equal(result$r_squared, expected_r_squared, tolerance = 1e-12)
})