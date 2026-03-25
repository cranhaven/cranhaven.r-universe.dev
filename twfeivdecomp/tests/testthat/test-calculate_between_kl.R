# Tests for calculate_between_kl() function

test_that("calculate_between_kl returns correct coefficient for a given variable", {
  data <- data.frame(
    id = rep(1:3, each = 4),
    time = rep(2000:2003, times = 3),
    outcome   = c(1.2, 2.1, 1.5, 2.3, 0.8, 1.6, 1.3, 2.0, 1.0, 1.9, 1.4, 2.2),
    instrument = c(0,1,0,1,  0,0,1,1,  1,0,1,0),
    control1_cohort_time_average = c(0.2, -0.1, 0.0, 0.3, 0.1, -0.2, 0.4, -0.3, 0.0, 0.2, -0.1, 0.1)
  )

  control_vars <- c("control1")

  between_kl_outcome <- calculate_between_kl(data, control_vars, "outcome")

  expected_between_kl_outcome <- lm(outcome ~ instrument + factor(time) + factor(id) + control1_cohort_time_average, data = data)$coef["instrument"]

  expect_equal(unname(between_kl_outcome), unname(expected_between_kl_outcome), tolerance = 1e-12)
})