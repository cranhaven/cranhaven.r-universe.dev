# Tests for calculate_V_b_kl_zp() function

test_that("calculate_V_b_kl_zp computes zp and its variance correctly", {
  data <- data.frame(
    id = rep(1:3, each = 3),
    time = rep(2000:2002, times = 3),
    projection_cohort_time = c(1.0, 0.5, -0.2,  0.8, 0.1, -0.3,  0.6, -0.4, -0.1),
    p_tilde_kl = c(0.9, 0.4, -0.1,  0.7, 0.0, -0.2,  0.5, -0.5, 0.0)
  )

  result <- calculate_V_b_kl_zp(data)

  expect_type(result, "list")
  expect_true(all(c("data", "V_b_kl_zp") %in% names(result)))
  expect_true(all(c("p_tilde", "zp") %in% names(result$data)))

  expected_p_tilde <- residuals(lm(projection_cohort_time ~ factor(id) + factor(time), data = data))
  expect_equal(unname(result$data$p_tilde), unname(expected_p_tilde), tolerance = 1e-12)

  expected_zp <- data$p_tilde_kl - expected_p_tilde
  expect_equal(unname(result$data$zp), unname(expected_zp), tolerance = 1e-12)

  N <- nrow(data)
  expected_var <- var(expected_zp) * (N - 1) / N
  expect_equal(result$V_b_kl_zp, expected_var, tolerance = 1e-12)
})