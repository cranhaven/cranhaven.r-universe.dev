# Tests for calculate_between_kl_zp() function

test_that("calculate_between_kl_zp returns correct coefficient for a given variable", {
  data <- data.frame(
    id = rep(1:3, each = 4),
    time = rep(2000:2003, times = 3),
    outcome = c(1.1, 2.0, 1.5, 2.5, 0.8, 1.7, 1.2, 2.2, 1.0, 1.8, 1.4, 2.1),
    zp = c(-0.2, 0.3, -0.1, 0.2, -0.3, 0.4, -0.2, 0.1, -0.1, 0.3, -0.2, 0.2)
  )

  between_kl_zp_outcome <- calculate_between_kl_zp(data, "outcome")
  expected_between_kl_zp_outcome <- lm(outcome ~ zp + factor(time) + factor(id), data = data)$coef["zp"]

  expect_equal(unname(between_kl_zp_outcome), unname(expected_between_kl_zp_outcome), tolerance = 1e-12)
})