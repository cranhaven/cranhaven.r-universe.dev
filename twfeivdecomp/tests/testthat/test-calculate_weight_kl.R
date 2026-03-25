# Tests for calculate_weight_kl() function

test_that("calculate_weight_kl computes weight correctly", {
  N <- 10
  r_squared <- 0.3
  V_Z <- 2
  V_b_kl_zp <- 1.5
  between_kl_treatment <- 0.8
  between_kl_zp_treatment <- 0.4

  weight_kl <- calculate_weight_kl(
    N, r_squared, V_Z, V_b_kl_zp,
    between_kl_treatment, between_kl_zp_treatment
  )

  expected_weight_kl <- N^2 * ((1 - r_squared) * V_Z * between_kl_treatment +
                       V_b_kl_zp * between_kl_zp_treatment)

  expect_equal(weight_kl, expected_weight_kl, tolerance = 1e-12)  
})