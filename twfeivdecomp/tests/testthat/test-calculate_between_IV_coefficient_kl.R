# Tests for calculate_between_IV_coefficient_kl() function

test_that("calculate_between_IV_coefficient_kl computes the between IV coefficient correctly", {
  r_squared <- 0.2
  V_Z <- 2
  V_b_kl_zp <- 1
  between_kl_outcome <- 1.5
  between_kl_zp_outcome <- 0.5
  between_kl_treatment <- 1.0
  between_kl_zp_treatment <- 0.2

  between_IV_coefficient_kl <- calculate_between_IV_coefficient_kl(
  r_squared, V_Z, between_kl_outcome,
  V_b_kl_zp, between_kl_zp_outcome,
  between_kl_treatment, between_kl_zp_treatment
  )

  expected_between_IV_coefficient_kl <- ((1 - r_squared) * V_Z * between_kl_outcome +
                 V_b_kl_zp * between_kl_zp_outcome) /
              ((1 - r_squared) * V_Z * between_kl_treatment +
                 V_b_kl_zp * between_kl_zp_treatment)

  expect_equal(between_IV_coefficient_kl, expected_between_IV_coefficient_kl, tolerance = 1e-12)
})