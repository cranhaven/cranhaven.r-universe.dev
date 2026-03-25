# Tests for print_summary() function

test_that("print_summary: no controls (Wald_DID_estimate present)", {
  data <- data.frame(
    design_type = c(
      "Exposed vs Unexposed",
      "Exposed vs Unexposed",
      "Exposed vs Not Yet Exposed"
    ),
    weight = c(0.2, 0.3, 0.5),
    Wald_DID_estimate = c(1.0, 2.0, -1.0),
    stringsAsFactors = FALSE
  )

  expected <- data.frame(
    design_type = c(
      "Exposed vs Not Yet Exposed",
      "Exposed vs Unexposed"
    ),
    weight_sum = c(0.5, 0.5),
    Weighted_average_Wald_DID = c(-1.00000, 1.60000),
    stringsAsFactors = FALSE
  )

  result <- print_summary(data, return_df = TRUE)

  result_sorted <- dplyr::arrange(as.data.frame(result), design_type)
  expected_sorted <- dplyr::arrange(as.data.frame(expected), design_type)
  expect_equal(result_sorted, expected_sorted)
})


test_that("print_summary: with controls (between_IV_coefficient_kl present)", {
  data <- data.frame(
    design_type = c(
      "Exposed vs Unexposed",
      "Exposed vs Unexposed",
      "Early Exposed vs Later Exposed",
      "Early Exposed vs Later Exposed"
    ),
    weight_kl = c(0.1, 0.4, 0.2, 0.3),
    between_IV_coefficient_kl = c(1.234567, 1.765432, -2.111111, 0.111111),
    stringsAsFactors = FALSE
  )

  mean_exposed_unexposed <- (1.234567 * 0.1 + 1.765432 * 0.4) / 0.5
  mean_early_later <- (-2.111111 * 0.2 + 0.111111 * 0.3) / 0.5

  expected <- data.frame(
    design_type = c(
      "Early Exposed vs Later Exposed",
      "Exposed vs Unexposed"
    ),
    weight_sum = c(0.5, 0.5),
    Weighted_average_between_IV_coefficient_kl =
      round(c(mean_early_later, mean_exposed_unexposed), 5),
    stringsAsFactors = FALSE
  )

  result <- print_summary(data, return_df = TRUE)

  result_sorted <- dplyr::arrange(as.data.frame(result), design_type)
  expected_sorted <- dplyr::arrange(as.data.frame(expected), design_type)
  expect_equal(result_sorted, expected_sorted)
})