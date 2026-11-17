test_that("API rate limit warning is displayed correctly", {
  # Capture the output
  expect_snapshot(check_unique_pat(n_patients = 60, use_api = TRUE))

  # Test that nothing is output when no warning needed
  expect_snapshot(check_unique_pat(n_patients = 40, use_api = TRUE))

  expect_snapshot(check_unique_pat(n_patients = 60, use_api = FALSE))
})

test_that("API usage advisory is displayed correctly for <50 patients", {
  # Capture the output for fewer than 50 patients
  expect_snapshot(advise_api_usage(n_patients = 30, use_api = TRUE))

  # Test that nothing is output when API is not used
  expect_snapshot(advise_api_usage(n_patients = 30, use_api = FALSE))
})
