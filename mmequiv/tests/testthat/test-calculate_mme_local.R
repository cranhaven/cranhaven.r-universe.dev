test_that("calculate_mme_local() is deprecated", {
  meds_list <- list(
    list(
      medication_name = "Buprenorphine buccal film (mcg) buccal",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    ),
    list(
      medication_name = "Hydrocodone (mg)",
      dose = 75,
      doses_per_24_hours = 3,
      days_of_medication = 10
    )
  )

  expect_snapshot(
    calculate_mme_local(10, 5, meds_list)
  )
})

test_that("Local MME calculation matches API calculation", {
  withr::local_options(lifecycle_verbosity = "quiet")

  # Define test input
  meds_list <- list(
    list(
      medication_name = "Buprenorphine buccal film (mcg) buccal",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    ),
    list(
      medication_name = "Hydrocodone (mg)",
      dose = 75,
      doses_per_24_hours = 3,
      days_of_medication = 10
    )
  )

  # Get results from both methods
  # Use with_mock_dir for API call to avoid rate limit during testing
  httptest2::with_mock_dir("calculate_mme_local", {
    api_result <- calculate_mme(meds_list, 10, 5, use_api = TRUE)
  })

  local_result <- calculate_mme_local(10, 5, meds_list)

  # Test overall structure is the same
  expect_identical(names(api_result), names(local_result))

  # Check calculation-specific fields
  expect_equal(
    api_result$therapy_obs_window_with,
    local_result$therapy_obs_window_with
  )
  expect_equal(
    api_result$therapy_obs_window_without,
    local_result$therapy_obs_window_without
  )

  # Check medication data
  # Note: We use expect_equal with tolerance for numeric comparisons
  api_meds <- api_result$medications
  local_meds <- local_result$medications

  # Sort by medication_name to ensure consistent order
  if (is.data.frame(api_meds)) {
    api_meds <- api_meds[order(api_meds$medication_name), ]
  } else {
    api_meds_names <- sapply(api_meds, function(x) x$medication_name)
    api_meds <- api_meds[order(api_meds_names)]
  }

  if (is.data.frame(local_meds)) {
    local_meds <- local_meds[order(local_meds$medication_name), ]
  } else {
    local_meds_names <- sapply(local_meds, function(x) x$medication_name)
    local_meds <- local_meds[order(local_meds_names)]
  }

  # Test medication calculations with tolerance
  expect_equal(api_meds$factor, local_meds$factor, tolerance = 1e-6)
  expect_equal(api_meds$mme, local_meds$mme, tolerance = 1e-6)
  expect_equal(
    api_meds$single_day_mme,
    local_meds$single_day_mme,
    tolerance = 1e-6
  )

  # Check MME definitions
  api_mme_with <- api_result$mme_definitions$with_buprenorphine
  local_mme_with <- local_result$mme_definitions$with_buprenorphine

  api_mme_without <- api_result$mme_definitions$without_buprenorphine
  local_mme_without <- local_result$mme_definitions$without_buprenorphine

  # Test MME calculations with tolerance
  mme_fields <- c("total_mme", "total_days", "mme1", "mme2", "mme3", "mme4")

  for (field in mme_fields) {
    expect_equal(
      as.numeric(api_mme_with[[field]]),
      as.numeric(local_mme_with[[field]]),
      tolerance = 1e-6,
      info = paste("with_buprenorphine", field)
    )

    expect_equal(
      as.numeric(api_mme_without[[field]]),
      as.numeric(local_mme_without[[field]]),
      tolerance = 1e-6,
      info = paste("without_buprenorphine", field)
    )
  }
})

test_that("Local MME calculation handles edge cases correctly", {
  withr::local_options(lifecycle_verbosity = "quiet")

  # Test with a single medication
  single_med <- list(
    list(
      medication_name = "Hydrocodone (mg)",
      dose = 10,
      doses_per_24_hours = 4,
      days_of_medication = 7
    )
  )

  # Ensure it works with single medication
  expect_no_error(calculate_mme_local(7, 7, single_med))

  # Test with only buprenorphine medications
  only_bupr_med <- list(
    list(
      medication_name = "Buprenorphine buccal film (mcg) buccal",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )

  # Ensure it handles buprenorphine-only case
  expect_no_error(calculate_mme_local(5, 5, only_bupr_med))

  # Test with vector inputs for therapy_days and observation_window_days
  complex_input <- list(
    list(
      medication_name = "Hydrocodone (mg)",
      dose = 10,
      doses_per_24_hours = 4,
      days_of_medication = 7
    ),
    list(
      medication_name = "Buprenorphine buccal film (mcg) buccal",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )

  # Ensure it works with vector inputs
  expect_no_error(calculate_mme_local(c(10, 15), c(5, 7), complex_input))
})

test_that("Provides error messages for invalid therapy_days and observation_window_days arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")

  meds_list <- list(
    list(
      medication_name = "Buprenorphine buccal film (mcg) buccal",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    ),
    list(
      medication_name = "Hydrocodone (mg)",
      dose = 75,
      doses_per_24_hours = 3,
      days_of_medication = 10
    )
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local("invalid", 5, meds_list)
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(0, 5, meds_list)
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(-10, 5, meds_list)
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, "invalid", meds_list)
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 0, meds_list)
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, -5, meds_list)
  )

  # New tests for vectors with 2 values
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(c(10, 0), 5, meds_list) # Second value is 0
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(c(10, -5), 5, meds_list) # Second value is negative
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(c(0, 10), 5, meds_list) # First value is 0
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(c(-1, 10), 5, meds_list) # First value is negative
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, c(5, 0), meds_list) # Second value is 0
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, c(0, 5), meds_list) # First value is 0
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, c(-5, 5), meds_list) # First value is negative
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(c(10, 20, 30), 5, meds_list) # Three values instead of 1 or 2
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, c(5, 10, 15), meds_list) # Three values instead of 1 or 2
  )
})

test_that("Accepts both single values and vectors of two values", {
  withr::local_options(lifecycle_verbosity = "quiet")

  meds_list <- list(
    list(
      medication_name = "Buprenorphine buccal film (mcg) buccal",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )

  # Mock the API call to avoid actually making requests
  httptest2::with_mock_dir("calculate_mme_local", {
    # Test with single values for both parameters
    expect_no_error(
      calculate_mme_local(10, 5, meds_list)
    )

    # Test with vector for therapy_days and single value for observation_window_days
    expect_no_error(
      calculate_mme_local(c(10, 15), 5, meds_list)
    )

    # Test with single value for therapy_days and vector for observation_window_days
    expect_no_error(
      calculate_mme_local(10, c(5, 7), meds_list)
    )

    # Test with vectors for both parameters
    expect_no_error(
      calculate_mme_local(c(10, 15), c(5, 7), meds_list)
    )
  })
})

test_that("Provides error messages for invalid medications argument", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, "not_a_list")
  )

  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, list()) # Empty list
  )

  # Test non-list element in medications
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, list("not_a_medication_list"))
  )

  # Test missing required fields
  bad_meds_missing_field <- list(
    list(dose = 50, doses_per_24_hours = 2, days_of_medication = 5) # Missing medication_name
  )
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, bad_meds_missing_field)
  )

  # Test invalid medication_name type
  bad_meds_name <- list(
    list(
      medication_name = 123, # Should be a string
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, bad_meds_name)
  )

  # Test invalid dose type or value
  bad_meds_dose <- list(
    list(
      medication_name = "Test Med",
      dose = "50", # Should be numeric
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, bad_meds_dose)
  )

  bad_meds_dose_negative <- list(
    list(
      medication_name = "Test Med",
      dose = -50, # Should be positive
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, bad_meds_dose_negative)
  )

  # Test invalid doses_per_24_hours
  bad_meds_doses_per_day <- list(
    list(
      medication_name = "Test Med",
      dose = 50,
      doses_per_24_hours = "2", # Should be numeric
      days_of_medication = 5
    )
  )
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, bad_meds_doses_per_day)
  )

  # Test invalid days_of_medication
  bad_meds_days <- list(
    list(
      medication_name = "Test Med",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 0 # Should be positive
    )
  )
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, bad_meds_days)
  )
})

test_that("Validates medication names against known list", {
  withr::local_options(lifecycle_verbosity = "quiet")

  # This will use the mocked data from httptest2
  valid_med_names <- httptest2::with_mock_dir("get_med_list", {
    get_med_list()$med_name
  })

  # Create a valid medication list using a known good name
  valid_medication <- list(
    list(
      medication_name = valid_med_names[1], # Take the first valid name
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )

  # Create an invalid medication name
  invalid_medication <- list(
    list(
      medication_name = "Not A Real Medication Name",
      dose = 50,
      doses_per_24_hours = 2,
      days_of_medication = 5
    )
  )

  # Test with valid medication - should not error
  httptest2::with_mock_dir("calculate_mme_local", {
    expect_no_error(calculate_mme_local(10, 5, valid_medication))
  })

  # Test with invalid medication - should error
  expect_snapshot(
    error = TRUE,
    calculate_mme_local(10, 5, invalid_medication)
  )
})
