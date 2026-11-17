test_that("Function works with data.frame and tibble", {
  test_data <- opioid_trial |>
    dplyr::filter(patient_id %in% sprintf("P%03d", 1:100))

  # Test that the function works with data.frame
  expect_no_error(calculate_mme(x = test_data))

  # Test that function works with tibble
  tibble <- tibble::as_tibble(test_data)
  expect_no_error(calculate_mme(x = tibble))
})

test_that("Input validation works correctly", {
  test_data <- opioid_trial |>
    dplyr::filter(patient_id %in% sprintf("P%03d", 1:100))

  # Test for missing id_col
  expect_snapshot(
    error = TRUE,
    calculate_mme(x = test_data, id_col = "missing_column")
  )

  # Test for missing required columns
  bad_data <- test_data
  bad_data$medication_name <- NULL

  expect_snapshot(
    error = TRUE,
    calculate_mme(x = bad_data)
  )

  bad_data2 <- test_data
  bad_data2$therapy_days_without <- NULL

  # Test for missing optional column that was specified
  expect_snapshot(
    error = TRUE,
    calculate_mme(
      x = bad_data2,
      therapy_days_without_col = "therapy_days_without"
    )
  )

  # Get list
  output <- calculate_mme(x = test_data)
  expect_snapshot(
    error = TRUE,
    calculate_mme(x = output)
  )
})

test_that("Basic functionality works with default column names", {
  test_data <- opioid_trial |>
    dplyr::filter(patient_id %in% sprintf("P%03d", 1:100))

  result <- calculate_mme(x = test_data)

  # Test structure of returned data
  expect_type(result, "list")
  expect_named(
    result,
    c(
      "medications",
      "patient_summary_with_buprenorphine",
      "patient_summary_without_buprenorphine"
    )
  )

  # Test that medication data frame has expected columns
  expect_true(all(
    c("factor", "mme", "single_day_mme") %in% names(result$medications)
  ))

  # Test that patient summaries have expected structure
  expect_equal(nrow(result$patient_summary_with_buprenorphine), 100)
  expect_equal(nrow(result$patient_summary_without_buprenorphine), 100)

  # Test that computed MME values were correctly assigned
  expect_equal(result$medications$factor[1], 2.4)
  expect_equal(result$medications$mme[2], 750)
  expect_equal(result$medications$single_day_mme[3], 30)
})

test_that("Custom column names are handled correctly", {
  # Create test data with non-default column names
  test_data <- opioid_trial |>
    dplyr::filter(patient_id %in% sprintf("P%03d", 1:100))

  test_data2 <- test_data |>
    dplyr::rename(
      subject_id = patient_id,
      med_name = medication_name,
      dosage = dose,
      doses_daily = doses_per_24_hours,
      duration_days = days_of_medication,
      therapy_duration = therapy_days,
      observation_period = observation_window_days,
      therapy_wo_bup = therapy_days_without,
      observation_wo_bup = observation_window_days_without
    )

  # Run the function with custom column names
  result <- calculate_mme(
    x = test_data2,
    id_col = "subject_id",
    medication_col = "med_name",
    dose_col = "dosage",
    doses_per_day_col = "doses_daily",
    days_col = "duration_days",
    therapy_days_col = "therapy_duration",
    observation_days_col = "observation_period",
    therapy_days_without_col = "therapy_wo_bup",
    observation_days_without_col = "observation_wo_bup"
  )

  expect_snapshot(result)

  # Test that patient summaries use the correct ID column name
  expect_true(
    "subject_id" %in% names(result$patient_summary_with_buprenorphine)
  )
  expect_false(
    "patient_id" %in% names(result$patient_summary_with_buprenorphine)
  )
})

test_that("Handles optional without_buprenorphine columns", {
  # Create test data without the optional columns
  test_data <- opioid_trial |>
    dplyr::filter(patient_id %in% sprintf("P%03d", 1:100))

  test_data2 <- test_data |>
    dplyr::select(-c(therapy_days_without, observation_window_days_without))

  # Test that the function works without the optional columns
  expect_no_error(
    calculate_mme(x = test_data2)
  )
})

test_that("Empty results are handled correctly", {
  # Create test data with empty result (no matching medications)
  test_data <- data.frame(
    patient_id = character(0),
    medication_name = character(0),
    dose = numeric(0),
    doses_per_24_hours = numeric(0),
    days_of_medication = numeric(0),
    therapy_days = numeric(0),
    observation_window_days = numeric(0)
  )

  # Test that function handles empty data correctly
  expect_snapshot(
    error = TRUE,
    calculate_mme(x = test_data)
  )
})
