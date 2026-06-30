test_that("fetch_finna performs a basic fetch correctly", {
  skip_on_cran()
  # Test 1: Basic Fetch
  fetch_results <- suppressWarnings(fetch_finna(query = "record_format:ead", limit = 10))
  expect_true(is.data.frame(fetch_results), "The result should be a data frame.")
  expect_gt(nrow(fetch_results), 0, "The number of rows should be greater than 0.")
  expect_true("value" %in% names(fetch_results), "The result should contain a 'value' column.")
  expect_true("translated" %in% names(fetch_results), "The result should contain a 'translated' column.")
  expect_true("count" %in% names(fetch_results), "The result should contain a 'count' column.")
  expect_true("href" %in% names(fetch_results), "The result should contain an 'href' column.")
})

test_that("fetch_finna handles invalid query gracefully", {
  # Test 2: Invalid Query
  expect_error(fetch_finna(query = ""), "Invalid query: Query string cannot be empty.")
})

test_that("fetch_finna handles empty facet data", {
  # Test 3: Fetch with empty facet data
  fetch_results_empty <- suppressWarnings(fetch_finna(query = "nonexistent_query"))
  expect_true(is.data.frame(fetch_results_empty), "The result should be a data frame even for nonexistent queries.")
  expect_true(nrow(fetch_results_empty) >= 0, "The number of rows should reflect the API response.")
})

test_that("fetch_finna handles parameters correctly", {
  # Test 4: Check different parameters and ensure correct handling
  fetch_results_params <- suppressWarnings(fetch_finna(query = "record_format:ead", limit = 5))
  expect_true(is.data.frame(fetch_results_params), "The result should be a data frame.")
  expect_lte(nrow(fetch_results_params), 5, "The number of rows should not exceed the limit.")
  expect_true("value" %in% names(fetch_results_params), "The result should contain a 'value' column.")
  expect_true("translated" %in% names(fetch_results_params), "The result should contain a 'translated' column.")
  expect_true("count" %in% names(fetch_results_params), "The result should contain a 'count' column.")
  expect_true("href" %in% names(fetch_results_params), "The result should contain an 'href' column.")
})
