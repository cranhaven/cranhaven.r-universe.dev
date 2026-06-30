test_that("search_publisher performs a basic search correctly", {
  skip_on_cran()
  # Test 1: Basic Search
  search_results <- suppressWarnings(search_publisher("sibelius", limit = 10))
  expect_true(is.data.frame(search_results))
  expect_gt(nrow(search_results), 0)
  expect_true("id" %in% names(search_results))
  expect_true("Publisher" %in% names(search_results))

  # Test 2: Check error handling with an invalid query
  search_results_error <- suppressWarnings(search_publisher(""))
  expect_null(search_results_error) # Expect NULL because the query is invalid

  # Test 3: Search with additional filters
  search_results_filtered <- suppressWarnings(search_publisher("sibelius", filters = c("search_daterange_mv:[1900 TO 1950]")))
  expect_true(is.data.frame(search_results_filtered))
  expect_gt(nrow(search_results_filtered), 0)

  # Test 4: Check different parameters, ensure that the number of rows returned is <= limit
  search_results_params <- suppressWarnings(search_publisher("sibelius", limit = 5, lng = "en"))
  expect_true(is.data.frame(search_results_params))
  expect_lte(nrow(search_results_params), 5) # The number of results should be less than or equal to the limit
})
