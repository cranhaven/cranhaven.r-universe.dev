test_that("search_finna performs a basic search correctly", {
  skip_on_cran()
  # Test 1: Basic Search
  search_results <- suppressWarnings(search_finna("sibelius", type = "Author"))
  expect_true(is.data.frame(search_results), "The result should be a data frame.")
  expect_gt(nrow(search_results), 0, "The number of rows should be greater than 0.")
  expect_true("Title" %in% names(search_results), "The result should contain a 'Title' column.")
  expect_true("Author" %in% names(search_results), "The result should contain an 'Author' column.")
  expect_true("Year" %in% names(search_results), "The result should contain a 'Year' column.")

  # Test 2: Check handling of invalid query
  search_results_error <- suppressWarnings(search_finna(NULL))
  expect_true(is.data.frame(search_results_error), "The result for an invalid query should be a data frame.")
  expect_gt(nrow(search_results_error), 0, "The number of rows for an invalid query should be greater than 0.")
  expect_true("Title" %in% names(search_results_error), "The result should contain a 'Title' column even for an invalid query.")

  # Test 3: Search with additional filters
  search_results_filtered <- suppressWarnings(search_finna("sibelius", filters = c("search_daterange_mv:[1900 TO 1950]")))
  expect_true(is.data.frame(search_results_filtered), "The filtered result should be a data frame.")
  expect_gt(nrow(search_results_filtered), 0, "The filtered result should contain records.")

  # Test 4: Check different parameters, ensure that the number of rows returned is <= limit
  search_results_params <- suppressWarnings(search_finna("sibelius", type = "Title", limit = 100, lng = "en-gb"))
  expect_true(is.data.frame(search_results_params))
  expect_gte(nrow(search_results_params), 100) # The number of results should be less than or equal to the limit
  expect_equal(attr(search_results_params, "language"), "en-gb")

  # Test 5: Validate result_count attribute
  search_results_large <- suppressWarnings(search_finna("sibelius", limit = 200))
  expect_true(!is.null(attr(search_results_large, "result_count")), "The result should include a result_count attribute.")
  expect_gt(attr(search_results_large, "result_count"), 0, "The result_count should be greater than 0.")

  # Test 6: Validate facet functionality
  search_results_geofilter <- suppressWarnings(search_finna(
    "trump",
    filters = c('{!geofilt sfield=location_geo pt=61.663987171517796,24.17263895273209 d=212.53603751769646},author_facet:"Häkkinen,Hannu"')
  ))
  expect_true(is.data.frame(search_results_geofilter), "The geofiltered result should be a data frame.")
  expect_gt(nrow(search_results_geofilter), 0, "The geofiltered result should contain records.")
  expect_true("Title" %in% names(search_results_geofilter), "The geofiltered result should contain a 'Title' column.")
  expect_true("Author" %in% names(search_results_geofilter), "The geofiltered result should contain an 'Author' column.")
})

