test_that("process_url warns on dummy URL", {
  # Provide a dummy URL that is expected to trigger a warning
  dummy_url <- "http://thisisadummyurl.com"
  expected_warning <- "Failed to fetch data from API: Could not resolve host: thisisadummyurl.com"

  expect_warning(api_call_wrapper(dummy_url), expected_warning)
})

