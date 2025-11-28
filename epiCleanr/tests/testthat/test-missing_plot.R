# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Sample data for testing
fake_data <- tidyr::expand_grid(
  state = state.name,
  month = 1:12, year = 2000:2023
) |>
  dplyr::mutate(
    measles = sample(0:1000, size = dplyr::n(), replace = TRUE),
    polio = sample(0:500, size = dplyr::n(), replace = TRUE),
    malaria = sample(0:300, size = dplyr::n(), replace = TRUE),
    cholera = sample(0:700, size = dplyr::n(), replace = TRUE)
  ) |>
  dplyr::mutate(across(
    c(measles, polio, malaria, cholera),
    ~ replace(., sample(1:dplyr::n(),
      size = dplyr::n() * 0.4
    ), NA)
  ))

# Test 1: Check for error if 'x_var' is NULL or does not exist in the data
testthat::test_that("Error is thrown if 'x_var' is NULL or not in data", {
  testthat::expect_error(
    epiCleanr::missing_plot(fake_data, NULL),
    "A valid 'x_var' must be provided"
  )
  testthat::expect_error(
    epiCleanr::missing_plot(fake_data, "invalid_x_var"),
    "A valid 'x_var' must be provided"
  )
})

# Test 2: Check error if 'y_var' provided with >1 variable in 'miss_vars'
testthat::test_that("Error if 'y_var' provided with >1 vars in 'miss_vars'", {
  testthat::expect_error(
    epiCleanr::missing_plot(
      fake_data, "year", "state",
      c("polio", "measles")
    ),
    "When 'y_var' is provided only one variable can be specified in 'miss_vars'"
  )
})

# Test 3: Check that the function returns a ggplot object
testthat::test_that("Returns ggplot object", {
  plot <- epiCleanr::missing_plot(fake_data, "year", "state", "polio", TRUE)
  expect_s3_class(plot, "ggplot")
})

# Test 4: Check that the function works without 'y_var' and 'miss_vars'
testthat::test_that("Works without 'y_var' and 'miss_vars'", {
  plot <- epiCleanr::missing_plot(fake_data, "year")
  expect_s3_class(plot, "ggplot")
})

# Test 5: Check that the function works with all provided parameters
testthat::test_that("Works with all provided parameters", {
  plot <- epiCleanr::missing_plot(fake_data, "year", "state", "polio", TRUE)
  expect_s3_class(plot, "ggplot")
})

# Test 6: Check the title construction with 'use_rep_rate = TRUE'
testthat::test_that("Title construction with 'use_rep_rate = TRUE'", {
  plot <- epiCleanr::missing_plot(fake_data, "year", "state", "polio", TRUE)
  testthat::expect_match(
    plot$labels$title, "Reporting rate of polio by year and state"
  )
})

# Test 7: Check the title construction with 'use_rep_rate = FALSE'
testthat::test_that("Title construction with 'use_rep_rate = FALSE'", {
  plot <- epiCleanr::missing_plot(fake_data, "year", "state", "polio", FALSE)
  testthat::expect_match(
    plot$labels$title,
    "The proportion of missing data for polio by year and state"
  )
})

# Test 8: Check the title construction without 'y_var'
testthat::test_that("Title construction without 'y_var'", {
  plot <- epiCleanr::missing_plot(fake_data, "year", NULL, "polio", TRUE)
  testthat::expect_match(plot$labels$title, "Reporting rate of polio by year")
})

# Test 9: Check the y-axis label with 'y_var'
testthat::test_that("Y-axis label with 'y_var'", {
  plot <- epiCleanr::missing_plot(
    fake_data, "year", "state", "polio", FALSE
  )
  expect_equal(plot$labels$y, "State")
})

# Test 10: Check title construction with and without 'miss_vars'"
testthat::test_that("Title construction with and without 'miss_vars'", {
  # Case when 'miss_vars' is provided
  plot_with_vars <- epiCleanr::missing_plot(
    fake_data, "year", "state", "polio", FALSE
  )
  expect_true(
    grepl(
      "The proportion of missing data for polio by year and state",
      plot_with_vars$labels$title
    )
  )

  # Case when 'miss_vars' is not provided
  plot_without_vars <- epiCleanr::missing_plot(
    fake_data,
    x_var = "year", miss_vars = NULL, use_rep_rate = FALSE
  )
  expect_true(grepl("year", plot_without_vars$labels$title))
})

# Test 11: Check title construction when there are more than one var
testthat::test_that("Title is constructed correctly with remaining variables", {
  # Call your function with appropriate arguments
  plot <- epiCleanr::missing_plot(dplyr::select(fake_data, -polio),
    x_var = "year", miss_vars = NULL,
    use_rep_rate = FALSE
  )

  # Construct the expected title
  expected_title <-
    paste(
      "The proportion of missing data for state,",
      "month, measles, malaria and cholera by year"
    )

  # Assert that the actual title matches the expected title
  expect_identical(plot$labels$title, expected_title)
})

# Test 12: Check title construction when there are too many variables
testthat::test_that("Title construction when there are too many variables", {
  plot <- epiCleanr::missing_plot(fake_data, "year", NULL, NULL, FALSE)
  testthat::expect_match(
    plot$labels$title,
    "The proportion of missing data for various variables by year"
  )
})

