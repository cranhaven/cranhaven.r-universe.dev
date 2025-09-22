test_that("summary class constructor works correctly", {
  summary_one <- summary$new("filter1", c(1, 2, 3), c(4, 5, 6))

  expect_equal(summary_one$get_filter(), "filter1")
  expect_equal(summary_one$get_failed_ions(), c(1, 2, 3))
  expect_equal(summary_one$get_passed_ions(), c(4, 5, 6))

  # Test for empty constructors too
  summary_two <- summary$new("filter1", NULL, NULL)
  expect_equal(summary_two$get_filter(), "filter1")
  expect_equal(summary_two$get_failed_ions(), c())
  expect_equal(summary_two$get_passed_ions(), NULL)
})
