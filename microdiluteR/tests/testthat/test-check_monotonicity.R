test_that("check_monotonicity() correctly distinguishes between monotonic and non-monotonic numeric vectors", {
  # Define test data
  monotonic_vec <- c(1,2,3)
  nonmonotonic_vec <- c(11,2,3)
  
  # Call function
  result_TRUE <- check_monotonicity(monotonic_vec)
  result_FALSE <- check_monotonicity(nonmonotonic_vec)

  # Run test
  expect_equal(result_TRUE, TRUE)
  expect_equal(result_FALSE, FALSE)
})
