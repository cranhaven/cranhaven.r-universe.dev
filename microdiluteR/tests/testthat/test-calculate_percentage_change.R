test_that("calculate_percentage_change() correctly calculates percentage change", {
  # Define test cases
  test_cases <- list(
    list(input = 10, reference = 5, expected_change = 100),
    list(input = 5, reference = 10, expected_change = -50),
    list(input = 0, reference = 0, expected_change = 0)
  )
  
  # Run tests
  for (cases in seq_along(test_cases)) {
    input <- test_cases[[cases]]$input
    reference <- test_cases[[cases]]$reference
    expected_change <- test_cases[[cases]]$expected_change
    
    # Call function
    result <- calculate_percentage_change(input, reference)
    
    # Test
    expect_equal(result, expected_change)
  }
})