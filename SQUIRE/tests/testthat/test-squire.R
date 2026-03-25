# Basic tests for SQUIRE package

test_that("SQUIRE function exists and has correct parameters", {
  expect_true(exists("SQUIRE"))
  
  # Check function signature
  squire_formals <- names(formals(SQUIRE))
  expected_params <- c("data", "treatments", "control_treatment", 
                       "validation_level", "min_timepoints", "min_replicates", "verbose")
  expect_true(all(expected_params %in% squire_formals))
})

test_that("SQUIRE handles missing data gracefully", {
  # Test with minimal invalid data
  bad_data <- data.frame(
    time = c(1, 2),
    treatment = c("A", "B"),
    replicate = c(1, 1),
    response = c(10, 20)
  )
  
  expect_no_error({
    result <- SQUIRE(bad_data, treatments = c("A", "B"), verbose = FALSE)
  })
  
  # Should not perform optimization with insufficient data
  result <- SQUIRE(bad_data, treatments = c("A", "B"), verbose = FALSE)
  expect_false(result$optimization_performed)
})

test_that("SQUIRE validates input data structure", {
  # Test with completely wrong data structure
  expect_error({
    SQUIRE(data.frame(x = 1:5), treatments = "A", verbose = FALSE)
  })
})
