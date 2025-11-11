# Tests for threatenedperu package
# Test main API function
test_that("is_threatened_peru works correctly", {
  # Test with known threatened species
  test_species <- c("Cattleya maxima", "Masdevallia veitchiana", "Unknown species")

  results <- is_threatened_peru(test_species)

  expect_length(results, 3)
  expect_equal(results[1], "CR")
  expect_equal(results[2], "VU")
  expect_equal(results[3], "Not threatened")

  # Test with return_details = TRUE
  detailed_results <- is_threatened_peru(test_species, return_details = TRUE)
  expect_s3_class(detailed_results, "data.frame")
  expect_true("Threat.Status" %in% names(detailed_results))
  expect_true("Matched.Name" %in% names(detailed_results))
})

