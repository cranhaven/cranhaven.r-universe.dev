test_that("match_treatment() correctly assigns treatments to well positions", {
  # Define test data
  treatment_list <- list(
    A1 = "A",
    B2 = "B",
    C3 = "C"
  )
  
  # Generate cases: well positions and expected concentrations
  cases <- list(
    list(well_position = "A1", expected_treatment = "A"),
    list(well_position = "B2", expected_treatment = "B"),
    list(well_position = "C3", expected_treatment = "C"),
    list(well_position = "D4", expected_treatment = "NA") # Not in treatment_list
  )
  
  # Test cases
  for (i in seq_along(cases)) {
    result <- match_treatment(cases[[i]]$well_position, treatment_list)
    expect_equal(result, cases[[i]]$expected_treatment)
  }
})
