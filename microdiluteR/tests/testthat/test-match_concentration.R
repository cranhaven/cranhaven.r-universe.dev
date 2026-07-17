test_that("match_concentration() correctly assigns concentrations to well positions", {
  # Define test data
  concentration_list <- list(
    A1 = 0.1,
    B2 = 0.2,
    C3 = 0.3
  )
  
  # Generate cases: well positions and expected concentrations
  cases <- list(
    list(well_position = "A1", expected_concentration = 0.1),
    list(well_position = "B2", expected_concentration = 0.2),
    list(well_position = "C3", expected_concentration = 0.3),
    list(well_position = "D4", expected_concentration = "NA") # Not in concentration_list
  )
  
  # Test cases
  for (i in seq_along(cases)) {
    result <- match_concentration(cases[[i]]$well_position, concentration_list)
    expect_equal(result, cases[[i]]$expected_concentration)
  }
})
