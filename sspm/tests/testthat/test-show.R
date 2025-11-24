# Test the show methods

test_that("Show methods print out fine", {

  expect_output(show(boundary))
  expect_output(show(boundary_discrete))
  expect_output(show(biomass_dataset))
  expect_output(show(biomass_dataset_smoothed))
  expect_output(show(sspm_formula))
  expect_output(show(discret_method))
  expect_output(show(sspm_model))
  expect_output(show(sspm_fit))

})
