# Extract methods

test_that("Extractors work as expected", {

  expect_class(boundary$sfa, "sf")
  expect_class(boundary_discrete$sfa, "sf")
  expect_class(biomass_dataset$sfa, "sf")
  expect_class(biomass_dataset_smoothed$sfa, "sf")
  expect_class(sspm_model$sfa, "sf")

})
