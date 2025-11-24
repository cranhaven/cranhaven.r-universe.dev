# Test aggregation

test_that("Aggregation works as expected", {

  expect_class({
    spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
                  fun = mean, type = "smoothed", group_by = "time")
  }, "sspm_dataset")

  expect_class({
    spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
                  fun = mean, type = "smoothed", group_by = "space",
                  level = "boundary")
  }, "sspm_dataset")

  # expect_class({
  #   spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
  #                 fun = mean, type = "smoothed", group_by = "space",
  #                 apply_to_df = TRUE)
  # }, "sspm_dataset")

  expect_class({
    spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
                  fun = mean, type = "smoothed", group_by = "space")
  }, "sspm_dataset")

  # expect_class({
  #   spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
  #                 fun = mean, type = "smoothed", group_by = "space",
  #                 fill = NA)
  # }, "sspm_dataset")

  # expect_class({
  #   spm_aggregate(biomass_dataset_smoothed, variable = "weight_per_km2",
  #                 fun = mean, type = "smoothed", group_by = "space",
  #                 fill = 0)
  # }, "sspm_dataset")

})
