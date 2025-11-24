# Test sspm functions

test_that("sspm object are created correcly", {

  sspm_model <- sspm(biomass = biomass_dataset_smoothed,
                     predictors = predator_dataset_smoothed)
  expect_class(sspm_model, "sspm")

  expect_class({
    sspm(biomass = biomass_dataset_smoothed)
  }, "sspm")

  sspm_model_split <- sspm_model %>%
    spm_split(year %in% c(1990:2017))
  expect_class(sspm_model_split, "sspm")
  expect_true(is_split(sspm_model_split))

  sspm_model_lag <- sspm_model_split %>%
    spm_lag(vars = c("weight_per_km2_biomass_test",
                     "weight_per_km2_predator_test"),
            n = 1)

  expect_class(sspm_model_lag, "sspm")

})
