# Test accessors
#
# test_that("Accessors work as expected on ``", {
#
# })

test_that("Accessors work as expected on `sspm_boundary` (discrete or not)", {

  expect_equal(spm_boundaries(boundary), sfa_boundaries)

  expect_equal(spm_boundary(boundary), "sfa")

  expect_equal(spm_boundary_area(boundary), "area")

  expect_equal(spm_patches_area(boundary_discrete), "patch_area")

  expect_equal(spm_points(boundary_discrete), borealis_points)

  expect_equal(spm_patches(boundary_discrete), borealis_patches)

  expect_equal(spm_discret_method(boundary_discrete), discret_method)

})

test_that("Accessors work as expected on `sspm_dataset` (smoothed or not)", {

  expect_data_frame(spm_data(biomass_dataset))
  expect_data_frame(spm_data(biomass_dataset_smoothed))

  expect_character(spm_unique_ID(biomass_dataset))
  expect_length(spm_unique_ID(biomass_dataset), 1)

  expect_character(spm_unique_ID(biomass_dataset_smoothed))
  expect_length(spm_unique_ID(biomass_dataset_smoothed), 1)

  expect_character(spm_coords_col(biomass_dataset))
  expect_length(spm_coords_col(biomass_dataset), 2)

  expect_character(spm_coords_col(biomass_dataset_smoothed))
  expect_length(spm_coords_col(biomass_dataset_smoothed), 2)

  expect_character(spm_time(biomass_dataset))
  expect_length(spm_time(biomass_dataset), 1)

  expect_character(spm_time(biomass_dataset_smoothed))
  expect_length(spm_time(biomass_dataset_smoothed), 1)

  expect_class(spm_boundaries(biomass_dataset), "sspm_boundary")
  expect_class(spm_boundaries(biomass_dataset_smoothed), "sspm_discrete_boundary")

  expect_null(spm_smoothed_data(biomass_dataset))
  expect_data_frame(spm_smoothed_data(biomass_dataset_smoothed))

  expect_list(spm_smoothed_fit(biomass_dataset))
  expect_length(spm_smoothed_fit(biomass_dataset), 0)
  expect_list(spm_smoothed_fit(biomass_dataset_smoothed))
  expect_length(spm_smoothed_fit(biomass_dataset_smoothed), 1)

  expect_length(spm_formulas(biomass_dataset), 0)
  expect_list(spm_formulas(biomass_dataset_smoothed))
  expect_length(spm_formulas(biomass_dataset_smoothed), 1)

  expect_null(spm_biomass_vars(biomass_dataset))
  expect_equal(spm_biomass_vars(catch_dataset), "catch")
  expect_equal(spm_density_vars(biomass_dataset), "weight_per_km2")

})

test_that("Accessors work as expected on `sspm`", {

  expect_class(spm_boundaries(sspm_model), "sspm_discrete_boundary")

  expect_list(spm_datasets(sspm_model))
  expect_length(spm_datasets(sspm_model), 3)

  expect_data_frame(spm_smoothed_data(sspm_model))

  expect_class(spm_boundaries(sspm_model), "sspm_discrete_boundary")

  expect_false(is_split(sspm_model))

  expect_character(spm_unique_ID(sspm_model))

})

test_that("Accesors work as expected on `discretization_method`", {

  expect_character(spm_name(discret_method))
  expect_function(method_func(discret_method))

})

test_that("Accesors work as expected on `sspm_formula`", {

  expect_formula(raw_formula(sspm_formula))
  expect_formula(translated_formula(sspm_formula))
  expect_list(formula_vars(sspm_formula))
  expect_character(formula_type(sspm_formula))

})

test_that("Accesors work as expected on `sspm_fit`", {

  expect_character(spm_unique_ID(sspm_fit))
  expect_character(spm_time(sspm_fit))
  expect_class(spm_get_fit(sspm_fit), "bam")

})

