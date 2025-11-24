# Test replacers for the sake of code coverage

test_that("Replacers work as expected", {

  # Boundaries

  expect_equal({
    spm_boundaries(boundary) <- sfa_boundaries
    spm_boundaries(boundary)
  }, sfa_boundaries)

  expect_equal({
    spm_boundary(boundary) <- "new_col"
    spm_boundary(boundary)
  }, "new_col")

  expect_equal({
    spm_boundary_area(boundary) <- "new_col_2"
    spm_boundary_area(boundary)
  }, "new_col_2")

  expect_equal({
    discret_method_2 <- discret_method
    spm_name(discret_method_2) <- "triangulate_delaunay"
    spm_discret_method(boundary_discrete) <- discret_method_2
    spm_discret_method(boundary_discrete)
  }, discret_method_2)

  expect_equal({
    spm_points(boundary_discrete) <- sfa_boundaries
    spm_points(boundary_discrete)
  }, sfa_boundaries)

  expect_equal({
    spm_patches(boundary_discrete) <- sfa_boundaries
    spm_patches(boundary_discrete)
  }, sfa_boundaries)

  expect_equal({
    spm_patches_area(boundary_discrete) <- "new_col_3"
    spm_patches_area(boundary_discrete)
  }, "new_col_3")

  # SSPM

  expect_class({
    spm_boundaries(sspm_model) <- boundary_discrete
    spm_boundaries(sspm_model)
  }, "sspm_discrete_boundary")

  expect_class({
    spm_datasets(sspm_model) <- list()
    spm_datasets(sspm_model)
  }, "list")

  expect_equal({
    spm_time(sspm_model) <- "time_col"
    spm_time(sspm_model)
  }, "time_col")

  expect_equal({
    spm_unique_ID(sspm_model) <- "ID"
    spm_unique_ID(sspm_model)
  }, "ID")

  expect_equal({
    spm_smoothed_data(sspm_model) <- mtcars
    spm_smoothed_data(sspm_model)
  }, mtcars)

  expect_true({
    is_split(sspm_model) <- TRUE
    is_split(sspm_model)
  })

  # Method

  expect_match({
    spm_name(discret_method) <- "New_Name"
    spm_name(discret_method)
  }, "New_Name")

  expect_function({
    method_func(discret_method) <- rnorm
    method_func(discret_method)
  })

  # Formula

  expect_match({
    sspm:::format_formula(raw_formula(sspm_formula) <- as.formula(a ~ b))
  }, "a ~ b")

  expect_match({
    sspm:::format_formula(translated_formula(sspm_formula) <- as.formula(c ~ d))
  }, "c ~ d")

  expect_names({
    formula_vars(sspm_formula) <- list(a = 1, b = 2)
    names(formula_vars(sspm_formula))
  }, identical.to = c("a", "b"))

  expect_match({
    formula_type(sspm_formula) <- "my_type"
    formula_type(sspm_formula)
  }, "my_type")

  expect_match({
    spm_lagged_vars(sspm_formula) <- "lag_vars"
    spm_lagged_vars(sspm_formula)
  }, "lag_vars")

  expect_match({
    spm_response(sspm_formula) <- "response"
    spm_response(sspm_formula)
  }, "response")

  # Dataset

  expect_data_frame({
    spm_data(biomass_dataset) <- mtcars
    spm_data(biomass_dataset)
  })

  expect_equal({
    spm_smoothed_data(biomass_dataset) <- mtcars
    spm_smoothed_data(biomass_dataset)
  }, mtcars)

  expect_match({
    spm_name(biomass_dataset) <- "NewDatasetName_2"
    spm_name(biomass_dataset)
  }, "NewDatasetName_2")

  expect_match({
    spm_coords_col(biomass_dataset) <- c("one_2", "two_2")
    spm_coords_col(biomass_dataset)[1]
  }, c("one_2"))

  expect_match({
    spm_coords_col(biomass_dataset) <- c("one_2", "two_2")
    spm_coords_col(biomass_dataset)[2]
  }, c("two_2"))

  expect_match({
    spm_unique_ID(biomass_dataset) <- "New_ID_2"
    spm_unique_ID(biomass_dataset)
  }, "New_ID_2")

  expect_match({
    spm_time(biomass_dataset) <- "new_time_2"
    spm_time(biomass_dataset)
  }, "new_time_2")

  expect_equal({
    gam_fit <- list(mgcv::gam(mtcars$mpg~mtcars$disp,
                              family = gaussian))
    spm_smoothed_fit(biomass_dataset_smoothed) <- gam_fit
    spm_smoothed_fit(biomass_dataset_smoothed)
  }, gam_fit)

  expect_equal({
    spm_formulas(biomass_dataset_smoothed) <- list(sspm_formula)
    spm_formulas(biomass_dataset_smoothed)
  }, list(sspm_formula))

  expect_equal({
    spm_biomass_vars(catch_dataset) <- c("catch", "catch_replaced")
    spm_biomass_vars(catch_dataset)
  }, c("catch", "catch_replaced"))

  expect_equal({
    spm_density_vars(biomass_dataset) <- c("weight_per_km2",
                                         "weight_per_km2_replaced")
    spm_density_vars(biomass_dataset)
  }, c("weight_per_km2",
       "weight_per_km2_replaced"))

  ## Fit

  expect_equal({
    spm_unique_ID(sspm_fit) <- "uniqueID"
    spm_unique_ID(sspm_fit)
  }, "uniqueID")

  expect_equal({
    spm_time(sspm_fit) <- "timevar"
    spm_time(sspm_fit)
  }, "timevar")

  expect_equal({
    spm_formulas(sspm_fit) <- sspm_formula
    spm_formulas(sspm_fit)
  }, sspm_formula)

  expect_equal({
    spm_get_fit(sspm_fit) <- fit_bam
    spm_get_fit(sspm_fit)
  }, fit_bam)

  expect_equal({
    spm_boundaries(sspm_fit) <- spm_boundaries(biomass_dataset_smoothed)
    spm_boundaries(sspm_fit)
  }, spm_boundaries(biomass_dataset_smoothed))

  expect_equal({
    spm_boundary(sspm_fit) <- "sfa"
    spm_boundary(sspm_fit)
  }, "sfa")

})
