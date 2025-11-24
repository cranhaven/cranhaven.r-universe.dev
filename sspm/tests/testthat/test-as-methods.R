# Testing as_object method

test_that("Discret method is casted correctly", {

  method <- "tesselate_voronoi"
  expect_class(as_discretization_method(method), "discretization_method")

  method <- "method_not_supported"
  expect_error(as_discretization_method(method),
               "Method 'method_not_supported' is not part of the supported methods.")

})

test_that("Bounds method is casted correctly", {

  expect_class(spm_as_boundary(sfa_boundaries, "sfa"), "sspm_boundary")
  expect_class(spm_as_boundary(sfa_boundaries, "sfa",
                               boundary_area = "area"), "sspm_boundary")
  expect_class(spm_as_boundary(sfa_boundaries, "sfa",
                               patches = borealis_patches,
                               patch_area = "patch_area"), "sspm_discrete_boundary")

  expect_error(spm_as_boundary(sfa_boundaries, "sfa",
                               patches = borealis_patches),
               "`patch_area` column already present, please cast with argument `patch_area")
  expect_error(spm_as_boundary(sfa_boundaries, "bad_column"),
               "`boundary` must be a column of `boundaries`")
  expect_error(spm_as_boundary(sfa_boundaries, "sfa",
                               boundary_area = "bad_column"),
               "`boundary_area` must be a column of `boundaries`")

})

test_that("sspm dataset is casted correctly", {

  # Test the 3 generic cases
  expect_error({
    spm_as_dataset(data = borealis_simulated,
                   time = "year_f",
                   coords = c('lon_dec', 'lat_dec'),
                   name = "Biomass",
                   uniqueID = "Bad column")
  }, "`uniqueID` must be a column of `data`")

  borealis_simulated_NU <- borealis_simulated
  borealis_simulated_NU$new_col <- "Non_unique"
  expect_error({
    spm_as_dataset(data = borealis_simulated_NU,
                   time = "year_f",
                   coords = c('lon_dec', 'lat_dec'),
                   name = "Biomass",
                   uniqueID = "new_col")
  }, "`uniqueID` must be unique for each row of `data`")

  expect_error({
    spm_as_dataset(data = borealis_simulated,
                   time = "Bad column",
                   coords = c('lon_dec', 'lat_dec'),
                   name = "Biomass",
                   uniqueID = "uniqueID")
  }, "`time` must be a column of `data`")

  # If data matrix is df, coords must be provided
  expect_error({
    spm_as_dataset(data = borealis_simulated,
                   time = "year_f",
                   name = "Biomass",
                   uniqueID = "uniqueID")
  }, "Argument `coords` must be provided when data matrix is a dataframe")

  # Coords must be columns of data
  expect_error({
    spm_as_dataset(data = borealis_simulated,
                   time = "year_f",
                   coords = c('Bad column 1', 'Bad column 2'),
                   name = "Biomass",
                   uniqueID = "uniqueID")
  }, "`coords` must be columns of `data`")

  # When works fine
  expect_class({
    spm_as_dataset(data = borealis_simulated,
                   time = "year_f",
                   coords = c('lon_dec', 'lat_dec'),
                   name = "Biomass",
                   uniqueID = "uniqueID")
  }, "sspm_dataset")

  expect_class({
    spm_as_dataset(data = borealis_simulated,
                   time = "year_f",
                   coords = list('lon_dec', 'lat_dec'),
                   name = "Biomass",
                   uniqueID = "uniqueID")
  }, "sspm_dataset")

  expect_class({
    spm_as_dataset(data = borealis_spatial,
                   time = "year_f",
                   coords = c('lon_dec', 'lat_dec'),
                   name = "Biomass",
                   uniqueID = "uniqueID")
  }, "sspm_dataset")

  test_polygons <- borealis_patches %>%
    mutate(year_f = as.factor("2000"),
           uniqueID = 1:nrow(borealis_patches))

  expect_class({
    spm_as_dataset(data = test_polygons,
                   time = "year_f",
                   name = "Biomass",
                   uniqueID = "uniqueID")
  }, "sspm_dataset")

})
