# Complete GLOSSA analysis

test_that("GLOSSA analysis", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "sp1.txt", package="glossa")
  file2 <- system.file("extdata", "sp2.csv", package="glossa")
  file3 <- system.file("extdata", "fit_layers.zip", package="glossa")
  file4 <- system.file("extdata", "project_layers_1.zip", package="glossa")
  file5 <- system.file("extdata", "oceania.gpkg", package="glossa")

  # Load data
  pa_data  <-  list("sp1" = glossa::read_presences_absences_csv(file_path = file1, file_name = "sp1"),
                    "sp2" = glossa::read_presences_absences_csv(file_path = file2, file_name = "sp2"))
  fit_layers <- file3
  proj_files <- list("proj1" = file4)
  study_area_poly <- glossa::read_extent_polygon(file_path = file5)
  predictor_variables <- NULL
  decimal_digits <- 4
  buffer <- 0.1
  scale_layers <- TRUE
  native_range <- c("fit_layers", "projections")
  suitable_habitat <- c("fit_layers", "projections")
  other_analysis <- c("variable_importance", "functional_responses", "cross_validation")
  seed <- 123
  waiter <- NULL

  glossa_results <- suppressWarnings(glossa::glossa_analysis(
    pa_data = pa_data,
    fit_layers = fit_layers,
    proj_files = proj_files,
    study_area_poly = study_area_poly,
    predictor_variables = predictor_variables,
    decimal_digits = decimal_digits,
    scale_layers = scale_layers,
    buffer = buffer,
    native_range = native_range,
    suitable_habitat = suitable_habitat,
    other_analysis = other_analysis,
    seed = seed,
    waiter = waiter
  ))

  expect_type(glossa_results, "list")
  expect_s3_class(glossa_results$presence_absence_list$model_pa[[1]], "data.frame")
  expect_s4_class(glossa_results$covariate_list$fit_layers, "SpatRaster")
  expect_s4_class(glossa_results$projections_results$fit_layers$suitable_habitat[[1]], "SpatRaster")
  expect_s3_class(glossa_results$other_results$variable_importance$suitable_habitat[[1]], "data.frame")
  expect_s3_class(glossa_results$other_results$cross_validation$suitable_habitat[[1]], "data.frame")
  expect_type(glossa_results$pa_cutoff$suitable_habitat[[1]], "double")
  expect_type(glossa_results$habitat_suitability$fit_layers$covered_area[[1]], "double")
})
