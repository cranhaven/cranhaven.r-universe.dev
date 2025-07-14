## ----eval=FALSE, include=TRUE-------------------------------------------------
# # install.packages("AgePopDenom")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("truenomad/AgePopDenom")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# library(AgePopDenom)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# init(
#   r_script_name = "full_pipeline.R",
#   cpp_script_name = "model.cpp"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# download_dhs_datasets(
#   country_codes = c("GMB"),
#   email = "my_email@example.com",
#   project = "Population project"
# )
# 
# process_dhs_data()

## ----eval=FALSE, include=TRUE-------------------------------------------------
# download_shapefile("GMB")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# download_pop_rasters("GMB")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# extract_afurextent()

## ----eval=FALSE, include=TRUE-------------------------------------------------
# run_full_workflow("GMB")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# fit_spatial_model(
#   country_code,
#   data,
#   scale_outcome = "log_scale",
#   shape_outcome = "log_shape",
#   covariates = "urban",
#   cpp_script_name = "02_scripts/model",
#   output_dir = "03_outputs/3a_model_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# control_params = list(
#   trace = 3,        # Higher values show more optimization details
#   maxit = 2000,     # Increase for complex spatial structures
#   abs.tol = 1e-10,  # Stricter convergence criteria
#   rel.tol = 1e-8    # Relative convergence tolerance
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# fit_spatial_model(
#   data = survey_data,
#   scale_outcome = "log_scale",
#   shape_outcome = "log_shape",
#   covariates = "urban",
#   cpp_script_name = "02_scripts/model",
#   manual_params = list(
#     beta1 = c(0.5, -0.3),
#     beta2 = c(0.2, 0.1),
#     gamma = 0.8,
#     log_sigma2 = log(0.5),
#     log_phi = log(100),
#     log_tau2_1 = log(0.1)
#   ),
#   control_params = list(
#     trace = 3,
#     maxit = 2000,
#     abs.tol = 1e-10
#   )
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_variogram_plot(
#   age_param_data,
#   fit_vario,
#   country_code,
#   scale_outcome = "log_scale",
#   output_dir = "03_outputs/3b_visualizations",
#   width = 12,
#   height = 9,
#   png_resolution = 300
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# create_prediction_data(
#   country_code,
#   country_shape,
#   pop_raster,
#   ur_raster,
#   adm2_shape,
#   cell_size = 5000,
#   ignore_cache = FALSE,
#   output_dir = "03_outputs/3a_model_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_gamma_predictions(
#   country_code,
#   age_param_data,
#   model_params,
#   predictor_data,
#   shapefile,
#   cell_size = 5000,
#   n_sim = 5000,
#   ignore_cache = FALSE,
#   output_dir = "03_outputs/3a_model_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_gamma_raster_plot(
#   predictor_data,
#   pred_list,
#   country_code,
#   output_dir = "03_outputs/3b_visualizations",
#   save_raster = TRUE
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_age_pop_table(
#   predictor_data,
#   scale_pred,
#   shape_pred,
#   country_code,
#   age_range = c(0, 99),
#   age_interval = 1,
#   ignore_cache = FALSE,
#   output_dir = "03_outputs/3c_table_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_age_pyramid_plot(
#   dataset,
#   country_code,
#   output_dir = "03_outputs/3b_visualizations"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# process_final_population_data(
#   input_dir = "03_outputs/3c_table_outputs",
#   excel_output_file = "03_outputs/3d_compiled_results/age_pop_denom_compiled.xlsx"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# init(
#   r_script_name = "full_pipeline.R",
#   cpp_script_name = "model.cpp",
#   open_r_script = FALSE
# )
# 
# # set up country code
# cntry_code = "GMB"
# 
# # Gather and process datasets ---------------------------------------
# 
# # Set parameters for simulation
# total_population <- 266
# urban_proportion <- 0.602
# total_coords <- 266
# lon_range <- c(-16.802, -13.849)
# lat_range <- c(13.149, 13.801)
# mean_web_x <- -1764351
# mean_web_y <- 1510868
# 
# # Simulate processed survey dataset for Gambia
# set.seed(123)
# df_gambia <- NULL
# df_gambia$age_param_data <- dplyr::tibble(
#   country = "Gambia",
#   country_code_iso3 = "GMB",
#   country_code_dhs = "GM",
#   year_of_survey = 2024,
#   id_coords = rep(1:total_coords, length.out = total_population),
#   lon = runif(total_population, lon_range[1], lon_range[2]),
#   lat = runif(total_population, lat_range[1], lat_range[2]),
#   web_x = rnorm(total_population, mean_web_x, 50000),
#   web_y = rnorm(total_population, mean_web_y, 50000),
#   log_scale = rnorm(total_population, 2.82, 0.2),
#   log_shape = rnorm(total_population, 0.331, 0.1),
#   urban = rep(c(1,0), c(
#     round(total_population * urban_proportion),
#     total_population - round(total_population * urban_proportion))),
#   b1 = rnorm(total_population, 0.0142, 0.002),
#   c = rnorm(total_population, -0.00997, 0.001),
#   b2 = rnorm(total_population, 0.00997, 0.002),
#   nsampled = sample(180:220, total_population, replace = TRUE))
# 
# # save as processed dhs data
# saveRDS(
#   df_gambia,
#   file = here::here(
#     "01_data", "1a_survey_data", "processed",
#     "dhs_pr_records_combined.rds"))
# 
# # Download shapefiles
# download_shapefile(cntry_code)
# 
# # Download population rasters from worldpop
# download_pop_rasters(cntry_code)
# 
# # Extract urban extent raster
# extract_afurextent()
# 
# # Run models and get outputs ------------------------------------------
# 
# # Run the full model workflow
# run_full_workflow(cntry_code)

