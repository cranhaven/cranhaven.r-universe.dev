
# Productivity / biomass predictions subroutine ---------------------------
# These functions expect a sspm_ft object

predict_productivity <- function(object, new_data, type, interval){

  # Gather_info
  time_col <- spm_time(object)
  bounds <- spm_boundaries(object)
  bounds_col <- spm_boundary(bounds)
  patch_area_col <- spm_patches_area(bounds)
  object_fit <- spm_get_fit(object)
  smoothed_data <- spm_smoothed_data(object) %>%
    dplyr::arrange(.data$patch_id, .data[[time_col]])

  # Predict with mgcv
  pred_log <- object_fit %>%
    mgcv::predict.bam(newdata = new_data, type = type)

  # Make a df with the log and non log version of those predictions
  preds_df <- data.frame(pred_log = pred_log) %>%
    dplyr::mutate(pred = exp(pred_log))

  # If we want to compute the intervals, do so with the helpers
  if (interval) {

    CI_df <- predict_intervals(object_fit, new_data)

    preds_df <- preds_df %>%
      dplyr::bind_cols(CI_df)
  }

  # Keep the minimum column set
  columns_to_keep <- smoothed_data %>%
    dplyr::select("patch_id", !!time_col,
                  !!bounds_col, !!patch_area_col)

  # Bind and turn into sf object
  preds_df <- cbind(preds_df, columns_to_keep)  %>%
    sf::st_as_sf() # TODO verify CRS

  return(preds_df)

}

# -------------------------------------------------------------------------

predict_biomass <- function(object, new_data, biomass, next_ts,
                            interval, aggregate){

  # Get data and fit
  time_col <- spm_time(object)
  bounds <- spm_boundaries(object)
  bounds_col <- spm_boundary(bounds)
  patch_area_col <- spm_patches_area(bounds)
  object_fit <- spm_get_fit(object)
  patches <- spm_patches(bounds)
  smoothed_data <- spm_smoothed_data(object) %>%
    dplyr::arrange(.data$patch_id, .data[[time_col]])

  # Verify that the biomass column character is present in the data
  checkmate::assert_class(biomass, "character")
  assert_column(smoothed_data, biomass)

  # Compute predictions for next timestep if desired
  if (next_ts){

    next_ts_params <- predict_next_ts(object, new_data, biomass)
    preds_df <- next_ts_params$preds_df

    if (interval) {

      CI_df <- predict_biomass_intervals(object_fit, patches, smoothed_data, time_col,
                                         next_ts_params$new_data, biomass, patch_area_col,
                                         next_ts = TRUE, bounds_col)

      preds_df <- preds_df %>%
        dplyr::bind_cols(CI_df)

    }

  } else { # Or not

    preds <- predict(object)

    preds_df <- smoothed_data %>%
      dplyr::select(dplyr::all_of(time_col), "patch_id", dplyr::all_of(biomass),
                    "catch_density", dplyr::all_of(bounds_col)) %>%
      dplyr::left_join(sf::st_drop_geometry(preds),
                       by = c(c(time_col, bounds_col), "patch_id")) %>%
      dplyr::group_by(.data$patch_id, .data[[bounds_col]]) %>%

      dplyr::mutate(biomass_density_with_catch =
                      .data$pred * dplyr::lag(.data[[biomass]])) %>%
      dplyr::mutate(biomass_density = .data$biomass_density_with_catch -
                      .data$catch_density) %>%

      dplyr::ungroup() %>%

      dplyr::select(-"pred", -"pred_log", -dplyr::all_of(biomass),
                    -"catch_density") %>%
      dplyr::relocate(dplyr::all_of(patch_area_col), .before = "geometry") %>%

      dplyr::mutate(biomass_with_catch = .data$biomass_density_with_catch *
                      .data[[patch_area_col]],
                    biomass = .data$biomass_density *
                      .data[[patch_area_col]]) %>%
      dplyr::relocate("biomass_with_catch", "biomass",
                      "biomass_density_with_catch", "biomass_density",
                      .before = "geometry") %>%
      sf::st_as_sf()

    if (interval) {

      CI_df <- predict_biomass_intervals(object_fit, patches, smoothed_data, time_col,
                                         new_data, biomass, patch_area_col,
                                         next_ts = FALSE, bounds_col)

      preds_df <- preds_df %>%
        dplyr::bind_cols(CI_df)

    }

  }

  if (aggregate) {

    if (interval) {

      preds_df <- preds_df %>%
        dplyr::group_by(.data[[bounds_col]], .data[[time_col]]) %>%
        dplyr::summarise(biomass = sum(.data$biomass),
                         biomass_CI_upper = sum(.data$biomass_CI_upper),
                         biomass_CI_lower = sum(.data$biomass_CI_lower),
                         biomass_PI_upper = sum(.data$biomass_PI_upper),
                         biomass_PI_lower = sum(.data$biomass_PI_lower))

    } else {

      preds_df <- preds_df %>%
        dplyr::group_by(.data[[bounds_col]], .data[[time_col]]) %>%
        dplyr::summarise(biomass = sum(biomass))

    }

  }

  return(preds_df)

}

predict_next_ts <- function(object, new_data, biomass){

  # Gather info
  time_col <- spm_time(object)
  bounds <- spm_boundaries(object)
  patches <- spm_patches(bounds)
  bounds_col <- spm_boundary(bounds)
  patch_area_col <- spm_patches_area(bounds)
  object_fit <- spm_get_fit(object)
  smoothed_data <- spm_smoothed_data(object)

  # Use helpers to get next year data
  next_ts_data <- make_next_ts_data(object, time_col, patches, bounds_col)
  new_data <- next_ts_data$new_data
  max_ts <- next_ts_data$max_ts

  # Apply the LINPRED in case it is needed
  linpred_lag_vars <- spm_lagged_vars(spm_formulas(object))

  if (!is.null(linpred_lag_vars)){
    linpred <- LINPRED(next_ts_data$data_filtered,
                       bounds, time_col, linpred_lag_vars,
                       k = 5, m = 1)
    mats <- linpred$vars

    # Modify the mats to fit the time step under scrutiny
    by_mat_nrow <- dim(mats$by_matrix)[1]
    grid_length <- nrow(patches)
    mats$lag_matrix <- mats$lag_matrix[1:grid_length,]
    mats$by_matrix <- mats$by_matrix[(by_mat_nrow-grid_length+1):by_mat_nrow,]

  } else {
    mats <- NULL
  }

  # Append the data
  new_data <- append(as.list(new_data), mats)

  # Calculate ratio, then density, and format the output
  ratio_next_ts <-
    exp(mgcv::predict.bam(object_fit, new_data))

  density_last_year <- smoothed_data %>%
    dplyr::filter(.data[[time_col]] %in% max_ts) %>%
    dplyr::pull(.data[[biomass]])

  preds_df <- patches %>%
    dplyr::mutate(density_next_ts = density_last_year * ratio_next_ts,
                  biomass = .data$density_next_ts * .data[[patch_area_col]],
                  year_f = next_ts_data$next_ts) %>%
    dplyr::select(-"density_next_ts")

  # Cosmetic changes
  preds_df <-  preds_df %>%
    dplyr::relocate("biomass") %>%
    dplyr::relocate(dplyr::all_of(bounds_col)) %>%
    dplyr::relocate(dplyr::all_of(time_col)) %>%
    dplyr::relocate("patch_id")

  return(list(preds_df = preds_df, new_data = new_data))

}

# Helpers -----------------------------------------------------------------

# Build the new_data for the next timestep predictions
make_next_ts_data <- function(object, time_col, patches, bounds_col,
                              year_lag = 5){

  # Check if all lagged var
  lagged_var_names <- get_lagged_var_names(object)

  # Get ts data
  all_ts <- as.numeric(as.character(
    unique(spm_smoothed_data(object)[[time_col]])))
  max_ts <- max(all_ts)
  next_ts <- max_ts + 1

  # Make a new grid with the next timestep
  new_grid <- patches %>%
    dplyr::select(dplyr::all_of(bounds_col), "patch_id") %>%
    tidyr::expand_grid(!!time_col := next_ts)

  # Bind the new grid to the smooth data
  spm_smoothed_data(object) <- spm_smoothed_data(object) %>%
    dplyr::bind_rows(new_grid)

  # Lag the vars that need lagging (even if already present,
  # it is needed to account for the new year).
  object <- object %>% spm_lag(lagged_var_names, 1, default = NA)

  # Create the new data
  new_data <- spm_smoothed_data(object) %>%
    dplyr::filter(.data[[time_col]] == next_ts) %>%
    st_drop_geometry()

  # Filter to the past 5 years
  data_filtered <- spm_smoothed_data(object) %>%
    dplyr::filter(.data[[time_col]] %in% (next_ts-year_lag):next_ts)

  return(list(new_data = new_data,
              data_filtered = data_filtered,
              max_ts = max_ts,
              next_ts = next_ts))
}

# Obtain var names for predicting the model, excluding penalty mats
get_var_names <- function(sspm_object, exclude_mats = TRUE,
                          exclude_special = TRUE) {
  var_names <- spm_get_fit(sspm_object)$var.summary %>%
    names()
  if (exclude_special){
    var_names <- var_names %>%
      stringr::str_subset("matrix", negate = TRUE)
  }
  if (exclude_mats){
    var_names <- var_names %>%
      stringr::str_subset("patch_id", negate = TRUE) %>%
      stringr::str_subset(spm_time(sspm_object), negate = TRUE) %>%
      stringr::str_subset(spm_boundary(sspm_object), negate = TRUE)
  }
  return(var_names)
}

# Get the vars that are lagged
get_lagged_var_names <- function(sspm_object){

  all_var_names <- get_var_names(sspm_object, exclude_special = TRUE)
  lagged_var_names <- all_var_names %>%
    stringr::str_subset("_lag_1")

  if(!all(all_var_names %in% lagged_var_names)){
    vars_to_print <- all_var_names[!(all_var_names %in% lagged_var_names)]
    cli::cli_alert_warning(paste0("Not all vars are lagged vars: ",
                                  paste0(c(vars_to_print), collapse = ", ")))
  }

  lagged_var_names <- lagged_var_names %>%
    stringr::str_remove("_lag_1")

  return(lagged_var_names)
}

# Prepare the prediction matrix
make_prediction_matrix <- function(the_data, time_col, patches){

  year_vector <- as.numeric(as.character(the_data[[time_col]]))
  year_values <- sort(unique(year_vector))

  predict_mat <- patches %>%
    # sf::st_set_geometry(NULL) %>%
    tidyr::expand_grid(!!time_col := year_values)

  return(predict_mat)
}
