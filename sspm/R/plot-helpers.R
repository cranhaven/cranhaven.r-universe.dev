
# Plotting helper functions -----------------------------------------------

plot_train_test <- function(x, scales){

  smoothed_data <- spm_smoothed_data(x)

  preds <- predict(x)$pred
  response <- spm_response(spm_formulas(x))

  smoothed_data_with_preds <- smoothed_data %>%
    dplyr::mutate(predicted = preds,
                  color = ifelse(.data$train_test, "TRAIN", "TEST"))

  # TRAIN / TEST pairs plot
  sspm_discrete_plot <-
    ggplot2::ggplot(data = smoothed_data_with_preds) +
    ggplot2::geom_point(ggplot2::aes(y = exp(.data[[response]]),
                                     x = .data$predicted,
                                     col = .data$color)) +
    ggplot2::theme_light() +
    ggplot2::labs(y = "observed", x = "predicted") +
    ggplot2::scale_color_viridis_d("Set") +
    ggplot2::facet_wrap(~.data[[spm_boundary(x)]],
                        scales = scales) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         lty = 2, linewidth = 0.2)

  return(sspm_discrete_plot)

}

# -------------------------------------------------------------------------

plot_productivity <- function(x, aggregate, interval, use_sf, page, nrow, ncol,
                              log, scales, point_size, line_size,
                              show_PI, show_CI){

  boundary_col <- spm_boundary(x)
  resp <- spm_response(spm_formulas(x))

  prod_preds <- predict(x, aggregate = aggregate,
                        interval = interval) %>%
    dplyr::mutate(color = "Predictions")

  actual <- spm_smoothed_data(x) %>%
    dplyr::mutate(pred = exp(.data[[resp]])) %>%
    dplyr::mutate(color = "Actual")

  prod_preds <- prod_preds %>%
    dplyr::bind_rows(actual)

  time_col <- spm_time(x)

  color_profile <- c("Predictions" = "red",
                     "Actual" = "black")

  sspm_discrete_plot <-
    spm_plot_routine(smoothed_data = prod_preds, var = "pred",
                     use_sf = use_sf, page = page, nrow = nrow,
                     ncol = ncol, time_col = time_col, log = log,
                     scales = scales, color_profile = color_profile,
                     aggregate = aggregate, interval = interval,
                     boundary_col = boundary_col, point_size = point_size,
                     line_size = line_size,
                     show_PI = show_PI, show_CI = show_CI )

}

plot_biomass <- function(x, biomass, biomass_origin, aggregate, interval,
                         use_sf, page, nrow, ncol, log, scales,
                         next_ts, smoothed_biomass, point_size, line_size,
                         show_PI, show_CI){

  # Check that biomass is a character
  checkmate::assert_character(biomass)

  # Start up color profile info vector
  color_profile <- c("Predictions" = "red")

  # Collect info
  boundary_col <- spm_boundary(x)
  patch_area_col <- spm_patches_area(spm_boundaries(x))
  time_col <- spm_time(x)
  boundary_col <- spm_boundary(x)

  # Start with predicting biomass
  biomass_preds <- predict(x, biomass = biomass,
                           aggregate = aggregate,
                           interval = interval) %>%
    dplyr::mutate(color = "Predictions")

  if (smoothed_biomass){

    # Prepare biomass_actual data
    biomass_actual <- process_actual_biomass(x, biomass_origin, biomass,
                                             patch_area_col, boundary_col,
                                             time_col, aggregate)

    # Put actual and predictions together
    biomass_preds <- biomass_preds %>%
      dplyr::bind_rows(biomass_actual)

    color_profile <-
      c(color_profile, "Smoothed" = "black")

  }

  if (next_ts) {

    next_ts_label <- "Prediction (1 step \n ahead, NO CATCH)"
    next_ts_preds <- process_next_ts(x, biomass, interval, aggregate,
                                     next_ts_label, boundary_col, time_col,
                                     biomass_preds)

    biomass_preds <- biomass_preds %>%
      dplyr::bind_rows(next_ts_preds)

    color_profile <-
      c(color_profile, "Prediction (1 step \n ahead, NO CATCH)" =
          "firebrick")
  }

  sspm_discrete_plot <-
    spm_plot_routine(smoothed_data = biomass_preds, var = "biomass",
                     use_sf = use_sf, page = page, nrow = nrow,
                     ncol = ncol, time_col = time_col, log = log,
                     scales = scales, color_profile = color_profile,
                     aggregate = aggregate, interval = interval,
                     boundary_col = boundary_col, point_size = point_size,
                     line_size = line_size,
                     show_PI = show_PI, show_CI = show_CI)

}

# -------------------------------------------------------------------------

process_next_ts <- function(x, biomass, interval, aggregate, next_ts_label,
                            boundary_col, time_col, biomass_preds){

  next_ts_preds <- predict(x, biomass = biomass,
                           next_ts = TRUE,
                           interval = interval,
                           aggregate = aggregate) %>%
    dplyr::mutate(color = next_ts_label)

  next_ts_timestep <- max(unique(next_ts_preds[[time_col]]))-1

  biomass_preds_previous <- biomass_preds %>%
    dplyr::filter(.data[[time_col]] == next_ts_timestep) %>%
    dplyr::mutate(color = next_ts_label) %>%
    tidyr::drop_na()

  next_ts_preds <- next_ts_preds %>%
    dplyr::bind_rows(biomass_preds_previous)

  return(next_ts_preds)
}

process_actual_biomass <- function(x, biomass_origin, biomass, patch_area_col,
                                   boundary_col, time_col, aggregate){

  if (is.null(biomass_origin)){
    assert_column(spm_smoothed_data(x), biomass)
    biomass_origin <- biomass
  } else {
    assert_column(biomass_origin, spm_smoothed_data(x))
  }

  biomass_actual <- spm_smoothed_data(x) %>%
    dplyr::mutate(biomass = .data[[biomass_origin]] *
                    .data[[patch_area_col]])

  if (aggregate){
    biomass_actual <-  biomass_actual %>%
      dplyr::group_by(.data[[boundary_col]], .data[[time_col]]) %>%
      dplyr::summarise(biomass = sum(.data$biomass)) %>%
      dplyr::ungroup()
  }

  biomass_actual <- biomass_actual %>%
    dplyr::mutate(color = "Smoothed")

  return(biomass_actual)

}

# Subroutine --------------------------------------------------------------

spm_plot_routine <- function(smoothed_data, var, use_sf, page, nrow, ncol,
                             time_col, log, scales, color_profile,
                             aggregate = FALSE, interval =  FALSE,
                             boundary_col = NULL, line_size, point_size,
                             show_PI = FALSE, show_CI = FALSE) {

  smoothed_data <- units::drop_units(smoothed_data) %>%
    dplyr::mutate(linesize = line_size, pointsize = point_size)

  if (log) {
    smoothed_data[[var]] <- log(smoothed_data[[var]])
    the_title <- paste0(var, " (log)")
  } else {
    the_title <- var
  }

  if (use_sf){

    base_plot <- ggplot2::ggplot(data = smoothed_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[var]])) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(fill = the_title) +
      ggplot2::theme_light()

    facet_by <- time_col

  } else {

    base_plot <- ggplot2::ggplot(data = smoothed_data) +
      ggplot2::geom_line(ggplot2::aes(x = .data[[time_col]],
                                      y = .data[[var]],
                                      color = .data$color),
                         linewidth = smoothed_data$linesize) +
      ggplot2::geom_point(ggplot2::aes(x = .data[[time_col]],
                                       y = .data[[var]],
                                       color = .data$color),
                          cex = smoothed_data$pointsize) +
      ggplot2::labs(y = the_title) +
      ggplot2::theme_light() +
      ggplot2::scale_color_manual(values = color_profile) +
      ggplot2::labs(color = "Type",
                    x = "Timestep")

    if (aggregate){

      facet_by <- boundary_col
      facet_col_levels <- length(unique(smoothed_data[[facet_by]]))

      if (facet_col_levels == 1) {
        nrow <- ncol <- 1
      }

    } else {
      facet_by <- "patch_id"
    }

    if (interval) {

      if (log) {

        CI_lower_name <- paste0(var, "_CI_log_lower")
        CI_upper_name <- paste0(var, "_CI_log_upper")
        PI_lower_name <- paste0(var, "_PI_log_lower")
        PI_upper_name <- paste0(var, "_PI_log_upper")

      } else {

        CI_lower_name <- paste0(var, "_CI_lower")
        CI_upper_name <- paste0(var, "_CI_upper")
        PI_lower_name <- paste0(var, "_PI_lower")
        PI_upper_name <- paste0(var, "_PI_upper")

      }

      if(show_CI){

        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            ggplot2::aes(x = .data[[time_col]],
                         ymin = .data[[CI_lower_name]],
                         ymax = .data[[CI_upper_name]],
                         fill = .data$color), alpha = 0.5)  +
          ggplot2::scale_fill_manual(values = color_profile) +
          ggplot2::labs(fill = "Type")

      }

      if (PI_lower_name %in% names(smoothed_data) &
          PI_upper_name %in% names(smoothed_data) &
          show_PI){
        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            ggplot2::aes(x = .data[[time_col]],
                         ymin = .data[[PI_lower_name]],
                         ymax = .data[[PI_upper_name]],
                         fill = .data$color), alpha = 0.3)
      }
    }

  }

  # Manage faceting + pagination
  if (is.character(page)) {

    if (page == "all") {

      n_per_page <- nrow * ncol
      n_pages <- facet_col_levels %/% (n_per_page) +
        (facet_col_levels %% n_per_page >= 1)

      sspm_discrete_plot <- list()

      for (page_nb in seq_len(length.out = n_pages)) {

        sspm_discrete_plot[[page_nb]] <- base_plot +
          ggforce::facet_wrap_paginate(~ .data[[facet_by]],
                                       nrow = nrow, ncol = ncol,
                                       page = page_nb, scales = scales)

      }

    } else {

      sspm_discrete_plot <- base_plot +
        ggforce::facet_wrap_paginate(~ .data[[facet_by]],
                                     nrow = nrow, ncol = ncol,
                                     page = 1, scales = scales)

    }

  } else if (is.numeric(page)) {

    sspm_discrete_plot <- base_plot +
      ggforce::facet_wrap_paginate(~ .data[[facet_by]],
                                   nrow = nrow, ncol = ncol,
                                   page = page, scales = scales)

  }

  return(sspm_discrete_plot)

}
