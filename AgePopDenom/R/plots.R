#' Generate Variogram Plot
#'
#' Creates a variogram plot showing the spatial dependence structure of the
#' data. The plot includes both the empirical variogram points and the fitted
#' theoretical variogram line. The empirical variogram points show the actual
#' semivariance values at different distances, while the red line shows the
#' fitted exponential
#' variogram model.
#'
#' @param age_param_data Data frame containing the age parameter data. Must
#'   include columns 'web_x' and 'web_y' for spatial coordinates and the
#'   response variable specified in scale_outcome.
#' @param fit_vario Fitted variogram object from automap package. Should contain
#'   components $psill (partial sill) and $range (range parameter) for the
#'   exponential variogram model.
#' @param country_code Character string of the country code (e.g. "TZA") used
#'   for plot title and output filename.
#' @param scale_outcome Character string specifying the column name for the
#'   scale parameter response variable (default: "log_scale"). This is the
#'   variable for which the variogram is computed.
#' @param output_dir Character string specifying the directory path where the
#'   plot will be saved as a PNG file.
#' @param width Plot width in pixels (default: 2000). Controls the output image
#'   width.
#' @param height Plot height in pixels (default: 1500). Controls the output
#'   image height.
#' @param png_resolution PNG resolution in DPI (dots per inch, default: 300).
#'   Higher values create larger, higher quality images.
#'
#' @return Invisibly returns the ggplot object containing the variogram plot.
#'   The plot is also saved as a PNG file in the specified output directory.
#'
#' @details
#' The function creates a variogram plot with the following elements:
#' - Points showing empirical semivariance values at different distances
#' - A red line showing the fitted exponential variogram model
#' - Clear axis labels and title
#' - Comma-formatted distance values on x-axis
#' - Clean theme with black and white style
#'
#' The output filename is constructed as lowercase country
#'   code + "_variogram.png"
#'
#' @examples
#' \donttest{
#' set.seed(123)  # For reproducibility
#' age_param_data <- data.frame(
#'   country = rep("TZA", 100),
#'   web_x = runif(100, 0, 100),
#'   web_y = runif(100, 0, 100),
#'   log_scale = rnorm(100, mean = 5, sd = 2)
#' )
#'
#' # Create a dummy fitted variogram object
#' fit_vario <- list(
#'   psill = c(0.1, 0.5),
#'   range = c(0, 50)
#' )
#'
#' vario_plot <- generate_variogram_plot(
#'    age_param_data = age_param_data,
#'    fit_vario = fit_vario,
#'    country_code = "TZA",
#'    output_dir = file.path(tempdir()))
#' }
#'
#' @export
generate_variogram_plot <- function(age_param_data, fit_vario, country_code,
                                    scale_outcome = "log_scale",
                                    output_dir, width = 12,
                                    height = 9,
                                    png_resolution = 300) {
  vgm_data <- age_param_data
  sp::coordinates(vgm_data) <- ~ web_x + web_y

  emp_vario <- gstat::variogram(
    stats::formula(paste0(scale_outcome, "~1")),
    vgm_data
  )

  # Create plot
  vario_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = as.data.frame(emp_vario),
      ggplot2::aes(x = dist, y = gamma),
      size = 1.2
    ) +
    ggplot2::geom_line(
      data = data.frame(
        dist = seq(0, max(emp_vario$dist), length.out = 100),
        gamma = fit_vario$psill[1] +
          fit_vario$psill[2] * (1 - exp(
            -3 * seq(0, max(emp_vario$dist),
              length.out = 100
            ) / fit_vario$range[2]
          ))
      ),
      ggplot2::aes(x = dist, y = gamma),
      color = "red"
    ) +
    ggplot2::labs(
      title =
        paste(toupper(country_code), "Variogram"),
      x = "\nDistance",
      y = "Semivariance \n "
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9)
    ) +
    ggplot2::scale_x_continuous(labels = scales::comma)

  # set up save path
  save_path <- file.path(
    output_dir,
    glue::glue("{tolower(country_code)}_variogram.png")
  )

  # Save plot
  ggplot2::ggsave(
    filename = save_path,
    plot = vario_plot,
    width = width,
    height = height, units = "cm",
    dpi = png_resolution
  )

  cli::cli_alert_success("Variogram saved to {save_path}")

  invisible(vario_plot)
}


#' Rasterize Spatial Data
#'
#' This function converts spatial data with x, y coordinates and a value field
#' into a raster using a specified resolution and CRS.
#'
#' @param x_coords Numeric vector of x-coordinates (e.g., longitude).
#' @param y_coords Numeric vector of y-coordinates (e.g., latitude).
#' @param values Numeric vector of values associated with each point.
#' @param cell_size Numeric. Grid cell size in meters (default: 5000).
#' @param crs Character, the coordinate reference system in EPSG format
#'            (e.g., "EPSG:3857").
#' @param fun Function to aggregate values in cells (default is `mean`).
#'
#' @return A `terra::SpatRaster` object.
#' @examples
#'
#' \donttest{
#' x_coords <- runif(100, -100, 100)
#' y_coords <- runif(100, -50, 50)
#' values <- rnorm(100, mean = 10, sd = 5)
#'
#' rasterize_data(x_coords, y_coords, values,
#'                cell_size = 5000, crs = "EPSG:3857", fun = mean)
#' }
#'
#' @export
rasterize_data <- function(x_coords, y_coords, values,
                           cell_size = 5000, crs, fun = mean) {
  # Combine inputs into a data frame
  spatial_data <- data.frame(x = x_coords, y = y_coords, value = values)

  # Create a SpatVector
  spat_vector <- terra::vect(spatial_data, geom = c("x", "y"), crs = crs)

  # Create a raster template with the correct extent and resolution
  raster_template <- terra::rast(spat_vector,
    resolution = cell_size, crs = crs
  )

  # Rasterize the data
  terra::rasterize(spat_vector, raster_template, field = "value", fun = fun)
}

#' Generate and Save Raster Plot for Gamma Predictions
#'
#' This function creates rasters from prediction data, combines them into a
#' stack, and saves the faceted plot to a specified file.
#'
#' @param predictor_data A data frame containing `web_x` and `web_y`
#'   coordinates and associated prediction values.
#' @param pred_list A list containing predictions (`shape_hat`, `scale_hat`,
#'   `mean_age_pred`) for creating rasters.
#' @param country_code A string representing the lowercase country code,
#'   used for naming the output file.
#' @param output_dir A string specifying the directory where the plot should
#'   be saved.
#' @param save_raster A logical input specifying whether to save output or not.
#'   Default is TRUE.
#' @param file_name_suffix A string specifying the suffix for the file name
#'   (default is "gamma_prediction_rasters").
#' @param width Numeric. Width of output plot in pixels (default: 2500).
#' @param height Numeric. Height of output plot in pixels (default: 2000).
#' @param png_resolution An integer specifying the resolution of the plot in DPI
#'   (default: 300).
#' @return The path to the saved raster plot.
#'
#' @examples
#'
#' \donttest{
#' predictor_data <- data.frame(
#'   web_x = runif(100, -100, 100),
#'   web_y = runif(100, -50, 50)
#' )
#'
#' pred_list <- list(
#'   shape_hat = rnorm(100, mean = 2, sd = 0.5),
#'   scale_hat = rnorm(100, mean = 10, sd = 2),
#'   mean_age_pred = rnorm(100, mean = 30, sd = 5)
#' )
#'
#' generate_gamma_raster_plot(predictor_data,
#'                            pred_list,
#'                            country_code = "COD",
#'                            output_dir = file.path(tempdir()))
#' }
#'
#' @export
generate_gamma_raster_plot <- function(predictor_data,
                                       pred_list,
                                       country_code,
                                       output_dir,
                                       save_raster = TRUE,
                                       file_name_suffix =
                                         "gamma_prediction_rasters",
                                       width = 2500,
                                       height = 2000,
                                       png_resolution = 300) {
  rast_shape <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$shape_hat,
    cell_size = 5000,
    crs = "EPSG:3857"
  )

  rast_scale <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$scale_hat,
    cell_size = 5000,
    crs = "EPSG:3857"
  )

  rast_mean_age <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$mean_age_pred,
    cell_size = 5000,
    crs = "EPSG:3857"
  )

  # Combine rasters into a stack
  raster_stack <- c(rast_shape, rast_scale, rast_mean_age)

  # Set names for visualization
  names(raster_stack) <- c(
    "Shape Parameter",
    "Scale Parameter",
    "Mean Age Prediction"
  )

  if (save_raster) {
    # Define output path
    output_file <- file.path(
      output_dir,
      glue::glue("{country_code}_{file_name_suffix}.png")
    )

    # Create output directory if it doesn't exist
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Save the plot
    png(output_file, width = width, height = height, res = png_resolution)

    # ave the current graphical parameters and restore them on exit
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar), add = TRUE)

    # Adjust layout and spacing
    graphics::par(
      mfrow = c(2, 2), # Arrange plots in 2 rows and 2 columns
      mar = c(5, 5, 4, 2) + 0.1, # Margins for axis labels
      oma = c(0, 0, 2, 0) # Outer margins for titles
    )

    # Plot each raster with clear labeling
    terra::plot(rast_scale,
      main = "Scale Parameter", cex.main = 1.2,
      cex.axis = 0.8, cex.lab = 1.2
    )
    terra::plot(rast_shape,
      main = "Shape Parameter", cex.main = 1.2,
      cex.axis = 0.8, cex.lab = 1.2
    )

    graphics::par(mfg = c(2, 1))
    terra::plot(rast_mean_age,
      main = "Mean Age Prediction", cex.main = 1.2,
      cex.axis = 0.8, cex.lab = 1.2
    )

    dev.off()

    cli::cli_alert_success("Raster plot saved to {output_file}")
  }
}

#' Generate and Save Age Pyramid Plot
#'
#' This function processes an input dataset to compute age distribution,
#' generates age pyramid plots by region showing both proportions and counts,
#' and saves the plots to a specified directory.
#'
#' @param dataset A list containing two data frames:
#'   - prop_df: Population proportions data frame
#'   - pop_df: Population counts data frame
#'   Each with columns for `country`, `region`, `district`, and columns ending
#'   with "mean"
#' @param country_code A string representing the country code (e.g., "ken").
#' @param output_dir A string specifying the directory where plots should be saved.
#' @param line_color A string specifying the color of the plot's lines. Default
#'   is `"#67000d"`.
#' @param fill_high A string specifying the fill color for high values. Default
#'   is `"#fee0d2"`.
#' @param fill_low A string specifying the fill color for low values. Default
#'   is `"#a50f15"`
#' @param break_axis_by break axis to show less cluttered age groups. Default
#'   is 10
#' @param caption A string specifying the caption text. Default is "Note: Total
#'   population includes ages 99+, pyramid shows ages 0-99"
#' @return A list containing both proportion and count plots.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' prop_df <- data.frame(
#'  country = rep("COD", 10),
#'  region = rep("RegionA", 10),
#'  district = paste("District", 1:10),
#'  popsize = runif(10, 2340, 28761),
#'  `0-4_mean` = runif(10, 0.1, 0.5),
#'  `5-9_mean` = runif(10, 0.05, 0.4),
#'  `10-14_mean` = runif(10, 0.03, 0.3)
#')
#'
#'pop_df <- data.frame(
#'  country = rep("COD", 10),
#'  region = rep("RegionA", 10),
#'  district = paste("District", 1:10),
#'  popsize = runif(10, 2340, 28761),
#'  `0-4_mean` = runif(10, 1000, 5000),
#'  `5-9_mean` = runif(10, 800, 4500),
#'  `10-14_mean` = runif(10, 700, 4000)
#')
#'
#' dataset <- list(prop_df = prop_df, pop_df = pop_df)
#'
#'res <- generate_age_pyramid_plot(
#'          dataset = dataset,
#'          country_code = "COD",
#'          output_dir = file.path(tempdir()))
#'}
#'
#' @export
generate_age_pyramid_plot <- function(
    dataset,
    country_code,
    output_dir,
    line_color = "#67000d",
    fill_high = "#fee0d2",
    fill_low = "#a50f15",
    break_axis_by = 10,
    caption =
      paste0(
        "Note: Total population includes ",
        "ages 99+, pyramid shows ages 0-99"
      )) {
  # Validate inputs ------------------------------------------------------------
  if (!all(c("prop_df", "pop_df") %in% names(dataset))) {
    stop("Dataset must be a list containing 'prop_df' and 'pop_df'")
  }

  required_cols <- c("country", "region", "popsize")
  for (df in list(dataset$prop_df, dataset$pop_df)) {
    if (!all(required_cols %in% names(df))) {
      stop(
        "Each dataset must contain: ",
        paste(required_cols, collapse = ", ")
      )
    }
  }

  # Process datasets for age structure
  process_age_data <- function(df, type = "count") {
    df |>
      dplyr::select(
        country, region, district,
        dplyr::contains("mean")
      ) |>
      dplyr::rename_with(
        ~ stringr::str_replace_all(
          ., c(
            "_mean" = "", "_pop" = "", "_prop" = "",
            "plus" = "+y"
          )
        ),
        dplyr::contains("mean")
      ) |>
      tidyr::pivot_longer(
        cols = -c(country, region, district),
        names_to = "age_group",
        values_to = ifelse(type == "count", "population", "proportion")
      ) |>
      dplyr::mutate(
        age_group = stringr::str_remove(age_group, "_mean"),
        age_group = stringr::str_replace_all(
          age_group, c("_" = "-", "to" = "-", " " = "")
        ),
        region = stringr::str_to_title(region),
        region = stringr::str_remove(region, " County")
      )
  }

  age_struct_pop <- process_age_data(dataset$pop_df, "count")
  age_struct_prop <- process_age_data(dataset$prop_df, "prop")

  # Get common age group levels
  levels_age <- unique(age_struct_pop$age_group)

  # Aggregate data by region
  summarize_data <- function(data, value_col) {
    data |>
      dplyr::group_by(country, region, age_group) |>
      dplyr::summarise(
        total = sum(.data[[value_col]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(!is.na(age_group)) |>
      dplyr::mutate(
        age_group = factor(age_group, levels = levels_age)
      )
  }

  pop_by_region <- summarize_data(age_struct_pop, "population")
  prop_by_region <- summarize_data(age_struct_prop, "proportion")

  # Generate plots
  create_pyramid <- function(data, value_type, total_label) {
    x_break_labels <- levels(data$age_group)[
      seq(1, length(levels(data$age_group)), by = break_axis_by)
    ]

    data |>
      dplyr::filter(age_group != "99+y") |>
      ggplot2::ggplot(
        ggplot2::aes(x = age_group, y = total, fill = as.numeric(age_group))
      ) +
      ggplot2::geom_bar(
        stat = "identity",
        color = line_color,
        linewidth = 0.4,
        position = "identity",
        width = 1
      ) +
      ggplot2::scale_y_continuous(
        labels = \(x) {
          if (value_type == "count") {
            format(x, scientific = FALSE, big.mark = ",")
          } else {
            scales::percent(x, accuracy = 1)
          }
        }
      ) +
      ggplot2::scale_x_discrete(
        breaks = x_break_labels,
        labels = x_break_labels
      ) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~region) +
      ggplot2::labs(
        title = glue::glue(
          "Age Pyramid by Region ({value_type})",
          "\n{stringr::str_to_title(data$country[1])} {total_label}"
        ),
        x = "Age Group \n",
        y = ifelse(value_type == "count",
          "\n Population",
          "\n Proportion of Population"
        ),
        fill = "Region",
        caption = caption
      ) +
      ggplot2::scale_fill_gradient(high = fill_high, low = fill_low) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold"),
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        plot.caption = ggplot2::element_text(
          hjust = 1,
          vjust = 1,
          face = "italic",
          size = 10,
          margin = ggplot2::margin(t = 10)
        ),
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
      )
  }

  # Calculate totals for labels
  total_pop <- sum(pop_by_region$total, na.rm = TRUE) |>
    round() |>
    format(big.mark = ",")

  # Create both plots
  pop_plot <- create_pyramid(
    pop_by_region, "count",
    glue::glue("(N = {total_pop})")
  )
  prop_plot <- create_pyramid(prop_by_region, "proportion", "")

  # Save plots
  for (plot_type in c("count", "prop")) {
    output_file <- file.path(
      output_dir,
      glue::glue("{country_code}_age_pyramid_{plot_type}.png")
    )

    plot_to_save <- if (plot_type == "count") pop_plot else prop_plot

    ggplot2::ggsave(
      output_file,
      plot = plot_to_save,
      width = 14,
      height = 14,
      dpi = 500,
      scale = 1,
      device = "png"
    )

    cli::cli_alert_success(
      "Age pyramid {plot_type} plot saved to {output_file}"
    )
  }

  return(list(
    count_plot = pop_plot,
    prop_plot = prop_plot
  ))
}

#' Generate Age Population Raster
#'
#' @description
#' Creates age-stratified population raster layers from predictor data and gamma
#' distribution parameters. Supports parallel processing and caching of results.
#' The output is a multi-layer raster stack with each layer representing the
#' population proportion for a specific age interval.
#'
#' @param predictor_data Data frame containing population and spatial data with
#'    columns: country, region, district, pop, web_x, web_y
#' @param scale_pred Matrix of scale parameters for gamma distribution
#'    predictions
#' @param shape_pred Matrix of shape parameters for gamma distribution
#'    predictions
#' @param age_range Numeric vector of length 2 specifying min and max ages,
#'    default c(0,99)
#' @param age_interval Numeric interval size between age groups in years,
#'    default 1
#' @param country_code Character ISO3 country code
#' @param ignore_cache Logical whether to ignore cached results, default FALSE
#' @param output_dir Character path to output directory
#' @param n_cores Integer number of cores for parallel processing, default
#'    detectCores()-2
#'
#' @return SpatRaster object (terra package) containing multiple layers, where
#'    each layer represents the population proportion for an age interval.
#'    Layer names indicate the age range (e.g., "Age 0 to 1 years").
#'    The raster uses EPSG:3857 projection with 5000m resolution.
#'
#' @details
#' The function processes age intervals sequentially, computing population
#' proportions using parallel processing. Results are cached as a GeoTIFF file
#' for future use. The output raster maintains spatial properties of the input
#' data and is suitable for GIS analysis and visualization.
#'
#' @examples
#' \donttest{
#' predictor_data <- data.frame(
#'  country = rep("CountryX", 100),
#'  region = rep("RegionA", 100),
#'  district = rep("District1", 100),
#'  pop = sample(100:1000, 100, replace = TRUE),
#'  web_x = runif(100, -100, 100),
#'  web_y = runif(100, -50, 50)
#')
#'
#' scale_pred <- matrix(runif(100 * 10, 1, 5), nrow = 100, ncol = 10)
#' shape_pred <- matrix(runif(100 * 10, 1, 5), nrow = 100, ncol = 10)
#'
#'res <- generate_age_pop_raster(predictor_data,
#'                        scale_pred,
#'                        shape_pred,
#'                        country_code = "COD",
#'                        output_dir = file.path(tempdir()),
#'                        n_cores = 1)
#'}
#'
#' @export
generate_age_pop_raster <- function(predictor_data,
                                    scale_pred,
                                    shape_pred,
                                    age_range = c(0, 10),
                                    age_interval = 1,
                                    country_code,
                                    ignore_cache = FALSE,
                                    output_dir,
                                    n_cores = parallel::detectCores() - 2) {
  # Construct output path
  output_path <- file.path(
    output_dir,
    glue::glue(
      "{tolower(country_code)}_age_pop_grid_",
      "{age_range[1]}_{age_range[2]}_yrs_by_{age_interval}yrs.tif"
    )
  )

  # Check cache
  if (!ignore_cache && file.exists(output_path)) {
    cli::cli_process_start(
      msg = "Importing cached age population raster...",
      msg_done = "Successfully imported cached age population raster."
    )
    raster_stack <- terra::rast(output_path)
    cli::cli_process_done()
    return(raster_stack)
  }

  # Ensure output directory exists
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Define age intervals
  limslow <- seq(age_range[1], age_range[2], age_interval)
  limsup <- seq(
    age_range[1] + age_interval, age_range[2] + age_interval,
    age_interval
  )

  n_sim <- ncol(shape_pred)
  raster_layers <- list()

  # Validate inputs
  if (!identical(dim(scale_pred), dim(shape_pred))) {
    stop("scale_pred and shape_pred must have the same dimensions")
  }

  if (nrow(scale_pred) != nrow(predictor_data)) {
    stop("Number of rows in predictions must match predictor_data")
  }

  # Processing loop
  for (runnum in seq_along(limslow)) {
    cli::cli_process_start(
      msg = glue::glue("Processing interval {runnum}/{length(limslow)}..."),
      msg_done = glue::glue("Completed interval {runnum}/{length(limslow)}.")
    )

    # Run parallel computation based on OS
    if (Sys.info()["sysname"] == "Darwin") {
      # For MacOS
      prop_age_pred <- pbmcapply::pbmclapply(
        1:n_sim,
        function(sim) {
          compute_age_proportions(
            sim = sim, scale = scale_pred, shape = shape_pred,
            run = runnum, limslow = limslow, limsup = limsup
          )
        },
        mc.cores = n_cores
      )
    } else {
      # For Linux/Windows
      future::plan(future::multisession, workers = n_cores)
      prop_age_pred <- future.apply::future_lapply(
        1:n_sim,
        function(sim) {
          compute_age_proportions(
            sim = sim, scale = scale_pred, shape = shape_pred,
            run = runnum, limslow = limslow, limsup = limsup
          )
        }
      )
    }

    # Combine simulation results
    prop_age_pred <- base::do.call(base::cbind, prop_age_pred)
    mean_prop_age <- base::rowMeans(prop_age_pred, na.rm = TRUE)

    # Create spatial data for rasterization
    spatial_data <- data.frame(
      x = predictor_data$web_x,
      y = predictor_data$web_y,
      proportion = mean_prop_age
    )

    # Convert to SpatVector
    spat_vector <- terra::vect(spatial_data,
      geom = c("x", "y"),
      crs = "EPSG:3857"
    )

    # Create raster template
    raster_template <- terra::rast(
      spat_vector,
      resolution = 5000,
      crs = "EPSG:3857"
    )

    # Rasterize proportions
    raster_layer <- terra::rasterize(
      spat_vector,
      raster_template,
      field = "proportion",
      fun = mean
    )

    # Store raster layer
    raster_layers[[runnum]] <- raster_layer

    cli::cli_process_done()
  }

  # Combine into raster stack
  raster_stack <- terra::rast(raster_layers)
  names(raster_stack) <- paste0("Age ", limslow, " to ", limsup, " years")

  # Save results
  terra::writeRaster(raster_stack, output_path, overwrite = TRUE)
  cli::cli_alert_success("Raster stack saved to {output_path}")

  return(raster_stack)
}
