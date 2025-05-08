#' Generate Pseudo-Absence Points Based on Presence Points, Covariates, and Study Area Polygon
#'
#' This function generates pseudo-absence points within the study area.
#'
#' @param presences Data frame containing presence points.
#' @param study_area Spatial polygon defining the study area (`sf` object).
#' @param raster_stack `SpatRaster` object containing covariate data.
#' @param predictor_variables Character vector of the predictor variables selected for this species.
#' @param coords Character vector specifying the column names for latitude and longitude. Defaults to `c("decimalLongitude", "decimalLatitude")`.
#' @param decimal_digits An integer specifying the number of decimal places to which coordinates should be rounded.
#' @param attempts Integer specifying the number of attempts to generate exact pseudo-absences. Defaults to 100.
#'
#' @return Data frame containing both presence and pseudo-absence points.
#'
#' @export
generate_pseudo_absences <- function(presences, study_area, raster_stack, predictor_variables, coords = c("decimalLongitude", "decimalLatitude"), decimal_digits = NULL, attempts = 100) {
  # Check inputs
  stopifnot(is.data.frame(presences))
  if (!is.null(study_area)){
    stopifnot(inherits(study_area, "sf") || inherits(study_area, "sfc"))
  }
  stopifnot(inherits(raster_stack[[1]], "SpatRaster"))

  # Initialize variables
  n_presences <- nrow(presences)
  timestamp_values <- presences$timestamp
  col_names <- c(coords, "timestamp", predictor_variables)
  absences <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(absences) <- col_names

  if (is.null(study_area)) {
    crs <- terra::crs(raster_stack[[1]])
    bounding_box <- sf::st_bbox(as.vector(terra::ext(raster_stack[[1]]))[c("xmin", "ymin", "xmax", "ymax")])
  } else {
    crs <- sf::st_crs(study_area)
    bounding_box <- sf::st_bbox(study_area)
  }
  bbox_hull_poly <- sf::st_as_sfc(bounding_box) %>%
    sf::st_set_crs(crs)

  sf::sf_use_s2(FALSE)

  # Loop for attempts to generate pseudo-absences
  curr_attempt <- 0
  while (nrow(absences) < n_presences & curr_attempt < attempts) {
    # Sample points from the bounding box
    new_abs <- sf::st_sample(bbox_hull_poly, size = n_presences - nrow(absences), type = "random", exact = TRUE, oriented=TRUE)

    # Remove points outside the study area polygon
    if (!is.null(study_area)) {
      # Set CRS to NA to avoid sf repetitive messages
      sf::st_crs(study_area) <- NA
      sf::st_crs(new_abs) <- NA
      absences_inside_pol <- sapply(sf::st_intersects(new_abs, study_area), any)
      new_abs <- new_abs[absences_inside_pol]
    }

    # Convert to data frame and round coordinates
    new_abs <- as.data.frame(sf::st_coordinates(new_abs))
    colnames(new_abs) <- coords

    # Sample timestamp values
    timestamp_sampled <- sample(timestamp_values, nrow(new_abs))
    new_abs$timestamp <- timestamp_sampled

    if (!is.null(decimal_digits) & nrow(new_abs) > 0) {
      new_abs <- GeoThinneR::thin_points(
        new_abs, long_col = coords[1], lat_col = coords[2], group_col = "timestamp",
        method = "precision", trials = 1, all_trials = FALSE,
        precision = decimal_digits
      )[[1]] # Remove close points by precision
    }

    # Extract covariate values and remove points with missing covariate values
    if (nrow(new_abs) > 0) {
      new_abs <- extract_noNA_cov_values(new_abs, raster_stack, predictor_variables)
    }

    if (nrow(new_abs) > 0) {
      timestamp_keeped <- new_abs$timestamp

      # Update remaining timestamp values
      timestamp_values <- timestamp_values[-match(timestamp_keeped, timestamp_values)]
    }

    # Bind with already sampled absences
    absences <- rbind(absences, new_abs)

    # Remove duplicate points
    absences <- remove_duplicate_points(absences, coords = c(coords, "timestamp"))

    # Remove points already present in the occurrence data
    absences <- dplyr::anti_join(absences, presences, by = c(coords, "timestamp"))

    # Increment attempt counter
    curr_attempt <- curr_attempt + 1

    # Check for maximum attempts
    if (curr_attempt >= attempts) {
      stop("Could not generate balanced pseudo-absences. Check your study area or spatial thinning.")
    }
  }

  # Set presence/absence indicator
  presences$pa <- 1
  absences$pa <- 0

  # Combine presences and absences
  return(rbind(presences, absences))
}
