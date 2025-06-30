#' Generate Pseudo-Absence Points Using Different Methods Based on Presence Points, Covariates, and Study Area Polygon
#'
#' Wrapper function for pseudo-absence generation methods, such as
#' background random points, target-group, and using buffer area.
#'
#' @param method Character; one of "random", "target_group", or "buffer_out".
#' @param presences Data frame of presence points with coordinates and timestamp.
#' @param raster_stack SpatRaster of covariates.
#' @param predictor_variables Character vector of selected predictors.
#' @param study_area Optional sf polygon (used for clipping).
#' @param target_group_points Optional data frame of sampling points (for target-group).
#' @param coords Vector of coordinate column names.
#' @param pa_buffer_distance Numeric; buffer radius in degrees around each presence. Default is 0.5.
#' @param ratio Ratio of pseudo-absences to presences.
#' @param attempts Max attempts to fulfill sample size.
#' @param seed Optional seed for reproducibility.
#'
#' @return A data frame of pseudo-absence points (pa = 0) with covariates.
#' @export
generate_pseudo_absences <- function(method = c("random", "target_group", "buffer_out"), presences, raster_stack, predictor_variables,
                                     study_area = NULL, target_group_points = NULL,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     pa_buffer_distance = 0.5,
                                     ratio = 1, attempts = 100, seed = NULL) {
  method <- match.arg(method)

  if (method == "random") {
    pseudo_absences <- suppressMessages(generate_pa_random(
      presences = presences,
      study_area = study_area,
      raster_stack = raster_stack,
      predictor_variables = predictor_variables,
      coords = coords,
      ratio = ratio,
      attempts = attempts,
      seed = seed
    ))

  } else if (method == "target_group") {
    if (is.null(target_group_points)) stop("Target group occurrence data must be provided for target-group method")

    pseudo_absences <- suppressMessages(generate_pa_target_group(
      presences = presences,
      target_group_points = target_group_points,
      study_area = study_area,
      raster_stack = raster_stack,
      predictor_variables = predictor_variables,
      coords = coords,
      ratio = ratio,
      attempts = attempts,
      seed = seed
    ))

  } else if (method == "buffer_out") {
    pseudo_absences <- suppressMessages(generate_pa_buffer_out(
      presences = presences,
      raster_stack = raster_stack,
      predictor_variables = predictor_variables,
      coords = coords,
      pa_buffer_distance = pa_buffer_distance,
      ratio = ratio,
      attempts = attempts,
      seed = seed
    ))

  } else {
    stop("Invalid pseudo-absence method. Must be one of: 'random', 'target_group', 'buffer_out'")
  }

  # Asign timestamp_original as 0
  pseudo_absences$timestamp_original <- 0

  # Return combined dataset
  return(rbind(presences, pseudo_absences))
}


#' Generate Random Pseudo-Absences
#'
#' This function generates pseudo-absence points randomly across the study area (random background),
#' optionally applying spatial thinning to match presence filtering strategy.
#'
#' @param presences Data frame containing presence points.
#' @param study_area Spatial polygon defining the study area (`sf` object).
#' @param raster_stack `SpatRaster` object containing covariate data.
#' @param predictor_variables Character vector of the predictor variables selected for this species.
#' @param coords Character vector specifying the column names for latitude and longitude. Defaults to `c("decimalLongitude", "decimalLatitude")`.
#' @param ratio Ratio of pseudo-absences to presences (default 1 = balanced).
#' @param attempts Integer specifying the number of attempts to generate exact pseudo-absences. Defaults to 100.
#' @param seed Optional random seed.
#'
#' @return Data frame containing pseudo-absence points with coordinates, timestamp, pa = 0, and covariates.
#' @export
generate_pa_random <- function(presences, study_area, raster_stack, predictor_variables,
                               coords = c("decimalLongitude", "decimalLatitude"),
                               ratio = 1, attempts = 100, seed = NULL) {
  # Check inputs
  stopifnot(is.data.frame(presences))
  if (!is.null(study_area)){
    stopifnot(inherits(study_area, "sf") || inherits(study_area, "sfc"))
  }
  stopifnot(inherits(raster_stack[[1]], "SpatRaster"))

  # Initialize variables
  set.seed(seed)
  n_presences <- nrow(presences)
  n_required <- n_presences * ratio
  if (ratio == 1){
    timestamp_values <- presences$timestamp
  } else {
    timestamp_values <- sample(presences$timestamp, n_required, replace = TRUE)
  }
  col_names <- c(coords, "timestamp", predictor_variables)
  absences <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(absences) <- col_names

  # Get bounding box
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
  while (nrow(absences) < n_required && curr_attempt < attempts) {
    # Sample points from the bounding box
    new_abs <- sf::st_sample(bbox_hull_poly, size = n_required - nrow(absences), type = "random", exact = TRUE, oriented=TRUE)

    # Remove points outside the study area polygon
    if (!is.null(study_area)) {
      # Set CRS to NA to avoid sf repetitive messages
      sf::st_crs(new_abs) <- NA
      sf::st_crs(study_area) <- NA
      new_abs <- new_abs[sapply(sf::st_intersects(new_abs, study_area), any)]
    }
    new_abs <- as.data.frame(sf::st_coordinates(new_abs))
    colnames(new_abs) <- coords

    # Assign random timestamp values
    new_abs$timestamp <- sample(timestamp_values, nrow(new_abs))

    # Extract covariate values and remove points with missing covariate values
    if (nrow(new_abs) > 0) {
      new_abs <- extract_noNA_cov_values(new_abs, raster_stack, predictor_variables)
    }

    # Bind with already sampled absences
    absences <- rbind(absences, new_abs)

    # Remove duplicate points
    absences <- remove_duplicate_points(absences, coords = c(coords, "timestamp"))
    # Remove points already present in the occurrence data
    absences <- dplyr::anti_join(absences, presences, by = c(coords, "timestamp"))

    curr_attempt <- curr_attempt + 1

    # Check for maximum attempts
    if (curr_attempt >= attempts) {
      warning("Could not generate balanced pseudo-absences. Check your study area or spatial thinning.")
      break
    }
  }

  if (nrow(absences) < n_required) {
    warning("Final pseudo-absences < requested number. Try increasing target group points or attempts.")
  }

  # Set absence indicator
  absences$pa <- 0
  return(absences)
}


#' Generate Pseudo-Absences Using Target-Group Background
#'
#' @param presences Data frame containing presence points.
#' @param target_group_points Data frame of all sampling locations (target group).
#' @param study_area Spatial polygon defining the study area (`sf` object).
#' @param raster_stack `SpatRaster` object containing covariate data.
#' @param predictor_variables Character vector of the predictor variables selected for this species.
#' @param coords Character vector specifying the column names for latitude and longitude. Defaults to `c("decimalLongitude", "decimalLatitude")`.
#' @param ratio Ratio of pseudo-absences to presences (default 1 = balanced).
#' @param attempts Integer specifying the number of attempts to generate exact pseudo-absences. Defaults to 100.
#' @param seed Optional random seed.
#'
#' @return Data frame containing pseudo-absence points with coordinates, timestamp, pa = 0, and covariates.
#' @export
generate_pa_target_group <- function(presences, target_group_points, study_area,
                                     raster_stack, predictor_variables,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     ratio = 1, attempts = 100, seed = NULL) {

  stopifnot(all(coords %in% colnames(presences)))
  stopifnot(all(coords %in% colnames(target_group_points)))

  # Initialize variables
  set.seed(seed)
  n_presences <- nrow(presences)
  n_required <- n_presences * ratio

  # Remove duplicated points
  target_group_points <- target_group_points[complete.cases(target_group_points[, c(coords, "timestamp")]), ]
  # Remove points outside the study area
  if (!is.null(study_area)) {
    target_group_points <- remove_points_polygon(target_group_points, study_area, FALSE, coords)
  }
  # Remove overlap between target group and presences
  target_group_points <- dplyr::anti_join(target_group_points, presences, by = c(coords, "timestamp"))

  if (nrow(target_group_points) < n_required) {
    n_required <- nrow(target_group_points)
    warning("Not enough unique target-group points to generate required pseudo-absences. Sampling as much as possible.")
  }

  # Initialize
  col_names <- c(coords, "timestamp", predictor_variables)
  absences <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(absences) <- col_names
  candidate_pool <- 1:nrow(target_group_points)

  curr_attempt <- 0
  while (nrow(absences) < n_required && curr_attempt < attempts) {
    # Sample rows
    new_abs <- target_group_points[sample(candidate_pool, n_required - nrow(absences)), ]

    # Extract covariates
    new_abs <- extract_noNA_cov_values(new_abs, raster_stack, predictor_variables)

    # Add to accumulated
    absences <- rbind(absences, new_abs)

    curr_attempt <- curr_attempt + 1

    # Check for maximum attempts
    if (curr_attempt >= attempts) {
      warning("Could not generate balanced pseudo-absences. Check your study area or spatial thinning.")
      break
    }
  }

  if (nrow(absences) < n_required) {
    warning("Final pseudo-absences < requested number. Try increasing target group points or attempts.")
  }

  # Set absence indicator
  absences$pa <- 0
  return(absences)
}

#' Generate Pseudo-Absences Using Buffer-Out Strategy
#'
#' This function generates pseudo-absences outside a buffer around presence points but within
#' the convex hull of those points. This prevents spatial overlap while preserving geographic realism.
#'
#' @param presences Data frame containing presence points.
#' @param raster_stack `SpatRaster` object containing covariate data.
#' @param predictor_variables Character vector of the predictor variables selected for this species.
#' @param coords Character vector specifying the column names for latitude and longitude. Defaults to `c("decimalLongitude", "decimalLatitude")`.
#' @param pa_buffer_distance Numeric; buffer radius in degrees around each presence. Default is 0.5.
#' @param ratio Ratio of pseudo-absences to presences (default 1 = balanced).
#' @param attempts Integer specifying the number of attempts to generate exact pseudo-absences. Defaults to 100.
#' @param seed Optional seed for reproducibility.
#'
#' @return A data frame of pseudo-absences with coordinates, timestamp, `pa = 0`, and covariate values.
#' @export
generate_pa_buffer_out <- function(presences, raster_stack, predictor_variables,
                                   coords = c("decimalLongitude", "decimalLatitude"),
                                   pa_buffer_distance = 0.5,
                                   ratio = 1, attempts = 100, seed = NULL) {
  stopifnot(all(coords %in% colnames(presences)))

  # Initialize variables
  set.seed(seed)
  n_presences <- nrow(presences)
  n_required <- n_presences * ratio
  if (ratio == 1){
    timestamp_values <- presences$timestamp
  } else {
    timestamp_values <- sample(presences$timestamp, n_required, replace = TRUE)
  }
  col_names <- c(coords, "timestamp", predictor_variables)
  absences <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(absences) <- col_names
  crs <- terra::crs(raster_stack[[1]])

  # Convert presences to sf
  sf_pts <- sf::st_as_sf(presences, coords = coords, crs = crs)

  # Create buffer and convex hull
  sf::sf_use_s2(FALSE)
  if (pa_buffer_distance != 0) {
    buffer_geom <- suppressWarnings(sf::st_buffer(sf_pts, dist = pa_buffer_distance))
    buffer_union <- sf::st_union(buffer_geom) |> sf::st_make_valid()
  }
  convex_hull <- sf::st_convex_hull(sf::st_union(sf_pts)) |> sf::st_make_valid()

  # Sampling loop
  curr_attempt <- 0
  while (nrow(absences) < n_required && curr_attempt < attempts) {
    # Sample points from the convex hull
    new_abs <- sf::st_sample(convex_hull, size = n_required - nrow(absences), type = "random", exact = TRUE, oriented=TRUE)

    # Remove points inside points buffer
    if (pa_buffer_distance != 0) {
      new_abs <- new_abs[!sapply(sf::st_intersects(new_abs, buffer_union), any)]
    }
    new_abs <- as.data.frame(sf::st_coordinates(new_abs))
    colnames(new_abs) <- coords

    # Assign random timestamp values
    new_abs$timestamp <- sample(timestamp_values, nrow(new_abs))

    # Extract covariate values and remove points with missing covariate values
    if (nrow(new_abs) > 0) {
      new_abs <- extract_noNA_cov_values(new_abs, raster_stack, predictor_variables)
    }

    # Bind with already sampled absences
    absences <- rbind(absences, new_abs)

    # Remove duplicate points
    absences <- remove_duplicate_points(absences, coords = c(coords, "timestamp"))
    # Remove points already present in the occurrence data
    absences <- dplyr::anti_join(absences, presences, by = c(coords, "timestamp"))

    curr_attempt <- curr_attempt + 1

    # Check for maximum attempts
    if (curr_attempt >= attempts) {
      warning("Could not generate balanced pseudo-absences. Check your study area or spatial thinning.")
      break
    }
  }

  if (nrow(absences) < n_required) {
    warning("Final pseudo-absences < requested number. Try increasing target group points or attempts.")
  }

  # Set absence indicator
  absences$pa <- 0
  return(absences)
}



