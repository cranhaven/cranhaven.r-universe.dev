# volume — aboveR
# Cut/fill volume estimation and impoundment capacity curves

#' Estimate Cut or Fill Volume Between Two Surfaces
#'
#' Computes the volume of material between a `surface` DEM and a
#' `reference` surface (e.g., a design grade or pre-mining DEM),
#' optionally clipped to a `boundary` polygon. Reports cut and fill
#' volumes separately and documents the integration method used.
#'
#' @param surface A [terra::SpatRaster] representing the actual surface.
#' @param reference A [terra::SpatRaster] or single numeric value
#'   representing the reference elevation. If numeric, a constant
#'   reference plane at that elevation is used.
#' @param boundary An optional [sf] polygon to clip both surfaces to
#'   before computation.
#' @param method Character. Volume integration method: `"trapezoidal"`
#'   (default) or `"simpson"`. Trapezoidal sums `abs(diff) * cell_area`.
#'   Simpson uses Simpson's 1/3 rule for higher accuracy.
#'
#' @returns A list with components:
#'   - `cut_volume_m3`: volume of material removed (positive)
#'   - `fill_volume_m3`: volume of material added (positive)
#'   - `net_volume_m3`: fill minus cut
#'   - `area_m2`: total analysed area
#'   - `mean_depth_m`: mean absolute difference
#'   - `max_cut_m`: deepest cut
#'   - `max_fill_m`: highest fill
#'   - `method`: integration method used
#'
#' @export
#'
#' @examples
#' surface <- terra::rast(system.file("extdata/dem_after.tif", package = "aboveR"))
#' reference <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' boundary <- sf::st_read(
#'   system.file("extdata/boundary.gpkg", package = "aboveR"),
#'   quiet = TRUE
#' )
#' vol <- estimate_volume(surface, reference, boundary)
#' cat("Net volume:", vol$net_volume_m3, "m3\n")
estimate_volume <- function(surface, reference, boundary = NULL,
                            method = c("trapezoidal", "simpson")) {
  method <- match.arg(method)
  validate_raster(surface, "surface")

  # Handle numeric reference (constant plane)
  if (is.numeric(reference) && length(reference) == 1) {
    ref_rast <- terra::init(surface, fun = reference)
    names(ref_rast) <- "elevation"
  } else {
    validate_raster(reference, "reference")
    validate_crs_match(surface, reference)
    ref_rast <- reference
  }

  # Clip to boundary if provided
  if (!is.null(boundary)) {
    validate_sf(boundary, "boundary", "POLYGON")
    bnd_vect <- terra::vect(boundary)
    surface  <- terra::crop(surface, bnd_vect) |> terra::mask(bnd_vect)
    ref_rast <- terra::crop(ref_rast, bnd_vect) |> terra::mask(bnd_vect)
  }

  aligned <- align_rasters(surface, ref_rast)
  surface  <- aligned$r1
  ref_rast <- aligned$r2

  diff_r <- surface - ref_rast
  vals <- terra::values(diff_r)[, 1]
  vals <- vals[!is.na(vals)]

  ca <- cell_area_m2(surface)

  if (method == "trapezoidal") {
    cut_vol  <- abs(sum(vals[vals < 0])) * ca
    fill_vol <- sum(vals[vals > 0]) * ca
  } else {
    # Simpson's 1/3: treats each cell row as a function to integrate
    # For raster data, this is equivalent to weighted sum with 1/3 rule weights
    nr <- terra::nrow(surface)
    nc <- terra::ncol(surface)
    diff_mat <- matrix(terra::values(diff_r), nrow = nr, ncol = nc, byrow = TRUE)

    simpson_weights <- rep(1, nr)
    if (nr >= 3) {
      for (k in seq(2, nr - 1, by = 2)) simpson_weights[k] <- 4
      for (k in seq(3, nr - 1, by = 2)) simpson_weights[k] <- 2
    }
    res_y <- terra::res(surface)[2]
    weighted_vals <- as.vector(diff_mat * simpson_weights)
    weighted_vals <- weighted_vals[!is.na(weighted_vals)]
    scale_factor <- res_y / 3 * terra::res(surface)[1]

    cut_vals  <- weighted_vals[weighted_vals < 0]
    fill_vals <- weighted_vals[weighted_vals > 0]
    cut_vol  <- abs(sum(cut_vals)) * scale_factor
    fill_vol <- sum(fill_vals) * scale_factor
  }

  cut_vals_raw  <- vals[vals < 0]
  fill_vals_raw <- vals[vals > 0]

  list(
    cut_volume_m3  = cut_vol,
    fill_volume_m3 = fill_vol,
    net_volume_m3  = fill_vol - cut_vol,
    area_m2        = length(vals) * ca,
    mean_depth_m   = mean(abs(vals)),
    max_cut_m      = if (length(cut_vals_raw) > 0) abs(min(cut_vals_raw)) else 0,
    max_fill_m     = if (length(fill_vals_raw) > 0) max(fill_vals_raw) else 0,
    method         = method
  )
}

#' Generate an Impoundment Capacity Curve
#'
#' Calculates storage volume at a series of water surface elevations
#' for a terrain depression (e.g., a pond, reservoir, or sediment basin).
#' The result is an elevation-area-volume curve.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param boundary An [sf] polygon defining the impoundment boundary
#'   (e.g., dam crest outline). If `NULL`, the full DEM extent is used.
#' @param elevations Numeric vector of water surface elevations to
#'   evaluate. If `NULL`, a sequence from the minimum to maximum DEM
#'   value within the boundary is generated with `n_steps` increments.
#' @param n_steps Integer. Number of elevation increments when
#'   `elevations` is `NULL`. Default 20.
#'
#' @returns A data frame with columns:
#'   - `elevation`: water surface elevation
#'   - `area_m2`: inundated area at this elevation
#'   - `volume_m3`: cumulative storage volume below this elevation
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' boundary <- sf::st_read(
#'   system.file("extdata/boundary.gpkg", package = "aboveR"),
#'   quiet = TRUE
#' )
#' curve <- impoundment_curve(dem, boundary, n_steps = 10)
#' plot(curve$elevation, curve$volume_m3, type = "l",
#'      xlab = "Elevation", ylab = "Volume (m3)")
impoundment_curve <- function(dem, boundary = NULL, elevations = NULL,
                              n_steps = 20L) {
  validate_raster(dem, "dem")

  if (!is.null(boundary)) {
    validate_sf(boundary, "boundary", "POLYGON")
    bnd_vect <- terra::vect(boundary)
    dem <- terra::crop(dem, bnd_vect) |> terra::mask(bnd_vect)
  }

  vals <- terra::values(dem)[, 1]
  vals <- vals[!is.na(vals)]

  if (is.null(elevations)) {
    elevations <- seq(min(vals), max(vals), length.out = n_steps)
  }

  ca <- cell_area_m2(dem)

  results <- data.frame(
    elevation = elevations,
    area_m2   = numeric(length(elevations)),
    volume_m3 = numeric(length(elevations))
  )

  for (i in seq_along(elevations)) {
    elev <- elevations[i]
    below <- vals[vals < elev]
    depths <- elev - below
    results$area_m2[i]   <- length(below) * ca
    results$volume_m3[i] <- sum(depths) * ca
  }

  results
}
