# flood — aboveR
# Flood inundation and depth analysis from DEMs

#' Generate a Flood Inundation Mask
#'
#' Creates a binary raster showing which cells would be inundated
#' at a given water surface elevation. Cells below the water level
#' are marked as flooded (1), cells above are `NA`.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param water_level Numeric. Water surface elevation in the same
#'   units as the DEM.
#' @param boundary An optional [sf] polygon to restrict analysis to
#'   (e.g., a floodplain boundary).
#'
#' @returns A [terra::SpatRaster] with values 1 (inundated) and
#'   `NA` (dry).
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' inundated <- flood_inundation(dem, water_level = 305)
#' terra::plot(inundated, main = "Flood Inundation at 305m")
flood_inundation <- function(dem, water_level, boundary = NULL) {
  validate_raster(dem, "dem")

  if (!is.numeric(water_level) || length(water_level) != 1) {
    stop("`water_level` must be a single numeric value.", call. = FALSE)
  }

  if (!is.null(boundary)) {
    validate_sf(boundary, "boundary", "POLYGON")
    bnd_vect <- terra::vect(boundary)
    dem <- terra::crop(dem, bnd_vect) |> terra::mask(bnd_vect)
  }

  flooded <- terra::classify(
    dem,
    rcl = matrix(c(-Inf, water_level, 1,
                    water_level, Inf, NA),
                 ncol = 3, byrow = TRUE)
  )
  names(flooded) <- "inundated"
  flooded
}

#' Compute Flood Depth at a Given Water Level
#'
#' Calculates the depth of water at each cell for a given water
#' surface elevation. Cells above the water level receive `NA`.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param water_level Numeric. Water surface elevation.
#' @param boundary An optional [sf] polygon to restrict analysis to.
#'
#' @returns A [terra::SpatRaster] of water depth values (positive,
#'   in DEM elevation units). Cells above the water level are `NA`.
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' depth <- flood_depth(dem, water_level = 305)
#' terra::plot(depth, main = "Flood Depth (m)")
flood_depth <- function(dem, water_level, boundary = NULL) {
  validate_raster(dem, "dem")

  if (!is.numeric(water_level) || length(water_level) != 1) {
    stop("`water_level` must be a single numeric value.", call. = FALSE)
  }

  if (!is.null(boundary)) {
    validate_sf(boundary, "boundary", "POLYGON")
    bnd_vect <- terra::vect(boundary)
    dem <- terra::crop(dem, bnd_vect) |> terra::mask(bnd_vect)
  }

  depth <- water_level - dem
  depth[depth <= 0] <- NA
  names(depth) <- "flood_depth"
  depth
}

#' Compute Height Above Nearest Drainage (HAND)
#'
#' Calculates a relative elevation model representing each cell's
#' height above the nearest drainage channel. This is a key input
#' for flood susceptibility mapping. Channels are identified using
#' Topographic Position Index (TPI).
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param channel_threshold Numeric. TPI percentile threshold for
#'   channel identification. Lower values select deeper channels.
#'   Default `0.1` (10th percentile).
#' @param window Integer. Focal window size for TPI computation.
#'   Must be odd. Default `15`.
#'
#' @returns A [terra::SpatRaster] of height-above-nearest-drainage
#'   values. Lower values indicate greater flood susceptibility.
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' hand <- height_above_drainage(dem, window = 7)
#' terra::plot(hand, main = "Height Above Nearest Drainage")
height_above_drainage <- function(dem, channel_threshold = 0.1,
                                   window = 15L) {
  validate_raster(dem, "dem")

  if (window %% 2 == 0) {
    stop("`window` must be an odd number.", call. = FALSE)
  }
  if (channel_threshold <= 0 || channel_threshold >= 1) {
    stop("`channel_threshold` must be between 0 and 1 (exclusive).",
         call. = FALSE)
  }

  # Compute TPI to identify channels
  focal_mean <- terra::focal(dem, w = window, fun = "mean", na.rm = TRUE)
  tpi <- dem - focal_mean

  # Identify channel cells
  tpi_vals <- terra::values(tpi)[, 1]
  tpi_vals <- tpi_vals[!is.na(tpi_vals)]
  cutoff <- stats::quantile(tpi_vals, probs = channel_threshold)

  channels <- tpi <= cutoff

  # Get channel cell elevations
  channel_elev <- dem
  channel_elev[!channels] <- NA

  # For each cell, compute height above nearest channel

  # Use focal expanding approach: fill channel elevations outward
  # by iterative focal max of channel elevations
  filled <- channel_elev
  for (i in seq_len(ceiling(max(terra::nrow(dem), terra::ncol(dem)) / 2))) {
    new_fill <- terra::focal(filled, w = 3, fun = "mean", na.rm = TRUE,
                              na.policy = "only")
    filled <- terra::cover(filled, new_fill)
    if (!any(is.na(terra::values(filled)))) break
  }

  hand <- dem - filled
  hand[hand < 0] <- 0
  names(hand) <- "hand"
  hand
}
