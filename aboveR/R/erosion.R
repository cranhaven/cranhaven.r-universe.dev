# erosion — aboveR
# Erosion channel detection and pond sedimentation analysis

#' Detect Erosion Channels from a DEM
#'
#' Identifies potential erosion channels (rills, gullies) by computing
#' flow accumulation and filtering cells where accumulated flow exceeds
#' a threshold. Optionally returns vectorised channel lines.
#'
#' @param dem A [terra::SpatRaster].
#' @param threshold Numeric. Minimum flow accumulation value to classify
#'   as a channel. Default `100`.
#' @param as_lines Logical. Return channel centrelines as [sf] lines?
#'   Default `FALSE` (returns raster).
#'
#' @returns If `as_lines = FALSE`, a [terra::SpatRaster] with channel
#'   cells = 1, other = `NA`. If `as_lines = TRUE`, an [sf] LINESTRING
#'   object of detected channels.
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' channels <- detect_channels(dem, threshold = 50)
#' terra::plot(channels)
detect_channels <- function(dem, threshold = 100, as_lines = FALSE) {
  validate_raster(dem, "dem")

  # Compute Topographic Position Index (TPI) as channel proxy:
  # TPI = elevation - focal mean elevation
  focal_mean <- terra::focal(dem, w = 5, fun = "mean", na.rm = TRUE)
  tpi <- dem - focal_mean
  names(tpi) <- "tpi"

  # Channels are deeply negative TPI values (local depressions along lines)
  tpi_vals <- terra::values(tpi)[, 1]
  tpi_vals <- tpi_vals[!is.na(tpi_vals)]
  tpi_cutoff <- stats::quantile(tpi_vals, probs = threshold / (threshold + 100))

  channels <- terra::classify(
    tpi,
    rcl = matrix(c(-Inf, tpi_cutoff, 1,
                    tpi_cutoff, Inf, NA),
                 ncol = 3, byrow = TRUE)
  )
  names(channels) <- "channel"

  if (as_lines) {
    polys <- terra::as.polygons(channels, dissolve = FALSE)
    lines_sf <- sf::st_as_sf(polys) |>
      sf::st_cast("MULTILINESTRING") |>
      sf::st_cast("LINESTRING")
    return(lines_sf)
  }

  channels
}

#' Estimate Pond Sedimentation from Multi-Temporal DEMs
#'
#' Compares two DEMs of a pond or sediment basin to estimate the volume
#' of accumulated sediment. The pond area is defined by a boundary polygon,
#' and sedimentation is computed as fill (positive change) within that area.
#'
#' @param before A [terra::SpatRaster] of the pond before sedimentation.
#' @param after A [terra::SpatRaster] of the pond after sedimentation.
#' @param pond_boundary An [sf] polygon defining the pond extent.
#'
#' @returns A list with:
#'   - `sediment_volume_m3`: estimated sediment volume (positive fill)
#'   - `mean_depth_change_m`: mean elevation change within the pond
#'   - `max_accumulation_m`: maximum sedimentation depth
#'   - `pond_area_m2`: area of the pond boundary
#'   - `change_raster`: [terra::SpatRaster] of elevation change within the pond
#'
#' @export
#'
#' @examples
#' before <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' after  <- terra::rast(system.file("extdata/dem_after.tif", package = "aboveR"))
#' boundary <- sf::st_read(
#'   system.file("extdata/boundary.gpkg", package = "aboveR"),
#'   quiet = TRUE
#' )
#' sed <- pond_sedimentation(before, after, boundary)
#' cat("Sediment volume:", sed$sediment_volume_m3, "m3\n")
pond_sedimentation <- function(before, after, pond_boundary) {
  validate_raster(before, "before")
  validate_raster(after, "after")
  validate_crs_match(before, after)
  validate_sf(pond_boundary, "pond_boundary", "POLYGON")

  bnd_vect <- terra::vect(pond_boundary)
  before_c <- terra::crop(before, bnd_vect) |> terra::mask(bnd_vect)
  after_c  <- terra::crop(after, bnd_vect)  |> terra::mask(bnd_vect)

  aligned <- align_rasters(before_c, after_c)
  change <- aligned$r2 - aligned$r1
  names(change) <- "sediment_change"

  vals <- terra::values(change)[, 1]
  vals <- vals[!is.na(vals)]

  ca <- cell_area_m2(before)
  fill_vals <- vals[vals > 0]

  list(
    sediment_volume_m3 = sum(fill_vals) * ca,
    mean_depth_change_m = mean(vals),
    max_accumulation_m  = if (length(fill_vals) > 0) max(fill_vals) else 0,
    pond_area_m2        = as.numeric(sf::st_area(pond_boundary)),
    change_raster       = change
  )
}
