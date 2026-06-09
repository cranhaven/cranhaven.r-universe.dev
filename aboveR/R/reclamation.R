# reclamation — aboveR
# Reclamation monitoring and surface roughness analysis

#' Assess Reclamation Progress Between Time Steps
#'
#' Compares a current DEM against a target (design grade) surface to
#' quantify how much of a reclamation area has been restored to the
#' desired elevation. Returns per-cell deviations and summary statistics.
#'
#' @param current A [terra::SpatRaster] of the current terrain surface.
#' @param target A [terra::SpatRaster] of the target / design grade surface.
#' @param boundary An optional [sf] polygon to restrict analysis to.
#' @param tolerance Numeric. Cells within +/- `tolerance` of the target
#'   are considered "on grade". Default `0.3` (metres).
#'
#' @returns A list with:
#'   - `deviation`: [terra::SpatRaster] of current - target values
#'   - `on_grade_pct`: percentage of cells within tolerance
#'   - `above_grade_pct`: percentage of cells above tolerance
#'   - `below_grade_pct`: percentage of cells below tolerance
#'   - `mean_deviation`: mean signed deviation
#'   - `rmse`: root mean square error
#'
#' @export
#'
#' @examples
#' current <- terra::rast(system.file("extdata/dem_after.tif", package = "aboveR"))
#' target  <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' prog <- reclamation_progress(current, target, tolerance = 1)
#' cat("On grade:", prog$on_grade_pct, "%\n")
reclamation_progress <- function(current, target, boundary = NULL,
                                 tolerance = 0.3) {
  validate_raster(current, "current")
  validate_raster(target, "target")
  validate_crs_match(current, target)

  if (!is.null(boundary)) {
    validate_sf(boundary, "boundary", "POLYGON")
    bnd_vect <- terra::vect(boundary)
    current <- terra::crop(current, bnd_vect) |> terra::mask(bnd_vect)
    target  <- terra::crop(target, bnd_vect)  |> terra::mask(bnd_vect)
  }

  aligned <- align_rasters(current, target)
  current <- aligned$r1
  target  <- aligned$r2

  deviation <- current - target
  names(deviation) <- "deviation"

  vals <- terra::values(deviation)[, 1]
  vals <- vals[!is.na(vals)]
  n <- length(vals)

  on_grade  <- sum(abs(vals) <= tolerance) / n * 100
  above     <- sum(vals > tolerance) / n * 100
  below     <- sum(vals < -tolerance) / n * 100

  list(
    deviation      = deviation,
    on_grade_pct   = on_grade,
    above_grade_pct = above,
    below_grade_pct = below,
    mean_deviation = mean(vals),
    rmse           = sqrt(mean(vals^2))
  )
}

#' Compute Surface Roughness of a DEM
#'
#' Calculates surface roughness as the standard deviation of elevation
#' within a moving window. Rougher surfaces indicate unreclaimed terrain,
#' active construction, or natural heterogeneity.
#'
#' @param dem A [terra::SpatRaster].
#' @param window Integer. Size of the moving window (number of cells).
#'   Must be odd. Default `5`.
#'
#' @returns A [terra::SpatRaster] of local roughness values
#'   (standard deviation of elevation).
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' rough <- surface_roughness(dem, window = 5)
#' terra::plot(rough)
surface_roughness <- function(dem, window = 5L) {
  validate_raster(dem, "dem")

  if (window %% 2 == 0) {
    stop("`window` must be an odd number.", call. = FALSE)
  }

  roughness <- terra::focal(dem, w = window, fun = "sd", na.rm = TRUE)
  names(roughness) <- "roughness"
  roughness
}
