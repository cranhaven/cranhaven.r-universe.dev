# terrain — aboveR
# Terrain derivative analysis: slope, aspect, hillshade, contours

#' Compute Slope and Aspect from a DEM
#'
#' Calculates slope (in degrees or percent) and aspect (compass bearing)
#' from a DEM in a single call.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param units Character. Slope units: `"degrees"` (default) or `"percent"`.
#'
#' @returns A two-layer [terra::SpatRaster]:
#'   - `slope`: terrain slope in the requested units
#'   - `aspect`: terrain aspect in degrees (0 = North, 90 = East, 180 = South,
#'     270 = West; flat areas = -1)
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' sa <- slope_aspect(dem)
#' terra::plot(sa)
slope_aspect <- function(dem, units = c("degrees", "percent")) {
  units <- match.arg(units)
  validate_raster(dem, "dem")

  slp <- terra::terrain(dem, v = "slope", unit = "degrees")

  if (units == "percent") {
    slp <- tan(slp * pi / 180) * 100
  }
  names(slp) <- "slope"

  asp <- terra::terrain(dem, v = "aspect", unit = "degrees")
  names(asp) <- "aspect"

  c(slp, asp)
}

#' Generate a Hillshade from a DEM
#'
#' Computes an analytical hillshade (shaded relief) from a DEM using
#' specified sun azimuth and altitude angles. Useful for terrain
#' visualisation and map backgrounds.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param azimuth Numeric. Sun azimuth in degrees (compass bearing).
#'   Default `315` (northwest).
#' @param altitude Numeric. Sun altitude angle in degrees above the
#'   horizon. Default `45`.
#'
#' @returns A [terra::SpatRaster] with hillshade values (0--255).
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' hs <- hillshade(dem)
#' terra::plot(hs, col = grey.colors(256))
hillshade <- function(dem, azimuth = 315, altitude = 45) {
  validate_raster(dem, "dem")

  slp <- terra::terrain(dem, v = "slope", unit = "radians")
  asp <- terra::terrain(dem, v = "aspect", unit = "radians")

  hs <- terra::shade(slp, asp,
                      angle = altitude,
                      direction = azimuth,
                      normalize = TRUE)
  names(hs) <- "hillshade"
  hs
}

#' Generate Contour Lines from a DEM
#'
#' Extracts contour lines at a specified interval and returns them as
#' an [sf] object with an `elevation` attribute.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param interval Numeric. Elevation interval between contour lines.
#'   Default `5`.
#' @param levels Numeric vector. Specific elevations to contour. If
#'   provided, `interval` is ignored.
#'
#' @returns An [sf] data frame with LINESTRING geometry and an
#'   `elevation` column.
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' cl <- contour_lines(dem, interval = 2)
#' plot(sf::st_geometry(cl))
contour_lines <- function(dem, interval = 5, levels = NULL) {
  validate_raster(dem, "dem")

  if (is.null(levels)) {
    vals <- terra::values(dem)[, 1]
    vals <- vals[!is.na(vals)]
    levels <- seq(
      floor(min(vals) / interval) * interval,
      ceiling(max(vals) / interval) * interval,
      by = interval
    )
  }

  contours <- terra::as.contour(dem, levels = levels)
  contours_sf <- sf::st_as_sf(contours)

  if ("level" %in% names(contours_sf)) {
    names(contours_sf)[names(contours_sf) == "level"] <- "elevation"
  } else {
    # terra uses the raster layer name as the column name
    col_names <- setdiff(names(contours_sf), attr(contours_sf, "sf_column"))
    if (length(col_names) > 0) {
      names(contours_sf)[names(contours_sf) == col_names[1]] <- "elevation"
    }
  }

  contours_sf
}

#' Compute General Zonal Statistics from a Raster
#'
#' Extracts summary statistics for each polygon zone, including
#' min, max, mean, median, standard deviation, and cell count.
#'
#' @param r A [terra::SpatRaster].
#' @param zones An [sf] data frame of polygons defining analysis zones.
#' @param id_field Character. Column name in `zones` to use as zone
#'   identifier.
#'
#' @returns An [sf] data frame with columns:
#'   - zone identifier
#'   - `min`, `max`, `mean`, `median`, `sd`: summary statistics
#'   - `cell_count`: number of non-NA cells
#'   - `area_m2`: zone area
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' zones <- sf::st_read(
#'   system.file("extdata/zones.gpkg", package = "aboveR"),
#'   quiet = TRUE
#' )
#' zs <- zonal_stats(dem, zones, id_field = "zone_id")
#' print(zs)
zonal_stats <- function(r, zones, id_field) {
  validate_raster(r, "r")
  validate_sf(zones, "zones", "POLYGON")

  if (!id_field %in% names(zones)) {
    stop("`id_field` '", id_field, "' not found in zones.", call. = FALSE)
  }

  # Reproject zones to raster CRS if needed
  raster_crs <- terra::crs(r)
  if (!identical(sf::st_crs(zones)$wkt, raster_crs)) {
    zones <- sf::st_transform(zones, raster_crs)
  }

  result <- lapply(seq_len(nrow(zones)), function(i) {
    z <- zones[i, ]
    vals <- terra::extract(r, terra::vect(z))[[2]]
    vals <- vals[!is.na(vals)]

    data.frame(
      min        = if (length(vals) > 0) min(vals) else NA_real_,
      max        = if (length(vals) > 0) max(vals) else NA_real_,
      mean       = if (length(vals) > 0) mean(vals) else NA_real_,
      median     = if (length(vals) > 0) stats::median(vals) else NA_real_,
      sd         = if (length(vals) > 1) stats::sd(vals) else NA_real_,
      cell_count = length(vals),
      area_m2    = as.numeric(sf::st_area(z))
    )
  })

  stats_df <- do.call(rbind, result)
  out <- zones[, id_field, drop = FALSE]
  cbind(out, stats_df)
}
