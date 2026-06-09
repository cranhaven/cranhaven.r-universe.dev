# change — aboveR
# Terrain change detection between DEM epochs

#' Compute Terrain Change Between Two DEMs
#'
#' Calculates the elevation difference between a *before* and *after* DEM,
#' returning both the continuous change values and a classified layer
#' (cut / stable / fill). Rasters are aligned automatically if their
#' extents or resolutions differ (same CRS required).
#'
#' @param before A [terra::SpatRaster] representing the earlier DEM.
#' @param after A [terra::SpatRaster] representing the later DEM.
#' @param tolerance Numeric. Changes within +/- `tolerance` are classified
#'   as stable. Default `0.1` (metres or native units).
#'
#' @returns A two-layer [terra::SpatRaster]:
#'   - `change`: continuous elevation difference (after - before)
#'   - `class`: integer classification (1 = cut, 2 = stable, 3 = fill)
#'
#' @export
#'
#' @examples
#' before <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' after  <- terra::rast(system.file("extdata/dem_after.tif", package = "aboveR"))
#' result <- terrain_change(before, after)
#' terra::plot(result[["change"]])
terrain_change <- function(before, after, tolerance = 0.1) {
  validate_raster(before, "before")
  validate_raster(after, "after")
  validate_crs_match(before, after)

  if (tolerance < 0) {
    stop("`tolerance` must be >= 0.", call. = FALSE)
  }

  aligned <- align_rasters(before, after)
  before <- aligned$r1
  after  <- aligned$r2

  change <- after - before

  cls <- terra::classify(
    change,
    rcl = matrix(c(-Inf, -tolerance, 1,
                    -tolerance, tolerance, 2,
                    tolerance, Inf, 3),
                 ncol = 3, byrow = TRUE)
  )

  names(change) <- "change"
  levels(cls) <- data.frame(id = 1:3, label = c("cut", "stable", "fill"))
  names(cls) <- "class"

  out <- c(change, cls)
  names(out) <- c("change", "class")
  out
}

#' Summarise Terrain Change by Zone
#'
#' Extracts change statistics for each polygon in a zone layer,
#' computing cut volume, fill volume, net change, and descriptive
#' statistics per zone.
#'
#' @param change_raster A [terra::SpatRaster] as returned by [terrain_change()]
#'   (uses the `"change"` layer).
#' @param zones An [sf] data frame of polygons defining analysis zones.
#' @param id_field Character. Column name in `zones` to use as zone identifier.
#'
#' @returns An [sf] data frame with columns:
#'   - zone identifier
#'   - `cut_volume`: total volume of material removed (m^3, positive)
#'   - `fill_volume`: total volume of material added (m^3, positive)
#'   - `net_volume`: fill - cut (m^3)
#'   - `area_m2`: zone area
#'   - `mean_change`: mean elevation change
#'   - `max_cut`: deepest cut (most negative value)
#'   - `max_fill`: highest fill (most positive value)
#'
#' @export
#'
#' @examples
#' before <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' after  <- terra::rast(system.file("extdata/dem_after.tif", package = "aboveR"))
#' chg <- terrain_change(before, after)
#' zones <- sf::st_read(
#'   system.file("extdata/zones.gpkg", package = "aboveR"),
#'   quiet = TRUE
#' )
#' change_by_zone(chg, zones, id_field = "zone_id")
change_by_zone <- function(change_raster, zones, id_field) {
  validate_raster(change_raster, "change_raster")
  validate_sf(zones, "zones", "POLYGON")

  if (!id_field %in% names(zones)) {
    stop("`id_field` '", id_field, "' not found in zones.", call. = FALSE)
  }

  # Use the continuous change layer
  if ("change" %in% names(change_raster)) {
    r <- change_raster[["change"]]
  } else {
    r <- change_raster[[1]]
  }

  # Reproject zones to raster CRS if needed
  if (sf::st_crs(zones) != terra::crs(r, describe = TRUE)$code) {
    zones <- sf::st_transform(zones, terra::crs(r))
  }

  ca <- cell_area_m2(r)

  result <- lapply(seq_len(nrow(zones)), function(i) {
    z <- zones[i, ]
    vals <- terra::extract(r, terra::vect(z))[[2]]
    vals <- vals[!is.na(vals)]

    cut_vals  <- vals[vals < 0]
    fill_vals <- vals[vals > 0]

    data.frame(
      cut_volume  = abs(sum(cut_vals)) * ca,
      fill_volume = sum(fill_vals) * ca,
      net_volume  = sum(vals) * ca,
      area_m2     = as.numeric(sf::st_area(z)),
      mean_change = mean(vals),
      max_cut     = if (length(cut_vals) > 0) min(cut_vals) else 0,
      max_fill    = if (length(fill_vals) > 0) max(fill_vals) else 0
    )
  })

  stats <- do.call(rbind, result)
  out <- zones[, id_field, drop = FALSE]
  cbind(out, stats)
}
