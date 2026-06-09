# highwall — aboveR
# Highwall classification from slope analysis

#' Classify Highwall Areas from a DEM
#'
#' Identifies steep terrain faces (highwalls) typical of surface mining
#' operations by computing slope from a DEM and classifying cells that
#' exceed a slope threshold. Returns a binary raster and optionally
#' vectorised polygons of highwall zones.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param slope_threshold Numeric. Minimum slope in degrees to classify
#'   as highwall. Default `60`.
#' @param min_area Numeric. Minimum contiguous area (in map units squared)
#'   for a highwall zone. Smaller patches are removed. Default `0`
#'   (keep all).
#' @param as_polygons Logical. Return vectorised polygons instead of a
#'   raster? Default `FALSE`.
#'
#' @returns If `as_polygons = FALSE`, a [terra::SpatRaster] with values
#'   1 (highwall) and `NA` (non-highwall). If `as_polygons = TRUE`, an
#'   [sf] data frame of highwall polygons with an `area_m2` column.
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' hw <- classify_highwall(dem, slope_threshold = 5)
#' terra::plot(hw)
classify_highwall <- function(dem, slope_threshold = 60, min_area = 0,
                              as_polygons = FALSE) {
  validate_raster(dem, "dem")

  slp <- terra::terrain(dem, v = "slope", unit = "degrees")
  hw <- terra::classify(
    slp,
    rcl = matrix(c(-Inf, slope_threshold, NA,
                    slope_threshold, Inf, 1),
                 ncol = 3, byrow = TRUE)
  )
  names(hw) <- "highwall"

  if (min_area > 0) {
    patches <- terra::patches(hw, directions = 8)
    freq_tbl <- terra::freq(patches)
    ca <- cell_area_m2(dem)
    small_ids <- freq_tbl$value[freq_tbl$count * ca < min_area]
    if (length(small_ids) > 0) {
      hw[patches %in% small_ids] <- NA
    }
  }

  if (as_polygons) {
    polys <- terra::as.polygons(hw, dissolve = TRUE)
    polys_sf <- sf::st_as_sf(polys)
    polys_sf$area_m2 <- as.numeric(sf::st_area(polys_sf))
    return(polys_sf)
  }

  hw
}

#' Detect Mining Benches from a DEM
#'
#' Identifies flat bench areas between highwall faces in surface mining
#' terrain. Benches are detected as relatively flat areas (low slope)
#' bounded by steep terrain (high slope), using slope segmentation
#' and area filtering.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param max_slope Numeric. Maximum slope in degrees for a cell to be
#'   considered part of a bench. Default `10`.
#' @param min_area Numeric. Minimum contiguous area (in map units
#'   squared) for a bench. Smaller patches are removed. Default `100`.
#' @param highwall_slope Numeric. Minimum slope for adjacent highwall
#'   terrain. Default `40`.
#'
#' @returns An [sf] data frame of bench polygons with columns:
#'   - `bench_id`: sequential bench identifier
#'   - `area_m2`: bench area in square metres
#'   - `mean_elev`: mean elevation of the bench surface
#'   - `mean_slope`: mean slope within the bench
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' benches <- bench_detection(dem, max_slope = 15, min_area = 0)
#' if (nrow(benches) > 0) plot(sf::st_geometry(benches))
bench_detection <- function(dem, max_slope = 10, min_area = 100,
                            highwall_slope = 40) {
  validate_raster(dem, "dem")

  slp <- terra::terrain(dem, v = "slope", unit = "degrees")

  # Identify flat areas (potential benches)
  flat <- terra::classify(
    slp,
    rcl = matrix(c(-Inf, max_slope, 1,
                    max_slope, Inf, NA),
                 ncol = 3, byrow = TRUE)
  )

  # Identify highwall areas
  hw <- terra::classify(
    slp,
    rcl = matrix(c(-Inf, highwall_slope, NA,
                    highwall_slope, Inf, 1),
                 ncol = 3, byrow = TRUE)
  )

  # Only keep flat patches that are adjacent to highwall terrain
  # Expand highwall by 1 cell to find adjacency
  hw_buffer <- terra::focal(hw, w = 3, fun = "max", na.rm = TRUE,
                             na.policy = "only")
  hw_buffer <- terra::cover(hw, hw_buffer)

  # Bench = flat AND adjacent to highwall
  bench <- flat * terra::cover(hw_buffer, terra::init(flat, 0))
  bench[bench == 0] <- NA

  # Patch and filter by area
  patches <- terra::patches(bench, directions = 8)
  if (all(is.na(terra::values(patches)))) {
    # No benches found — return empty sf
    return(sf::st_sf(
      bench_id = integer(0), area_m2 = numeric(0),
      mean_elev = numeric(0), mean_slope = numeric(0),
      geometry = sf::st_sfc(crs = terra::crs(dem))
    ))
  }

  ca <- cell_area_m2(dem)
  freq_tbl <- terra::freq(patches)

  if (min_area > 0) {
    keep_ids <- freq_tbl$value[freq_tbl$count * ca >= min_area]
    if (length(keep_ids) == 0) {
      return(sf::st_sf(
        bench_id = integer(0), area_m2 = numeric(0),
        mean_elev = numeric(0), mean_slope = numeric(0),
        geometry = sf::st_sfc(crs = terra::crs(dem))
      ))
    }
    patches[!patches %in% keep_ids] <- NA
  }

  polys <- terra::as.polygons(patches, dissolve = TRUE)
  polys_sf <- sf::st_as_sf(polys)

  # Compute bench attributes
  polys_sf$bench_id <- seq_len(nrow(polys_sf))
  polys_sf$area_m2 <- as.numeric(sf::st_area(polys_sf))

  elev_vals <- terra::extract(dem, terra::vect(polys_sf), fun = mean,
                               na.rm = TRUE)
  slope_vals <- terra::extract(slp, terra::vect(polys_sf), fun = mean,
                                na.rm = TRUE)

  polys_sf$mean_elev <- elev_vals[[2]]
  polys_sf$mean_slope <- slope_vals[[2]]

  # Keep only relevant columns
  keep_cols <- c("bench_id", "area_m2", "mean_elev", "mean_slope",
                 attr(polys_sf, "sf_column"))
  polys_sf[, keep_cols]
}
