# utils — aboveR
# Shared utilities: CRS validation, bbox conversion, network checks

#' Validate That Two Rasters Share a CRS
#'
#' @param r1 A [terra::SpatRaster].
#' @param r2 A [terra::SpatRaster].
#' @returns `TRUE` invisibly if CRS matches; throws an error otherwise.
#' @noRd
validate_crs_match <- function(r1, r2) {
  crs1 <- terra::crs(r1, proj = TRUE)
  crs2 <- terra::crs(r2, proj = TRUE)
  if (crs1 != crs2) {
    stop(
      "CRS mismatch between rasters.\n",
      "  before: ", crs1, "\n",
      "  after:  ", crs2, "\n",
      "Reproject one raster to match the other before analysis.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate SpatRaster Input
#'
#' @param x Object to check.
#' @param name Label for error messages.
#' @returns `TRUE` invisibly.
#' @noRd
validate_raster <- function(x, name = "x") {
  if (!inherits(x, "SpatRaster")) {
    stop("`", name, "` must be a terra SpatRaster.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate sf Input
#'
#' @param x Object to check.
#' @param name Label for error messages.
#' @param geom_type Optional expected geometry type (e.g., "POLYGON").
#' @returns `TRUE` invisibly.
#' @noRd
validate_sf <- function(x, name = "x", geom_type = NULL) {
  if (!inherits(x, "sf")) {
    stop("`", name, "` must be an sf object.", call. = FALSE)
  }
  if (!is.null(geom_type)) {
    types <- unique(as.character(sf::st_geometry_type(x)))
    expected <- c(geom_type, paste0("MULTI", geom_type))
    if (!any(types %in% expected)) {
      stop(
        "`", name, "` must have ", geom_type, " geometry, got: ",
        paste(types, collapse = ", "),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Align Two Rasters to Common Extent and Resolution
#'
#' Crops to intersection and resamples the second raster to match the first
#' if resolutions differ.
#'
#' @param r1,r2 [terra::SpatRaster] objects.
#' @returns A list with aligned `r1` and `r2`.
#' @noRd
align_rasters <- function(r1, r2) {
  common_ext <- terra::intersect(terra::ext(r1), terra::ext(r2))
  if (is.null(common_ext)) {
    stop("Rasters do not overlap.", call. = FALSE)
  }
  r1 <- terra::crop(r1, common_ext)
  r2 <- terra::crop(r2, common_ext)

  res1 <- terra::res(r1)
  res2 <- terra::res(r2)
  if (!isTRUE(all.equal(res1, res2, tolerance = 1e-6))) {
    r2 <- terra::resample(r2, r1, method = "bilinear")
  }
  list(r1 = r1, r2 = r2)
}

#' Convert AOI to sf Object
#'
#' Accepts sf, sfc, bbox vector, or SpatExtent and returns an sf polygon.
#'
#' @param aoi Input area of interest.
#' @param target_crs Optional EPSG code to reproject to.
#' @returns An sf polygon.
#' @noRd
as_sf_polygon <- function(aoi, target_crs = NULL) {
  if (inherits(aoi, "SpatExtent")) {
    aoi <- sf::st_as_sfc(sf::st_bbox(c(
      xmin = aoi[1], ymin = aoi[3], xmax = aoi[2], ymax = aoi[4]
    )))
    aoi <- sf::st_sf(geometry = aoi)
  } else if (is.numeric(aoi) && length(aoi) == 4) {
    bbox <- sf::st_bbox(c(xmin = aoi[1], ymin = aoi[2], xmax = aoi[3], ymax = aoi[4]),
                         crs = sf::st_crs(4326))
    aoi <- sf::st_as_sfc(bbox)
    aoi <- sf::st_sf(geometry = aoi)
  } else if (inherits(aoi, "sfc")) {
    aoi <- sf::st_sf(geometry = aoi)
  } else if (!inherits(aoi, "sf")) {
    stop("`aoi` must be an sf object, sfc, or numeric bbox c(xmin, ymin, xmax, ymax).",
         call. = FALSE)
  }

  if (!is.null(target_crs)) {
    aoi <- sf::st_transform(aoi, target_crs)
  }
  aoi
}

#' Check S3 Access to KyFromAbove Bucket
#'
#' Returns `TRUE` only when the `ABOVER_KFA_TEST` environment variable
#' is set, ensuring KyFromAbove examples never run on CRAN or in
#' environments without verified S3 access.
#'
#' @returns Logical scalar indicating whether the KyFromAbove test
#'   environment variable is set.
#'
#' @export
#'
#' @examples
#' has_s3_access()
has_s3_access <- function() {
  nzchar(Sys.getenv("ABOVER_KFA_TEST"))
}

#' Get Path to Bundled Sample Data
#'
#' @param file Filename within inst/extdata.
#' @returns Full file path.
#' @noRd
sample_data <- function(file) {
  system.file("extdata", file, package = "aboveR", mustWork = TRUE)
}

#' Cell Area in Square Metres
#'
#' @param r A SpatRaster.
#' @returns Numeric scalar: area of one cell in m^2.
#' @noRd
cell_area_m2 <- function(r) {
  res_x <- terra::res(r)[1]
  res_y <- terra::res(r)[2]
  if (terra::is.lonlat(r)) {
    cs <- terra::cellSize(r, unit = "m")
    mean(terra::values(cs), na.rm = TRUE)
  } else {
    res_x * res_y
  }
}

#' Sample Points Along a Line
#'
#' @param line An sf LINESTRING.
#' @param spacing Numeric distance between sample points.
#' @returns sf POINT object with a `distance` column.
#' @noRd
sample_line_points <- function(line, spacing) {
  total_length <- as.numeric(sf::st_length(line))
  distances <- seq(0, total_length, by = spacing)
  pts <- sf::st_line_sample(line, sample = distances / total_length)
  pts <- sf::st_cast(pts, "POINT")
  sf::st_sf(distance = distances[seq_along(pts)], geometry = pts)
}
