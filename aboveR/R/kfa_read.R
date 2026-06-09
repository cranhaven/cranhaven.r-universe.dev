# kfa_read — aboveR
# Read KyFromAbove data products (DEM, point cloud, ortho)

#' Read KyFromAbove DEMs for an Area of Interest
#'
#' Finds DEM tiles covering the AOI, reads them as Cloud-Optimized
#' GeoTIFFs via `/vsicurl/`, optionally merges into a single raster,
#' and crops to the AOI extent.
#'
#' @param aoi An [sf] object or numeric bbox.
#' @param phase Integer. 1 (5ft), 2 (2ft), or 3 (2ft).
#' @param merge Logical. Mosaic multiple tiles into one SpatRaster? Default `TRUE`.
#' @param crop Logical. Crop result to AOI extent? Default `TRUE`.
#' @param cache Logical. Cache downloaded tiles locally? Default `FALSE`.
#'
#' @returns A [terra::SpatRaster] object.
#'
#' @export
#'
#' @examplesIf aboveR::has_s3_access()
#' dem <- kfa_read_dem(
#'   aoi = c(-84.55, 37.95, -84.45, 38.05),
#'   phase = 2
#' )
kfa_read_dem <- function(aoi, phase = 2L, merge = TRUE, crop = TRUE,
                         cache = FALSE) {
  tiles <- kfa_find_tiles(aoi, product = "dem", phase = phase)
  if (nrow(tiles) == 0) {
    stop("No DEM tiles found for this AOI and phase.", call. = FALSE)
  }

  urls <- resolve_tile_urls(tiles)
  validate_kfa_urls(urls)

  if (cache) {
    cache_dir <- file.path(tools::R_user_dir("aboveR", "cache"), "dem",
                            paste0("phase", phase))
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    local_files <- file.path(cache_dir, vapply(urls, sanitize_filename, character(1)))
    for (i in seq_along(urls)) {
      if (!file.exists(local_files[i])) {
        safe_download(urls[i], local_files[i], max_size_mb = 500)
      }
    }
    rasters <- lapply(local_files, terra::rast)
  } else {
    # Read via /vsicurl/ for COG access
    vsicurl_urls <- paste0("/vsicurl/", urls)
    rasters <- lapply(vsicurl_urls, terra::rast)
  }

  if (merge && length(rasters) > 1) {
    r <- do.call(terra::merge, rasters)
  } else if (length(rasters) == 1) {
    r <- rasters[[1]]
  } else {
    r <- terra::sprc(rasters)
    r <- terra::merge(r)
  }

  if (crop) {
    aoi_sf <- as_sf_polygon(aoi, target_crs = terra::crs(r))
    r <- terra::crop(r, terra::vect(aoi_sf))
  }

  r
}

#' Read KyFromAbove Point Cloud for an Area of Interest
#'
#' Finds point cloud tiles (LAZ for Phase 1, COPC for Phase 2/3)
#' covering the AOI and reads them via [lidR::readLAS()].
#'
#' @param aoi An [sf] object or numeric bbox.
#' @param phase Integer. 1, 2, or 3.
#'
#' @returns A [lidR::LAS] object.
#'
#' @export
#'
#' @examplesIf aboveR::has_s3_access()
#' \donttest{
#' las <- kfa_read_pointcloud(
#'   aoi = c(-84.55, 37.95, -84.54, 37.96),
#'   phase = 2
#' )
#' }
kfa_read_pointcloud <- function(aoi, phase = 2L) {
  tiles <- kfa_find_tiles(aoi, product = "pointcloud", phase = phase)
  if (nrow(tiles) == 0) {
    stop("No point cloud tiles found for this AOI and phase.", call. = FALSE)
  }

  urls <- resolve_tile_urls(tiles)
  validate_kfa_urls(urls)

  # Download to tempdir since lidR needs local files for LAZ
  tmp_dir <- tempdir()
  local_files <- file.path(tmp_dir, vapply(urls, sanitize_filename, character(1)))
  for (i in seq_along(urls)) {
    if (!file.exists(local_files[i])) {
      safe_download(urls[i], local_files[i], max_size_mb = 2000, timeout_sec = 600L)
    }
  }

  if (length(local_files) == 1) {
    lidR::readLAS(local_files[1])
  } else {
    ctg <- lidR::readLAScatalog(local_files)
    aoi_sf <- as_sf_polygon(aoi, target_crs = KFA_CRS)
    lidR::clip_roi(ctg, sf::st_geometry(aoi_sf))
  }
}

#' Read KyFromAbove Orthoimagery for an Area of Interest
#'
#' Finds ortho (nadir) or oblique imagery tiles covering the AOI
#' and reads them as RGB SpatRaster.
#'
#' @param aoi An [sf] object or numeric bbox.
#' @param phase Integer. 1, 2, or 3.
#' @param type Character. `"nadir"` (default) or `"oblique"` (Phase 3 only).
#'
#' @returns A [terra::SpatRaster] object with 3 bands (RGB).
#'
#' @export
#'
#' @examplesIf aboveR::has_s3_access()
#' \donttest{
#' ortho <- kfa_read_ortho(
#'   aoi = c(-84.55, 37.95, -84.54, 37.96),
#'   phase = 3
#' )
#' }
kfa_read_ortho <- function(aoi, phase = 3L, type = c("nadir", "oblique")) {
  type <- match.arg(type)

  if (type == "oblique" && phase != 3L) {
    stop("Oblique imagery is only available in Phase 3.", call. = FALSE)
  }

  product <- if (type == "oblique") "oblique" else "ortho"
  tiles <- kfa_find_tiles(aoi, product = product, phase = phase)
  if (nrow(tiles) == 0) {
    stop("No imagery tiles found for this AOI and phase.", call. = FALSE)
  }

  urls <- resolve_tile_urls(tiles)
  validate_kfa_urls(urls)

  vsicurl_urls <- paste0("/vsicurl/", urls)
  rasters <- lapply(vsicurl_urls, terra::rast)

  if (length(rasters) > 1) {
    r <- do.call(terra::merge, rasters)
  } else {
    r <- rasters[[1]]
  }

  aoi_sf <- as_sf_polygon(aoi, target_crs = terra::crs(r))
  terra::crop(r, terra::vect(aoi_sf))
}

#' Resolve S3 URLs from Tile Index Results
#'
#' Extracts or builds S3 URLs from tile index columns.
#'
#' @param tiles sf data frame from [kfa_find_tiles()].
#' @returns Character vector of HTTPS URLs.
#' @noRd
resolve_tile_urls <- function(tiles) {
  urls <- tiles$s3_url
  if (is.null(urls) && "aws_url" %in% names(tiles)) {
    urls <- tiles$aws_url
  }
  if (is.null(urls) || all(is.na(urls))) {
    if ("key" %in% names(tiles)) {
      urls <- paste0(KFA_BASE_URL, "/", tiles$key)
    } else {
      stop("Cannot determine S3 URLs from tile index.", call. = FALSE)
    }
  }
  urls
}
