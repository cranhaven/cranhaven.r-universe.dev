# kfa_tiles — aboveR
# KyFromAbove tile discovery via tile index GeoPackages or STAC

#' Find KyFromAbove Tiles Covering an Area of Interest
#'
#' Queries KyFromAbove tile indexes to find elevation, point cloud,
#' or imagery tiles that intersect a given area of interest. Tries
#' the STAC catalog first (if available), then falls back to the
#' tile index GeoPackage on S3.
#'
#' @param aoi An [sf] object or numeric bbox (`c(xmin, ymin, xmax, ymax)`)
#'   defining the area of interest. Any CRS is accepted; reprojection to
#'   EPSG:3089 (Kentucky Single Zone) is handled internally.
#' @param product Character. One of `"dem"`, `"pointcloud"`, `"ortho"`,
#'   `"contours"`, `"oblique"`.
#' @param phase Integer. KyFromAbove acquisition phase: 1, 2, or 3.
#'   Phase 1 DEMs are 5ft resolution; Phase 2/3 are 2ft.
#' @param method Character. `"auto"` (default) tries STAC first, then
#'   tile index. `"stac"` uses STAC only. `"index"` uses tile index only.
#'
#' @returns An [sf] data frame with columns: `tilename`, `s3_url`, `phase`,
#'   `product`, and `geometry`.
#'
#' @export
#'
#' @examplesIf aboveR::has_s3_access()
#' # Find Phase 2 DEM tiles for a bounding box in Fayette County
#' tiles <- kfa_find_tiles(
#'   aoi = c(-84.55, 37.95, -84.45, 38.05),
#'   product = "dem",
#'   phase = 2
#' )
kfa_find_tiles <- function(aoi, product = "dem", phase = 2L,
                           method = c("auto", "stac", "index")) {
  method <- match.arg(method)
  product <- match.arg(product, names(KFA_PRODUCTS))
  phase <- as.integer(phase)
  if (!phase %in% 1:3) {
    stop("`phase` must be 1, 2, or 3.", call. = FALSE)
  }

  aoi_sf <- as_sf_polygon(aoi, target_crs = KFA_CRS)

  # Try STAC first if method allows
  if (method %in% c("auto", "stac")) {
    stac_result <- tryCatch(
      kfa_stac_search(aoi_sf, collection = paste0(product, "_phase", phase)),
      error = function(e) NULL
    )
    if (!is.null(stac_result) && nrow(stac_result) > 0) {
      return(stac_result)
    }
    if (method == "stac") {
      stop("STAC catalog returned no results for this query.", call. = FALSE)
    }
  }

  # Fallback to tile index
  idx <- kfa_tile_index(product = product, phase = phase)
  idx <- sf::st_transform(idx, KFA_CRS)
  aoi_sf <- sf::st_transform(aoi_sf, KFA_CRS)

  hits <- idx[sf::st_intersects(idx, sf::st_union(aoi_sf), sparse = FALSE)[, 1], ]

  if (nrow(hits) == 0) {
    message("No tiles found for this AOI/product/phase combination.")
    return(hits)
  }

  hits$phase <- phase
  hits$product <- product
  hits
}

#' Load and Cache a KyFromAbove Tile Index
#'
#' Downloads a tile index GeoPackage from the KyFromAbove S3 bucket
#' and caches it locally. Subsequent calls use the cached copy unless
#' it is older than `max_age_days`.
#'
#' @param product Character. One of `"dem"`, `"pointcloud"`, `"ortho"`.
#' @param phase Integer. KyFromAbove acquisition phase: 1, 2, or 3.
#' @param max_age_days Integer. Re-download if cache is older than this. Default 30.
#'
#' @returns An [sf] data frame representing the tile index grid.
#'
#' @export
#'
#' @examplesIf aboveR::has_s3_access()
#' idx <- kfa_tile_index(product = "dem", phase = 2)
#' head(idx)
kfa_tile_index <- function(product = "dem", phase = 2L, max_age_days = 30L) {
  product <- match.arg(product, names(KFA_PRODUCTS))
  phase <- as.integer(phase)

  cache_dir <- tools::R_user_dir("aboveR", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cache_file <- file.path(cache_dir,
                           paste0("tileindex_", product, "_phase", phase, ".gpkg"))

  # Check cache freshness
  use_cache <- file.exists(cache_file) &&
    difftime(Sys.time(), file.mtime(cache_file), units = "days") < max_age_days

  if (!use_cache) {
    # Build the URL for the tile index GPKG
    # Convention: indexes/KyFromAbove_{Product}_Phase{N}_TileIndex.gpkg
    product_label <- switch(product,
      dem = "DEM",
      pointcloud = "PointCloud",
      ortho = "Ortho",
      contours = "Contours",
      oblique = "Oblique"
    )
    index_url <- paste0(
      KFA_BASE_URL, "/indexes/KyFromAbove_", product_label,
      "_Phase", phase, "_TileIndex.gpkg"
    )

    validate_kfa_url(index_url)
    safe_download(index_url, cache_file, max_size_mb = 200, timeout_sec = 120L)
  }

  sf::st_read(cache_file, quiet = TRUE)
}
