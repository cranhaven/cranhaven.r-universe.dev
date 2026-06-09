# kfa_stac — aboveR
# STAC catalog access for KyFromAbove (when catalog is live)
# Reference: github.com/ianhorn/kyfromabove-stac

#' Search KyFromAbove STAC Catalog
#'
#' Queries the KyFromAbove STAC catalog for items matching an area
#' of interest and product type. Requires the \pkg{rstac} package.
#'
#' The KyFromAbove STAC catalog is under active development by
#' Ian Horn (github.com/ianhorn/kyfromabove-stac). This function
#' will be activated when the catalog is live. In the meantime,
#' use [kfa_find_tiles()] with `method = "index"`.
#'
#' @param aoi An [sf] object or numeric bbox.
#' @param collection Character. STAC collection ID (e.g.,
#'   `"dem_phase2"`).
#' @param datetime Character. ISO 8601 datetime or range
#'   (e.g., `"2023-01-01/2023-12-31"`).
#'
#' @returns An [sf] data frame of STAC items with asset URLs, or
#'   an error if the STAC catalog is not yet available.
#'
#' @export
#'
#' @examplesIf FALSE
#' # STAC catalog not yet available — use kfa_find_tiles() instead
#' tiles <- kfa_find_tiles(
#'   aoi = c(-84.55, 37.95, -84.45, 38.05),
#'   product = "dem", phase = 2
#' )
kfa_stac_search <- function(aoi, collection = NULL, datetime = NULL) {
  if (!requireNamespace("rstac", quietly = TRUE)) {
    stop(
      "Package 'rstac' is required for STAC catalog access.\n",
      "Install it with: install.packages('rstac')",
      call. = FALSE
    )
  }
  stop(
    "KyFromAbove STAC catalog not yet available.\n",
    "Use kfa_find_tiles() with method = 'index' instead.",
    call. = FALSE
  )
}
