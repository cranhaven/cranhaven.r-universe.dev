#' Extract feature
#'
#' @description Extract a numeric or logical summary from segmented raster
#' regions using a user-defined reducer, returning one value per segment as a
#' raster map or a named vector.
#'
#' @details Segments labeled `0` can be ignored via `ignore_label_0 = TRUE`. The
#' function in `fun` must return a single numeric or logical value for any input
#' vector (e.g., `mean`, `median`, or a custom reducer).
#'
#' @param segmentation single-layer [terra::SpatRaster-class]. Segmentation map
#'   of r, typically created with functions such as [sky_grid_segmentation()],
#'   [ring_segmentation()] or [sector_segmentation()], but any raster with
#'   integer segment labels is accepted.
#' @param fun function taking a numeric/logical vector and returning a single
#'   numeric or logical value (default `mean`).
#' @param return character of length one. Either `"raster"` (default) or
#'   `"vector"`, controlling whether to return a map with per-segment values or
#'   a named vector (one value per segment).
#' @param ignore_label_0 logical of length one. If `TRUE`, the segment labeled
#'   `0` is ignored.
#'
#' @inheritParams binarize_with_thr
#'
#' @return If `return = "raster"`, a [terra::SpatRaster-class] where each pixel
#'   holds its segment’s feature value. If `return = "vector"`, a named numeric
#'   (or logical) vector with one value per segment.
#'
#' @export
#'
#' @examples
#' r <- read_caim()
#' z <- zenith_image(ncol(r),lens())
#' a <- azimuth_image(z)
#' g <- sky_grid_segmentation(z, a, 10)
#' print(extract_feature(r$Blue, g, return = "vector"))
#' # plot(extract_feature(r$Blue, g, return = "raster"))
extract_feature <- function(r, segmentation,
                            fun = mean,
                            return = "raster",
                            ignore_label_0 = TRUE) {
  .assert_single_layer(r)
  .assert_single_layer(segmentation)
  .assert_same_geom(r, segmentation)
  if (!is.function(fun) && !methods::is(fun, "standardGeneric")) {
    stop("`fun` must be a function or a standard generic.")
  }
  .assert_choice(return, c("vector", "raster"))
  .check_vector(ignore_label_0, "logical", 1)

  if (ignore_label_0 == TRUE) segmentation[segmentation == 0] <- NA

  feature <- tapply(terra::values(r), terra::values(segmentation), fun)

  if (return == "raster") {
    id <- as.numeric(names(feature))
    return(terra::subst(segmentation, id, feature))
  } else {
    ids <- names(feature)
    feature <- as.numeric(feature)
    names(feature) <- ids
    return(feature)
  }
}
