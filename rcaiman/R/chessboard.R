#' Perform chessboard segmentation
#'
#' Segment a raster into square regions of equal size arranged in a
#' chessboard-like pattern.
#'
#' This function divides the extent of a [terra::SpatRaster-class] into
#' non-overlapping square segments of the given size, producing a segmentation
#' map where each segment has a unique integer label. It can be an alternative
#' to [sky_grid_segmentation()] in special cases.
#'
#' @inheritParams polar_qtree
#'
#' @param size Numeric vector of length one. Size (in pixels) of each square
#'   segment. Must be a positive integer.
#'
#' @return [terra::SpatRaster-class] with one layer and integer values, where
#'   each unique value corresponds to a square-segment ID.
#'
#' @export
#'
#' @examples
#' caim <- read_caim()
#' seg <- chessboard(caim, 20)
#' plot(caim$Blue)
#' plot(extract_feature(caim$Blue, seg))
chessboard <- function(r, size) {
  .assert_spatraster(r)
  .check_vector(size, "integerish", sign = "positive")

  x <- ncol(r)/size %>% trunc()
  y <- nrow(r)/size %>% trunc()
  .r <- terra::rast(ncol = x+1, nrow = y+1)
  terra::values(.r) <- 1:terra::ncell(.r)
  .r <- terra::disagg(.r, size)
  terra::ext(.r) <- terra::ext(0,ncol(.r),0,nrow(.r))
  .r <- terra::crop(.r, r)
  terra::crs(.r) <- terra::crs(r)
  .r
}
