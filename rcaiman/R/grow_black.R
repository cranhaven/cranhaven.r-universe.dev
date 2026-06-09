#' Grow black regions in a binary mask
#'
#' @description
#' Grow black pixels in a binary mask using a kernel of user-defined size.
#' Useful to reduce errors associated with inter-class borders.
#'
#' @details
#' Expands the regions with value `FALSE` (typically rendered as black) in a
#' binary image by applying a square-shaped buffer. Any white pixels (value
#' `TRUE`) within a distance equal to or less than `dist_to_black` from a black
#' pixel will be turned black.
#'
#' @param dist_to_black numeric vector of length one. Buffer distance (pixels)
#'   used to expand black regions.
#'
#' @inheritParams compute_canopy_openness
#'
#' @return
#' Logical [terra::SpatRaster-class] with the same dimensions as `bin`. Compared
#' to the input `bin`, black regions (`FALSE`) have been expanded by the
#' specified buffer distance.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' r <- read_caim()
#' bin <- binarize_with_thr(r$Blue, thr_isodata(r$Blue[]))
#' plot(bin)
#' bin <- grow_black(bin, 11)
#' plot(bin)
#' }
grow_black <- function(bin, dist_to_black) {
  .this_requires_EBImage()
  .assert_logical_mask(bin)
  .check_vector(dist_to_black, "integerish", 1, sign = "positive")

  kern <- EBImage::makeBrush(dist_to_black, "box")
  bin <- EBImage::erode(as.array(bin), kern) %>%
    terra::setValues(bin, .)
  binarize_with_thr(bin, 0)
}
