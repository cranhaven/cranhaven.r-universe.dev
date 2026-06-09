#' Display a canopy image
#'
#' Wrapper for [EBImage::display()] that streamlines the visualization of
#' canopy images, optionally overlaying binary masks and segmentation borders.
#' It is intended for quick inspection of processed or intermediate results in
#' a graphical viewer.
#'
#' @param caim [terra::SpatRaster-class]. Typically the output of [read_caim()].
#'   Can be multi- or single-layer.
#'
#' @inheritParams compute_canopy_openness
#' @inheritParams estimate_sun_angles
#'
#' @return Invisible `NULL`. Called for side effects (image viewer popup).
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' r <- normalize_minmax(caim$Blue)
#' g <- ring_segmentation(z, 30)
#' bin <- binarize_by_region(r, g, method = "thr_isodata")
#' display_caim(caim$Blue, bin, g)
#' }
display_caim <- function(caim = NULL, bin = NULL, g = NULL) {
  .this_requires_EBImage()

  if (!is.null(caim)) {
    .assert_spatraster(caim)
  }
  if (!is.null(bin)) {
    .assert_logical_mask(bin)
  }
  if (!is.null(g)) {
    .assert_single_layer(g)
  }


  # Highlight region borders if `g` is given
  if (!is.null(g)) {
    laplacian <- matrix(c(0, 1, 0, 1, -4, 1, 0, 1, 0), nrow = 3)
    g <- terra::focal(g, laplacian)
    g <- g != 0
  }

  layers <- list()

  if (!is.null(caim)) {
    layers <- c(layers, normalize_minmax(caim))
  }
  if (!is.null(bin)) {
    layers <- c(layers, bin)
  }
  if (!is.null(g)) {
    layers <- c(layers, g)
  }

  if (length(layers) > 0) {
    x <- do.call(c, layers)
    x <- terra::t(x)  # needed for compatibility
    as.array(x) %>% EBImage::display()
  } else {
    warning("Nothing to display")
  }
}

