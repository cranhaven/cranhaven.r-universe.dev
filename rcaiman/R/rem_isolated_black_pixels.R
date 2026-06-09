#' Remove isolated black pixels
#'
#' @description
#' Replace single black pixels (`FALSE`) that are fully surrounded by white
#' pixels (`TRUE`) with white. Uses 8-connectivity.
#'
#' @inheritParams write_bin
#'
#' @returns Logical [terra::SpatRaster-class] of one layer.
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#'
#' path <- system.file("external/example.txt", package = "rcaiman")
#' sky <- read_sky_cie(gsub(".txt", "", path), caim$Blue, z, a)
#' plot(sky$rr_raster)
#' sky <- sky$rr_raster * sky$model$rr$zenith_dn
#'
#' bin <- binarize_with_thr(r / sky, 0.9)
#' plot(bin)
#' bin2 <- rem_isolated_black_pixels(bin)
#' plot(bin2)
#' }
rem_isolated_black_pixels <- function(bin) {
  .assert_logical_mask(bin)

  # Define a 3x3 Laplacian-like kernel
  ma <- matrix(c(1, 1, 1,
                 1, -8, 1,
                 1, 1, 1), ncol = 3)

  # Identify isolated black pixels: they have 8 white neighbors
  isolated <- terra::focal(bin, w = ma) == 8

  # Set them to white
  bin[isolated] <- 1

  # Ensure return values are logical (TRUE/FALSE)
  as.logical(bin)
}
