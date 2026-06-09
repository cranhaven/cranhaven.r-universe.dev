#' Out-of-the-box reliable binarized image
#'
#' Robust binarization without parameter tuning.
#'
#' @details
#' Runs a predefined pipeline that incrementally refines a binary sky mask by
#' combining gradient-based enhancement, local thresholding, polar segmentation,
#' and a spectral index sensitive to sunlit canopy.
#'
#' \enumerate{
#'   \item \emph{Enhancement.} Compute complementary gradients with
#'   [complementary_gradients()] and build an enhancer that mixes the
#'   strongest complementary response with the blue band:
#'   `mem <- mean(normalize_minmax(max(yellow_blue, red_cyan)), normalize_minmax(Blue^(1/2.2)))`.
#'   Gamma correction (see [invert_gamma_correction()]) is applied to the blue band to
#'   reduce sky brightness variability.
#'
#'   \item \emph{Local thresholding.} Apply [apply_by_direction()] on `mem`
#'   with `method = "thr_isodata"` to obtain an initial binary mask. Local
#'   thresholding is required because background non-uniformity remains in the
#'   enhanced image.
#'
#'   \item \emph{Cleanup.} Remove isolated pixels and apply a one-pixel binary
#'   dilation. This compensates small artifacts produced by band misalignment
#'   resulting from the radiometric-first policy of [read_caim_raw()].
#'
#'   \item \emph{Polar quadtree segmentation.} Segment this preclassification of
#'   sky and non-sky pixels with [polar_qtree()] parameterized to yield circular
#'   trapezoids never smaller than \eqn{3 \times 3} degrees and to minimize
#'   segments with mixed classes.
#'
#'   \item \emph{Object-based image analysis.}
#'   Keep segments that contain between 10 and 90 percent of sky pixels.
#'   For each kept segment, estimate a local sky reference as the maximum blue
#'   value, use it to normalize per segment
#'   (`ratio <- Blue / sky_segment_max`), interpret the normalization as the degree
#'   of membership to the sky class, and then defuzzify with a fixed threshold
#'   `0.5`.
#'
#'   \item \emph{Blue–Red Index (BRI).} Compute
#'   \deqn{\mathrm{BRI} = \frac{B - R}{B + R}}
#'   where \eqn{B} and \eqn{R} are blue and red digital numbers. BRI decreases
#'   on sunlit canopy because direct sunlight is warmer than diffuse skylight.
#'   Use a scene-adaptive threshold given by the median BRI over the current
#'   non-sky region to flip misclassified sky pixels to non-sky.
#'
#'   \item \emph{Zenith mask.} Apply the final zenith-angle gate (e.g.,
#'   keep \eqn{\theta_z \le 88^\circ}).
#' }
#'
#'
#' @note
#' This function is part of a paper under preparation.
#'
#' @inheritParams complementary_gradients
#' @inheritParams sky_grid_segmentation
#' @inheritParams compute_canopy_openness
#' @inheritParams apply_by_direction
#'
#' @return Logical [terra::SpatRaster-class] (`TRUE` for sky, `FALSE` for non-sky)
#'   with the same number of rows and columns as `caim`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#' bin <- ootb_bin(caim, z, a, m)
#' plot(bin)
#' }

ootb_bin <- function(caim, z, a, m, parallel = TRUE){
  #basic checks handled by the functions called below
  com <- complementary_gradients(caim)
  mem <- max(com$yellow_blue, com$red_cyan)
  mem <- mean(normalize_minmax(mem), normalize_minmax(caim$Blue^(1/2.2)))
  thr <- apply_by_direction(mem, z, a, m, spacing = 15,
                            fov = 60, parallel = parallel,
                            method = "thr_isodata")
  bin <- binarize_with_thr(mem, thr$dn)
  bin <- rem_isolated_black_pixels(bin)
  bin <- grow_black(bin, 1)

  seg <- polar_qtree(bin, z, a, 0, 18, 3)
  gf <- extract_feature(bin, seg, mean)
  m2 <- select_sky_region(gf, 0.1, 0.9)
  sky <- extract_feature(caim$Blue, seg * m2, max)
  ratio <- caim$Blue/sky
  ratio[is.na(ratio)] <- 1
  bin <- binarize_with_thr(ratio, 0.5) & bin

  bri <- (caim$Blue - caim$Red) / (caim$Blue + caim$Red)
  thr <- median(bri[!bin], na.rm = TRUE)
  bin <- bin & binarize_with_thr(bri, pmin(0.05, thr))

  bin <- bin & select_sky_region(z, 0, 88)
  bin
}
