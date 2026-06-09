#' Defuzzify a fuzzy classification
#'
#' Converts fuzzy membership values into a binary classification using a
#' regional approach that preserves aggregation consistency between the fuzzy
#' and binary representations.
#'
#' The conversion is applied within segments defined by `segmentation`,
#' ensuring that, in each segment, the aggregated Boolean result matches the
#' aggregated fuzzy value. This approach is well suited for converting subpixel
#' estimates, such as gap fraction, into binary outputs.
#'
#' @param mem numeric [terra::SpatRaster-class] of one layer. Degree of
#'   membership in a fuzzy classification.
#' @param segmentation single-layer [terra::SpatRaster-class] with integer
#'   values.
#'
#' @return Logical [terra::SpatRaster-class] of the same dimensions as `mem`,
#'   where each pixel value represents the binary version of `mem` after
#'   applying the regional defuzzification procedure.
#'
#' @note This method is also available in the HSP software package. See
#'   [hsp_compat()].
#'
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
#' sky_cie <- read_sky_cie(gsub(".txt", "", path), z, a)
#'
#' sky_above <- ootb_sky_above(sky_cie$model$rr$sky_points, z, a, sky_cie)
#'
#' ratio <- r / sky_above$dn_raster
#' ratio <- normalize_minmax(ratio, 0, 1, TRUE)
#' plot(ratio)
#' g <- sky_grid_segmentation(z, a, 10)
#' bin2 <- defuzzify(ratio, g)
#' plot(bin2) # unsatisfactory results due to light conditions
#' }
defuzzify <- function (mem, segmentation) {
  .assert_single_layer(mem)
  if (any(.get_max(mem) > 1, .get_min(mem) < 0)) {
    warning("Check if 'mem' values are degree of membership in a fuzzy classification.")
  }
  .assert_single_layer(segmentation)
  .assert_same_geom(mem, segmentation)

  mem[is.na(mem)] <- 0

  .fun <- function(x) {
    no_of_pixels <- round(mean(x) * length(x))
    if (no_of_pixels > 0 &
        no_of_pixels != length(x)) {
      indices <- order(x, decreasing = TRUE)[1:no_of_pixels]
      x[indices] <- 1
      x[x != 1] <- 0
    } else {
      x <- as.numeric(x > 0.5)
    }
    x
  }

  cells <- mem
  terra::values(cells) <- 1:ncell(mem)
  cells <- tapply(terra::values(cells),
                   terra::values(segmentation), function(x) x)
  cells <- unlist(cells)
  bin <- tapply(terra::values(mem), terra::values(segmentation), .fun)
  mem[cells] <- unlist(bin)
  as.logical(mem)
}
