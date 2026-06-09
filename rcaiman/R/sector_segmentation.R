#' Assign azimuth-sector labels
#'
#' @description
#' Segment a hemispherical view into equal azimuth sectors by slicing the
#' azimuth angle from `0` to `360` deg at fixed steps.
#'
#' @inheritParams sky_grid_segmentation
#' @param angle_width numeric vector of lenght one. Sector width in degrees.
#'   Must divide the 0–360 deg range into an integer number of sectors.
#'
#' @return Single-layer [terra::SpatRaster-class] with integer values.
#'   Segments will resemble pizza slices.
#'
#' @export
#'
#' @examples
#' z <- zenith_image(600, lens())
#' a <- azimuth_image(z)
#' sectors <- sector_segmentation(a, 15)
#' plot(sectors == 1)
sector_segmentation <- function(a, angle_width) {
  .check_r_z_a_m(NULL, NULL, a)
  .check_vector(angle_width, "numeric", 1, sign = "positive")
  if (tryCatch(!.check_vector(90 / angle_width, "integerish", 1,
                             sign = "positive"), error = function(e) TRUE )) {
    stop(
      "`angle_width` must divide the 0-90 range into a whole number."
    )
  }

  intervals <- seq(0, 360, angle_width)
  c1 <- intervals[1:(length(intervals) - 1)]
  c2 <- intervals[2:length(intervals)]
  c3 <- 1:(length(intervals) - 1)

  rcl <- matrix(c(c1, c2, c3), ncol = 3)
  sectors <- terra::classify(a, rcl)
  sectors[is.na(sectors)] <- 0
  names(sectors) <- "Sectors"
  sectors
}
