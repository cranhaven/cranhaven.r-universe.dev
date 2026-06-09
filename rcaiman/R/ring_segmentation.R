#' Assign zenith-ring labels
#'
#' @description
#' Segment a hemispherical view into concentric rings by slicing the zenith
#' angle from `0` to `90` deg at equal steps.
#'
#' @param angle_width numeric vector of length one. Ring width in degrees.
#'   Must divide the 0-90 deg range into an integer number of segments.
#' @param return character vector of length one. Output mode: "id" (default)
#'   or "angle".
#'
#' @inheritParams sky_grid_segmentation
#'
#' @return Single-layer [terra::SpatRaster-class]: ring IDs if `return = "id"`,
#'   or mean zenith angle (deg) if `return = "angle"`.
#' @export
#'
#' @examples
#' z <- zenith_image(600, lens())
#' rings <- ring_segmentation(z, 15)
#' plot(rings == 1)
ring_segmentation <- function(z, angle_width, return = "id") {
  .check_r_z_a_m(NULL, z)
  .check_vector(angle_width, "numeric", 1, sign = "positive")
  if (tryCatch(!.check_vector(90 / angle_width, "integerish", 1,
                             sign = "positive"), error = function(e) TRUE )) {
    stop(
      "`angle_width` must divide the 0-90 range into a whole number."
    )
  }
  .assert_choice(return, c("id", "angle"))

  intervals <- seq(0, 90, angle_width)
  c1 <- intervals[1:(length(intervals) - 1)]
  c2 <- intervals[2:length(intervals)]

  if (return == "angle") {
    c3 <- (c1 + c2) / 2
  } else {
    c3 <- 1:(length(intervals) - 1)
  }
  rcl <- matrix(c(c1, c2, c3), ncol = 3)
  rings <- terra::classify(z, rcl)
  rings[is.na(rings)] <- 0
  names(rings) <- "Rings"
  rings
}
