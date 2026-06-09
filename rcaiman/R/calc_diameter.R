#' Calculate diameter
#'
#' Calculate the diameter in pixels of a 180 deg fisheye image.
#'
#' This function is useful when the recording device has a field of view smaller
#' than 180 deg. Given a lens projection function and data points consisting of
#' radii (pixels) and their corresponding zenith angles (\eqn{\theta}), it
#' returns the horizon radius (i.e., the radius for \eqn{\theta} equal to 90 deg).
#'
#' When working with non-circular hemispherical photography, this function
#' helps determine the diameter that a circular image would have if the
#' equipment recorded the whole hemisphere, required to build the
#' correct zenith image to use as input for [expand_noncircular()].
#'
#' The required data (radius–angle pairs) can be obtained following the
#' instructions in the
#' [user manual of Hemisfer software](https://www.schleppi.ch/patrick/hemisfer/help/en/lens.htm).
#' A slightly simpler alternative is:
#'
#' 1. Find a vertical wall and a leveled floor, both well-constructed.
#' 2. Draw a triangle of \eqn{5 \times 4 \times 3} meters on the floor, with the
#'    4-meter side along the wall.
#' 3. Place the camera over the vertex 3 meters away from the wall, at a chosen
#'    height (e.g., 1.3 m).
#' 4. Make a mark on the wall at the chosen height over the wall-vertex nearest
#'    to the camera vertex. Make four more marks at 1 m intervals along a
#'    horizontal line. This creates marks for 0, 18, 34, 45, and 54 deg
#'    \eqn{\theta}.
#' 5. Before taking the photograph, align the zenith coordinates with the 0 deg
#'    \eqn{\theta} mark and ensure the optical axis is level.
#'
#' The [line selection tool](https://imagej.net/ij/docs/guide/146-19.html#toc-Subsection-19.2)
#' of [ImageJ](https://imagej.net/ij/) can be used to measure the distance in
#' pixels between points on the image. Draw a line and use the menu
#' Analyze > Measure to obtain its length.
#'
#' For obtaining the projection of a new lens, see [calibrate_lens()].
#'
#' @param radius numeric vector. Distance in pixels from the zenith.
#' @param angle numeric vector. Zenith angle in degrees.
#'
#' @inheritParams zenith_image
#'
#' @return Numeric vector of length one. Estimated diameter in pixels, rounded
#'   to the nearest even integer (see [zenith_image()] for details).
#'
#' @export
#'
#' @examples
#' # Nikon D50 and Fisheye Nikkor 10.5mm lens
#' calc_diameter(lens("Nikkor_10.5mm"), 1202, 54)
calc_diameter <- function(lens_coef, radius, angle) {
  .check_vector(radius, "numeric", sign = "positive")
  .check_vector(angle, "numeric", sign = "positive")
  if (length(radius) != length(angle)) {
    stop("`radius` and `angle` must have the same length.")
  }

  Rfor90 <- calc_relative_radius(90, lens_coef)
  RforMyAngle <- calc_relative_radius(angle, lens_coef)

  fun <- function(radius, RforMyAngle) {
    Rfor90 * radius / RforMyAngle * 2
  }

  if (length(radius) == 1) {
    diameter <- round(fun(radius, RforMyAngle))
  } else {
    diameters <- unlist(Map(fun, radius, RforMyAngle))
    diameter <- round(stats::median(diameters))
    attr(diameter, "IQR") <- stats::IQR(diameters)
  }

  if (tryCatch(!.check_vector(diameter, "evan_integerish", 1,
                              sign = "positive"), error = function(e) TRUE )) {
    diameter <- diameter + 1
  }

  diameter
}

