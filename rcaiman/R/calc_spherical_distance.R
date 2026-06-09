#' Calculate spherical distance
#'
#' Computes the angular distance, in radians, between directions defined by
#' zenith and azimuth angles on the unit sphere.
#'
#' @details This function calculates the angle between two directions
#' originating from the center of a unit sphere, using spherical trigonometry.
#' The result is commonly referred to as *spherical distance* or *angular
#' distance*. These terms are interchangeable when the sphere has radius one, as
#' is standard in many applications, including celestial coordinate systems and,
#' by extension, canopy hemispherical photography.
#'
#' Spherical distance corresponds to the arc length of the shortest path between
#' two points on the surface of a sphere. When the radius is one, this arc
#' length equals the angle itself, expressed in radians.
#'
#' @param z1 numeric vector. Zenithal angle in radians.
#' @param a1 numeric vector. Azimuthal angle in radians.
#' @param z2 numeric vector of length one. Zenithal angle in radians.
#' @param a2 numeric vector of length one. Azimuthal angle in radians.
#'
#' @returns Numeric vector of the same length as `z1` and `a1`, containing the
#'   spherical distance (in radians) from each (`z1`, `a1`) point to the
#'   reference direction (`z2`, `a2`).
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' z1 <- rnorm(10, 45, 20) * pi/180
#' a1 <- rnorm(10, 180, 90) * pi/180
#' calc_spherical_distance(z1, a1, 0, 0)
calc_spherical_distance  <- function(z1, a1, z2, a2) {
  # .check_vector(z1, "numeric", sign = "any") #to make it faster
  # .check_vector(a1, "numeric", sign = "any")
  # .check_vector(z2, "numeric", 1, sign = "any")
  # .check_vector(a2, "numeric", 1, sign = "any")

  #https://stackoverflow.com/questions/14026297/acos1-returns-nan-for-some-values-not-others
  acos(pmax(pmin(cos(z1) * cos(z2) +
                   sin(z1) * sin(z2) * cos(abs(a2 - a1)), 1), -1)) %>% unname()
}
calc_spherical_distance <- compiler::cmpfun(calc_spherical_distance)
