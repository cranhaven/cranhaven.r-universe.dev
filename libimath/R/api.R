#' Rotate Point
#'
#' This rotates a point around the origin at the angles specified. This
#' function is primarily just included as an example of integrating the Imath
#' library into a package. See imath-info.cpp in the source for the corresponding
#' C++ code.
#'
#' @param point A length-3 numeric vector (x, y, z)
#' @param angles A length-3 numeric vector (rotation angles in radians)
#' @return The rotated point as an R numeric vector
#' @export
#' @examples
#' # This rotates a point around an angle.
#' point = c(1.0, 0.0, 0.0)
#' angles = c(0.0, pi/4, 0.0)
#' imath_rotate_point(point, angles)
imath_rotate_point = function(point, angles) {
  rotated = .Call(
    "imath_rotate_point",
    point,
    angles,
    PACKAGE = "libimath"
  )
  return(rotated)
}


#' Print the Imath library version info
#'
#' @return None.
#' @export
#' @examples
#' # Print the Imath version provided in the static library
#' print_imath_version()
print_imath_version = function() {
  .Call(
    "C_print_imath_version",
    PACKAGE = "libimath"
  )
  return(invisible())
}
