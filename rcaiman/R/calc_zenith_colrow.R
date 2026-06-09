#' Calculate zenith raster coordinates
#'
#' @description
#' Calculate zenith raster coordinates from points digitized with the
#' open-source software package ‘ImageJ’.
#'
#' @details
#' In this context, “zenith” denotes the location in the image that corresponds
#' to the projection of the vertical direction when the optical axis is aligned
#' vertically.
#'
#' The technique described under the headline ‘Optical center characterization’
#' of the [user manual of the software
#' Can-Eye](https://can-eye.paca.hub.inrae.fr/documentation/documentation) can
#' be used to acquire the data for determining the zenith coordinates. This
#' technique was used by \insertCite{Pekin2009;textual}{rcaiman}, among others.
#' Briefly, it consists in drilling a small hole in the cap of the fisheye lens
#' (away from the center), and taking about ten photographs without removing the
#' cap. The cap must be rotated about 30º before taking each photograph.
#'
#' The [point selection tool of ‘ImageJ’
#' software](https://imagej.net/ij/docs/guide/146-19.html#sec:Multi-point-Tool)
#' should be used to manually digitize the white dots and create a CSV file to
#' feed this function. After digitizing the points on the image, use the
#' dropdown menu Analyze>Measure to open the Results window. To obtain the CSV
#' file, use File>Save As...
#'
#' Another method (only valid when enough of the circle perimeter is depicted in
#' the image) is taking a very bright picture (e.g., of a white-painted corner
#' of a room) with the lens uncovered (do not use any mount). Then, digitize
#' points over the circle perimeter. This was the method used for producing the
#' example file (see Examples). It is worth noting that the perimeter of the
#' circle depicted in a circular hemispherical photograph is not necessarily the
#' horizon.
#'
#' @note
#' This function assumes that all data points belong to the same circle,
#' meaning that it does not support multiple holes when the Can-Eye procedure of
#' drilling the lens cap is applied. The circle is fitted using the method
#' presented by \insertCite{Kasa1976;textual}{rcaiman}.
#'
#' @param path_to_csv character vector of length one. Path to CSV file created
#'   with the ImageJ point selection tool.
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @return Numeric vector of length two. Raster coordinates of the zenith. These
#'   coordinates follow image (raster) convention: the origin is in the
#'   upper-left, and the vertical axis increases downward, like a spreadsheet.
#'   This contrasts with Cartesian coordinates, where the vertical axis
#'   increases upward.
#'
#' @examples
#' \dontrun{
#' path <- system.file("external/points_over_perimeter.csv",
#'                     package = "rcaiman")
#' calc_zenith_colrow(path)
#' }
calc_zenith_colrow <- function(path_to_csv) {
  .check_vector(path_to_csv, "character", 1)
  .assert_file_exists(path_to_csv)

  x <- utils::read.csv(path_to_csv)
  coords <- cbind(x$X, x$Y)

  x <- coords[, 1]
  y <- coords[, 2]
  A <- cbind(2 * x, 2 * y, rep(1, length(x)))
  b <- x^2 + y^2
  sol <- solve(t(A) %*% A, t(A) %*% b)

  cx <- sol[1]
  cy <- sol[2]

  out <- c(col = cx, row = cy)
  out
}
