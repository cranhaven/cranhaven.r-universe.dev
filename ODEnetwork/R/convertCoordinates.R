#' Converts coordinates between cartesian and polar
#' 
#' Converts a given matrix with two rows from polar to cartesian coordinates and vice versa.
#'
#' @param coords [\code{matrix}]\cr
#'   Matrix with two columns.
#'   Each row contains the pair (x, y) in cartesian coordinates or
#'   (radius, angle) in polar coordinates. The angle is given in radian [0, 2*pi]
#' @param convertto [\code{character}]\cr
#'   Defines the target coordinate system for conversion.
#'   Options are "cartesian" and "polar".
#'   Default: "cartesian"
#' @return a matrix with converted coordinates
#' @export
#' @examples
#' if (interactive()) {
#'   coordsK <- rbind(c(3, 0), c(1, 3), c(0, 2), c(-3, 1), c(-1, 0), c(-1, -3), c(0, -2), c(2, -3))
#'   coordsP <- convertCoordinates(coordsK, "polar")
#' }
convertCoordinates <- function(coords, convertto = "cartesian") {
  assertMatrix(coords, mode="numeric", any.missing=FALSE, min.rows=1L, ncols=2L)
  assertChoice(convertto, c("cartesian", "polar"))
  
  if (ncol(coords) != 2)
    stop("The matrix with coordinates has to contain two columns.")
  
  if (convertto == "cartesian") {
    coords <- cbind(  x = coords[, 1] * cos(coords[, 2])
                    , y = coords[, 1] * sin(coords[, 2]))
  } else {
    coords <- cbind(  r = sqrt(coords[, 1]^2 + coords[, 2]^2)
                    , phi = atan2(coords[, 2], coords[, 1]))
    blnNeg <- coords[, 2] < 0
    coords[blnNeg, 2] <- coords[blnNeg, 2] + 2*pi
  }

  # Rueckgabe
  return(coords)
}
