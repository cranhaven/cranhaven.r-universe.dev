#' Constructor of the class ODEnetwork
#' 
#' Creates a list of class \code{ODEnetwork}.
#' The coordinate type can be set to cartesian (position and
#' velocity) or to polar coordinates (angle and magnitude).
#'
#' @param masses [\code{vector}] of length n\cr
#'   The masses of the mechanical oscillators.
#' @param dampers [\code{matrix}] quadratic of size n\cr
#'   The dampers of the mechanical oscillators on the main diagonal.
#'   Connecting dampers between oscillators on the upper triangle.
#'   (Will be copied automatically to create a symmetric matrix.)
#' @param springs [\code{matrix}] quadratic of size n\cr
#'   The springs are defined in the network like matrix of dampers.
#' @param cartesian [\code{boolean(1)}]\cr
#'    If \code{TRUE}, \code{state1} and \code{state2} are position and velocity,
#'    otherwise angle and magnitude.
#'    Default is \code{TRUE}.
#' @param distances [\code{matrix}] quadratic of size n\cr
#'    Describes the length of each spring.
#'    Elements on the main diagonal describe spring length connecting the masses to the ground.
#'    All upper triangle elements describe spring distance between two masses i < j.
#'    Default is \code{NA}, which is equivalent to a zero matrix.
#'    (Value will be copied automatically to lower triangle creating a symmetric matrix.)
#' @return a list of class [\code{\link{ODEnetwork}}].
#' @export
#' @examples
#' mM <- c(40, 10, 10)
#' mD <- diag(c(1, 5, 0))
#' mD[1, 2] <- 1
#' mD[2, 3] <- 1
#' mK <- diag(c(50, 50, 0))
#' mK[1, 2] <- 10
#' mK[2, 3] <- 10
#' odenet <- ODEnetwork(mM, mD, mK)
ODEnetwork <- function(masses, dampers, springs, cartesian=TRUE, distances=NA) {
  assertNumeric(masses, any.missing=FALSE, min.len=1L)
  assertVector(masses, strict=TRUE)
  assertMatrix(dampers, mode="numeric", any.missing=FALSE, min.rows=1L, min.cols=1L)
  assertMatrix(springs, mode="numeric", any.missing=FALSE, min.rows=1L, min.cols=1L)
  assertLogical(cartesian, any.missing=FALSE, len=1L)
  
  # test on equal dimenstions
  if (stats::var(c(length(masses), dim(dampers), dim(springs))) != 0)
    stop("All parameter have be of the same length or size!")
  # mass has to be positive
  if (sum(masses <= 0) > 0)
    stop("All masses have to be positive.")
  # positive springs
  if (sum(springs < 0) > 0)
    stop("Springs must be nonzero.")
  # check distances
  if (!is.matrix(distances) && !is.numeric(distances))
    distances <- diag(0, length(masses))
  if (is.matrix(distances))
    distances[!is.numeric(distances)] <- 0
  
  # copy upper triangle to lower triangle => symmetric matrix
  dampers[lower.tri(dampers)] <- t(dampers)[lower.tri(dampers)]
  springs[lower.tri(springs)] <- t(springs)[lower.tri(springs)]
  distances[lower.tri(distances)] <- t(distances)[lower.tri(distances)]
  
  # set state type
  if (cartesian)
    coordtype <- "cartesian"
  else
    coordtype <- "polar"
  
  # add empty states
  states <- cbind(state1 = rep(0, length(masses)), state2 = rep(0, length(masses)))
  
  # return network
  odenet = list(masses = masses, dampers = dampers, springs = springs
                , distances = distances, coordtype = coordtype, state = states
                )
  class(odenet) = "ODEnetwork"
  return(odenet)
}
