#' Computing a conditional sub-matrix of G
#'
#' \code{conditinoalG} calculates a conditional variance matrix.
#'
#' @param G A variance matrix (must be symmetric and positive definite).
#' @param condition_on Either an integer with the column number indicating which
#'   trait to condition on or a vector with several column numbers (integers).
#' @details The function calculates a sub-matrix of \code{G} conditional on the
#'   traits defined by the the \code{condition_on} vector. The function is based
#'   on equation 3 in Hansen et al. (2003).
#' @return A matrix that is a sub-matrix of the input matrix conditional on the
#'   non-included traits.
#' @references Hansen TF, Armbruster WS, Carlsson ML & Pélabon C. 2003.
#'   Evolvability and genetic constraint in Dalechampia blossoms: genetic
#'   correlations and conditional evolvability. J. Exp. Zool. 296B:23-39.
#' @author Geir H. Bolstad
#' @examples
#' # Constructing a G-matrix:
#' G <- matrix(c(
#'   1, 1, 0, 1,
#'   1, 2, 1, 1,
#'   0, 1, 2, 1,
#'   1, 1, 1, 3
#' ), ncol = 4)
#'
#' # Computing a conditional 2x2 sub-matrix by conditioning on
#' # trait 3 and 4:
#' G_sub_conditional <- conditionalG(G, condition_on = c(3, 4))
#' G_sub_conditional
#'
#' # The average evolvabilities of this matrix can then be
#' # compared can than be compared to the average evolvabilities
#' # of the corresponding unconditional sub-matrix of G:
#' evolvabilityMeans(G_sub_conditional)
#' evolvabilityMeans(G[-c(3, 4), -c(3, 4)])
#' @keywords array algebra
#' @export
conditionalG <- function(G, condition_on = NULL) {
  if (is.null(condition_on)) {
    stop("Specify one or several column numbers in a vector for 
          the argument condition_on (e.g. condition_on = c(1, 3) 
          for column 1 and 3)")
  }
  if (any(G[upper.tri(G)] != G[t(lower.tri(G))])) {
    stop("G is not symmetric.")
  }
  condition_on <- sort(condition_on)
  var_y <- G[-condition_on, -condition_on]
  var_x <- G[condition_on, condition_on]
  cov_yx <- G[condition_on, -condition_on]
  cov_xy <- G[-condition_on, condition_on]
  return(var_y - cov_xy %*% solve(var_x) %*% cov_yx)
}
