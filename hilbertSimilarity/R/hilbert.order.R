#' Estimate the Hilbert order for a given matrix
#'
#' Estimate the Hilbert order, or the number of bins in each dimension, so that if the matrix was
#' random every row in the matrix would correspond to a single bin.
#'
#' @param mat the matrix for which to estimate the Hilbert order
#'
#' @details Assuming the matrix is fully random, there is no need to generate more voxels
#'            (the combination of bins over all dimensions) than there are rows in the matrix. The number can be
#'            derived from the following formula:
#'            \deqn{c^{d} < N}{c^d < N}
#'            where \emph{c} is the number of bins, \emph{d} is the number of dimensions and \emph{N} is the total
#'            number of cells in the dataset. \emph{c} can be computed easily using the following formula:
#'            \deqn{c = \lfloor \sqrt[d]{N}}{c = floor(N^1/d)}
#'            The number of cuts for \code{\link{do.cut}} is the number of bins plus 1.
#'
#' @return the suggested number of bins to use for the specified \code{mat}.
#' @example examples/example.cut.R
#'
#' @author Yann Abraham
#' @export
hilbert.order <- function(mat) {
  return(round(nrow(mat)^(1/ncol(mat)),0))
}
