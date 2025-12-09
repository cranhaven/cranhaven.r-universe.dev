#' print.fitted_dlm
#'
#' This method is wrapper for the summary.fitted_dlm methdd.
#'
#' @param x A fitted_dlm object.
#' @param ... Arguments passed to summary.fitted_dlm
#'
#' @return No return value, called to print a summary of the fitted kDGLM model.
#'
#' @export
#' @keywords internal
#' @seealso \code{\link{summary.fitted_dlm}}
print.fitted_dlm <- function(x, ...) {
  summary.fitted_dlm(x, ...)
}

#' print.dlm_distr
#'
#' This method is wrapper for the summary.dlm_distr function.
#'
#' @param x A dlm_distr object.
#' @param ... Arguments passed to summary.dlm_distr
#'
#' @return No return value, called to print a summary of a kDGLM outcome.
#'
#' @export
#' @keywords internal
#' @seealso \code{\link{summary.dlm_distr}}
print.dlm_distr <- function(x, ...) {
  summary.dlm_distr(x, ...)
}

#' print.dlm_block
#'
#' This method is wrapper for the summary.dlm_block function.
#'
#' @param x A dlm_block object.
#' @param ... Arguments passed to summary.dlm_block
#'
#' @return No return value, called to print a summary of a kDGLM structure.
#'
#' @export
#' @keywords internal
#' @seealso \code{\link{summary.dlm_block}}
print.dlm_block <- function(x, ...) {
  summary.dlm_block(x, ...)
}

#' print.searched_dlm
#'
#' This method is wrapper for the block_superpos function.
#'
#' @param x A searched_dlm object.
#' @param ... Arguments passed to summary.searched_dlm
#'
#' @return No return value, called to print a summary of a searched_dlm object.
#'
#' @export
#' @keywords internal
#' @seealso \code{\link{summary.searched_dlm}}
print.searched_dlm <- function(x, ...) {
  summary.searched_dlm(x, ...)
}

#' coefficients.fitted_dlm
#'
#' This method is wrapper for the coef method.
#'
#' @param object A fitted_dlm object.
#' @param ... Arguments passed to coef.
#'
#' @importFrom stats coefficients
#'
#' @return A list containing:
#' \itemize{
#'    \item data data.frame: A table with the model evaluated at each observed time.
#'    \item theta.mean matrix: The mean of the latent states at each time. Dimensions are n x t, where t is the size of t.eval and n is the number of latent states.
#'    \item theta.cov array: A 3D-array containing the covariance matrix of the latent states at each time. Dimensions are n x n x t, where t is the size of t.eval and n is the number of latent states.
#'    \item lambda.mean matrix: The mean of the linear predictor at each time. Dimensions are k x t, where t is the size of t.eval and k is the number of linear predictors.
#'    \item lambda.cov array: A 3D-array containing the covariance matrix for the linear predictor at each time. Dimensions are k x k x t, where t is the size of t.eval and k is the number of linear predictors.
#'    \item log.like, mae, mase, rae, mse, interval.score: The metric value at each time.
#'    \item conj.param list: A list containing, for each outcome, a data.frame with the parameter of the conjugated distribution at each time.
#' }
#'
#' @export
#' @keywords internal
#' @seealso \code{\link{coef.fitted_dlm}}
coefficients.fitted_dlm <- function(object, ...) {
  coef.fitted_dlm(object, ...)
}

#' +.fitted_dlm
#'
#' Define add operator for class dlm_block.
#' This method is wrapper for the block_superpos function.
#'
#' @param e1 A dlm_block.
#' @param e2 A dlm_block.
#'
#' @return The combined blocks as a dlm_block.
#'
#' @export
#' @keywords internal
#' @seealso \code{\link{block_superpos}}
`+.dlm_block` <- function(e1, e2) {
  block_superpos(e1, e2)
}

#' *.fitted_dlm
#'
#' Define product operator for class dlm_block.
#' This method is wrapper for the block_mult function.
#'
#' @param e1 A dlm_block (if e2 is an integer) or an integer (if e2 is a dlm_block).
#' @param e2 An integer (if e1 is an dlm_block) or a dlm_block (if e1 is an integer).
#'
#' @return The combined replicated blocks as a dlm_block.
#'
#' @export
#' @keywords internal
#' @seealso \code{\link{block_mult}}
`*.dlm_block` <- function(e1, e2) {
  if (is.numeric(e2)) {
    return(block_mult(e1, e2))
  } else {
    return(block_mult(e2, e1))
  }
}

#' @importFrom generics specify
#' @export
generics::specify

#' @importFrom stats simulate
#' @export
stats::simulate

#' @importFrom generics forecast
#' @export
generics::forecast
