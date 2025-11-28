#' Cross-validation errors.
#'
#' Function to compute the cross-validation error.
#'
#' @param BT_cv_fit a \code{\link{BTCVFit}} object.
#' @param cv.folds a numeric corresponding to the number of folds.
#' @param folds a numerical vector containing the different \code{folds.id}. Note that if the latter was not defined by the user, those are randomly generated based on the \code{cv.folds} input.
#'
#' @return Vector containing the cross-validation errors w.r.t. the boosting iteration.
#'
#' @details
#' This function computes the global cross-validation error as a function of the boosting iteration. Differently said, this measure is obtained by
#' computing the average of out-of-fold errors.
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}.
#'
#' @references M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2022). \strong{Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link}.
#' Accepted for publication in \emph{Scandinavian Actuarial Journal}.
#'
#' M. Denuit, J. Huyghe and J. Trufin (2022). \strong{Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking}.
#' Paper submitted for publication.
#'
#' M. Denuit, J. Trufin and T. Verdebout (2022). \strong{Boosting on the responses with Tweedie loss functions}. Paper submitted for publication.
#'
#' @rdname BT_cv_errors
#' @keywords internal
.BT_cv_errors <- function(BT_cv_fit, cv.folds, folds) {
  UseMethod("BT_cv_errors", BT_cv_fit)
}

#' @keywords internal
BT_cv_errors.BTCVFit <- function(BT_cv_fit, cv.folds, folds) {
  .check_if_BTCV_fit(BT_cv_fit)

  in_group <- tabulate(folds, nbins = cv.folds)
  cv_error <- vapply(seq_len(cv.folds),
                     function(xx) {
                       model <- BT_cv_fit[[xx]]
                       model$BTErrors$validation.error * in_group[xx]
                     }, double(BT_cv_fit[[1]]$BTParams$n.iter)) # Similar structure for each BT_cv_fit.
  ## this is now a (num_trees, cv.folds) matrix
  ## and now a n.trees vector
  return(rowSums(cv_error) / length(folds))
}
