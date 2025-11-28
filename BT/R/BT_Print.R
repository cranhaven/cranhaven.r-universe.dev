#' Printing function.
#'
#' Function to print the BT results.
#'
#' @param x a \code{\link{BTFit}} object.
#' @param \dots arguments passed to \code{print.default}.
#'
#' @return No value returned.
#'
#' @details Print the different input parameters as well as obtained results (best iteration/performance & relative influence) given the chosen approach.
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{.BT_relative_influence}}, \code{\link{BT_perf}}.
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
#' @rdname print.BTFit
#' @export
#'
print.BTFit <- function(x, ...) {
  # Print call
  if (!is.null(x$call))
    print(x$call)

  #  Print out number of iterations and distribution used
  .print_iters_and_dist(x)

  # Print out performance measures
  best_iter <- .print_perf_measures(x)

  # Print out relative influence of variables
  ri <- .BT_relative_influence(x, n.iter = best_iter)
  cat(
    "There were",
    length(x$var.names),
    "predictors of which",
    sum(ri > 0),
    "had non-zero influence.\n"
  )

  return(invisible(x))
}


#### Helper Functions ####
#' @keywords internal
.print_iters_and_dist <- function(x) {
  .check_if_BT_fit(x)
  if (x$BTParams$ABT) {
    cat(
      "An adaptive boosting tree model with Tweedie parameter :",
      x$distribution,
      " has been fitted.\n",
      length(.iteration_error(x, 'train')),
      "iterations were performed.\n"
    )
  } else{
    cat(
      "A boosting tree model with Tweedie parameter :",
      x$distribution,
      " has been fitted.\n",
      length(.iteration_error(x, 'train')),
      "iterations were performed.\n"
    )
  }
}

#' @keywords internal
.print_perf_measures <- function(x) {
  # Calculate the best number of iterations - returns test set if possible
  .check_if_BT_fit(x)

  # Set default answer - final iteration
  best_iter <- length(.iteration_error(x, 'train'))

  # OOB best iteration.
  if (.has_bagging(x)) {
    best_iter <- print(.BT_callPerformance(x, method = "OOB"))
  }
  # CV best iteration
  if (.has_cross_validation(x)) {
    best_iter <- print(.BT_callPerformance(x, method = "cv"))
  }
  # Validation set best iteration
  if (.has_train_validation_split(x)) {
    best_iter <- print(.BT_callPerformance(x, method = "validation"))
  }

  return(best_iter)
}
