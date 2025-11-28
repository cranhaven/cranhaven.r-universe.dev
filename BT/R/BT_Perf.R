#' Performance assessment.
#'
#' Function to compute the performances of a fitted boosting tree.
#'
#' @param BTFit_object a \code{\link{BTFit}} object resulting from an initial call to \code{\link{BT}}
#' @param plot.it a boolean indicating whether to plot the performance measure. Setting \code{plot.it = TRUE} creates two plots.
#' The first one plots the \code{object$BTErrors$training.error} (in black) as well as the \code{object$BTErrors$validation.error} (in red) and/or the \code{object$BTErrors$cv.error} (in green) depending on the \code{method} and
#' parametrization. These values are plotted as a function of the iteration number. The scale of the error measurement, shown on the left vertical axis, depends on the arguments used in the
#' initial call to \code{\link{BT}} and the chosen \code{method}.
#' @param oobag.curve indicates whether to plot the out-of-bag performance measures in a second plot. Note that this option makes sense if the \code{bag.fraction} was properly defined in the
#' initial call to \code{\link{BT}}.
#' @param overlay if set to \code{TRUE} and \code{oobag.curve=TRUE} then a right y-axis is added and the estimated cumulative improvement in the loss function is
#' plotted versus the iteration number.
#' @param method indicates the method used to estimate the optimal number of boosting iterations. Setting \code{method = "OOB"} computes the out-of-bag estimate and \code{method = "validation"}
#' uses the validation dataset to compute an out-of-sample estimate. Finally, setting \code{method = "cv"} extracts the optimal number of iterations using cross-validation, if
#' \code{\link{BT}} was called with \code{cv.folds > 1}. If missing, a guessing method is applied.
#' @param main optional parameter that allows the user to define specific plot title.
#'
#' @return Returns the estimated optimal number of iterations. The method of computation depends on the \code{method} argument.
#'
#' @author Gireg Willame \email{g.willame@@detralytics.eu}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{BT_call}}.
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
#' @rdname BT_perf
#' @export
#'
BT_perf <- function(BTFit_object,
                    plot.it = TRUE,
                    oobag.curve = FALSE,
                    overlay = TRUE,
                    method,
                    main = "") {
  if (!is.logical(plot.it) || (length(plot.it)) > 1 || is.na(plot.it))
    stop("plot.it must be a logical - excluding NA")

  performance <- .BT_callPerformance(BTFit_object, method)
  if (plot.it) {
    plot(
      performance,
      out_of_bag_curve = oobag.curve,
      overlay = overlay,
      main = main
    )
  }

  as.numeric(performance)
}

#' @keywords internal
.BT_callPerformance <- function(BTFit_object, method) {
  # Check inputs
  .check_if_BT_fit(BTFit_object)

  ## Guess the method
  if (missing(method)) {
    method <- .guess_error_method(BTFit_object)
    message("Using ", method, " method...")
  }

  result <- switch(
    method,
    OOB = .best_iter_out_of_bag(BTFit_object),
    validation = .best_iter_validation(BTFit_object),
    cv = .best_iter_cv(BTFit_object),
    stop("method must be validation, cv or OOB")
  )

  attr(result, "info") <-
    list(method = method, BTFit_object = BTFit_object)
  class(result) <- "BTPerformance"
  return(result)
}

#' @keywords internal
as.double.BTPerformance <- function(x, ...) {
  as.double(unclass(x))
}

#' @keywords internal
print.BTPerformance <- function(x, ...) {
  info <- attr(x, 'info')
  method_descriptor <-
    switch(
      info$method,
      cv = "cross-validation",
      validation = "validation-set",
      OOB = "out-of-bag",
      stop("Unknown method.")
    )

  cat("The best ", method_descriptor, " iteration was ", x, ".\n",
      sep = "")
  invisible(x)
}

#' @keywords internal
plot.BTPerformance <-
  function(x,
           out_of_bag_curve = FALSE,
           overlay = TRUE,
           main = "",
           ...) {
    info <- attr(x, 'info')
    .perf_plot(info$BTFit_object,
              x,
              out_of_bag_curve,
              overlay,
              info$method,
              main)
  }

#' @keywords internal
.best_iter_validation <- function(BTFit_object) {
  .check_if_BT_fit(BTFit_object)

  if (!.has_train_validation_split(BTFit_object)) {
    stop(
      'In order to use method = "validation" BT must be called with a properly defined train.fraction parameter.'
    )
  }

  best_iter_val <-
    which.min(.iteration_error(BTFit_object, 'validation'))
  return(best_iter_val)
}

#' @keywords internal
.best_iter_cv <- function(BTFit_object) {
  .check_if_BT_fit(BTFit_object)

  if (!.has_cross_validation(BTFit_object)) {
    stop('In order to use method="cv" BT must be called with cv_folds>1.')
  }

  best_iter_cv <- which.min(.iteration_error(BTFit_object, 'cv'))
  return(best_iter_cv)
}

#' @keywords internal
.best_iter_out_of_bag <- function(BTFit_object) {
  .check_if_BT_fit(BTFit_object)

  if (BTFit_object$BTParams$bag.fraction == 1)
    stop("Cannot compute OOB estimate or the OOB curve when bag.fraction=1")
  if (all(!is.finite(BTFit_object$BTErrors$oob.improvement)))
    stop("Cannot compute OOB estimate or the OOB curve. No finite OOB estimates of improvement")

  message(
    "OOB generally underestimates the optimal number of iterations although predictive performance is reasonably competitive.
            Using cv_folds>1 when calling BT usually results in improved predictive performance."
  )
  smoother <- .generate_smoother_oobag(BTFit_object)
  best_iter_oob <- smoother$x[which.min(-cumsum(smoother$y))]
  return(best_iter_oob)
}

#' @keywords internal
.generate_smoother_oobag <- function(BTFit_object) {
  .check_if_BT_fit(BTFit_object)
  smoother <- NULL
  x <- seq_len(BTFit_object$BTParams$n.iter)
  smoother <- loess(BTFit_object$BTErrors$oob.improvement ~ x,
                    enp.target = min(max(4, length(x) / 10), 50))
  smoother$y <- smoother$fitted
  smoother$x <- x
  return(smoother)
}

#' @keywords internal
.guess_error_method <- function(BTFit_object) {
  if (.has_train_validation_split(BTFit_object)) {
    "validation"
  } else if (.has_cross_validation(BTFit_object)) {
    "cv"
  } else{
    "OOB"
  }
}
