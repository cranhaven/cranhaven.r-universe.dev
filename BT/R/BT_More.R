#' Perform additional boosting iterations.
#'
#' Method to perform additional iterations of the Boosting Tree algorithm, starting from an initial \code{\link{BTFit}} object.
#' This does not support further cross-validation. Moreover, this approach is only allowed if \code{keep.data=TRUE} in the original call.
#'
#' @param BTFit_object a \code{\link{BTFit}} object.
#' @param new.n.iter number of new boosting iterations to perform.
#' @param is.verbose a logical specifying whether or not the additional fitting should run "noisely" with feedback on progress provided to the user.
#' @param seed optional seed used to perform the new iterations. By default, no seed is set.
#'
#' @return Returns a new \code{\link{BTFit}} object containing the initial call as well as the new iterations performed.
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{BTFit}}.
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
#' @rdname BT_more
#' @export
#'
BT_more <-
  function(BTFit_object,
           new.n.iter = 100,
           is.verbose = FALSE,
           seed = NULL) {
    # Set the seed (if any) and store the call.
    if (!is.null(seed))
      set.seed(seed)
    the_call <- match.call()

    # Check inputs
    .check_if_BT_fit(BTFit_object)
    .check_n_iter(new.n.iter)
    .check_is_verbose(is.verbose)

    if (is.null(BTFit_object$BTData)) {
      stop("keep.data was set to FALSE on original BT call - Impossible to continue the training.")
    }

    if (.has_cross_validation(BTFit_object)) {
      warning("BT.more is incompatible with cross-validation; losing cv results.")
    }

    # Call BT package
    BT_more_fit <- BT_callBoosting(
      BTFit_object$BTData$training.set,
      BTFit_object$BTData$validation.set,
      BTFit_object$distribution,
      BTFit_object$BTParams$ABT,
      BTFit_object$BTParams$tree.control,
      BTFit_object$BTParams$interaction.depth,
      BTFit_object$BTParams$bag.fraction,
      BTFit_object$BTParams$shrinkage,
      new.n.iter,
      BTFit_object$BTParams$colsample.bytree,
      BTFit_object$BTParams$train.fraction,
      BTFit_object$keep.data,
      is.verbose,
      BTFit_object$response,
      BTFit_object$w,
      BTFit_object$var.names
    )

    # Set class
    class(BT_more_fit) <- "BTFit"

    # Store correct parameters
    BT_more_fit$cv.folds <- BTFit_object$cv.folds
    BT_more_fit$call <- the_call
    BT_more_fit$Terms <- BTFit_object$Terms
    BT_more_fit$seed <- seed

    # Transfer old results across
    BT_more_fit$BTInit <- BTFit_object$BTInit
    BT_more_fit$BTErrors <-
      structure(list(
        training.error = c(
          BTFit_object$BTErrors$training.error,
          BT_more_fit$BTErrors$training.error
        ),
        validation.error = c(
          BTFit_object$BTErrors$validation.error,
          BT_more_fit$BTErrors$validation.error
        ),
        oob.improvement = c(
          BTFit_object$BTErrors$oob.improvement,
          BT_more_fit$BTErrors$oob.improvement
        )
      ),
      class = "BTErrors")


    BT_more_fit$BTIndivFits <-
      structure(c(BTFit_object$BTIndivFits, BT_more_fit$BTIndivFits),
                class = "BTIndivFits")

    BT_more_fit$BTParams$n.iter <-
      length(BT_more_fit$BTIndivFits) # is equal to BTFit_object$n.iter + new.n.iter.

    return(BT_more_fit)
  }
