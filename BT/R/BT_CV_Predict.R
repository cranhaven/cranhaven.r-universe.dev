#' Predictions for CV fitted BT models.
#'
#' Compute predictions from cross-validated Boosting Trees model.
#'
#' @param object a \code{\link{BTCVFit}} object containing CV BT models.
#' @param data the database on which one wants to predict the different CV BT models.
#' @param cv.folds a positive integer specifying the number of folds to be used in cross-validation of the BT fit.
#' @param folds vector of integers specifying which row of data belongs to which cv.folds.
#' @param best.iter.cv the optimal number of trees with a CV approach.
#' @param \dots not currently used.
#'
#' @return Returns a vector of predictions for each cv folds.
#'
#' @details
#' This function has not been coded for public usage but rather to assess the cross-validation performances.
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
#' @rdname predict.BTCVFit
#' @keywords internal
predict.BTCVFit <-
  function(object,
           data,
           cv.folds,
           folds,
           best.iter.cv,
           ...) {
    # We directly apply the predict on the dataset -> no database extraction.

    # Check match between data and folds.
    if (nrow(data) != length(folds)) {
      stop('Error in predict.BTCVFit - folds and data should have the same number of records.')
    }

    # Flatten data for prediction.
    result <- rep(NA, length = nrow(data))

    for (index in seq_len(cv.folds)) {
      currModel <- object[[index]]
      flag <- which(folds == index)
      result[flag] <- predict(currModel,
                              newdata = data[flag, currModel$var.names, drop =
                                               FALSE],
                              n.iter = best.iter.cv)
    }
    return(result)
  }
