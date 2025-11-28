#' Predict method for BT Model fits.
#'
#' Predicted values based on a boosting tree model object.
#'
#' @param object a \code{\link{BTFit}} object.
#' @param newdata data frame of observations for which to make predictions. If missing or not a data frame, if \code{keep.data=TRUE} in the initial fit then the original training set will be used.
#' @param n.iter number of boosting iterations used for the prediction. This parameter can be a vector in which case predictions are returned for each iteration specified.
#' @param type the scale on which the BT makes the predictions. Can either be "link" or "response". Note that, by construction, a log-link function is used during the fit.
#' @param single.iter if \code{single.iter=TRUE} then \code{predict.BTFit} returns the predictions from the single tree \code{n.iter}.
#' @param \dots not currently used.
#'
#' @return Returns a vector of predictions. By default, the predictions are on the score scale.
#' If \code{type = "response"}, then \code{BT} converts back to the same scale as the outcome. Note that, a log-link is supposed by construction.
#'
#' @details
#' \code{predict.BTFit} produces a predicted values for each observation in \code{newdata} using the first \code{n.iter} boosting iterations.
#' If \code{n.iter} is a vector then the result is a matrix with each column corresponding to the \code{BT} predictions with \code{n.iter[1]} boosting iterations, \code{n.iter[2]} boosting
#' iterations, and so on.
#'
#' As for the fit, the predictions do not include any offset term.
#' In the Poisson case, please remind that a weighted approach is initially favored.
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
#' @rdname predict.BTFit
#' @export
#'
predict.BTFit <-
  function(object,
           newdata,
           n.iter,
           type = "link",
           single.iter = FALSE,
           ...) {
    # Check inputs
    if (!is.element(type, c("link", "response"))) {
      stop("type must be either 'link' or 'response'")
    }

    if (missing(newdata) || !is.data.frame(newdata)) {
      if (object$keep.data) {
        message(
          "As newdata is missing or is not a data frame, the training set has been used thanks to the keep.data = TRUE parameter."
        )
        newdata <- object$BTData$training.set
      } else{
        stop("newdata must be provided as a data frame.")
      }
    }

    if (!all(object$var.names %in% colnames(newdata))) {
      stop("newdata must contain the same explanatory variable as the original fitted BT object.")
    }

    if (missing(n.iter)) {
      stop("Number of iterations to be used in prediction must be provided.")
    }

    if (length(n.iter) == 0) {
      stop("n.iter cannot be NULL or a vector of zero length.")
    }

    if (any(n.iter != as.integer(n.iter)) ||
        is.na(all(n.iter == as.integer(n.iter)))
        ||
        any(n.iter <= 0)) {
      # at least one iteration - not only the init considered to avoid problem.
      stop("n.iter must be a vector of non-negative integers.")
    }

    if (!(single.iter %in% c(TRUE, FALSE))) {
      stop("single.iter should be either TRUE or FALSE.")
    }

    if (any(n.iter > object$BTParams$n.iter)) {
      n.iter[n.iter > object$BTParams$n.iter] <- object$BTParams$n.iter
      warning("Number of trees exceeded number fit so far. Using ",
              paste(n.iter, collapse = " "),
              ".")
    }

    outMatrix <- matrix(NA, nrow = nrow(newdata), ncol = length(n.iter))

    if (single.iter) {
      for (i in seq(1, length(n.iter))) {
        iIter <- n.iter[i]
        # Link-scale output.
        outMatrix[, i] <-
          log(predict(
            object$BTIndivFits[[iIter]],
            newdata = newdata,
            type = "vector"
          ))
      }
    } else{
      # Compute cumulative results for each iteration in the vector n.iter
      lastIter <- max(n.iter)
      shrinkage <- object$BTParams$shrinkage

      currPred <-
        rep(log(object$BTInit$initFit), nrow(newdata)) # GLM used as first prediction.

      for (iIter in seq(1, lastIter)) {
        currPred <-
          currPred + shrinkage * log(predict(
            object$BTIndivFits[[iIter]],
            newdata = newdata,
            type = "vector"
          ))
        if (iIter %in% n.iter) {
          outMatrix[, which(n.iter == iIter)] <- currPred
        }
      }
    }

    if (type == "response")
      outMatrix <- exp(outMatrix) # Exponential link-function.
    if (length(n.iter) == 1)
      outMatrix <- as.vector(outMatrix)
    return(outMatrix)
  }
