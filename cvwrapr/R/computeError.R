#' Compute CV statistics from a prediction matrix
#'
#' Compute CV statistics from a matrix of predictions.
#'
#' Note that for the setting where `family = "cox"` and
#' `type.measure = "deviance"` and `grouped = TRUE`, `predmat` needs to have
#' a `cvraw` attribute as computed by `buildPredMat()`. This is because the
#' usual matrix of pre-validated fits does not contain all the information
#' needed to compute the model deviance for this setting.
#'
#' @param predmat Array of predictions. If `y` is univariate, this has
#' dimensions `c(nobs, nlambda)`. If `y` is multivariate with `nc`
#' levels/columns (e.g. for `family = "multionmial"` or
#' `family = "mgaussian"`), this has dimensions `c(nobs, nc, nlambda)`.
#' Note that these should be on the same scale as `y` (unlike in the
#' glmnet package where it is the linear predictor).
#' @param y Response variable. Either a vector or a matrix, depending on the
#' type of model.
#' @param lambda Lambda values associated with the errors in `predmat`.
#' @param foldid Vector of values identifying which fold each observation is
#' in.
#' @param type.measure Loss function to use for cross-validation. See
#' `availableTypeMeasures()` for possible values for `type.measure`. Note that
#' the package does not check if the user-specified measure is appropriate
#' for the family.
#' @param family Model family; used to determine the correct loss function.
#' @param weights Observation weights.
#' @param grouped This is an experimental argument, with default `TRUE`,
#' and can be ignored by most users. For all models except `family = "cox"`,
#' this refers to computing `nfolds` separate statistics, and then using
#' their mean and estimated standard error to describe the CV curve. If
#' `FALSE`, an error matrix is built up at the observation level
#' from the predictions from the `nfolds` fits, and then summarized (does
#' not apply to `type.measure="auc"`). For the "cox" family,
#' `grouped=TRUE` obtains the CV partial likelihood for the Kth fold by
#' \emph{subtraction}; by subtracting the log partial likelihood evaluated on
#' the full dataset from that evaluated on the on the (K-1)/K dataset. This
#' makes more efficient use of risk sets. With `grouped=FALSE` the log
#' partial likelihood is computed only on the Kth fold.
#'
#' @return An object of class "cvobj".
#' \item{lambda}{The values of lambda used in the fits.}
#' \item{cvm}{The mean cross-validated error: a vector of length
#' `length(lambda)`.}
#' \item{cvsd}{Estimate of standard error of `cvm`.}
#' \item{cvup}{Upper curve = `cvm + cvsd`.}
#' \item{cvlo}{Lower curve = `cvm - cvsd`.}
#' \item{lambda.min}{Value of `lambda` that gives minimum `cvm`.}
#' \item{lambda.1se}{Largest value of `lambda` such that the error is within
#' 1 standard error of the minimum.}
#' \item{index}{A one-column matrix with the indices of `lambda.min` and
#' `lambda.1se` in the sequence of coefficients, fits etc.}
#' \item{name}{A text string indicating the loss function used (for plotting
#' purposes).}
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(500), nrow = 50)
#' y <- rnorm(50)
#' cv_fit <- kfoldcv(x, y, train_fun = glmnet::glmnet,
#'                   predict_fun = predict, keep = TRUE)
#' mae_err <- computeError(cv_fit$fit.preval, y, cv_fit$lambda,
#'                         cv_fit$foldid, type.measure = "mae",
#'                         family = "gaussian")
#'
#' @export
computeError <- function(predmat, y, lambda, foldid, type.measure, family,
                         weights = rep(1, dim(predmat)[1]),
                         grouped = TRUE) {
  ### parameter checks
  checkValidTypeMeasure(type.measure, family)

  if (length(lambda) != dim(predmat)[length(dim(predmat))])
    stop("lambda should be a vector of length `dim(predmat)[length(dim(predmat))]`")

  ### end parameter checks

  nfolds <- length(sort(unique(foldid)))

  # Note: computeRawError can change type.measure and grouped
  cvstuff <- computeRawError(predmat, y, type.measure, family, weights, foldid,
                             grouped)

  grouped <- cvstuff$grouped
  if ((dim(predmat)[1] / nfolds < 3) && grouped) {
    warning(paste("Option grouped = FALSE enforced in computeError,",
                  "since < 3 observations per fold"),
            call. = FALSE)
    grouped <- FALSE
  }

  out <- computeStats(cvstuff, foldid, lambda, grouped)
  out$type.measure <- cvstuff$type.measure

  # compute the lambda.min and lambda.1se values
  lamin <- with(out, getOptLambda(lambda, cvm, cvsd, type.measure))
  out <- c(out, as.list(lamin))

  out$name <- getTypeMeasureName(out$type.measure, family)
  out$type.measure <- NULL

  class(out) <- c("cvobj", class(out))
  return(out)
}
