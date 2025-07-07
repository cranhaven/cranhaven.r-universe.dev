#' K-fold cross-validation wrapper
#'
#' Does k-fold cross-validation for a given model training function and
#' prediction function. The hyperparameter to be cross-validated is assumed
#' to be `lambda`. The training and prediction functions are assumed to be
#' able to fit/predict for multiple `lambda` values at the same time.
#'
#' The model training function is assumed to take in the data matrix as `x`,
#' the response as `y`, and the hyperparameter to be cross-validated as
#' `lambda`. It is assumed that in its returned output, the hyperparameter
#' values actually used are stored as `lambda`. The prediction function
#' is assumed to take in the new data matrix as `newx`, and a `lambda`
#' sequence as `s`.
#'
#' @param x Input matrix of dimension `nobs` by `nvars`; each row is an
#' observation vector.
#' @param y Response variable. Either a vector or a matrix, depending on the
#' type of model.
#' @param train_fun The model training function. This needs to take in an
#' input matrix as `x` and a response variable as `y`.
#' @param predict_fun The prediction function. This needs to take in the
#' output of `train_fun` as `object` and new input matrix as `newx`.
#' @param type.measure Loss function to use for cross-validation. See
#' `availableTypeMeasures()` for possible values for `type.measure`. Note that
#' the package does not check if the user-specified measure is appropriate
#' for the family.
#' @param family Model family; used to determine the correct loss function.
#' One of "gaussian", "binomial", "poisson", "cox", "multinomial",
#' "mgaussian", or a class "family" object.
#' @param lambda Option user-supplied sequence representing the values of the
#' hyperparameter to be cross-validated.
#' @param train_params Any parameters that should be passed to
#' `train_fun` to fit the model (other than `x` and `y`). Default is the
#' empty list.
#' @param predict_params Any other parameters that should be passed tp
#' `predict_fun` to get predictions (other than `object` and `newx`). Default
#' is the empty list.
#' @param train_row_params A vector which is a subset of `names(train_params)`,
#' indicating which parameters have to be subsetted in the CV loop (other
#' than `x` and `y`. Default is `c()`.
#' Other parameters which should probably be included here
#' are "weights" (for observation weights) and "offset".
#' @param predict_row_params A vector which is a subset of
#' `names(predict_params)`, indicating which parameters have to be subsetted
#' in the CV loop (other than `newx`). Default is `c()`.
#' Other parameters which should probably be included here
#' are "newoffset".
#' @param nfolds Number of folds (default is 10). Smallest allowable value
#' is 3.
#' @param foldid An optional vector of values between `1` and `nfolds`
#' (inclusive) identifying which fold each observation is in. If supplied,
#' `nfolds` can be missing.
#' @param parallel If `TRUE`, use parallel `foreach` to fit each
#' fold.  Must register parallel backend before hand. Default is `FALSE`.
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
#' @param keep If `keep = TRUE`, a prevalidated array is returned containing
#' fitted values for each observation and each value of lambda. This means
#' these fits are computed with this observation and the rest of its fold
#' omitted. The `foldid` vector is also returned. Default is `keep = FALSE`.
#' @param save_cvfits If `TRUE`, the model fits for each CV fold are returned
#' as a list. Default is `FALSE`.
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
#' \item{fit.preval}{If `keep=TRUE`, this is the array of prevalidated fits.
#' Some entries can be `NA`, if that and subsequent values of `lambda` are not
#' reached for that fold.}
#' \item{foldid}{If `keep=TRUE`, the fold assignments used.}
#' \item{overallfit}{Model fit for the entire dataset.}
#' \item{cvfitlist}{If `save_cvfits=TRUE`, a list containing the model
#' fits for each CV fold.}
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(500), nrow = 50)
#' y <- rnorm(50)
#' cv_fit <- kfoldcv(x, y, train_fun = glmnet::glmnet,
#'                   predict_fun = predict)
#'
#' @importFrom foreach foreach `%dopar%`
#' @export
kfoldcv <- function(x,
                    y,
                    train_fun,
                    predict_fun,
                    type.measure = "deviance",
                    family = "gaussian",
                    lambda = NULL,
                    train_params = list(),
                    predict_params = list(),
                    train_row_params = c(),
                    predict_row_params = c(),
                    nfolds = 10,
                    foldid = NULL,
                    parallel = FALSE,
                    grouped = TRUE,
                    keep = FALSE,
                    save_cvfits = FALSE) {
  # arguments x, y, newx and lambda have a special status at the moment
  # we may want to remove this special status in the future
  train_params$x <- x
  train_params$y <- y
  train_params$lambda <- lambda
  predict_params$newx <- x
  train_row_params <- c("x", "y", train_row_params)
  predict_row_params <- c("newx", predict_row_params)
  N <- nrow(x)

  ### parameter checking section
  checkValidTypeMeasure(type.measure, family)

  if ("family" %in% names(train_params) &&
      is.character(train_params$family) &&
      train_params$family != family)
    warning(paste("family parameter in train_params doesn't match",
                  "kfoldcv's family parameter"))

  if (!is.null(lambda) && length(lambda) < 2)
    stop("Need more than one value of lambda for kfoldcv")

  # train_row_params should be a subset of train_params' names
  if (length(setdiff(train_row_params, names(train_params))) > 0)
    stop("train_row_params should be a subset of names(train_params)")

  # predict_row_params should be a subset of predict_params' names
  if (length(setdiff(predict_row_params, names(predict_params))) > 0)
    stop("predict_row_params should be a subset of names(predict_params)")

  ### end parameter checking section

  # get observation weights from train_params (if available)
  if ("weights" %in% names(train_params)) {
    weights <- train_params$weights
    if (!("weights" %in% train_row_params))
      warning(paste0("observation weights provided in train_params, so",
                     "'weights' should probably be in train_row_params"))
  } else {
    weights <- rep(1, N)
  }

  # set nfolds and foldid
  if (is.null(foldid)) {
    foldid <- sample(rep(seq(nfolds), length = N))
    foldid_vals <- 1:nfolds
  } else {
    # if foldid is not 1 to "nfolds", we make it so
    foldid_vals <- sort(unique(foldid))
    nfolds <- length(foldid_vals)
  }
  if (nfolds < 3)
    stop("nfolds must be >= 3; nfolds = 10 recommended")

  # overall fit for the whole dataset
  train_params_copy <- train_params
  train_obj <- do.call(train_fun, train_params)

  # fit for each of the folds
  cvfitlist <- as.list(seq(nfolds))  # to store the fits
  if (!parallel) {
    for (i in seq_along(foldid_vals)) {
      out_idx <- (foldid == foldid_vals[i])

      # update the training parameters before fitting
      for (param in train_row_params) {
        if (is.matrix(train_params_copy[[param]])) {
          train_params[[param]] <- train_params_copy[[param]][!out_idx, , drop = FALSE]
        } else {
          train_params[[param]] <- train_params_copy[[param]][!out_idx]
        }
      }
      cvfitlist[[i]] <- do.call(train_fun, train_params)
    }
  } else {
    # compute CV model fits in parallel
    cvfitlist <- foreach(i = seq_along(foldid_vals)) %dopar% {
      out_idx <- (foldid == foldid_vals[i])

      # update the training parameters before fitting
      for (param in train_row_params) {
        if (is.matrix(train_params_copy[[param]])) {
          train_params[[param]] <- train_params_copy[[param]][!out_idx, , drop = FALSE]
        } else {
          train_params[[param]] <- train_params_copy[[param]][!out_idx]
        }
      }
      do.call(train_fun, train_params)
    }
  }

  # build prediction matrix
  lambda <- train_obj$lambda
  predict_params$s <- lambda
  predmat <- buildPredMat(cvfitlist, y, lambda, family, foldid, predict_fun,
                          predict_params, predict_row_params,
                          type.measure, weights, grouped)

  # compute error metric
  # Note: computeError can change type.measure and grouped
  out <- computeError(predmat, y, lambda, foldid, type.measure, family,
                      weights, grouped)

  # add items to returned output
  if (keep) out <- c(out, list(fit.preval = predmat, foldid = foldid))
  out$overallfit <- train_obj
  if (save_cvfits) out$cvfitlist <- cvfitlist

  if (!("cvobj" %in% class(out))) class(out) <- c("cvobj", class(out))
  return(out)
}
