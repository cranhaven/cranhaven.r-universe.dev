#' Build a prediction matrix from CV model fits
#'
#' Build a matrix of predictions from CV model fits.
#'
#' @param cvfitlist A list of length `nfolds`, with each element being
#' the model fit for each fold.
#' @param y Response. It is only used to determine what dimensions the
#' prediction array needs to have.
#' @param lambda Lambda values for which we want predictions.
#' @param family Model family; one of "gaussian", "binomial", "poisson",
#' "cox", "multinomial", "mgaussian", or a class "family" object.
#' @param foldid Vector of values identifying which fold each observation is
#' in.
#' @param predict_fun The prediction function; see `kfoldcv()` documentation
#' for details.
#' @param predict_params Any other parameters that should be passed tp
#' `predict_fun` to get predictions (other than `object` and `newx`); see
#' `kfoldcv()` documentation for details.
#' @param predict_row_params A vector which is a subset of
#' `names(predict_params)`, indicating which parameters have to be subsetted
#' in the CV loop (other than `newx`); see `kfoldcv()` documentation for
#' details.
#' @param type.measure Loss function to use for cross-validation.
#' Only required for `family = "cox"`.
#' @param weights Observation weights. Only required for `family = "cox"`.
#' @param grouped Experimental argument; see `kfoldcv()` documentation for
#' details. Only required for `family = "cox"`.
#'
#' @return A matrix of predictions.
buildPredMat <- function(cvfitlist, y, lambda, family, foldid, predict_fun,
                         predict_params, predict_row_params = c(),
                         type.measure = NULL, weights = NULL, grouped = NULL) {
  if (!("s" %in% predict_params)) predict_params$s <- lambda

  # family = "cox" needs its own way of building up the prediction matrix
  # because of the grouped = TRUE and type.measure = "deviance" case
  if (is.character(family) && family == "cox")
    return(buildPredMat.cox(cvfitlist, y, lambda, foldid, predict_fun,
                            predict_params, predict_row_params,
                            type.measure, weights, grouped))

  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)
  nlambda <- length(lambda)

  # determine dimensions for the prediction matrix
  nc <- NULL
  if (is.character(family) && family %in% c("multinomial", "mgaussian")) {
    nc <- dim(y)
    if (is.null(nc)) {
      y <- as.factor(y)
      ntab <- table(y)
      nc <- as.integer(length(ntab))
    }
    else nc <- nc[2]
  }
  predmat <- array(NA, c(length(foldid), nc, nlambda))

  predict_params_copy <- predict_params
  for (i in seq_along(foldid_vals)) {
    out_idx <- (foldid == foldid_vals[i])
    predict_params$object <- cvfitlist[[i]]

    # update the training parameters before fitting
    for (param in predict_row_params) {
      if (is.matrix(predict_params_copy[[param]])) {
        predict_params[[param]] <- predict_params_copy[[param]][out_idx, , drop = FALSE]
      } else {
        predict_params[[param]] <- predict_params_copy[[param]][out_idx]
      }
    }

    preds <- do.call(predict_fun, predict_params)

    # if fold lambda is longer than overall lambda, we need to cut off the extra
    # if fold lambda is shorter than overall lambda, we copy the last column
    # to the end
    if (is.character(family) && family %in% c("multinomial", "mgaussian")) {
      nlami <- min(dim(preds)[3], nlambda)
      predmat[out_idx, , seq(nlami)] <- preds[, , seq(nlami)]
      if (nlami < nlambda)
        predmat[out_idx, , seq(from = nlami, to = nlambda)] <- preds[, , nlami]
    } else {
      nlami <- min(ncol(preds), nlambda)
      predmat[out_idx, seq(nlami)] <- preds[, seq(nlami)]
      if (nlami < nlambda)
        predmat[out_idx, seq(from = nlami, to = nlambda)] <- preds[, nlami]
    }

  }

  # add dimension names
  rn <- rownames(predict_params$newx)
  sn <- paste0("s", seq(0, length = nlambda))
  if (is.character(family) && family %in% c("multinomial", "mgaussian")) {
    cn <- dimnames(preds)[[2]]
    dimnames(predmat) <- list(rn, cn, sn)
  } else {
    dimnames(predmat) <- list(rn, sn)
  }

  if (is.character(family) && family %in% c("binomial", "multinomial"))
    attr(predmat, "classnames") <- cvfitlist[[1]]$classnames
  if ("family" %in% class(family))
    attr(predmat, "family") <- family

  return(predmat)
}

buildPredMat.cox <- function(cvfitlist, y, lambda, foldid, predict_fun,
                             predict_params, predict_row_params,
                             type.measure, weights, grouped) {
  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)
  nlambda <- length(lambda)

  if ((length(weights) / nfolds < 10) && !grouped) {
    warning(paste("Too few (< 10) observations per fold for cox family;",
                  "grouped = TRUE enforced.",
                  "Alternatively, use smaller value for nfolds"),
            call. = FALSE)
    grouped <- TRUE
  }

  devtrue <- type.measure == "deviance"
  cvraw <- if(devtrue) matrix(NA, nfolds, nlambda) else NULL
  predmat <- matrix(NA, length(foldid), nlambda)
  predict_params_copy <- predict_params
  for (i in seq_along(foldid_vals)) {
    out_idx <- (foldid == foldid_vals[i])
    predict_params <- predict_params_copy
    predict_params$object <- cvfitlist[[i]]

    # make predictions for the whole dataset
    preds <- do.call(predict_fun, predict_params)

    nlami <- min(ncol(preds), nlambda)
    if (devtrue) {
      if (grouped) {
        plfull <- coxnet.deviance(pred = preds, y = y, weights = weights)
        plminusk <- coxnet.deviance(pred = preds[!out_idx, ],
                                    y = y[!out_idx, ],
                                    weights = weights[!out_idx])
        cvraw[i, seq(nlami)] <- (plfull - plminusk)[seq(nlami)]
      } else {
        plk <- coxnet.deviance(pred = preds[out_idx, ],
                               y = y[out_idx, ],
                               weights = weights[out_idx])
        cvraw[i, seq(nlami)] <- plk[seq(nlami)]
      }
    }

    # if fold lambda is longer than overall lambda, we need to cut off the extra
    # if fold lambda is shorter than overall lambda, we copy the last column
    # to the end
    predmat[out_idx, seq(nlami)] <- preds[out_idx, seq(nlami)]
    if (nlami < nlambda) {
      if (devtrue) cvraw[i,seq(from = nlami, to = nlambda)] <- cvraw[i, nlami]
      predmat[out_idx, seq(from = nlami, to = nlambda)] <- preds[out_idx, nlami]
    }
  }
  if (devtrue) attr(predmat, "cvraw") <- cvraw

  # add dimension names
  rn <- rownames(predict_params$newx)
  sn <- paste0("s", seq(0, length = nlambda))
  dimnames(predmat) <- list(rn, sn)

  return(predmat)
}
