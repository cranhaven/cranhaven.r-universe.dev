#' Build Prediction Matrix (Default Method)
#'
#' These are not intended for use by users. Constructs a prediction matrix for cross-validation folds, using the coefficient paths in \code{outlist}. Inspired by internal functions in the \pkg{glmnet} package.
#' @param outlist A list of fitted models.
#' @param lambda A vector of penalty values.
#' @param x A predictor matrix.
#' @param foldid Fold identifiers.
#' @param alignment Alignment method (not used).
#' @param \dots Other arguments
#' @return A numeric matrix of predicted values with dimensions
#'   \code{nrow(x)} by \code{length(lambda)}. Rows correspond to
#'   observations and columns correspond to penalty values.
#' @export
buildPredmat.default <- function(outlist, lambda, x, foldid, alignment, ...){


  predmat <- matrix(NA, nrow(x), length(lambda))
  nfolds <- max(foldid)
  nlams <- double(nfolds)
  nlambda <- length(lambda)
  for (i in seq(nfolds)) {
    which = foldid == i
    fitobj <- outlist[[i]]
    # preds <- switch(alignment,
    #                 lambda=predict(fitobj, x[which, , drop = FALSE], s=lambda,,...),
    #                 fraction=predict(fitobj, x[which, , drop = FALSE],...) )
    preds <- x[which, , drop = FALSE] %*% fitobj$beta
    nlami <- min(ncol(preds),nlambda)
    predmat[which, seq(nlami)] <- preds[,seq(nlami)]
    if(nlami<nlambda){
      predmat[which,seq(from=nlami,to=nlambda)] <- preds[,nlami]
    }
  }
  rn <- rownames(x)
  sn <- paste("s",seq(0,length=nlambda),sep="")
  dimnames(predmat) <- list(rn,sn)

  predmat
}

#' Build Prediction Matrix
#'
#' These are not intended for use by users. Constructs a prediction matrix for cross-validation folds, using the coefficient paths in \code{outlist}.
#' Inspired by internal functions in the \code{glmnet} package.
#' @param outlist A list of fitted models.
#' @param lambda A vector of penalty values.
#' @param x A predictor matrix.
#' @param foldid Fold identifiers.
#' @param alignment Alignment method (not used).
#' @param \dots Other arguments
#' @return The return value depends on the method; for the default method,
#'   see \code{\link{buildPredmat.default}}.
#' @export
buildPredmat <- function(outlist, lambda, x, foldid, alignment, ...){
  UseMethod("buildPredmat")
}

