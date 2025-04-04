#' Prediction according to `soft tree'.
#' @param fit The soft tree.
#' @param newdata Data to classify.
#' @return The matrix of predicted class probabilities.
#' @export
predictSoftsplits <- function(fit, newdata)
{
  conv <- conv.softsplits(fit,names(newdata))
  ndata <- nrow(newdata)
  nvars <- ncol(newdata)
  newdata <- sapply(newdata, as.double)
  if ( any(is.na(newdata)) ) {
    stop("data frame contains NA:s or it is not convertible to double")
  }
  temp <- .C("pred_ss",
    as.double(as.matrix(newdata)),
    ndata,
    nvars,
    nrow(fit),
    conv$varindexes,
    conv$splits,
    conv$ncat,
    conv$lb,
    conv$ub,
    conv$childref,
    conv$yval,
    conv$nclass,
    prob=as.double(matrix(-1,nrow=ndata,ncol=conv$nclass)),
    PACKAGE="SplitSoftening",
    NAOK=TRUE)
  data.names <- dimnames(newdata)
  if (is.null(data.names) || is.null(data.names[[1]])) {
    row.names <- 1:ndata
  } else {
    row.names <- data.names[[1]]
  }
  temp <- matrix(temp$prob,nrow=ndata,dimnames=list(row.names,attr(fit,"ylevels")))
  return(temp)
}

