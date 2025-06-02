
#' Transform data by the estimated response function
#'
#' @description Transforms data matrix by estimated response functions.
#' @usage transformRSdata(X,Beta=Beta,Mmat.q=Mmat.q)
#' @param X An n by m categorical data matrix.
#' @param Beta An n by q-1 matrix of coefficiets for response functions.
#' @param Mmat.q A q by 3+1 matrix of I-spline basis functions, evaluated at the midpoints between boundaries.
#' @return Returns a list with the following elements.
#' \item{\code{Y.hat}}{An n by m matrix of corrected data matrix.}
#' \item{\code{MB}}{An n by q matrix of values of response functions evaluated at the midpoint between boundaries.}
#' @export

transformRSdata <- function(X,Beta=Beta,Mmat.q=Mmat.q){

  MB <- t(tcrossprod(Mmat.q, (Beta)))

  Y.hat <- t(apply(cbind(1:nrow(Beta), MB), 1, function(x, y) x[-1][as.numeric(y[x[1], ])], y = X))

  rownames(Y.hat) <- paste("sub.",c(1:nrow(Y.hat)),sep="")
  colnames(Y.hat) <- paste("item",c(1:ncol(Y.hat)),sep="")

  return(list(Y.hat=Y.hat,MB=MB))

}
