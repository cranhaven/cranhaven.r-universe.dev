
#' Convert data matrix to rank-ordered boundary data
#'
#' @description Converts data matrix to rank-ordered boundary data.
#' @usage convert.X2F(X,q=q)
#' @param X An n by m categorical data matrix.
#' @param q An integer indicating the maximum rating.
#' @return An n by q-1 scaled rank-ordered boundary data.
#' @importFrom cds createcdsdata
#' @export

convert.X2F <- function(X,q=q){

  n <- nrow(X) ; m <- ncol(X)
  p <- m+q-1
  cdsdat <- createcdsdata(x=X,q=q)

  Fr <- cdsdat$Fr.rs[c(1:n),]
  Fmat <- Fr[,c((m+1):p)]/max(Fr) #convereted rank-ordered boundary data is scaled so that the range is [0,1].

  Fmat

}
