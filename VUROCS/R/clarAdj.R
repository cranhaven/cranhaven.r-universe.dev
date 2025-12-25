#' @title Adjusted Cumulative LGD Accuracy Ratio
#' @description Calculates for a vector of realized categories \code{y} and a vector of predicted categories \code{hx} the cumulative LGD accuarcy ratio (CLAR) according to Ozdemir and Miu (2009) and adjusts it such that the measure has a value of zero if the two ordinal rankings are in reverse order.
#' @param hx a vector of predicted categories.
#' @param y a vector of realized categories.
#' @return The function returns the adjusted CLAR for a vector of realized categories \code{y} and a vector of predicted categories \code{hx}.
#' @examples clarAdj(rep(1:5,each=3),c(3,3,3,rep(2:5,each=3)))
#' @references Ozdemir, B., Miu, P., 2009. Basel II Implementation. A Guide to Developing and Validating a Compliant Internal Risk Rating System. McGraw-Hill, USA.


clarAdj<-function(y,hx){

  if (any(is.na(hx)) | any(is.na(y))) {
    stop("\n both 'hx' and 'y' must not contain NA values")
  }
  
  if (length(hx)!=length(y)) {
    stop("\n both 'hx' and 'y' must be of the same length")
  }
  
  index=order(hx,decreasing=TRUE)
  hx=hx[index]
  y=y[index]
  adj=clar(y[order(y)],hx)
  (clar(y,hx)-adj)/(1-adj)
}