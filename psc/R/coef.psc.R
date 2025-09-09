#' Returns the coefficeint estimate of a psc object.
#'
#' Returns basic measures of the posterior distribution obtained from the
#' pscfit.R function
#'
#' @param object a 'psc' object
#' @param level the level at which credibility intervals are assessed, defaults to 0.05
#' @param ... not used
#' @return The summary of the posterior distribution for the efficacy parameter in terms of the median and 95% HPD
#' @export
coef.psc <- function(object, ..., level=0.05){

  cl.nm <- attributes(object$DC_clean$model_extract$sig)$dimnames[[1]]
  y <- object$posterior[,which(!names(object$posterior)%in%cl.nm)]


  lap <- lapply(y,quantile,na.rm=T,p=c(0.5,level/2,1-level/2))
  lap <- t(matrix(unlist(lap),3,ncol(y)))
  p_l <- colSums(y<0)/nrow(y)
  p_u <- colSums(y>0)/nrow(y)


  nm.l <- paste(100*(level/2),"%",sep="")
  nm.u <- paste(100*(1-level/2),"%",sep="")

  res <- data.frame(cbind(lap,p_l,p_u))
  names(res) <- c("median",nm.l,nm.u,"Pr(x<0)","Pr(x>0)")
  as.matrix(res)

}
