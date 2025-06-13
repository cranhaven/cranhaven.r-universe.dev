#' Plot lines of PN densities
#'
#' @description Add lines of PN estimated denisty or log-density in pn.dens or pn.hist plots.
#'
#' @usage pn.lines(x, model, log=FALSE, ...)
#'
#'@param x the response vector
#'@param model a variable returned by \code{\link{pn.mle}}
#'@param log Logical, plot log-density if TRUE (default = FALSE)
#'@param ... further arguments to \code{\link{lines}}
#'
#' @export


pn.lines <- function(x, model, log=FALSE, ...){
  alpha <- model$alpha
  y <- as.matrix(x)
  if (dim(y)[2] != 1) stop("The pn.lines function is only appropriate for the univariate analysis.\n")

  lim <- -9
  xx=seq(min(y),max(y),(max(y)-min(y))/1000)
  if(!log) lines(xx,pn.d(xx, alpha), ...)
  else{
    aux <- log(pn.d(xx, alpha))
    aux[which(aux < lim)] <- NA
    lines(xx,aux,...)
  }
}
