#' Estimated densities (PN)
#'
#' @description Plot the estimated density or log-density (PN)
#'
#' @usage pn.dens(x, model, log=FALSE, ylab=NULL, xlab = NULL, main = NULL, ...)
#'
#'@param x the response vector
#'@param model a variable returned by \code{\link{pn.mle}}
#'@param log Logical, plot log-density if TRUE (default = FALSE)
#'@param ylab Title of the ylab, if NULL default is selected
#'@param xlab Title of the xlab, if NULL default is selected
#'@param main Main Title, if NULL default is selected
#'@param ... further arguments to \cite{\link{plot}}
#'
#' @export



pn.dens <- function(x, model, log=FALSE, ylab=NULL, xlab = NULL, main = NULL, ...){
  alpha <- model$alpha
  dados <- as.matrix(x)
  if (dim(dados)[2] != 1) stop("The pn.dens function is only appropriate for the univariate analysis.\n")

  if(length(ylab) == 0) ylab = "Density"
  if(length(xlab) == 0) xlab = "x"

  if(length(main) == 0){
    main <- "Density plot"
    if(log) main <- "log-density plot"
  }



  lim <- -9

  xx=seq(min(dados),max(dados),(max(dados)-min(dados))/1000)
  if(!log) plot(xx,pn.d(xx, alpha ),type="l",ylab=ylab, xlab = xlab, main = main, ...)
  else{
    aux <- log(pn.d(xx, alpha))
    aux[which(aux < lim)] <- NA
    plot(xx,aux,type="l",ylab=ylab, xlab = xlab, main = main, ...)
  }
}


