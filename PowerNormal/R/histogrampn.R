#' Histrogram and estimated densities plots (PN)
#'
#' @description Plot the histogram along with the estimated density (PN)
#'
#' @usage pn.hist(x, model, breaks, main,..., col.lines, lwd, lty  )
#'
#'@param x the response vector
#'@param model a variable returned by \code{\link{pn.mle}}
#'@param breaks the same option in \cite{\link{histogram}}
#'@param main the main title (have useful default values)
#'@param ... further arguments to \cite{\link{histogram}}
#'@param col.lines line color
#'@param lwd line width
#'@param lty  line type
#'
#' @export

pn.hist <- function(x, model, breaks=40, main=paste("Histogram of PN model fit"), ...,
                    col.lines="red",lwd=1,lty=1){
  alpha <- model$alpha
  dados <- as.matrix(x)
  if (dim(dados)[2] != 1) stop("The pn.hist function is only appropriate for the univariate analysis.\n")
  #### grafico
  x <- dados
  hist(x, breaks = breaks,probability=T,main=main,...)
  xx=seq(min(dados),max(dados),(max(dados)-min(dados))/1000)
  lines(xx,pn.d(xx, alpha ),col= col.lines, lwd=lwd,lty=lty)
}
