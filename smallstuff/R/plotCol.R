#########1#########2#########3#########4#########5#########6#########7#########8
#' Plot Colors
#'
#' Plot one or more colors
#'
#' @param col vector with colors
#' @return A plot showing the colors in \code{col}
#' @examples
#' plotCol("maroon")
#' @export
################################################################################
plotCol <- function(col) {
  plot(1:length(col),rep(1,length(col)),pch=15,cex=8,col=col,
       xlim=c(0,(length(col)+1)),xlab=NA,ylab=NA,yaxt="n",xaxt="n")
}
