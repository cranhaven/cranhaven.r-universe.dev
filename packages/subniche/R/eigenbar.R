#' @title The eigenvalue of the OMI analysis
#' @aliases eigenbar
#' @description The function plot the eigenvalues of the OMI analysis
#' @param subnic an object of class \code{subniche}.
#' @param col.sel the color of the selected axes
#' @param col.unsel the color of the other axes
#' @param main a main title for the plot, see \link[graphics]{title} for more details.
#' @param ylab label for y-axis, see \link[graphics]{title} for more details.
#' @param names.arg	 a vector of names to be plotted below each bar or group of bars. If this argument is omitted, then the names are taken from the names attribute of height if this is a vector, or the column names if it is a matrix.
#' @param ...	further arguments passed to or from other methods see \link[graphics]{barplot}
#' @rdname eigenbar
#' @export eigenbar
#' @details The black bars represents the selected axes for the OMI analysis
#' See \doi{10.7717/peerj.3364} for more details on the subniche concept.
#' @examples
#' library(subniche)
#' data(doubs)
#' dudi1 <- dudi.pca(doubs$env, scale = TRUE, scan = FALSE, nf = 3)
#' nic1 <- niche(dudi1, doubs$fish, scann = FALSE)
#' # number of sites
#' N <- dim(nic1$ls)[1]
#' #Create a factor which defines the subsets
#' fact <- factor(c(rep(1,N/2),rep(2,N/2)))
#' # nic1 will be use as reference and fact will be use to define the subniches environment
#' subnic1 <- subniche(nic1, fact)
#' eigenbar(subnic1)
#' @importFrom graphics par

eigenbar <- function(subnic, col.sel="black", col.unsel="grey",
                     ylab = "Eigen values in %", names.arg = NULL,
                     main=NA, ...){
  if(is.null(names.arg)){
    names.arg <- paste("PC", 1:subnic$rank, sep="")
  }
  eig <- round(subnic$eig/sum(subnic$eig) * 100, 2)
  colfac <- factor(c(rep(1,subnic$nf),rep(2,subnic$rank-subnic$nf)))
  barplot(eig,col=c(col.sel, col.unsel)[colfac],names.arg = names.arg, ylab = ylab, ...)
}
