#' Plots a Julia Set for a tree
#'
#' Finds the Julia Set for the y evaluated polynomial of a tree and plots in a square image.
#'
#' @param tree phylo object
#' @param pixelLength number of pixels on one side of the image
#' @param center complex number giving the center of the image on the complex plane
#' @param maxZ the max value for the real and imaginary axis
#' @param maxIter maximum count for iterations
#' @param col colours to be used for the image
#' @param y the y value to evaluate the polynomial at
#' @useDynLib treenomial
#' @importFrom Rcpp sourceCpp
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics par
#' @examples
#'
#' library(treenomial)
#' library(ape)
#' treeJuliaSet(stree(5,type = "right"), y = 1+1i)
#'
#' @export
treeJuliaSet <- function(tree, pixelLength = 700, center = 0, maxZ = 2, maxIter = 100, col = c("white", colorRampPalette(c("dodgerblue4", "lightblue"))(98) , "black"), y){
  oldpar <- par(no.readonly =TRUE)
  on.exit(par(oldpar))

  par(mar = c(1,1,1,1))

  coeff <- treeToPoly(tree, type = "yEvaluated", y = y, numThreads = 0)

  res <- juliaSet(coeffs = coeff, pixelLength = pixelLength, center = center, maxZ = maxZ, maxIter = maxIter)

  graphics::image(z = log(res), axes = FALSE, asp = 1, col = col, useRaster = TRUE)
}
