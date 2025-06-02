#' Synthetic four-class dataset
#'
#' A synthetic dataset with two attributes and four classes of 100 points each,  generated from a
#' multivariate t distribution with five degrees of freedom and centered, respectively,
#' on [0;0], [0;4], [4;0] and [4;4].
#'
#' @docType data
#'
#' @usage data(fourclass)
#'
#' @format A data frame with three variables: x1, x2 and y (the true class).
#'
#' @keywords datasets
#'
#' @references
#' M.-H. Masson and T. Denoeux. ECM: An evidential version of the fuzzy c-means algorithm.
#' Pattern Recognition, Vol. 41, Issue 4, pages 1384-1397, 2008.
#'
#'
#' @examples
#' data(fourclass)
#' plot(fourclass$x1,fourclass$x2,xlab=expression(x[1]),ylab=expression(x[2]),
#' col=fourclass$y,pch=fourclass$y)
"fourclass"
