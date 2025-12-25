#' @title  Discrete wavelet transform
#'
#' @description This function performs a level J wavelet transform of the input
#' array (1d, 2d, or 3d) using the pyramid algorithm (Mallat 1989).
#' @param x a 1, 2, or 3 dimensional data array. The size of each dimension must
#' be dyadic.
#' @param wf the type of wavelet family used. See R-package waveslim for options.
#' @param J is the level (depth) of the decomposition. For default \code{NULL} the max
#' depth is used making  \code{wt(x)} equal to multiplying \code{x} with the
#' corresponding wavelet matrix.
#' @details This is a C++/R wrapper function for a C implementation of the
#' discrete wavelet transform by Brandon Whitcher, Rigorous Analytics Ltd, licensed
#' under the BSD 3 license https://cran.r-project.org/web/licenses/BSD_3_clause,
#' see the Waveslim package;
#' Percival and Walden (2000); Gencay, Selcuk and Whitcher (2001).
#'
#' Given a data array (1d, 2d or 3d) with dyadic
#' sizes this transform is computed efficiently via the pyramid
#' algorithm see Mallat (1989).
#'
#' This functionality is used in the computations underlying \code{\link{softmaximin}}
#' to perform multiplications involving the wavelet (design) matrix efficiently.
#'
#' @author Adam Lund, Brandon Whitcher
#' @examples
#' ###1d
#' x <- as.matrix(rnorm(2^3))
#' range(x - iwt(wt(x)))
#'
#' ###2d
#' x <- matrix(rnorm(2^(3 + 4)), 2^3, 2^4)
#' range(x - iwt(wt(x)))
#'
#' ###3d
#' x <- array(rnorm(2^(3 + 4 + 5)), c(2^3, 2^4, 2^5))
#' range(x - iwt(wt(x)))
#'
#' @return
#' \item{...}{An array with dimensions equal to those of \code{x}.}
#'
#' @references
#' Gencay, R., F. Selcuk and B. Whitcher (2001) An Introduction to Wavelets and
#' Other Filtering Methods in Finance and Economics, Academic Press.
#'
#' Mallat, S. G. (1989) A theory for multiresolution signal decomposition: the
#' wavelet representation, IEEE Transactions on Pattern Analysis and Machine
#' Intelligence, 11, No. 7, 674-693.
#'
#' Percival, D. B. and A. T. Walden (2000) Wavelet Methods for Time Series
#' Analysis, Cambridge University Press.
#'
#' @export
wt <- function(x, wf = "la8", J = NULL){
dim = sum(dim(x) > 1)
p1 <- dim(x)[1]
if(is.null(J)){J = log(min(dim(x)[dim(x) > 1]), 2)}
p3 <- p2 <- 1
if(dim > 1){p2 <- dim(x)[2]}
if(dim > 2){p3 <- dim(x)[3]}

out <- WT(matrix(x, p1, p2 * p3), dim,  wf, J, p1, p2, p3) #p1 x p2 * p3
out <- drop(array(out, c(p1, p2, p3)))
if(is.null(dim(out))){out <- as.matrix(out)}
out
}

