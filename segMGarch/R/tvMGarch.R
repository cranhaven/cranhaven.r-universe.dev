#' An S4 class for a nonstationary multivariate class model.
#' @name tvMGarch-class
#' @rdname tvMGarch-class
#' @description A specification class to create an object of a nonstationary multivariate class model reserved for real (empirical) applications. It inherits from \code{simMGarch}.
#' @slot out_of_sample_prop Proportion of y to keep for out-of-sample forecasting expressed in \%.
#' @slot out_of_sample_y The out of sample y matrix reserved for forecasting and backtesting exercises.
#' @slot in_sample_y The in-sample y matrix reserved for estimation (calibration) and change-point detection.
#' @references
#' Cho, Haeran, and Karolos Korkas. "High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155 (2018).
#' @examples
#' simObj <- new("simMGarch")
#' simObj@d <- 10
#' simObj@n <- 1000
#' simObj@changepoints <- c(250,750)
#' simObj <- pc_cccsim(simObj)
#' empirObj <- new("tvMGarch") #simulated, but treated as a real dataset for illustration
#' empirObj@y <- simObj@y
#' empirObj@out_of_sample_prop <- 0.1
#' #empirObj=garch.seg(object=empirObj,do.parallel = 4)##Not run
#' @import Rcpp foreach doParallel parallel iterators methods
#' @importFrom  corpcor cor.shrink
#' @useDynLib segMGarch, .registration = TRUE
#' @export
setClass("tvMGarch", 
         slots = c(
           out_of_sample_prop = "numeric",
           out_of_sample_y = "matrix",
           in_sample_y = "matrix"
         ), 
         prototype = list(
           out_of_sample_prop = .1
         ),
         contains="simMGarch"
)