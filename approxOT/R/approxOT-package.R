#' An R package to perform exact and approximate optimal transport.
#'
#' R and C++ functions to perform exact and approximate optimal transport. All C++ methods are linkable to other R packages via their header files.
#' @author Eric Dunipace
#' @docType package
#' @name approxOT
#' @useDynLib approxOT, .registration = TRUE
#' @importFrom Rcpp sourceCpp 
#' @importFrom Rcpp evalCpp
#' @rdname approxOT-package
"_PACKAGE"