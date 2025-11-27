#' manifold: Operations for Riemannian manifolds
#'
#' Implements operations for Riemannian manifolds, e.g., geodesic distance, Riemannian metric, exponential and logarithm maps, etc. Also incorporates random object generator on the manifolds
#' 
#' 
#' References: 
#' Dai X, Lin Z, Müller HG. Modeling sparse longitudinal data on Riemannian manifolds. Biometrics. 2021;77(4):1328–41. 
#' Dai X, Lopez-Pintado S. Tukey’s depth for object data. Journal of the American Statistical Association. 2021;In press. 
#' 
#' 
#' Maintainer:  Xiongtao Dai \email{xdai@@berkeley.edu}
#' 
#' @author
#' Xiongtao Dai \email{xdai@@berkeley.edu}
#' Zhenhua Lin
#'
#'
#' 
#'
#' @docType package
#' @name manifold
#' @useDynLib manifold
#' @import Rcpp
#' @importFrom Matrix bdiag
#' @importFrom stats setNames pchisq prcomp cov rnorm rexp rt
#' @importFrom utils methods
NULL
