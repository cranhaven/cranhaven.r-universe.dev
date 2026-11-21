#' KernelICA-package
#'
#' The kernel independent component analysis (kernel ICA) method introduced by Bach and Jordan in 2002 (see references).
#' A separate function for the incomplete Cholesky decomposition used in kernel ICA is also provided.
#'
#' @import Rcpp
#' @useDynLib KernelICA
#'
#' @references
#' Francis R. Bach, Michael I. Jordan\cr
#' \emph{Kernel independent component analysis}\cr
#' Journal of Machine Learning Research 2002\cr
#' \doi{10.1162/153244303768966085}
#' 
#' Francis R. Bach, Michael I. Jordan\cr
#' \emph{Predictive low-rank decomposition for kernel methods.}\cr
#' Proceedings of the Twenty-second International Conference on Machine Learning (ICML) 2005\cr
#' \doi{10.1145/1102351.1102356}.
#' 
#' Sean Martin, Andrew M. Raim, Wen Huang, Kofi P. Adragni\cr
#' \emph{ManifoldOptim: An R Interface to the ROPTLIB Library for Riemannian Manifold Optimization}\cr
#' Journal of Statistical Software 2020\cr
#' \doi{10.18637/jss.v093.i01}
"_PACKAGE"
