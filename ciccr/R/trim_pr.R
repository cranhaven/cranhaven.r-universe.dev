#' @title Trimming the estimates to be strictly between 0 and 1
#'
#' @description Trimming the estimates to be strictly between 0 and 1
#'
#' @param ps n-dimensional vector of estimated probabilities
#' @param eps a small constant that determines the trimming of the estimated probabilities.
#' Specifically, the estimate probability is trimmed to be between eps and 1-eps (default = 1e-8).
#'
#' @return
#' \item{ps_tr}{n-dimensional trimmed estimates}
#'
#' @export
trim_pr = function(ps, eps=1e-8){

  ps_tr = eps*(ps < eps) + ps*(ps >= eps)*(ps <= 1-eps) + (1-eps)*(ps > 1-eps)

  ps_tr
}
