#' The subsampling rule
#'
#' @param n sample size.
#'
#' @return
#' the subsampling size
#'
sampling_rule <- function(n){
        res =   0.75*(0.5*n - 0.3*max(n-5,0) - 0.15*max(n-1000,0) - 0.05*(1-log(3000)/log(n))*max(n-3000,0))
  return(res)}
