#' Unbiased estimator for alpha (PN)
#'
#' @description Unbiased estimator for \code{alpha}
#'
#' @usage pn.bias(x)
#'
#'@param x the response vector
#'
#' @export



pn.bias <- function(x){
  dados <- as.matrix(x)
  n <- length(dados)
  alpha_cor <- -(n-1)/(sum(log(pnorm(x))))


list(alpha_cor = alpha_cor, loglik = lv_pn(alpha_cor,x))
}


