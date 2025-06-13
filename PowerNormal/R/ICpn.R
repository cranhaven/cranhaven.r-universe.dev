#' Confidence interval for alpha (PN)
#'
#' @description Confidence interval for alpha
#'
#' @usage pn.IC(x, model, p)
#'
#'@param x the response vector
#'@param p confidence level
#'
#' @export



pn.IC <- function(x, p){
  dados <- as.matrix(x)
  n <- length(dados)

list(L_Inf = -0.5*qchisq((1-p)/2,2*n)/sum(log(pnorm(dados))),
     L_Sup = -0.5*qchisq(1-(1-p)/2,2*n)/sum(log(pnorm(dados))))
}


