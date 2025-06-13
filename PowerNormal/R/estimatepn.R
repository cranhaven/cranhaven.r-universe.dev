#' Fit univariate PN distribution
#'
#' @description Return EM algorithm output for FM-PN distributions
#'
#' @usage pn.mle(x)
#'
#'@param x the response vector
#'
#'@export



pn.mle <-  function(x){
n <- length(x)
alpha_est <- -n/(sum(log(pnorm(x))))
      list(alpha=alpha_est,loglik = lv_pn(alpha_est,x))
}


