#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
waic <-
function (stanfit, s){
    log_lik <- extract(stanfit, "logLike")$logLike
    lppd <- sum(log(colMeans(exp(log_lik))))
    p_waic_1 <- 2*sum(log(colMeans(exp(log_lik))) - colMeans(log_lik))
    p_waic_2 <- sum(colVars(log_lik))
    waic_2 <- -2*lppd + 2*p_waic_2
    # return(list(waic=waic_2, p_waic=p_waic_2, lppd=lppd, p_waic_1=p_waic_1))
    return(waic_2)
}
