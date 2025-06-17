#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
varp <-
function(sigma, lesb){
    lesj <- seq(1,length(lesb) - 1)
    phi_bj <- sapply(lesb/sigma, pnorm)
    pj <- phi_bj[-1] - phi_bj[-length(phi_bj)]
    esp <- sum(lesj*pj)
    varint <- sum(pj*(lesj - esp)^2)
    return(varint)
}
