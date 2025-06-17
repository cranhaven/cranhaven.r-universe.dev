#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
sigmaHI <-
function(wm, meanbeta, a = NULL, model, tau, threshold){
    lesb <- calcul.biBis(wm, meanbeta, a = NULL, model, tau)
    
    f <- function(sigma, lesbbis){
        phi_bj <- sapply(lesbbis/sigma, pnorm)
        res <- phi_bj[2] - phi_bj[1] + phi_bj[length(phi_bj)] - phi_bj[length(phi_bj) - 1] - threshold
        return(res^2)
    }
    optim <- optimize(f, c(0.01, 1000), lesbbis = lesb)
    if(optim$objective > 0.001){
        stop('Warning: There is no minimum')
    }else{
        sigma <- optim$minimum
    }
    return(sigma)
}
