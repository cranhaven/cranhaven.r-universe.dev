#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
sigmaLI <-
function(wm, meanbeta, a = NULL, model, tau){
    lesb <- calcul.biBis(wm, meanbeta, a = NULL, model, tau)
    
    var_unif <- ((length(lesb) - 1)^2-1)/12
    f <- function(sigma, lesb){(varp(sigma, lesb) - var_unif)^2}
    optim <- optimize(f, c(0.01,1000), lesb = lesb)
    if(optim$objective > 0.001){
        stop('Warning: There is no minimum')
    }else{
        sigma <- optim$minimum
    }
    return(sigma)
}
