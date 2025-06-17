#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
calcul.biBis <-
function(wm, meanbeta, a = NULL, model, tau){
    xistar <- calcul.xi(wm, meanbeta, a, model)
    if(model == "logistic"){
        f <- function(b, xj, xjbis){1/(1 + exp(-a - exp(b)*xj)) + 1/(1 + exp(-a - exp(b)*xjbis)) - 2*tau}
        lesb <- c()
        for(i in 1:(length(xistar)-1)){
            b <- uniroot(f, c(-1000000, 100000), xj = xistar[i], xjbis = xistar[i + 1] )$root
            lesb <- c(lesb, b)
        }
    }else{
        if(model == "power_log"){
            f <- function(b, xj, xjbis){xj^exp(b) + xjbis^exp(b) - 2*tau}
            lesb <- c()
            for(i in 1:(length(xistar) - 1)){
                b <- uniroot(f, c(-1000, 1000), xj = xistar[i], xjbis = xistar[i + 1] )$root
                lesb <- c(lesb, b)
            }
        }else{
            if(model == "power"){
                f <- function(b, xj, xjbis){xj^b + xjbis^b - 2*tau}
                lesb <- c()
                for(i in 1:(length(xistar)-1)){
                    b <- uniroot(f, c(-1000,1000), xj = xistar[i], xjbis = xistar[i + 1] )$root
                    lesb <- c(lesb, b)
                }
            }else{stop("Error: Please insert a valid model")}
        }
    }
    lesb <- c(-100000, lesb, 100000)
    return(lesb)
}
