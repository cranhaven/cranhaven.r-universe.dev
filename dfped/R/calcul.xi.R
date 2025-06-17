#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
calcul.xi <-
function(wm, meanbeta, a = NULL, model){
    if(model == "logistic"){
        x <- -(a +log(1/wm -1))/exp(meanbeta)
    }else{
        if(model == "power"| model == "power_log"){
            x <- wm^exp(meanbeta)
        }else{
            stop("Error: Please insert a valid model")	
        }
    }
    return(x)
}
