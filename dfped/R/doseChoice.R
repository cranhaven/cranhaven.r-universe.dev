#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
doseChoice <-
function(probaTox, probaEff, p, targetTox, givenDose){
    currentDose <- givenDose[length(givenDose)]
    maxDose <- max(givenDose)
    diffTox <- abs(probaTox - targetTox)
    dTox <- which(diffTox == min(diffTox))
    dp <- which(p == max(p))
    newDose_temp <- min(dTox, dp)
    
    if(newDose_temp > maxDose+1){
        newDose <- maxDose + 1
    }else{newDose <- newDose_temp}
    return(newDose)
}
