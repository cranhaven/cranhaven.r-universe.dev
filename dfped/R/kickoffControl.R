#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
kickoffControl <-
function(tox, currentDose, cohortSize, nbDoses){
    nbTox <- which(tox %in% 1)
    newDose <- NA
    
    kickofftemp<- FALSE
    if(length(nbTox) != 0|nbDoses == currentDose){
        kickofftemp <-TRUE
    }else{
        newDose <- min(currentDose + 1, nbDoses)
    }
    return(list(kickofftemp,newDose))
}
