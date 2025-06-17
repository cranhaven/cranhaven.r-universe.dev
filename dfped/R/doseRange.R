#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
doseRange <-
function(Clch, Clad, doseAd){
    AUC <- doseAd/Clad
    doseCh <- c()
    for(i in 1:length(doseAd)){
        d_ch_i <- Clch*AUC[1,i]
        doseCh <- c(doseCh, mean(d_ch_i)) 
    }
    return(doseCh)
}
