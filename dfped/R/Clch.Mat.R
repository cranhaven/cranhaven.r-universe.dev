#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
Clch.Mat <-
function(age, w, Clad, Wad, dataMolecule){
    F_ad <- dataMolecule[[1]]
    f_abs <- dataMolecule[[2]]
    Eg <- dataMolecule[[3]]
    Eh <- dataMolecule[[4]]
    fu_ad <- dataMolecule[[5]]
    perc_CYPg <- dataMolecule[[6]]
    perc_CYPh <- dataMolecule[[7]]
    perc_alb <- dataMolecule[[8]]
    perc_alpha1AG <- dataMolecule[[9]]
    
    res <- Clchu(age, w, Clad, Wad, f_abs, fu_ad, F_ad, Eg, Eh, perc_CYPh)*fuCh(age, fu_ad, perc_alb, perc_alpha1AG)/Fch(age, f_abs, Eg, Eh, perc_CYPg, perc_CYPh)
    
    return(res)
}
