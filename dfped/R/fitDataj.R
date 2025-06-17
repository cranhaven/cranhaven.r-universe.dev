#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
fitDataj <-
function(crmModel, nbPatientsj, nbDoses, tox, eff, givenDose, skeletonTox,
                     skeletonEff, mu, sigma, model){
    
    # If only one skeleton - bivariate CRM
    crm_dat_j <- list(K= nbDoses, J = nbPatientsj, y = tox, v = eff, r = skeletonTox[ ,model],
                      q = skeletonEff[ ,model], d= givenDose, moy = mu, standardError = sigma)
    fitj <- sampling(crmModel, data= crm_dat_j, chains=3, warmup=1000, iter=2000)
    return(fitj)	
}
