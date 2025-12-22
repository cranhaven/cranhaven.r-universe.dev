#' Counter Factual Model - summary
#'
#' A function to estimate the survival function based on parameter estimates -
#' used in ootstrapping CFM for CIs
#'
#' @param i indicator object
#' @param pscOb a pscOb object
#' @param lam parameters of the flexible spline model
#' @param kn knots included in the flexible spline model
#' @param k number of knots in the flexible spline model
#' @param tm time at which to assess the survival function
#' @param cov a matrix of covariates
#' @param rest a set of parameter covariate draws
#' @param beta parameter with which to adjust the baseline function
#' @importFrom survival Surv
#' @return A set of survival estimates
boot_sest <- function(i,pscOb=pscOb,lam=lam,kn=kn,k=k,cov=cov,tm=tm,rest=rest,beta=beta){
  hc <- rest[i,1:length(pscOb$haz_co)]
  cc <- rest[i,-c(1:length(pscOb$haz_co))]
  se <- spline_surv_est(lam=lam,kn=kn,k=k,haz_co=hc,
                        cov_co=cc,cov=cov,tm=tm,beta=beta)
  se$S
}
