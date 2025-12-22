#' Counter Factual Model - summary
#'
#' A function to estimate the survival function based on parameter estimates -
#' used in ootstrapping CFM for CIs
#'
#' @param lam parameters of the flexible spline model
#' @param kn knots included in the flexible spline model
#' @param k number of knots in the flexible spline model
#' @param haz_co parameters for the baseline hazard function in the flexible
#' spline model
#' @param cov_co covariate parameters of the flexible spline model
#' @param cov a matrix of covaraites from the Data Cohort
#' @param tm time at which to assess the survival function
#' @param beta parameter with which to adjust the baseline function (defaults to
#' beta=0)
#' @return A data frame containing survival estimates for a give time
spline_surv_est <- function(lam,kn,k,haz_co,cov_co,cov=cov,tm=tm,beta=0){

  ret <- S <- f <- NULL
  if(is.null(cov)) cov <- rep(0,length(cov_co))

  logt <- log(tm)
  lp <- mean(cov%*%cov_co);lp

  ### ordering
  ord <- order(logt)
  tm <- tm[ord]
  logt <- logt[ord]


  z <- NULL
  z_h <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)

    zt_h <- (modp(logt-kn[(i+1)])^2 - lam[(i+1)]*modp(logt-kn[1])^2 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^2)
    z_h <- cbind(z_h,zt_h)
  }

  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  h0 <- (H0/tm)*(haz_co[2]+3*z_h%*%haz_co[3:(2+k)])

  H<- H0*exp(lp+beta)
  h<- h0*exp(lp+beta)
  S <- exp(-H)
  f <- S*h

  ret <- data.frame("time"=tm,"S"=S,"f"=f)
  ret
}
