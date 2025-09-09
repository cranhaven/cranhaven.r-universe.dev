#' A function to obtain survival estimates from a flexible parametric model
#'
#' This function provides basic survival estimates from a flexible parametric survival
#' model
#'
#'
#' @param DC_clean a cleaned dataset ontained using dataComb().
#' @param beta a parameter to determine if the survival probabilities should be
#' adjusted by some (log) hazard ratio.  Defaults to beta=0, i.e. no adjustment.
#' @param s if specified will return the time at which some threshold is passed
#' (e.g. s=0.5 for median survival time)
#' @return a list of times and assoicated survival probabilities
#' @details A fucntion which extracts survival probabilities from a flexsurvreg object
#' @export
surv_fpm <- function(DC_clean,beta=0,s=NULL){

  me <- DC_clean$model_extract
  dc <- DC_clean
  time <- dc$outcome$time
  logt <- log(time+1e-06)

  lam <- me$lam
  kn <- me$kn
  cov_co <- me$cov.co
  haz_co <- me$haz_co
  k <- me$k

  linPred <- dc$cov[,1:length(me$cov_co)]%*%me$cov_co
  adjLP <- mean(linPred) + beta

  z <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
  }

  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  H<- H0*exp(adjLP)
  S <- exp(-H)


  ord <- order(time)
  ret <- list("time"=time[ord],"S"=S[ord])

  if(!is.null(s)){
    cond <-  abs(ret$S-s)
    ret <- ret$time[which(cond ==min(cond))]
  }

  return(ret)

}
