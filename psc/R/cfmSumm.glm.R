#' Counter Factual Model - summary
#'
#' A generic function to provide a summary of a Counter factual model of class
#' 'glm'
#'
#' @param pscOb an object of class 'psc'
#' @param bootCI a boolean to determine if bootstrapping CIs are required
#' @param nboot Number of bootstraps
#' @param resp Should results be on the response scale?
#' @importFrom enrichwith enrich
#' @return A summary of a cfm object
cfmSumm.glm <- function(pscOb,bootCI=TRUE,nboot=1000,resp=TRUE){

  ### Set-Up
  cov <- pscOb$DC$X
  cov_co <- pscOb$cov_co;cov_co
  lp <- cov %*% cov_co
  ret <- lp

  ### Getting beta
  if(is.null(beta)) {
    beta <- pscOb$postEst$mean
  }

  ### Getting family
  if(resp){
    fam <- enrichwith::enrich(pscOb$family)
    ret <- fam$linkinv(lp)
  }

  summ <- mean(ret);summ

  ### Bootstrapping 95% CI
  if(bootCI){
    vc <- pscOb$sig
    mu <- pscOb$cov_co
    rest <- rmvnorm(nboot,mu,vc)

    y_boot <- mapply(boot_lp,1:nboot,MoreArgs=list(pscOb,resp=resp,rest))
    mn_boot <- unlist(lapply(y_boot,mean))
    ci <- quantile(mn_boot,p=c(0.025,0.975))
    summ <- c(summ,ci)
    names(summ)[1] <- "mn"
  }

  ret <- list("lp"=ret,"summ"=summ)
  return(ret)

}


