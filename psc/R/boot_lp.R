#' Counter Factual Model - summary
#'
#' A function to estimate the linear predictor - used in bootstrapping CFM for
#' CIs
#'
#' @param i indicator object
#' @param pscOb an object of class 'psc'
#' @param resp A boolean object to determine if results should be presented on
#' the response scale
#' @param rest A matrix of sample covariate estimates
#' @importFrom enrichwith enrich
#' @return A simulated set of responses
boot_lp <- function(i,pscOb,resp=resp,rest=NULL){

  cc <- rest[i,]
  cov <- pscOb$DC$X

  lp <- cov %*% cc
  ret <- lp

  if(resp){
    fam <- enrichwith::enrich(pscOb$family)
    ret <- fam$linkinv(lp)
  }

  ret
}
