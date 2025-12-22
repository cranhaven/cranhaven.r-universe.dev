#' Likelihood function for a psc model of class 'glm'
#'
#' A function which defines the likelihood for a PSC model where the Counter
#' Factual Model (CFM) takes the form of a 'glm' object and an efficacy
#' parameter (\eqn{\beta}) is being estimated.  For more details on fitting please see
#' ?pscfit and ?pscEst
#'
#' @param beta a parameter to be estimate
#' @param pscOb a pscOb object containing a cleaned dataset including
#' covariates to match the CFM
#' @details A likelihood function for use by pscfit for a model of class
#' 'glm'
#' @return the results of a likelihood functions
#' @importFrom enrichwith enrich
lik.glm <- function (beta,pscOb){

  ## Getting data& model objects
  event <- pscOb$DC$Y
  cov <- pscOb$DC$X
  cov_co <- pscOb$cov_co
  fam <- enrichwith::enrich(pscOb$family)

  ### Specifying treatment for MTC
  if(!is.null(pscOb$DC$trt)){
    trt <- pscOb$DC$trt
    trt <- factor(trt)
    lev <- levels(trt)

    ### Error trap - beta supplied must match number of treatment levels
    if(length(beta)!=length(lev)){ stop(paste("beta does not match numebr of
           levels for mulitple treatment effects. No. levels for beta =",
                                              length(beta)," to correspond to", lev))
    }

    ### Linear predictor of the treatment effect
    beta <- model.matrix(~-1+trt)%*%beta
  }

  ## Setting likelihood
  lp <- cov %*% cov_co + beta;lp
  mu <- fam$linkinv(lp)
  theta <- fam$theta(mu)
  btheta <- fam$bfun(theta)
  -sum(event * theta - btheta)
}
