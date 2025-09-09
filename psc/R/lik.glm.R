#' Likelihood function for a psc model of class 'glm'
#'
#' A function which defines the likelihood for a PSC model where the Counter
#' Factual Model (CFM) takes the form of a 'glm' object and an efficacy
#' parameter (\eqn{\beta}) is being estimated.  For more details on fitting please see
#' ?pscfit and ?pscEst
#'
#' @param beta a parameter to be estimate
#' @param DC_clean a cleaned dataset including covariates to match the CFM
#' @details A likelihood function for use by pscfit for a model of class
#' 'glm'
#'
lik.glm <- function (beta, DC_clean){
  event <- as.numeric(DC_clean$out[,1]);event
  if(DC_clean$model_extract$family$family=="binomial") event = as.numeric(as.factor(event)) -1
  cov <- DC_clean$cov;cov
  fam <- enrich(DC_clean$model_extract$family)
  cov_co <- DC_clean$model_extract$cov_co;cov_co
  lp <- cov %*% cov_co + beta;lp
  mu <- fam$linkinv(lp)
  theta <- fam$theta(mu)
  btheta <- fam$bfun(theta)
  -sum(event * theta - btheta)
}
