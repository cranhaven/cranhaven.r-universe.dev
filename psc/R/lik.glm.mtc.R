#' Likelihood function for a psc model of class 'glm' with multiple
#' treatment comparisons
#'
#' A function which defines the likelihood for a PSC model where the Counter
#' Factual Model (CFM) takes the form of a 'glm' object and a mulitple efficacy
#' parameters (\eqn{\beta}) is being estimated.  For more details on fitting please see
#' ?pscfit and ?pscEst
#'
#' @param beta a parameter to be estimate
#' @param DC_clean a cleaned dataset including covariates to match the CFM
#' @details A likelihood function for use by pscfit for a model of class 'flexsurvreg'
#'     where multiple treatment comparisons are required
#'
lik.glm.mtc <- function(beta,DC_clean){

  event <- as.numeric(DC_clean$out[,1]);event
  if(DC_clean$model_extract$family$family=="binomial") event = as.numeric(as.factor(event)) -1

  cov <- DC_clean$cov

  trt.id <- which(colnames(cov)=="trt")
  trt <- cov[,trt.id]
  cov <- cov[,-trt.id]

  trt <- factor(trt)
  lev <- levels(trt)

  if(length(beta)!=length(lev)) stop("beta does not match numebr of levels in treatment")
  Beta <- model.matrix(~-1+trt)%*%beta

  fam <- enrich(DC_clean$model_extract$family);fam
  cov_co <- DC_clean$model_extract$cov_co;cov_co

  lp <- cov %*% cov_co + Beta;lp
  mu <- fam$linkinv(lp)
  theta <- fam$theta(mu)
  btheta <- fam$bfun(theta)

  -sum(event * theta - btheta)
  }
