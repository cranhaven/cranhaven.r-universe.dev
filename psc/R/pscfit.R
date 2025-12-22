#' Personalised Synthetic Controls model fit
#'
#' Function which allows comparison of a data cohort against a parametric Counter Factual Model (CFM).
#' The function allows models of the type 'flexsurvreg' and 'glm' to be supplied.
#' The function performs by calculating the linear predictor as a combination of
#' the CFM and the dataset supplied and then selects a likelihood
#' based on the type of model specified.  Likelihood is estimated using a Baysian MCMC
#' procedure wherebey the parameters of the CFM acts as informative priors.
#'
#' Model currently supports estimation of more than one treatment (using the
#' 'trt') option and esitmation restricted to sub-groups of the data cohort
#' (using the 'id' option.
#'
#' @param CFM An R model object of class 'glm' or 'flexsurvspline'
#' @param DC A dataset including columns to match to covariates in the model
#' @param nsim The number of simulations for the MCMC routine
#' @param id Numeric vector stating which patient(s) from the dataset should be included in the analysis.
#'  Defaults to all patients
#' @param trt An optional vector denoting treatment allocations for multiple treatment comparisons.  Defaults to NULL.
#' @param nchain Number of chains used in posterior MCMC estimation.  Defaults to nchain=3.
#' @param thin Thin applied to posterior draws.  Defaults to thin=2.
#' @param burn Number of posterior samples to use as burn-in.  Defaults to burn=500
#' @details the \code{pscfit} function compares a dataset ('DC') against a parametric model.
#'   This is done by selecting a likelihood which is identified by the type of CFM that is supplied.
#'   At present, two types of model are supported, a flexible parmaeteric survival model of type 'flexsurvreg'
#'   and a geleneralised linear model of type 'glm'.
#'
#'   Where the CFM is of type 'flexsurvreg' the likeihood supplied is of the form:
#'
#'   \deqn{L(D \vert \Lambda, \Gamma_i) = \prod^{n}_{i=1} f(t_i \vert  \Lambda, \Gamma_i)^{c_i}
#'   S(t_i|\Lambda, \Gamma_i)^{(1-c_i)}}
#'
#'   Where \eqn{\Lambda} defines the cumulative baseline hazard function,
#'   \eqn{\Gamma} is the linear predictor and \eqn{t} and \eqn{c} are the
#'   event time and indicator variables.
#'
#'   Where the CFM is of the type 'glm' the likelihood supplied is of the form:
#'
#'    \deqn{L(x \vert \Gamma_i) = \prod^{n}_{i=1} b(x \vert \Gamma_i) \exp{\{\Gamma_i^T t(x)
#'    - c(\Gamma_i)\} } }
#'
#'   Where \eqn{b(.)}, \eqn{t(.)} and \eqn{c(.)} represent the functions of the
#'   exponential family. In both cases, \eqn{\Gamma} is defined as:
#'
#'   \deqn{\Gamma = \gamma x + \beta}
#'
#'   Where \eqn{\gamma} are the model coefficients supplied by the CFM and \eqn{\beta}
#'   is the parameter set to measure the difference between the CFM and the DC.
#'
#'   Estimation is performed using a Bayesian MCMC procedure.  Prior distributions
#'    for \eqn{\Gamma} (& \eqn{\Lambda}) are derived directly from the model
#'    coefficients (mean and variance covariance matrix) or the CFM. A bespoke MCMC
#'    routine is performed to estimate \eqn{\beta}.  Please see '?mcmc' for more detials.
#'
#'    For the standard example where the DC contains information from only a single
#'    treatment, trt need not be specified.  Where comparisons between the CFM and
#'    multiple treatments are require, a covariate of treamtne allocations must be
#'    specified sperately (using the 'trt' option).
#'
#' @return a object of class 'psc' with attributes model.type, the cleaned Dataset and the
#'   posterior distribution of the fitted model
#'
#'   Attributes include \itemize{
#'
#'  \item {A 'cleaned' dataset including extracted components of the CFM and the
#'  cleaned DC included in the procedure}
#'  \item {An object defining the class of model (and therefore the procedure
#'  applied - see above)}
#'  \item {A matrix containing the draws of the posterior distributions}
#'  }
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' psc <- pscfit(gemCFM,e4_data,nsim=1500,nchain=1)
#' print(psc)
#' @export
pscfit <- function (CFM, DC, nsim = 2000, id = NULL, trt = NULL,nchain=2,thin=2,burn=500){

  #### Step 1 - create pscCFM object (may not be required if pscCFM object supplied)
  if(!"pscCFM"%in%class(CFM)){
    CFM  <- pscCFM(CFM)
  }

  ### Step 2 - create pscfit data object
  pscOb <- pscData(CFM,DC,id=id,trt=trt)

  ### Step 3 - get initial values
  pscOb <- init(pscOb)

  ### Step 4 - MCMC estimation
  pscOb <- pscEst(pscOb,nsim,nchain)

  ### Step 5 - Summarising posterior
  pscOb <- postSummary(pscOb,thin=thin,burn=burn)

  ### Giving 'class to result'
  class(pscOb) <- "psc"
  pscOb

}



