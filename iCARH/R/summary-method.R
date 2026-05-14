#' @title Summarize and return model parameters
#'
#'@description Group of functions to summarize and return model parameters of interest
#'
#' @describeIn iCARH.params Summary of model parameters
#'
#' @param fit Object returned by iCARH.model
#' @param pars Parameters of interest ("theta","alpha","beta","phi"). All parameters by default.
#' @param path.names Specify pathway names.
#' @param prob Confidence level. Defaults to 0.95.
#' @param use_cache passed to stan summary method.
#' @param digits The number of significant digits for printing out the summary; 
#'  defaults to 2. The effective sample size is always rounded to integers.
#' @param ... not used currently
#'
#' @return contain summaries for all chains. Included in the summaries are means, standard deviations (Est.Error), effective sample sizes (Eff.Sample), and split Rhats. 
#' Monte Carlo standard errors (MC.Error) are also reported.
#'
#' @examples data.sim = iCARH.simulate(4, 10, 14, 8, 2, path.probs=0.3, Zgroupeff=c(0,4),
#' beta.val=c(1,-1,0.5, -0.5))
#' XX = data.sim$XX
#' Y = data.sim$Y
#' Z = data.sim$Z
#' pathways = data.sim$pathways
#' \donttest{
#' rstan_options(auto_write = TRUE)
#' options(mc.cores = 2)
#' fit = iCARH.model(XX, Y, Z, groups=rep(c(0,1), each=5), pathways, 
#' control = list(adapt_delta = 0.99, max_treedepth=10), iter = 2, chains = 2)
#' if(!is.null(fit$icarh))
#' iCARH.params(fit)}
#'
#'
#' @importFrom rstan summary
#' @export iCARH.params

iCARH.params <- function(fit, pars=c("theta","alpha","beta","phi"), path.names=NULL, prob = 0.95, use_cache = TRUE, digits=2, ...){
  probs = c((1 - prob) / 2, 1 - (1 - prob) / 2)
  fit_summary = summary(fit$icarh, pars = pars, probs = probs, use_cache = use_cache)$summary
  ci <- paste0( probs * 100, "%")
  colnames(fit_summary) <- c("Estimate", "MC.Error", "Est.Error", ci, "Eff.Sample", "Rhat")
  xnames = attr(fit$X, "dimnames")[[3]]
  ynames = attr(fit$Y, "dimnames")[[3]]
  P = dim(iCARH.getPathwaysCoeff(fit))[2]
  rhats = iCARH.checkRhats(fit)
  cat("\nResponse: ")
  if(setequal(fit$drug, c(0,1))) cat(" Binary.\n") else cat(" Continuous.\n")
  cat("Data: ")
  cat(nrow(fit$X), " time points, ", ncol(fit$X), " observations, ", P, "pathways.\n ")
  cat("X has ", length(xnames), " variables, Y has ", length(ynames), " variables.\n")
  cat("MCMC samples: ")
  cat(fit$icarh@sim$chains, " chains, ", fit$icarh@sim$iter, " iterations each with ",
      fit$icarh@sim$warmup, " warmup samples.\n")
  
  if ("theta" %in% pars){
    cat("\nTemporal Effects (theta):\n")
    tempo = fit_summary[grepl("^theta\\[([0-9]+,*)*\\]$", rownames(fit_summary)),]
    rownames(tempo) = xnames
    colnames(tempo) = colnames(fit_summary)
    print(tempo, digits=digits)
  }
  if("alpha" %in% pars){
    cat("\nTreatment Effect (alpha):\n")
    treat = fit_summary[grepl("^alpha\\[([0-9]+,*)*\\]$",rownames(fit_summary)),]
    rownames(treat) = xnames
    colnames(treat) = colnames(fit_summary)
    print(treat, digits=digits)
  }
  if(("beta" %in% pars) & !is.null(fit$Y) ){
    cat("\nEffect of Y variables (beta):\n")
    yeff = fit_summary[grepl("^beta\\[([0-9]+,*)*\\]$",rownames(fit_summary)),]
    rownames(yeff) = paste0(rep(xnames, each=length(ynames)),"/",ynames)
    colnames(yeff) = colnames(fit_summary)
    print(yeff, digits=digits)
  }
  if("phi" %in% pars){
    cat("\nPathway Coefficients:\n")
    path.names = if(is.null(path.names)) paste("path",1:P) else path.names
    path = fit_summary[grepl("^phi\\[([0-9]+,*)*\\]$",rownames(fit_summary)),]
    rownames(path) = paste0(rep(path.names, each=2), c("controls", "cases"), sep="/")
    colnames(path) = colnames(fit_summary)
    print(path, digits=digits)
  }
  
  cat("\nWAIC: ", iCARH.waic(fit), ".\n")
  return(fit_summary)
}
