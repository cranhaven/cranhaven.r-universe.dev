#' Make predictions using a fitted varying coefficient model
#'
#'
#' @export
#'
#' @import foreach
#' @importFrom fields rdist.earth
#' @importFrom stats sd quantile
#' @useDynLib telefit, .registration = TRUE
#' 
#' @param fit svcFit object containing posterior samples
#' @param Xn [nr*nt, p] matrix of local covariates at new timepoint
#' @param Zn [nr, nt] matrix of remote covariates at new timepoints
#' @param burn number of posterior samples to burn from fit
#' @param cat.probs vector of probabilities for also returning categorical 
#'  predictions from the posterior prediction samples; NULL otherwise
#' @param stData Object with class 'stData' containing data needed to fit this 
#'  model. The data is used to compute empirical quantiles for making 
#'  categorical predictions.
#' @param stDataNew object of class stData that includes information needed for 
#'  making forecasts.
#' @param conf Parameter specifying the HPD level to compute for posterior 
#'  predictive samples
#' 
#' @example examples/svcMod.R


svcPredict = function(fit, Xn=NULL, Zn=NULL, stData=NULL, stDataNew=NULL, 
                      burn=0, cat.probs = c(1/3, 2/3), conf = .95) {
  
  if(!is.null(stDataNew)) {
    Xn = as.matrix(arrayToLong(stDataNew$X, fit$coords, 1)[,-(1:3)])
    tLabs = stDataNew$tLabs
    stopifnot(!is.null(Zn))
  } else {
    stopifnot(!is.null(Xn), !is.null(Zn))
  }
  
  D = rdist.earth(fit$coords, miles=fit$miles)
  
  if(burn>0) {
    for(i in 1:length(fit$parameters$samples)) {
      fit$parameters$samples[[i]] = fit$parameters$samples[[i]][-(1:burn),]
    }
  }
  
  # draw from posterior predictive distribution
  res = .Call(`r_svcpredict`, 
              matrix(fit$parameters$samples$T, ncol = length(fit$priors$T$Psi)), 
              matrix(fit$parameters$samples$beta, 
                     ncol = nrow(fit$priors$beta$Linv)),
              fit$parameters$samples$theta, fit$parameters$samples$sigmasq,
              fit$parameters$samples$sigmasqeps, fit$parameters$samples$rho, 
              Xn, Zn, D, fit$priors$cov$nu)
  
  # if not working directly with stXXX objects, return basic output
  if(is.null(stData))
    return(res)
  
  # reshape posterior predictive samples
  nt0 = ncol(Zn)
  n = nrow(D)
  nSamples = nrow(res$y)
  tmp = array(dim = c(n, nt0, nSamples))
  for(j in 1:nt0) {
    tmp[,j,] = t(res$y[,((j-1)*n+1):(j*n)])
  }
  res = tmp
  rm(tmp)
  
  # compute empirical breakpoints at each location to define forecast categories
  if(!is.null(cat.probs)) {
    category.breaks = t(apply(stData$Y, 1,
                              function(r) { quantile(r, probs = cat.probs)}))
  }
  
  
  # package results
  Y = foreach(t = 1:nt0, .combine='c') %do% {
    
    # generate HPD intervals
    forecast.mcmc = mcmc(t(res[,t,]))
    forecast.hpd = HPDinterval(forecast.mcmc, prob = conf)
    
    if(!is.null(cat.probs)) {
      
      # build categorical predictive distribution (process by location)
      pred.cat = foreach(s = 1:nrow(res), .combine='rbind') %do% {
        # extract posterior samples for specified location and timepoint
        y = res[s,t,]
        # return posterior probabilities of categories
        table(findInterval(y,category.breaks[s,]))/length(y)
      }
      colnames(pred.cat) = 1 + as.numeric(colnames(pred.cat))
      
      # extract categorical predictions (process by location)
      Y.cat = apply(pred.cat, 1, which.max)
    }
    
    pred = data.frame(
      Y = colMeans(forecast.mcmc),
      se = apply(forecast.mcmc, 2, sd),
      Y.lwr = forecast.hpd[,1],
      Y.upr = forecast.hpd[,2],
      Y.cat = Y.cat
    )
    
    r = list(
      pred = pred,
      pred.cat = pred.cat,
      yrLab = tLabs[t]
    )
    list(r)
  }

  # format return
  ret = list(
    pred = Y,
    samples = list(forecast=res),
    coords.s = fit$coords,
    miles = fit$miles,
    cat.probs = cat.probs,
    category.breaks = category.breaks,
    tLabs = tLabs,
    Y.lab = stData$Y.lab
  )
  
  class(ret) = 'svcPredict'
  
  ret
}