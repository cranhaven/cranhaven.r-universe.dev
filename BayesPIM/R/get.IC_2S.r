#' Compute Information Criteria for a Bayesian Prevalence-Incidence Mixture Model
#'
#' Computes and returns information criteria for a fitted Bayesian prevalence-incidence mixture model, including the Widely Applicable Information Criterion 1 (WAIC-1), WAIC-2, and the Deviance Information Criterion (DIC). These criteria are commonly used for model comparison and evaluation in Bayesian analysis. See Gelman et al. (2014) for further details on these criteria.
#' 
#' @details
#' This function calculates information criteria for a fitted Bayesian prevalence-incidence mixture model (`bayes.2S`). The information criteria include:
#' 
#' - **WAIC-1**: Based on the sum of posterior variances of log-likelihood contributions.
#' - **WAIC-2**: Similar to WAIC-1 but incorporates an alternative variance estimate.
#' - **DIC**: Measures model fit by penalizing complexity via the effective number of parameters.
#' 
#' The computation is performed by evaluating log-likelihood values for MCMC samples. By default, all MCMC samples after burn-in are used, though a subset can be specified via the `samples` argument.
#' 
#' Parallelization is available via the `foreach` package, utilizing multiple cores if `cores` is set accordingly. If `cores = NULL`, all available cores will be used.
#'
#' @param mod A fitted prevalence-incidence mixture model of class \code{bayes.2S}.
#' @param samples The number of MCMC samples to use. Maximum is the number of post-burn-in samples available in the \code{bayes.2S} object.
#' @param cores The number of cores for parallel processing using \code{foreach}. If \code{NULL} (default), all available cores will be used.
#'  
#' @return A \code{matrix} containing WAIC-1, WAIC-2, and DIC values for the model.
#' 
#' @references 
#' Gelman, A., Hwang, J., & Vehtari, A. (2014). Understanding predictive information criteria for Bayesian models. Stat Comput, 24(6), 997–1016.
#' 
#' @examples
#' \donttest{
#' # Generate data according to the Klausch et al. (2024) PIM
#' set.seed(2025)
#' dat = gen.dat(kappa = 0.7, n= 1e3, theta = 0.2,
#'               p = 1, p.discrete = 1,
#'               beta.X = c(0.2,0.2), beta.W = c(0.2,0.2),
#'               v.min = 20, v.max = 30, mean.rc = 80,
#'               sigma.X = 0.2, mu.X=5, dist.X = "weibull",
#'               prob.r  = 1)
#' 
#' # An initial model fit with fixed test sensitivity kappa (approx. 1-3 minutes, depending on machine)
#' mod = bayes.2S( Vobs = dat$Vobs,
#'                 Z.X = dat$Z,
#'                 Z.W = dat$Z,
#'                 r= dat$r,
#'                 kappa = 0.7,
#'                 update.kappa = FALSE,
#'                 ndraws= 1e4,
#'                 chains = 2,
#'                 prop.sd.X = 0.008,
#'                 parallel = TRUE,
#'                 dist.X = 'weibull'
#' )
#' 
#' # Get information criteria
#' get.IC_2S(mod, samples = 1e3, cores = 2)
#' }
#' 
#' @export
get.IC_2S = function(mod, samples = nrow(mod$par.X.bi[[1]]), cores = NULL){
  update.kappa = mod$update.kappa
  vanilla = mod$vanilla
  p1.X = ncol(mod$Z.X)+1
  if(!vanilla) p1.W = ncol(mod$Z.W)
  
  if(is.null(cores)) cores = detectCores()
  m.X = trim.mcmc(mod$par.X.all, burnin = round(nrow(as.matrix(mod$par.X.all[1]))/2)+1)
  m.X = as.matrix(m.X)
  if(!update.kappa) m.X = cbind(m.X, mod$kappa)
  if(samples >  nrow(m.X)) {stop ('more samples than mcmc draws selected')}
  
  ncol.X= ncol(m.X)
  m.X[,(p1.X + 1)] = log(m.X[,(p1.X + 1)])
  m.s = m.X[sample(1:nrow(m.X), samples, replace=FALSE),]
  
  cl    = makePSOCKcluster(cores)
  clusterSetRNGStream(cl)
  registerDoParallel(cl)
  s = round(seq (1, samples, length.out = cores+1))
  pst.mean = apply(m.X, 2, mean) 

  run = foreach(j = 1:cores # ndraws=rep(mc, cores)+1:cores
                , .export = c('pdist', 'qdist', 'rdist', 'ddist', 'Lobs_2S',
                              'ploglog', 'rloglog','qloglog', 'dloglog','P_vobs', 'geom', 'geom.inf', 'P_vobs2',
                              'pllogis', 'rllogis', 'dllogis', 'qllogis', 'trans.par', 'trans.par.ind.norm')
                
  ) %dopar% {
    m.s.cores = m.s[s[j]:(s[j+1]-1),]
    out = apply(m.s.cores,1, function(x) Lobs_2S(est=x, mod=mod, log.scale = FALSE, sumup=FALSE) ) 
  }
  stopCluster(cl)
  
  run = do.call(cbind, run)
  
  lppd = sum (log(apply(run,1, mean)))
  q1   = sum (apply( log(run), 1, mean))
  q2   = sum( apply( log(run), 1, var) )
  q3   = mean( apply( log(run),2, sum) )
  q4   = sum(Lobs_2S(pst.mean, mod=mod, log.scale = TRUE, sumup=FALSE))
  DIC  = -2* ( q4 - 2*(q4-q3) )
  WAIC1    = -2*(-lppd + 2*q1)
  WAIC2    = -2*(lppd - q2)
  mat=matrix(nrow=1, ncol=3)
  mat[1,]=c(WAIC1, WAIC2, DIC)
  colnames(mat) = c('WAIC1', 'WAIC2', 'DIC')
  mat
}