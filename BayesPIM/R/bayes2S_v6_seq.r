#' @title bayes.2S_seq: Bayesian Prevalence-Incidence Mixture Model (sequential processing)
#'
#' @description Estimates the Pattern Mixture model of Klausch et al. (2025) using a Bayesian Gibbs sampler. Identical to \link{bayes.2S} but uses a \code{for} loop over MCMC chains instead of \code{foreach}.
#'
#' @details 
#' This function is dentical to \link{bayes.2S} with the only difference being that the \code{chains} MCMC chains are run in sequence using a \code{for} loop instead of parallel processing. This can be useful if operating systems do not support \code{foreach} or for simulation studies that parallize replication of experiments using \code{foreach} and/or need a worker that does not apply \code{foreach} internally.
#'    
#' @param Vobs A list of length \eqn{n} of numeric vectors representing screening times. The first element of each vector should always be \code{0} and the last element \code{Inf} in the case of right censoring.
#' @param Z.X A numeric matrix of dimension \eqn{n \times p_x} containing covariates for the AFT incidence model. Missing values are not allowed.
#' @param Z.W A numeric matrix of dimension \eqn{n \times p_w} containing covariates for the probit prevalence model. Missing values are not allowed.
#' @param r A binary vector of length \eqn{n} indicating whether a baseline test was conducted (\code{1} for yes, \code{0} for no / missing baseline test). 
#'
#' @param dist.X Character. Specifies the distribution for the time-to-incidence variable; choices are \code{'weibull'}, \code{'lognormal'}, or \code{'loglog'} (log-logistic).
#' @param kappa Numeric. The test sensitivity value to be used if \code{update.kappa = FALSE}; otherwise, the starting value for estimating \eqn{\kappa}.
#' @param update.kappa Logical. If \code{TRUE}, the test sensitivity (\eqn{\kappa}) is updated during the Gibbs sampler.
#' @param kappa.prior A numeric vector of length 2. When specified, a Beta distribution prior is used for \eqn{\kappa}, centered at \code{kappa.prior[1]} with standard deviation \code{kappa.prior[2]}. If \code{NULL}, a uniform prior (Beta(1,1)) is used.
#'
#' @param ndraws Integer. The total number of MCMC draws for the main Gibbs sampler.
#' @param prop.sd.X Numeric. The standard deviation for the proposal (jumping) distribution in the Metropolis sampler used for updating \eqn{\beta_{xj}}. Can be searched for automatically using \link{search.prop.sd}.
#' @param chains Integer. The number of MCMC chains to run.
#' @param thining Integer. The thinning interval for the MCMC sampler.
#' @param parallel Logical. If \code{TRUE}, parallel processing is enabled for the Gibbs sampler. Each chain is assigned to one core (via the \code{foreach} package). Alternatively, use \link{bayes.2S_seq} which employs a \code{for} loop over the chains.
#' @param update.till.converge Logical. If \code{TRUE}, the model is updated iteratively until convergence criteria are met. Convergence is assessed using the Gelman-Rubin diagnostic (\eqn{R<1.1}) and a minimum effective sample size (\code{min_effss}) for each parameter, respectively.
#' @param maxit Numeric. The maximum number of MCMC draws allowed before interrupting the update process when \code{update.till.converge} is enabled. Default is \code{Inf} (i.e., no automatic interruption).
#' @param conv.crit Character. Specifies whether the convergence check uses the point estimate (\code{'point'}) or the upper bound (\code{'upper'}) of the Gelman-Rubin diagnostic estimate (\eqn{\hat{R}}).
#' @param min_effss Integer. The minimum effective sample size required for each parameter before convergence is accepted during iterative updating.
#'
#' @param beta.prior Character. Specifies the type of prior for the incidence regression coefficients (\eqn{\beta_{xj}}); options are \code{'norm'} for normal and \code{'t'} for student-t.
#' @param beta.prior.X Numeric. The hyperparameter for the prior distribution of the regression coefficients (\eqn{\beta_{xj}}) in the AFT incidence model. For a normal prior, this is the standard deviation; for a student-t prior, it represents the degrees of freedom. The default produces a standard-normal prior.
#' @param sig.prior.X Numeric. The hyperparameter (standard deviation) for a half-normal prior on the scale parameter (\eqn{\sigma}) of the AFT incidence model.
#' @param tau.w Numeric. The hyperparameter (standard deviation) for the normal prior distribution of the regression coefficients (\eqn{\beta_{wj}}) in the probit prevalence model. The default produces a standard-normal prior.
#' @param fix.sigma.X Logical. If \code{TRUE}, the scale parameter (\eqn{\sigma}) in the AFT incidence model is fixed at the value provided in \code{sig.prior.X}; if \code{FALSE}, it is updated.
#'
#' @param prev.run Optional. An object of class \code{BayesPIM} containing results from a previous run. When provided, data and prior settings are adopted from the previous run, and the MCMC chain continues from the last draw.
#' @param update.burnin Logical. If \code{TRUE} (default) and \code{prev.run} is provided, the burn-in period is updated to half of the total draws (sum of previous and current runs); otherwise, the burn-in is maintained as half of the draws from the initial run.
#' @param ndraws.update Integer. The number of MCMC draws for updating a previous run or for convergence updates. If unspecified, \code{ndraws} is used.
#'
#' @param prev Logical. If \code{TRUE}, prevalence adjustment is applied; if \code{FALSE}, prevalence is assumed to be zero.
#' @param vanilla Logical. If \code{TRUE}, a vanilla run is performed that assumes no prevalence adjustment and fixes \eqn{\kappa = 1}, equivalent to a standard Bayesian interval-censored survival regression.
#' @param ndraws.naive Integer. The number of MCMC draws for a preliminary vanilla run used to obtain starting values. Increase if initial values lead to issues (e.g., an infinite posterior).
#' @param naive.run.prop.sd.X Numeric. The standard deviation for the proposal distribution used in the vanilla run. Adjust only if the acceptance rate is significantly off target, as indicated by an interruption message.
#'
#' @param par.exp Logical. If \code{TRUE}, the parameter expansion technique of Liu & Wiu (1999) with a Haar prior is employed for updating the regression coefficients (\eqn{\beta_{wj}}) in the prevalence model. Experimental: tests suggest that it does not improve convergence or reduce autocorrelation.
#' @param collapsed.g Logical. If \code{TRUE}, the latent prevalence class membership update in the Gibbs sampler is integrated (collapsed) over the latent incidence time variable. This setting is recommended to improve convergence.
#'
#' @param k.prior Experimental prior parameter for generalized gamma; currently not used.
#' @param fix.k Experimental fixing of prior parameter for generalized gamma; currently not used.
#'
#' @return A list containing the following elements:
#'
#' \item{par.X.all}{An \code{mcmc.list} of MCMC samples for the incidence and prevalence model parameters.}
#' \item{par.X.bi}{An \code{mcmc.list} of MCMC samples for the incidence and prevalence model parameters after burn-in removal.}
#' \item{X}{A matrix of posterior draws for the latent event times \eqn{x_i}, with one column per chain.}
#' \item{C}{A matrix of posterior draws for prevalence class membership \eqn{g_i}, with one column per chain.}
#' \item{ac.X}{A matrix with MCMC draws in rows and chains in columns, where each row indicates whether the Metropolis sampler accepted (1) or rejected (0) a sample.}
#' \item{ac.X.cur}{Same as \code{ac.X}, but only for the last update.}
#' \item{dat}{A data frame containing the last observed interval.}
#' \item{priors}{A list of prior specifications for the model parameters, including \code{beta.prior.X} (incidence regression coefficients) and \code{sig.prior.X} (scale parameter for the AFT model).}
#' \item{runtime}{The total runtime of the MCMC sampler.}
#'
#' Additionally, most input arguments are returned as part of the output for reference.
#'
#' @references 
#' T. Klausch, B. I. Lissenberg-Witte, and V. M. Coupe (2024). "A Bayesian prevalence-incidence mixture model for screening outcomes with misclassification.", doi:10.48550/arXiv.2412.16065.
#' 
#' J. S. Liu and Y. N. Wu, “Parameter Expansion for Data Augmentation,” Journal of the American Statistical Association, vol. 94, no. 448, pp. 1264–1274, 1999, doi: 10.2307/2669940.
#'
#' @examples
#' \donttest{
#' library(BayesPIM)
#' 
#' # Generate data according to the Klausch et al. (2025) PIM
#' set.seed(2025)
#' dat <- gen.dat(kappa = 0.7, n = 1e3, theta = 0.2,
#'                p = 1, p.discrete = 1,
#'                beta.X = c(0.2, 0.2), beta.W = c(0.2, 0.2),
#'                v.min = 20, v.max = 30, mean.rc = 80,
#'                sigma.X = 0.2, mu.X = 5, dist.X = "weibull",
#'                prob.r = 1)
#' 
#' # Initial model fit with fixed test sensitivity kappa (approx. 4-12 minutes runtime)
#' mod <- bayes.2S_seq(Vobs = dat$Vobs,
#'                 Z.X = dat$Z,
#'                 Z.W = dat$Z,
#'                 r = dat$r,
#'                 kappa = 0.7,
#'                 update.kappa = FALSE,
#'                 ndraws = 1e4,
#'                 chains = 4,
#'                 prop.sd.X = 0.008,
#'                 parallel = TRUE,
#'                 dist.X = "weibull")
#' 
#' # Inspect results
#' mod$runtime  # Runtime of the Gibbs sampler
#' plot(trim.mcmc(mod$par.X.all, thining = 10))  # MCMC chains including burn-in, also see ?trim.mcmc
#' plot(trim.mcmc(mod$par.X.bi, thining = 10))   # MCMC chains excluding burn-in
#' apply(mod$ac.X, 2, mean)  # Acceptance rates per chain
#' gelman.diag(mod$par.X.bi)  # Gelman convergence diagnostics
#' 
#' # Model updating
#' mod_update <- bayes.2S(prev.run = mod)      # Adds ndraws additional MCMC draws
#' mod_update <- bayes.2S(prev.run = mod, 
#'                        ndraws.update = 1e3) # Adds ndraws.update additional MCMC draws
#' 
#' # Example with kappa estimated/updated
#' mod2 <- bayes.2S_seq(Vobs = dat$Vobs,
#'                  Z.X = dat$Z,
#'                  Z.W = dat$Z,
#'                  r = dat$r,
#'                  kappa = 0.7,
#'                  update.kappa = TRUE,
#'                  kappa.prior = c(0.7, 0.1), # Beta prior, mean = 0.7, s.d. = 0.1
#'                  ndraws = 1e4,
#'                  chains = 4,
#'                  prop.sd.X = 0.008,
#'                  parallel = TRUE,
#'                  dist.X = "weibull")
#' 
#' # Inspect results
#' mod2$runtime # runtime of Gibbs sampler
#' plot( trim.mcmc( mod2$par.X.all, thining = 10) ) # kappa returned as part of the mcmc.list
#' }
#' 
#' @export
bayes.2S_seq <- function(
    # Group 1: Data inputs
  Vobs, 
  Z.X = NULL, 
  Z.W = NULL, 
  r = NULL,
  
  # Group 2: Basic model settings
  dist.X = 'weibull', 
  kappa = 0.5, 
  update.kappa = FALSE, 
  kappa.prior = NULL, 
  
  # Group 3: Main MCMC sampler settings
  ndraws = 1000, 
  prop.sd.X = NULL, 
  chains = 3, 
  thining = 1, 
  parallel = TRUE, 
  update.till.converge = FALSE, 
  maxit = Inf, 
  conv.crit = 'upper', 
  min_effss = chains * 10, 
  
  # Group 4: Prior specifications
  beta.prior = 'norm', 
  beta.prior.X = 1, 
  sig.prior.X = 1, 
  tau.w = 1, 
  fix.sigma.X = FALSE, 
  
  # Group 5: Updating previous run settings
  prev.run = NULL, 
  update.burnin = TRUE, 
  ndraws.update = NULL, 
  
  # Group 6: Additional model flags
  prev = TRUE, 
  vanilla = FALSE, 
  ndraws.naive = 5000, 
  naive.run.prop.sd.X = prop.sd.X, 
  par.exp = FALSE, 
  collapsed.g = TRUE, 
  
  # Group 7: Experimentals
  k.prior = 1, 
  fix.k = FALSE
) {
  
  t0 = Sys.time()
  if(!is.numeric(maxit)) stop('maxit has to be numeric.')
  
  if(!is.null(prev.run)){
    chains = length(prev.run$par.X.all)
    dims = dim(as.matrix(prev.run$par.X.all[1]))
    start.val.X = matrix(ncol=dims[2], nrow= chains )
    for(i in 1:chains){
      start.val.X[i,] = as.matrix(prev.run$par.X.all[i])[dims[1],]
    }
    Z.X = prev.run$Z.X
    Z.W = prev.run$Z.W
    r   = prev.run$r
    Vobs = prev.run$Vobs
    kappa = prev.run$kappa[length(prev.run$kappa)]
    update.kappa = prev.run$update.kappa
    prev = prev.run$prev
    kappa.prior = prev.run$kappa.prior
    if(is.infinite(maxit)) maxit = prev.run$maxit
    vanilla = prev.run$vanilla
    par.exp = prev.run$par.exp
    collapsed.g = prev.run$collapsed.g
    ndraws.prev = prev.run$ndraws
    if(is.null(ndraws.update)) ndraws = prev.run$ndraws else ndraws = ndraws.update
    if(is.null(prop.sd.X)){ prop.sd.X =  prev.run$prop.sd.X }
    beta.prior.X = prev.run$priors$beta.prior.X
    sig.prior.X = prev.run$priors$sig.prior.X
    thining = prev.run$thining
    X.prev = prev.run$X
    C.prev = prev.run$C
    dist.X = prev.run$dist.X
    fix.sigma.X = prev.run$fix.sigma.X
    beta.prior = prev.run$beta.prior
    kappa.prior = prev.run$kappa.prior
    fix.k = prev.run$fix.k
    prev.runtime = prev.run$runtime
    message('Updating previous MCMC run. \n')
  }
  
  if(update.kappa & !is.null(kappa.prior)){
    f = find.ab(m=kappa.prior[1], s=kappa.prior[2])
    kappa.ab = c(f$a,f$b)
    if(f$conv == 1 | f$value>0.01) stop('Did not find hyperparameters for pi(kappa|a,b). Specify different kappa.prior.')
  }
  if(update.kappa & is.null(kappa.prior)) kappa.ab = c(1,1)
  
  # Prepare data
  g.fixed = sapply(Vobs, function(x) (length(x) == 1))
  for(i in 1:length(Vobs)) if(g.fixed[i]) Vobs[[i]] = c(0, Inf)
  
  burnin=round(ndraws/2)
  pobs = P_vobs (Vobs, kappa = kappa)
  
  d = sapply(Vobs, function(x) as.numeric(is.finite(x[length(x)]) )+1)
  L = sapply(Vobs, function(x) x[length(x)-1])
  R = sapply(Vobs, function(x) x[length(x)])
  
  pobs_vec = unlist(pobs)
  Vobs_L = unlist( lapply(Vobs, function(x) x[1:(length(x)-1)] ) )
  Vobs_R = unlist( lapply(Vobs, function(x) x[2:(length(x))] ) )
  m   = sapply(Vobs, length)-1
  
  #
  
  if(is.null(d) | is.null(L) |is.null(R)){
    stop("Provide data.")
  }
  if(is.null(prop.sd.X)  ){
    stop("Provide proposal SD")
  }
  
  if(!is.null(Z.X)) {
    Z.X = as.matrix(Z.X)
    if( is.null(colnames(Z.X)) ) colnames(Z.X) = paste('ZX.',1:ncol(Z.X))
    p.X = ncol(Z.X)
    Z1.X = cbind(1,Z.X)
  }
  if(is.null(Z.X)){
    p.X = 0
    Z1.X  = matrix(1, ncol=1, nrow=length(d))
  }
  
  if(!is.null(Z.W)) {
    Z.W = as.matrix(Z.W)
    if( is.null(colnames(Z.W)) ) colnames(Z.W) = paste('ZW.',1:ncol(Z.W))
    Z1.W = cbind(1,Z.W)
  }
  if(is.null(Z.W)){
    Z1.W  = matrix(1, ncol=1, nrow=length(d))
  }
  
  p1.X = ncol(Z1.X)
  p1.W = ncol(Z1.W)
  
  sig_inv = solve(t(Z1.W) %*% Z1.W + diag(rep(tau.w^-1,ncol(Z1.W)) ))
  sig_inv_Xt = sig_inv %*% t(Z1.W)
  
  if(!vanilla & is.null(prev.run)){
    adapted = 2
    message('Searching starting values by one naive run \n')
    done = FALSE
    while(!done){
      mod.simple = bayes.2S_seq(Vobs[!g.fixed], kappa, Z.X = Z.X[!g.fixed,], r=r[!g.fixed], parallel = TRUE,
                                ndraws=ndraws.naive, chains=3, thining=1, prop.sd.X=naive.run.prop.sd.X,
                                beta.prior.X = beta.prior.X, sig.prior.X = sig.prior.X, fix.sigma.X = fix.sigma.X,
                                prev.run = NULL, dist.X = dist.X, update.burnin = TRUE,
                                beta.prior = 't', vanilla = TRUE)
      pars.simple = as.matrix(mod.simple$par.X.bi)
      ini = pars.simple[sample(1: nrow(pars.simple), chains, replace=FALSE),]
      ac.rates = apply(mod.simple$ac.X,2, mean)
      if( sum(ac.rates > .85) >0) {
        message('Naive run acceptance rates >0.85. Increasing naive.run.proposal.sd.X.\n')
        naive.run.prop.sd.X = naive.run.prop.sd.X*adapted
        adapted = adapted + 1
      }
      if( sum(ac.rates < .15) >0) {
        message('Naive run acceptance rates <0.15. Decreasing naive.run.proposal.sd.X.\n')
        naive.run.prop.sd.X = naive.run.prop.sd.X/adapted
        adapted = adapted + 1
      }
      if(adapted>20) stop('Cannot find naive.run.prop.sd.X that produces good acceptance rate.')
      if(sum(ac.rates > .85) == 0 & sum(ac.rates < .15) == 0) done = TRUE
    }
    message('Now doing main run. \n')
  }
  
  ## Start run
  run = list()  
  for(j in 1:chains){

    # Begin Gibbs
    ac.X = ac.S = 1
    
    # Init
    if(!vanilla & is.null(prev.run)){
      beta.X.ini  = ini[j, 2:(1+p.X)]
      mu.X.ini    = ini[j, 1]
      sigma.X.ini = ini[j, ncol(ini)]
      beta_w.ini  = runif(p1.W,-1,1)
      beta_w      = matrix(NA, nrow = ndraws +1, ncol = p1.W)
      beta_w[1,]  = beta_w.ini
      C.aug       = rep(0, length(L))
      C.aug[g.fixed] = 1
      if(update.kappa) kappa  = runif(1, 0.2,0.8)
      if(dist.X == 'gengamma') log.k.ini = runif(1, -2, 2)
    } else{
      beta.X.ini  = runif(p.X, -1,1)
      mu.X.ini    = runif(1, -1,1)
      sigma.X.ini = runif(1, -2,2)
      if(dist.X == 'gengamma') log.k.ini = runif(1, -2, 2)
    }
    
    if(fix.sigma.X) sigma.X.ini = log(sig.prior.X)
    if(fix.k) log.k.ini = log(k.prior)
    
    if(!is.null(prev.run)){
      mu.X.ini = start.val.X[j,1]
      if(p.X > 0) beta.X.ini = start.val.X[j,2:(p.X+1)] else beta.X.ini = NULL
      sigma.X.ini = log(start.val.X[j,(p.X+2)])
      
      if(!vanilla){
        beta_w      = matrix(NA, nrow = ndraws +1, ncol = p1.W)
        beta_w.ini = start.val.X[j,(p.X+3):(p.X+p1.W+2)]
        beta_w[1,]  = beta_w.ini
      }
      
      n.prev = length(Vobs)
      X = X.prev[ ((n.prev*(j-1))+1): (n.prev*j) ]
      if(!vanilla){
        C.aug = C.prev[ ((n.prev*(j-1))+1): (n.prev*j) ]
      }
      if(update.kappa) kappa = start.val.X[j,ncol(start.val.X)]
    }
    
    if(dist.X != 'gengamma'){
      cur.par.Xreg = matrix(ncol=p.X+2, nrow=ndraws+1)
      cur.par.Xreg[1,1:(p.X+1)] = c(mu.X.ini,beta.X.ini)
      cur.par.Xreg[1,(p.X+2)]   = sigma.X.ini
    }    else{
      cur.par.Xreg = matrix(ncol=p.X+3, nrow=ndraws+1)
      cur.par.Xreg[1,1:(p.X+1)] = c(mu.X.ini,beta.X.ini)
      cur.par.Xreg[1,(p.X+2)]   = sigma.X.ini
      cur.par.Xreg[1,(p.X+3)]   = log.k.ini
    }
    
    if(is.null(prev.run)){
      X   = pst.X.2S( cbind(rep(.1,length(d)),NA) , rgamma(length(d),.1), d = d, L= L, R= R, dist = "exp")
      if(dist.X == 'weibull' | dist.X == 'loglog') cur.par.X = trans.par(Z1.X, par = cur.par.Xreg[1,])
      if(dist.X == 'lognormal') cur.par.X = trans.par.ind.norm(Z1 = Z1.X, p = cur.par.Xreg[1,1:p1.X], v= cur.par.Xreg[1,(p1.X+1)])
      if(dist.X == 'gengamma')  cur.par.X = trans.par.gengamma(Z1 = Z1.X, par = cur.par.Xreg[1,])
      X   = pst.X.2S(par=cur.par.X, d = d, L= L, R= R, dist = dist.X)
    }
    
    i=1
    log.pst.X  = pst.aft(par=cur.par.Xreg[i,, drop=FALSE], t=X, Z=Z1.X, tau= beta.prior.X, sig.prior=sig.prior.X,
                         k.prior = k.prior,
                         dist=dist.X, beta.prior = beta.prior)
    if(is.infinite(log.pst.X) ) stop('Bad starting values')
    
    
    prop.sd.X.mat = diag(prop.sd.X^2,p1.X+1)
    if(dist.X == 'gengamma') prop.sd.X.mat = diag(prop.sd.X^2,p1.X+2)
    
    for(i in 1:ndraws){
      # if (i %% 100 == 0) {
      #   message("Completed", i, "of", n, "iterations\n")
      # }
      #Update  X parameters
      # if(i == burnin) {
      #   S = cov(cur.par.Xreg[(burnin-101):(burnin-1),])
      #   prop.sd.X.mat = 2.4^2/(nrow(Z1.X)+p1.X+1) * S #+ 0.0001 * diag(1, p1.X+1)
      # }
      mh  = mhstep.aft(x=cur.par.Xreg[i,, drop=FALSE], t=X, Z=Z1.X, tau= beta.prior.X, sig.prior=sig.prior.X, k.prior = k.prior,
                       prop.var=prop.sd.X.mat, dist=dist.X, fix.sigma=fix.sigma.X, fix.k = fix.k)
      cur.par.Xreg[i+1,] = mh$s
      
      # Assess acceptance for MH steps
      ac.X[i+1] = cur.par.Xreg[i+1,1] != cur.par.Xreg[i,1]
      
      # parameter transform for augmenting X
      if(dist.X == 'weibull' | dist.X == 'loglog') cur.par.X = trans.par(Z1.X, par = cur.par.Xreg[(i+1),])
      if(dist.X == 'lognormal') cur.par.X = trans.par.ind.norm(Z1 = Z1.X, p = cur.par.Xreg[(i+1),1:p1.X], v= cur.par.Xreg[(i+1),(p1.X+1)])
      if(dist.X == 'gengamma')  cur.par.X = trans.par.gengamma(Z1 = Z1.X, par = cur.par.Xreg[(i+1),])
      
      if(!vanilla & prev){
        # Update pobs
        if(update.kappa) pobs = P_vobs_Rcpp(Vobs, kappa = kappa[i])
        
        # Calculate theta1
        mu_w    = as.numeric(Z1.W %*% as.matrix(as.numeric(beta_w[i,])))
        theta1  = pnorm( mu_w )
        
        # Update C
        if(i>1){
          if(!collapsed.g) C.aug   = augment_C_Rcpp(pobs, Vobs, X, kappa = kappa[i], theta1 = theta1,
                                                    r= r, g_fixed = g.fixed)
          else  C.aug   = augment_C_collapsed_rcpp(w_sums = aug.X$sums, Vobs, kappa = kappa[i], theta1 = theta1, r= r, g_fixed = g.fixed)
        }
        
        # Update W
        W.aug   = augment.W(g = C.aug, mu_w )
        
        # Update beta_w
        if(par.exp){
          alpha_sq     = fc_w_par.exp_Haar(y = W.aug, X = Z1.W, sig_inv_Xt = sig_inv_Xt) #solve(t(X)X) and t(X) can be calculated outside loop
          beta_w[i+1,] = fc_beta(X = Z1.W, W.aug/sqrt(alpha_sq), sig_inv_Xt = sig_inv_Xt, sig_inv=sig_inv)
        } else{
          beta_w[i+1,] = fc_beta(X = Z1.W, W.aug, sig_inv_Xt = sig_inv_Xt, sig_inv=sig_inv)
        }
        
        # Update X
        if(dist.X %in% c('weibull','lognormal')) aug.X = augment.X_rcpp( unlist(pobs), Vobs, Vobs_L, Vobs_R, cur.par.X, dist.X, C=C.aug, collapsed.g = collapsed.g)
        else aug.X = augment.X( unlist(pobs), Vobs, Vobs_L, Vobs_R, cur.par.X, dist.X, C=C.aug, collapsed.g = collapsed.g)
        X = aug.X$X
        
        # Update kappa
        if(update.kappa) kappa[i+1] = fc_kappa_rcpp(Vobs, j_ = aug.X$k, a=kappa.ab[1], b=kappa.ab[2], g=C.aug, r = r, g_fixed= g.fixed)
        else kappa[i+1] = kappa[i]
      }
      
      if(!vanilla & !prev) {
        if(update.kappa) pobs = P_vobs (Vobs, kappa = kappa[i]) #
        aug.X = augment.X( unlist(pobs), Vobs, Vobs_L, Vobs_R, cur.par.X, dist.X, C=C.aug)
        X = aug.X$X
        if(update.kappa) kappa[i+1] = pst.kappa.noprev( Vobs, j_ = aug.X$k, a=kappa.ab[1], b=kappa.ab[2])
        else kappa[i+1] = kappa[i]
      }
      if(vanilla)  X = pst.X.2S(par=cur.par.X, d = d, L= L, R= R, dist = dist.X)
      X[X==0] = 10^-300
    }
    out=list()
    out$X = X
    if(!vanilla & prev) {
      out$par.X = cbind(cur.par.Xreg[-1,], beta_w[-1,])
      out$C.aug = C.aug
    }
    if(!vanilla & !prev) {
      out$par.X = cur.par.Xreg[-1,]
      out$C.aug = C.aug
    }
    if(vanilla)  out$par.X = cur.par.Xreg[-1,]
    out$kappa = kappa[-1]
    out$ac.X = ac.X
    run[[j]] = out
  }
  
  # Unwrap chains wihout trimming
  mcmc.par.X= list()
  i=1
  par.X = run[[i]]$par.X
  if(vanilla) {
    if(dist.X != 'gengamma'){
      colnames(par.X) = c("Intercept", colnames(Z.X), "sigma")
      par.X[,ncol(par.X)] = exp(par.X[,ncol(par.X)])
    }
    if(dist.X == 'gengamma'){
      colnames(par.X) = c("Intercept", colnames(Z.X), "sigma",'k')
      par.X[,ncol(par.X)-1] = exp(par.X[,ncol(par.X)-1])
      par.X[,ncol(par.X)] = exp(par.X[,ncol(par.X)])
    }
  }
  if(!vanilla & prev) {
    colnames(par.X) = c("Intercept X", colnames(Z.X), "sigma", "Intercept W", colnames(Z.W))
    par.X[,ncol(Z.X)+2] = exp(par.X[,ncol(Z.X)+2])
  }
  if(!vanilla & !prev) {
    colnames(par.X) = c("Intercept", colnames(Z.X), "sigma")
    par.X[,ncol(par.X)] = exp(par.X[,ncol(par.X)])
  }
  if(update.kappa) {
    par.X = cbind(par.X, run[[i]]$kappa)
    colnames(par.X)[ncol(par.X)] = 'kappa'
  }
  mcmc.par.X[[i]] = mcmc(par.X)
  ac.X = run[[i]]$ac.X
  X.draw = run[[i]]$X
  if(!vanilla) C.draw = run[[i]]$C.aug
  
  if(length(run)>1){
    for(i in 2:length(run)){
      par.X = run[[i]]$par.X
      if(vanilla) {
        if(dist.X != 'gengamma'){
          colnames(par.X) = c("Intercept", colnames(Z.X), "sigma")
          par.X[,ncol(par.X)] = exp(par.X[,ncol(par.X)])
        }
        if(dist.X == 'gengamma'){
          colnames(par.X) = c("Intercept", colnames(Z.X), "sigma",'k')
          par.X[,ncol(par.X)-1] = exp(par.X[,ncol(par.X)-1])
          par.X[,ncol(par.X)] = exp(par.X[,ncol(par.X)])
        }
      }
      if(!vanilla & prev) {
        colnames(par.X) = c("Intercept X", colnames(Z.X), "sigma", "Intercept W", colnames(Z.W))
        par.X[,ncol(Z.X)+2] = exp(par.X[,ncol(Z.X)+2])
      }
      if(!vanilla & !prev) {
        colnames(par.X) = c("Intercept", colnames(Z.X), "sigma")
        par.X[,ncol(par.X)] = exp(par.X[,ncol(par.X)])
      }
      if(update.kappa) {
        par.X = cbind(par.X, run[[i]]$kappa)
        colnames(par.X)[ncol(par.X)] = 'kappa'
      }
      mcmc.par.X[[i]] = mcmc(par.X)
      ac.X = cbind(ac.X, run[[i]]$ac.X)
      X.draw = c(X.draw, run[[i]]$X)
      if(!vanilla) C.draw = c(C.draw, run[[i]]$C.aug)
    }}
  
  par.X.all = mcmc.list(mcmc.par.X)
  
  if(!is.null(prev.run)){
    par.X.all = bind.mcmclists(prev.run$par.X.all, par.X.all)
    ac.X.cur = ac.X
    ac.X = rbind(prev.run$ac.X, ac.X)
    mc.prev  = nrow(as.matrix(prev.run$par.X.all[1]))
    if(update.burnin) burnin   = round((mc.prev + ndraws)/2)
  }
  nr       = nrow(as.matrix(par.X.all[1]))
  par.X.bi = trim.mcmc(par.X.all, burnin = burnin, thining = thining)
  
  # Undo Vobs recoding
  for(i in 1:length(Vobs)) if(g.fixed[i] == 1) Vobs[[i]] = 0
  
  
  t1 = Sys.time()
  runtime = t1-t0
  if(!is.null(prev.run)) runtime = runtime + prev.runtime
  ### save built info
  dat = data.frame(L=L, R=R)
  priors = list()
  priors$beta.prior.X = beta.prior.X
  priors$sig.prior.X = sig.prior.X
  
  
  ret = list()
  ret$par.X.all = par.X.all
  ret$par.X.bi = par.X.bi
  ret$X = X.draw
  ret$ac.X = ac.X
  if(!is.null(prev.run)){
    ret$ac.X.cur = ac.X.cur
  }
  ret$dat = dat
  ret$Vobs = Vobs
  ret$Z.X = Z.X
  ret$Z.W = Z.W
  ret$r    = r
  ret$kappa = kappa
  ret$kappa.prior = kappa.prior
  ret$maxit = maxit
  ret$prev = prev
  ret$vanilla = vanilla
  ret$par.exp = par.exp
  ret$collapsed.g = collapsed.g
  ret$update.kappa = update.kappa
  ret$priors = priors
  ret$thining = thining
  ret$prop.sd.X = prop.sd.X
  ret$dist.X = dist.X
  ret$fix.sigma.X = fix.sigma.X
  ret$burnin = burnin
  ret$beta.prior = beta.prior
  ret$runtime = runtime
  ret$ndraws = ndraws
  if(!vanilla) ret$C = C.draw
  ret$kappa.prior = kappa.prior
  ret$fix.k = fix.k
  
  if(update.till.converge){
    if(vanilla) {
      g <- gelman.diag(ret$par.X.bi[,-(p1.X+2)])
      effs <- effectiveSize( ret$par.X.bi[,-(p1.X+2)] )
    }
    else{
      if(!fix.sigma.X) g <- gelman.diag(ret$par.X.bi) else g <- gelman.diag(ret$par.X.bi[,-(p1.X+1)])
      if(!fix.sigma.X) effs <- effectiveSize( ret$par.X.bi ) else effs <- effectiveSize( ret$par.X.bi[,-(p1.X+1)] )
    }
    
    if(conv.crit == 'point') ind = 1
    if(conv.crit == 'upper') ind = 2
    cri1 = sum(g[[1]][,ind]<1.1) == length(g[[1]][,ind])
    cri2 = sum(effs > min_effss) == length(effs)
    cri3 = dim(ret$par.X.all[[1]])[1] >= maxit
    
    while(!(cri1 & cri2) & !cri3){
      message(paste('Completed', dim(ret$par.X.all[[1]])[1], 'draws. \n'))
      message(paste('Acceptance rates were:', paste(round( apply(ret$ac.X,2,mean), 2), collapse = ', '),'\n'))
      message('Not converged. \n')
      # message(paste( round(g[[1]][,ind], 2), '\n'))
      # message(paste( round(effs), '\n'))
      if(is.null(ndraws.update)) ret = bayes.2S(prev.run = ret, ndraws = ndraws)
      else ret = bayes.2S(prev.run = ret, ndraws.update = ndraws.update)
      if(vanilla) {
        g <- gelman.diag(ret$par.X.bi[,-(p1.X+2)])
        effs <- effectiveSize( ret$par.X.bi[,-(p1.X+2)] )
      }
      else{
        if(!fix.sigma.X) g <- gelman.diag(ret$par.X.bi) else g <- gelman.diag(ret$par.X.bi[,-(p1.X+1)])
        if(!fix.sigma.X) effs <- effectiveSize( ret$par.X.bi ) else effs <- effectiveSize( ret$par.X.bi[,-(p1.X+1)] )
      }
      cri1 = sum(g[[1]][,ind]<1.1) == length(g[[1]][,ind])
      cri2 = sum(effs > min_effss) == length(effs)
      cri3 = dim(ret$par.X.all[[1]])[1] >= maxit
    }
    message(paste('Completed', dim(ret$par.X.all[[1]])[1], 'draws. \n'))
    if(!cri3) message('Converged. \n') else message('Maxit reached. Not converged. \n')
    ret$convergence = !cri3
  }
  return(ret)
}
