#' Compute forecasts based on posterior samples
#'
#' Predict response at new timepoints by drawing samples of the response from
#' the posterior predictive distribution.  Since this requires sampling 
#' teleconnection effects, this method can return estimates of the 
#' teleconnection effects as a by-product.
#'
#'
#' @export
#' 
#' @importFrom coda mcmc HPDinterval
#' @importFrom itertools ichunk
#' @import foreach
#' @importFrom fields rdist.earth
#' @importFrom stats quantile
#' 
#' @useDynLib telefit, .registration = TRUE
#' 
#' 
#' @param stFit Object with class 'stFit' containing posterior parameter samples
#'  needed to composition sample the teleconnection effects and generate 
#'  posterior predictions. The data needed from stFit need only be manually 
#'  entered if not using a stData object.
#' @param stData Object with class 'stData' containing data needed to fit this 
#'  model. The data need only be manually entered if not using a stData object.
#' @param stDataNew object of class stData that includes information needed for 
#'  making forecasts.  If response data is included, this function will 
#'  automatically run stEval using the empirical climatology as the reference
#'  forecast
#' @param burn number of posterior samples to burn before drawing composition
#'  samples
#' @param prob confidence level for approximate confidence intervals of 
#'  teleconnection effects (only needed if returnAlphas==TRUE)
#' @param ncores Since the teleconnection effects and posterior predictions can 
#'  be sampled in parallel, this parameter lets users specify the number of 
#'  cores to use to draw teleconnection and prediction samples
#' @param returnAlphas TRUE to return the teleconnection effects sampled 
#'  at knot locations.  Note that only basic summary information about the 
#'  teleconnection effects will be returned.
#' @param returnFullAlphas TRUE to return the teleconnection effects.  
#'  Note that only basic summary information about the 
#'  teleconnection effects will be returned.
#' @param conf Parameter specifying the HPD level to compute for posterior 
#'  predictive samples
#' @param tLabs Forecast timepoint labels
#' @param X [ns, p, nt] array of design matrices with local covariates
#' @param Y [ns, nt] matrix with response data
#' @param Z [nr, nt] matrix with remote covariates
#' @param Xnew [ns, p, nt0] array of design matrices with local covariates 
#'  at forecast timepoints
#' @param Znew [nr, nt0] matrix with remote covariates at forecast timepoints
#' @param coords.s matrix with coordinates where responses were 
#'  observed (lon, lat)
#' @param coords.r matrix with coordinates where remote covariates
#'  were observed (lon, lat)
#' @param cat.probs vector of probabilities for also returning categorical 
#'  predictions from the posterior prediction samples; NULL otherwise
#'  
#' @example examples/stPredict.R
  
stPredict = function( stFit, stData, stDataNew, burn = 1, prob = .95, 
                      ncores = 1, conf = .95, tLabs = stDataNew$tLabs,
                      X = stData$X, Y = stData$Y, Z = stData$Z, 
                      Xnew = stDataNew$X, Znew = stDataNew$Z,
                      coords.s = stData$coords.s, coords.r = stData$coords.r,
                      returnAlphas = T, cat.probs = c(1/3, 2/3),
                      returnFullAlphas = F) {
  
  # extract some configurations
  localOnly = stFit$localOnly
  remoteOnly = stFit$remoteOnly
  varying = stFit$varying
  miles = stFit$miles
  
  maxIt = length(stFit$parameters$samples$ll)
  nSamples = length(burn:maxIt)
  
  n = nrow(coords.s)
  r = nrow(coords.r)
  r_knots = nrow(stFit$coords.knots)
  p = dim(X)[2]
  t = dim(X)[3]
  
  Dy = rdist.earth(coords.s, miles=miles)
  Dz_knots = rdist.earth(stFit$coords.knots, miles=miles)
  Dz_to_knots = rdist.earth(coords.r, stFit$coords.knots, miles=miles)
  
  Z = as.matrix(Z)
  Znew = as.matrix(Znew)
  
  # format data
  Yl = matrix(as.numeric(Y), ncol=1)
  
  # format design matrix
  if(remoteOnly) {
    # remoteOnly => intercept only model
    
    Xl = matrix(1, nrow = nrow(Yl), ncol = 1)
    p = 1
    priors$beta$Lambda = matrix(priors$beta$Lambda[1,1], ncol=1, nrow=1)
    
    Xlnew = as.matrix(arrayToLong(Xnew, coords.s, 1)[,-(1:3)])
    Xlnew = matrix(1, nrow(Xlnew), ncol=1)
  } else {
    Xl = as.matrix(arrayToLong(X, coords.s, 1)[,-(1:3)])
    
    Xlnew = as.matrix(arrayToLong(Xnew, coords.s, 1)[,-(1:3)])
  }
  
  # compute (remote) eofs
  eof = prcomp(stData$Z, center = F)
  W = -eof$x
  Tmat = -eof$rotation
  
  # normalize eofs
  # sc = apply(W, 2, function(x){sqrt(t(x)%*%x)})
  # W = sweep(W, 2, sc, '/')
  # Tmat = sweep(Tmat, 2, sc, '*')
  
  # compute empirical breakpoints at each location to define forecast categories
  if(!is.null(cat.probs)) {
    category.breaks = t(apply(stData$Y, 1,
                              function(r) { quantile(r, probs = cat.probs)}))
  }
  
  # check for parallel backend
  if(!getDoParRegistered()) {
    warning('No parallel backend registered.')
  }
  
  # make looping more efficient
  mcoptions = list(preschedule=FALSE)
  
  # estimate chunksize that will minimize number of function calls
  chunkSize = ceiling((maxIt-burn+1)/ncores)
  
  # draw composition samples
  composition = foreach( inds = ichunk(burn:maxIt, chunkSize = chunkSize),
                         .combine = mergeComposition,
                         .options.multicore=mcoptions, 
                         .export = c('Xl', 'Z', 'Yl', 'Dy', 'Dz_knots', 
                          'Dz_to_knots', 'p', 'n', 'r', 'r_knots', 't',  'stFit', 
                          'Xlnew', 'Znew', 'localOnly', 'returnFullAlphas', 
                          'W', 'category.breaks') ) %dopar% {
    inds = unlist(inds)            
                           
    if(stFit$varying) {
    } else {
      .Call(`r_stpcomposition`, matrix(Xl, ncol=p), Z, Yl, 
            Dy, Dz_knots, Dz_to_knots, p, n, r, r_knots, t, 
            stFit$priors$cov.s$smoothness, stFit$priors$cov.r$smoothness,
            matrix(stFit$parameters$samples$beta[inds,], ncol=p), 
            stFit$parameters$samples$sigmasq_y[inds],
            stFit$parameters$samples$sigmasq_r[inds], 
            stFit$parameters$samples$sigmasq_eps[inds],
            stFit$parameters$samples$rho_y[inds], 
            stFit$parameters$samples$rho_r[inds],
            stFit$parameters$samples$ll[inds],
            Xlnew, Znew, localOnly, returnFullAlphas,
            stFit$parameters$samples$sigmasq_r_eps[inds], W, 
            matrix(category.breaks, nrow = nrow(coords.s)))
    }
  }
  
  # remove unwanted information
  attr(composition, 'rng') = NULL
  
  # post process teleconnection effects
  if(!localOnly) {
    if(returnAlphas) {
      
      # convert results away from unnecessary matrix format
      composition$alpha_knots$est = as.numeric(composition$alpha_knots$est)
      composition$alpha_knots$sd = as.numeric(composition$alpha_knots$sd)
      composition$eof_alpha_knots$est = as.numeric(composition$eof_alpha_knots$est)
      composition$eof_alpha_knots$sd = as.numeric(composition$eof_alpha_knots$sd)
      composition$eof_alpha_knots$posProb = as.numeric(composition$eof_alpha_knots$posProb)
      composition$eof_alpha_knots$negProb = as.numeric(composition$eof_alpha_knots$negProb)
      
      # compute approximate intervals, etc.
      composition$alpha_knots$summary = summariseAlpha(composition$alpha_knots, 
                                                       prob, coords.s, 
                                                       stFit$coords.knots)
      composition$eof_alpha_knots$summary = 
        summariseEOFAlpha(composition$eof_alpha_knots, prob, coords.s)
      
      # remove information redundant with the summary
      composition$alpha_knots$est = NULL
      composition$alpha_knots$sd = NULL
      composition$eof_alpha_knots$est = NULL
      composition$eof_alpha_knots$sd = NULL
      composition$eof_alpha_knots$negProb = NULL
      composition$eof_alpha_knots$posProb = NULL
    }
    
    if(returnFullAlphas) {
      # convert results away from unnecessary matrix format
      composition$alpha$est = as.numeric(composition$alpha$est)
      composition$alpha$sd = as.numeric(composition$alpha$sd)
      
      # compute approximate intervals, etc.
      composition$alpha$summary = summariseAlpha(composition$alpha, 
                                                       prob, coords.s, 
                                                       coords.r)
      
      # remove information redundant with the summary
      composition$alpha$est = NULL
      composition$alpha$sd = NULL
    }
  }
  
                         
  # package results (exactly split task across ncores)
  nt0 = ncol(composition$forecast$forecast)
  chunkSize = ceiling(nt0/ncores)
  Y = foreach(inds = ichunk(1:nt0, chunkSize = chunkSize), .combine='c',
              .export = c('cat.probs', 'category.breaks', 'composition', 
                          'tLabs')) %dopar% {
      foreach(t = unlist(inds)) %do% {

    # TODO: move this code somewhere where it can be called outside of sampling

    # generate HPD intervals
    forecast.mcmc = mcmc(t(composition$forecast$forecast[,t,]))
    forecast.hpd = HPDinterval(forecast.mcmc, prob = conf)

    if(!is.null(cat.probs)) {
      # build categorical predictive distribution (process by location)
      pred.cat = foreach(s = 1:nrow(composition$forecast$forecast),
                           .combine='rbind') %do% {
        # extract posterior samples for specified location and timepoint
        y = composition$forecast$forecast[s,t,]
        # return posterior probabilities of categories
        tabulate(findInterval(y,category.breaks[s,]) + 1, 
                 nbins = ncol(category.breaks) + 1)/length(y)
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
    
    if(!is.null(composition$forecast$local)) {
      pred$Y.local = colMeans(t(composition$forecast$local[,t,]))
      pred$Y.remote = colMeans(t(composition$forecast$remote[,t,]))
    }
    
    r = list(
      pred = pred,
      pred.cat = pred.cat,
      yrLab = tLabs[t]
    )
    r
  }}

  attr(composition$forecast$forecast, 'dimnames') = NULL
  if(!localOnly) {
    attr(composition$forecast$local, 'dimnames') = NULL
    attr(composition$forecast$remote, 'dimnames') = NULL
  }

  # format return
  ret = list(
    pred = Y,
    samples = composition$forecast,
    coords.s = coords.s,
    localOnly = localOnly,
    varying = varying,
    tLabs = tLabs,
    Y.lab = stData$Y.lab,
    cat.probs = cat.probs,
    category.breaks = category.breaks
  )
  if(!localOnly) {
    ret$alpha = composition$alpha$summary
    ret$alpha_knots = composition$alpha_knots$summary
    ret$alpha_knots_cov = composition$alpha_knots$cov
    ret$eof_alpha_knots = composition$eof_alpha_knots$summary
  }
  class(ret) = 'stPredict'
  
  # evaluate performance if response data is given
  if(!is.null(stDataNew$Y)) {
    if(is.null(ncol(stDataNew$Y)))
      ret = stEval(ret, stDataNew$Y, stDataNew$Y)
    else
      ret = stEval(ret, stDataNew$Y, rowMeans(stDataNew$Y))
  }
  
  ret
}