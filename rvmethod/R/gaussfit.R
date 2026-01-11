#' Gaussian Function
#'
#' This function returns the unnormalized (height of 1.0) Gaussian curve with a
#' given center and spread.
#'
#' @param x the vector of values at which to evaluate the Gaussian
#' @param mu the center of the Gaussian
#' @param sigma the spread of the Gaussian (must be greater than 0)
#' @return vector of values of the Gaussian
#' @examples x = seq(-4, 4, length.out = 100)
#' y = gaussfunc(x, 0, 1)
#' plot(x, y)
#' 
#' @import stats
#'
#' @export
gaussfunc = function(x, mu, sigma){
  return(exp(-((x - mu)^2)/(2*(sigma^2))))
}

#' A Single Gaussian Absorption Feature
#'
#' This function returns a Gaussian absorption feature with continuum 1.0 and a
#' specified amplitude, center, and spread.
#'
#' @param x the vector of values at which to evaluate
#' @param a1 the amplitude of the feature
#' @param mu1 the center of the feature
#' @param sig1 the spread of the feature (must be greater than 0)
#' @return vector of values of the specified inverted Gaussian
#' @examples x = seq(5000, 5003, length.out=200)
#' y = gauss1func(x, 0.3, 5001.5, 0.1)
#' plot(x, y)
#'
#' @export
gauss1func = function(x, a1, mu1, sig1){
  return(1 - a1*gaussfunc(x, mu1, sig1))
}

#' Two Gaussian Absorption Features
#'
#' This function returns two Gaussian absorption features, both with continuum
#' 1.0 and each with a specified amplitude, center, and spread.
#'
#' @param x the vector of values at which to evaluate
#' @param a1 the amplitude of the first feature
#' @param a2 the amplitude of the second feature
#' @param mu1 the center of the first feature
#' @param mu2 the center of the second feature
#' @param sig1 the spread of the first feature (must be greater than 0)
#' @param sig2 the spread of the second feature (must be greater than 0)
#' @return vector of values of the two specified inverted Gaussians
#' @examples x = seq(5000, 5003, length.out=200)
#' y = gauss2func(x, 0.3, 0.5, 5001.5, 5002, 0.1, 0.1)
#' plot(x, y)
#'
#' @export
gauss2func = function(x, a1, a2, mu1, mu2, sig1, sig2){
  return(1 - a1*gaussfunc(x, mu1, sig1) - a2*gaussfunc(x, mu2, sig2))
}

#' Three Gaussian Absorption Features
#'
#' This function returns three Gaussian absorption features, both with continuum
#' 1.0 and each with a specified amplitude, center, and spread.
#'
#' @param x the vector of values at which to evaluate
#' @param a1 the amplitude of the first feature
#' @param a2 the amplitude of the second feature
#' @param a3 the amplitude of the third feature
#' @param mu1 the center of the first feature
#' @param mu2 the center of the second feature
#' @param mu3 the center of the third feature
#' @param sig1 the spread of the first feature (must be greater than 0)
#' @param sig2 the spread of the second feature (must be greater than 0)
#' @param sig3 the spread of the third feature (must be greater than 0)
#' @return vector of values of the three specified inverted Gaussians
#' @examples x = seq(5000, 5003, length.out=200)
#' y = gauss3func(x, 0.3, 0.5, 0.4, 5001.5, 5002, 5000.4, 0.1, 0.1, 0.13)
#' plot(x, y)
#'
#' @export
gauss3func = function(x, a1, a2, a3, mu1, mu2, mu3, sig1, sig2, sig3){
  return(1 - a1*gaussfunc(x, mu1, sig1) - a2*gaussfunc(x, mu2, sig2) - a3*gaussfunc(x, mu3, sig3))
}

#' Fit Gaussians to Three Absorption Features
#'
#' This function takes a spectrum and the wavelength bounds of three neighboring
#' absorption features and uses the functions \code{gauss1func}, \code{gauss2func}, and/or
#' \code{gauss3func} to fit Gaussians to them simultaneously. The final fit is the first
#' of the following five outcomes for which the nonlinear regression algorithm
#' converges: (i) all three Gaussians, (ii) the left two Gaussians, (iii) the
#' right two Gaussians, (iv) just the middle Gaussian, (v) the middle Gaussian
#' with an amplitude of 0. Only the fit parameters for the middle Gaussian are
#' returned.
#'
#' @param wvl the vector of wavelengths of the spectrum to fit to
#' @param flx the vector of normalized flux of the spectrum to fit to
#' @param bnds0 a vector of length 2 with the lower and upper bounds of the left absorption feature
#' @param bnds1 a vector of length 2 with the lower and upper bounds of the middle absorption feature
#' @param bnds2 a vector of length 2 with the lower and upper bounds of the right absorption feature
#' @return a list with three components:
#' \item{mu}{the fitted value of the center parameter for the middle Gaussian}
#' \item{sig}{the fitted value of the spread parameter for the middle Gaussian}
#' \item{amp}{the fitted value of the amplitude parameter for the middle Gaussian}
#' @examples x = seq(5000, 5003, length.out=200)
#' y = gauss3func(x, 0.3, 0.5, 0.4, 5001.5, 5002, 5000.4, 0.1, 0.1, 0.13)
#' y = rnorm(200, mean=y, sd=0.01)
#' plot(x, y)
#' abline(v=c(5000.8, 5001.2, 5001.75, 5002.3))
#' pars = fit3gauss(x, y, c(5000, 5000.8), c(5001.2, 5001.75), c(5001.75, 5002.3))
#' fitted = gauss1func(x, pars$amp, pars$mu, pars$sig)
#' lines(x, fitted, col=2)
#'
#' @export
fit3gauss = function(wvl, flx, bnds0, bnds1, bnds2){
  w = which((wvl >= bnds0[1]) & (wvl <= bnds0[2]))
  a1_0 = 1 - min(flx[w])
  mu1_0 = wvl[w][which.min(flx[w])]
  sig1_0 = (bnds0[2] - bnds0[1])/5
  w = which((wvl >= bnds1[1]) & (wvl <= bnds1[2]))
  a2_0 = 1 - min(flx[w])
  mu2_0 = wvl[w][which.min(flx[w])]
  sig2_0 = (bnds1[2] - bnds1[1])/5
  w = which((wvl >= bnds2[1]) & (wvl <= bnds2[2]))
  a3_0 = 1 - min(flx[w])
  mu3_0 = wvl[w][which.min(flx[w])]
  sig3_0 = (bnds2[2] - bnds2[1])/5
  w = which((wvl >= bnds0[1]) & (wvl <= bnds2[2]))

  mdl = tryCatch(nls(flx[w] ~ gauss3func(wvl[w], a1, a2, a3, mu1, mu2, mu3, sig1, sig2, sig3),
                     start = list(a1 = a1_0, a2 = a2_0, a3 = a3_0, mu1 = mu1_0, mu2 = mu2_0,
                                  mu3 = mu3_0, sig1 = sig1_0, sig2 = sig2_0, sig3 = sig3_0),
                     lower = c(0,0,0,bnds0[1],bnds1[1],bnds2[1], 0,0,0),
                     upper = c(1,1,1,bnds0[2],bnds1[2],bnds2[2],5*sig1_0, 5*sig2_0, 5*sig3_0),
                     algorithm = 'port'),
                 error = function(e1) tryCatch(nls(flx[w] ~ gauss2func(wvl[w], a1, a2, mu1, mu2, sig1, sig2),
                                                   start = list(a1 = a1_0, a2 = a2_0, mu1 = mu1_0, mu2 = mu2_0,
                                                                sig1 = sig1_0, sig2 = sig2_0),
                                                   lower = c(0,0,bnds0[1],bnds1[1],0,0),
                                                   upper = c(1,1,bnds0[2],bnds1[2],5*sig1_0, 5*sig2_0),
                                                   algorithm = 'port'),
                                               error = function(e2) tryCatch(nls(flx[w] ~ gauss2func(wvl[w], a2, a3, mu2, mu3, sig2, sig3),
                                                                                 start = list(a2 = a2_0, a3 = a3_0, mu2 = mu2_0,
                                                                                              mu3 = mu3_0, sig2 = sig2_0, sig3 = sig3_0),
                                                                                 lower = c(0,0,bnds1[1],bnds2[1],0,0),
                                                                                 upper = c(1,1,bnds1[2],bnds2[2], 5*sig2_0, 5*sig3_0),
                                                                                 algorithm = 'port'),
                                                                             error = function(e3) tryCatch(nls(flx[w] ~ gauss1func(wvl[w], a2, mu2, sig2),
                                                                                                               start = list(a2 = a2_0, mu2 = mu2_0, sig2 = sig2_0),
                                                                                                               lower = c(0,bnds1[1],0),
                                                                                                               upper = c(1,bnds1[2], 5*sig2_0),
                                                                                                               algorithm = 'port'),
                                                                                                           error = function(e4) NA))))

  if(class(mdl) == 'logical'){
    return(list(mu = 0, sig = sig2_0, amp = a2_0))
  }else if(length(unlist(strsplit(as.character(mdl$m$formula())[3], 'mu'))) == 4){
    pars = coef(mdl)
    return(list(mu = pars[5], sig = pars[8], amp = pars[2]))
  }else if(length(unlist(strsplit(as.character(mdl$m$formula())[3], 'mu'))) == 3){
    pars = coef(mdl)
    if(length(unlist(strsplit(as.character(mdl$m$formula())[3], 'mu1'))) == 2){
      return(list(mu = pars[4], sig = pars[6], amp = pars[2]))
    }else{
      return(list(mu = pars[3], sig = pars[5], amp = pars[1]))
    }
  }else{
    return(list(mu = pars[2], sig = pars[3], amp = pars[1]))
  }
}


#' Fit Gaussians to Absorption Features of a Normalized Spectrum
#'
#' This function takes a spectrum, which ideally is a high signal-to-noise template
#' spectrum estimated with the \code{estimate_template} function, and the
#' absorption features found with the \code{findabsorptionfeatures} function
#' and uses nonlinear least-squares to fit Gaussians to the features. This follows
#' the procedure described in \href{https://arxiv.org/abs/2005.14083}{Holzer et al. (2020)}.
#'
#' @param wvl vector of wavelengths of the spectrum
#' @param flx vector of normalized flux of the spectrum
#' @param ftrs a list of length 2 vectors that each give the lower and upper bounds of found absorption features. The \code{wvbounds} component of the \code{findabsorptionfeatures} function output is designed to be this.
#' @param cores the integer count of cores to parallelize over. If set to 1, no parallelization is done.
#' @param mse_max1 the maximum mean squared error required for a fit from one Gaussian to be considered a good fit for an absorption feature
#' @param mse_max2 the maximum mean squared error required for a fit of two Gaussians to be considered a good fit for an absorption feature
#' @return a list with the following components:
#' \item{parameters}{a dataframe with the wavelength bounds, fitted amplitude,
#' fitted center, fitted spread, and fit type for absorption features with a
#' good fit. A fit type of 0 indicates that the feature had a good fit of a single
#' Gaussian. A fit type of 1 indicates that the feature did not have a good fit
#' with a single Gaussian initially, but after fitting with two it did.}
#' \item{fitted}{the vector of fitted values (with the same length as the
#' \code{wvl} parameter) using only the features that produced a good fit.}
#' \item{goodfits}{a vector of the indices for which rows in the \code{ftrs}
#' parameter were well fitted with either 1 or 2 Gaussians at the end}
#' \item{mse}{a vector with the mean squared error for each of the features in
#' the \code{ftrs} parameter using the final fitted values}
#' @examples data(template)
#' ftrs = findabsorptionfeatures(template$Wavelength,
#'                               template$Flux,
#'                               pix_range = 8, gamma = 0.05,
#'                               alpha = 0.07, minlinedepth = 0.015)
#' gapp = Gaussfit(template$Wavelength, template$Flux, ftrs)
#' plot(template$Wavelength, template$Flux)
#' lines(template$Wavelength, gapp$fitted, col=2)
#'
#' @export
Gaussfit = function(wvl, flx, ftrs, cores=1,
                    mse_max1 = 0.00014, mse_max2 = 0.0001){
  if(cores > 1){
    output = parallel::mclapply(c(2:(length(ftrs$wvbounds)[1] - 1)),
                      function(k) fit3gauss(wvl, flx, ftrs$wvbounds[[k-1]],
                                            ftrs$wvbounds[[k]], ftrs$wvbounds[[k+1]]),
                      mc.cores = cores)
  }else{
    output = lapply(c(2:(length(ftrs$wvbounds)[1] - 1)),
                    function(k) fit3gauss(wvl, flx, ftrs$wvbounds[[k-1]],
                                          ftrs$wvbounds[[k]], ftrs$wvbounds[[k+1]]))
  }

  amps <- mus <- sigs <- rep(0, length(ftrs$wvbounds)[1])
  amps[2:(length(amps)-1)] = sapply(c(1:(length(amps)-2)),
                                    function(k) output[[k]]$amp)
  mus[2:(length(amps)-1)] = sapply(c(1:(length(amps)-2)),
                                   function(k) output[[k]]$mu)
  sigs[2:(length(amps)-1)] = sapply(c(1:(length(amps)-2)),
                                    function(k) output[[k]]$sig)
  output = NULL

  w = which((wvl >= ftrs$wvbounds[[length(mus)-1]][1]) &
              (wvl <= ftrs$wvbounds[[length(mus)-1]][2]))
  a1_0 = 1 - min(flx[w])
  mu1_0 = wvl[w][which.min(flx[w])]
  sig1_0 = (ftrs$wvbounds[[length(mus)-1]][2] - ftrs$wvbounds[[length(mus)-1]][1])/5
  w = which((wvl >= ftrs$wvbounds[[length(mus)]][1]) &
              (wvl <= ftrs$wvbounds[[length(mus)]][2]))
  a2_0 = 1 - min(flx[w])
  mu2_0 = wvl[w][which.min(flx[w])]
  sig2_0 = (ftrs$wvbounds[[length(mus)]][2] - ftrs$wvbounds[[length(mus)]][1])/5
  w = which((wvl >= ftrs$wvbounds[[length(mus)-1]][1]) &
              (wvl <= ftrs$wvbounds[[length(mus)]][2]))
  mdl = tryCatch(nls(flx[w] ~ gauss2func(wvl[w], a1, a2, mu1, mu2, sig1, sig2),
                     start = list(a1 = a1_0, a2 = a2_0, mu1 = mu1_0, mu2 = mu2_0,
                                  sig1 = sig1_0, sig2 = sig2_0),
                     lower = c(0,0,ftrs$wvbounds[[length(mus)-1]][1],
                               ftrs$wvbounds[[length(mus)]][1],0,0),
                     upper = c(1,1,ftrs$wvbounds[[length(mus)-1]][2],
                               ftrs$wvbounds[[length(mus)]][2],5*sig1_0, 5*sig2_0),
                     algorithm = 'port'), error = function(e) NA)
  if(class(mdl) == 'logical'){
    amps[length(amps)] = 0
    mus[length(mus)] = mu2_0
    sigs[length(sigs)] = sig2_0
  }else{
    amps[length(amps)] = coef(mdl)[2]
    mus[length(mus)] = coef(mdl)[4]
    sigs[length(sigs)] = coef(mdl)[6]
  }

  w = which((wvl >= ftrs$wvbounds[[1]][1]) &
              (wvl <= ftrs$wvbounds[[1]][2]))
  a1_0 = 1 - min(flx[w])
  mu1_0 = wvl[w][which.min(flx[w])]
  sig1_0 = (ftrs$wvbounds[[1]][2] - ftrs$wvbounds[[1]][1])/5
  w = which((wvl >= ftrs$wvbounds[[2]][1]) &
              (wvl <= ftrs$wvbounds[[2]][2]))
  a2_0 = 1 - min(flx[w])
  mu2_0 = wvl[w][which.min(flx[w])]
  sig2_0 = (ftrs$wvbounds[[2]][2] - ftrs$wvbounds[[2]][1])/5
  w = which((wvl >= ftrs$wvbounds[[1]][1]) &
              (wvl <= ftrs$wvbounds[[2]][2]))
  mdl = tryCatch(nls(flx[w] ~ gauss2func(wvl[w], a1, a2, mu1, mu2, sig1, sig2),
                     start = list(a1 = a1_0, a2 = a2_0, mu1 = mu1_0, mu2 = mu2_0,
                                  sig1 = sig1_0, sig2 = sig2_0),
                     lower = c(0,0,ftrs$wvbounds[[1]][1],
                               ftrs$wvbounds[[2]][1],0,0),
                     upper = c(1,1,ftrs$wvbounds[[1]][2],
                               ftrs$wvbounds[[2]][2],5*sig1_0, 5*sig2_0),
                     algorithm = 'port'), error = function(e) NA)
  if(class(mdl) == 'logical'){
    amps[1] = 0
    mus[1] = mu1_0
    sigs[1] = sig1_0
  }else{
    amps[1] = coef(mdl)[1]
    mus[1] = coef(mdl)[3]
    sigs[1] = coef(mdl)[5]
  }

  ftrs$Gauss_amp = amps
  ftrs$Gauss_mu = mus
  ftrs$Gauss_sig = sigs

  fitted = rep(1, length(wvl))
  for(i in 1:(length(ftrs$wvbounds))){
    w = which((wvl >= ftrs$wvbounds[[i]][1] - 2) & (wvl <= ftrs$wvbounds[[i]][1] + 2))
    fitted[w] = fitted[w] - ftrs$Gauss_amp[i]*gaussfunc(wvl[w], ftrs$Gauss_mu[i],
                                                        ftrs$Gauss_sig[i])
  }

  msefunc = function(i){
    keep = which((wvl >= ftrs$wvbounds[[i]][1]) &
                   (wvl <= ftrs$wvbounds[[i]][2]))
    return(mean((flx[keep] - fitted[keep])^2))
  }
  if(cores > 1){
    mse = unlist(parallel::mclapply(c(1:(length(ftrs$wvbounds))), 
                                    msefunc, mc.cores = cores))
  }else{
    mse = sapply(c(1:(length(ftrs$wvbounds)[1])), msefunc)
  }

  misfits = which(mse > mse_max1)
  gdfits = which(mse <= mse_max1)
  goodfitted = rep(1, length(wvl))
  for(i in gdfits){
    w = which((wvl >= ftrs$wvbounds[[i]][1] - 2) & (wvl <= ftrs$wvbounds[[i]][1] + 2))
    goodfitted[w] = goodfitted[w] - ftrs$Gauss_amp[i]*gaussfunc(wvl[w], ftrs$Gauss_mu[i],
                                                                ftrs$Gauss_sig[i])
  }

  residspec = rep(1, length(wvl)) + flx - goodfitted
  fixed = c()
  fixedftrs = data.frame(Wv_lbounds = c(NA), Wv_ubounds = c(NA),
                         Gauss_amp = c(NA), Gauss_mu = c(NA), Gauss_sig = c(NA))
  ftrs = data.frame(Wv_lbounds = sapply(c(1:length(ftrs$wvbounds)), function(i) ftrs$wvbounds[[i]][1]),
                    Wv_ubounds = sapply(c(1:length(ftrs$wvbounds)), function(i) ftrs$wvbounds[[i]][2]),
                    Gauss_amp = ftrs$Gauss_amp, Gauss_mu = ftrs$Gauss_mu,
                    Gauss_sig = ftrs$Gauss_sig)
  k=1
  while(k <= length(misfits)){
    ftr = misfits[k]
    w = which((wvl >= ftrs$Wv_lbounds[ftr]) &  (wvl <= ftrs$Wv_ubounds[ftr]))
    a2_0 = max(c(1 - min(flx[w])/2,0.00001))
    mu2_0 = wvl[w][1] + (wvl[w][length(w)] - wvl[w][1])/5
    sig2_0 = max(c((ftrs$Wv_ubounds[ftr] - ftrs$Wv_lbounds[ftr])/6,0.00001))
    a3_0 = max(c(1 - min(flx[w])/2,0.00001))
    mu3_0 = wvl[w][1] + 4*(wvl[w][length(w)] - wvl[w][1])/5
    sig3_0 = max(c((ftrs$Wv_ubounds[ftr] - ftrs$Wv_lbounds[ftr])/6,0.00001))
    mdl = tryCatch(nls(residspec[w] ~ gauss2func(wvl[w], a2, a3, mu2, mu3, sig2, sig3),
                       start = list(a2 = a2_0, a3 = a3_0, mu2 = mu2_0, mu3 = mu3_0,
                                    sig2 = sig2_0, sig3 = sig3_0),
                       lower = c(0,0,ftrs$Wv_ubounds[ftr-1],ftrs$Wv_lbounds[ftr],0,0),
                       upper = c(1,1,ftrs$Wv_ubounds[ftr],ftrs$Wv_lbounds[ftr+1],
                                 3*sig2_0+0.00001, 3*sig3_0+0.00001),
                       algorithm = 'port'), error = function(e) NA)
    if(class(mdl) == 'logical'){
      k = k+1
      next
    }
    newfit = fitted.values(mdl)
    if(mean((residspec[w] - newfit)^2) < mse_max2){
      fixed = c(fixed, ftr)
      midpoint = ftrs$Wv_lbounds[ftr] + (ftrs$Wv_ubounds[ftr] - ftrs$Wv_lbounds[ftr])/2
      pars = coef(mdl)
      tempdf = data.frame(Wv_lbounds = c(ftrs$Wv_lbounds[ftr], midpoint),
                          Wv_ubounds = c(midpoint,ftrs$Wv_ubounds[ftr]),
                          Gauss_amp = pars[1:2],
                          Gauss_mu = pars[3:4],
                          Gauss_sig = pars[5:6])
      if(dim(fixedftrs)[1] == 1){
        fixedftrs = tempdf
      }else{
        fixedftrs = rbind(fixedftrs, tempdf)
      }
      w = which((wvl >= ftrs$Wv_lbounds[ftr]-2) & (wvl <= ftrs$Wv_ubounds[ftr]+2))
      newfit = gauss2func(wvl[w], pars[1],pars[2],pars[3],pars[4],pars[5],pars[6])
      residspec[w] = rep(1, length(w)) + residspec[w] - newfit
    }
    k = k+1
  }
  remaining = setdiff(misfits, fixed)
  for(ftr in remaining){
    w = which((wvl >= ftrs$Wv_lbounds[ftr]) & (wvl <= ftrs$Wv_ubounds[ftr]))
    a2_0 = max(c(1 - min(flx[w]),0.00001))
    mu2_0 = wvl[w][1] + (wvl[w][length(w)] - wvl[w][1])/2
    sig2_0 = max(c((ftrs$Wv_ubounds[ftr] - ftrs$Wv_lbounds[ftr])/5,0.00001))
    mdl = tryCatch(nls(residspec[w] ~ gauss1func(wvl[w], a2, mu2, sig2),
                       start = list(a2 = a2_0, mu2 = mu2_0, sig2 = sig2_0),
                       lower = c(0,ftrs$Wv_lbounds[ftr],0),
                       upper = c(1,ftrs$Wv_ubounds[ftr], 5*sig3_0+0.00001),
                       algorithm = 'port'), error = function(e) NA)
    if(class(mdl) == 'logical'){
      k = k+1
      next
    }
    newfit = fitted.values(mdl)
    if(mean((residspec[w] - newfit)^2) < mse_max2){
      pars = coef(mdl)
      tempdf = data.frame(Wv_lbounds = c(ftrs$Wv_lbounds[ftr]),
                          Wv_ubounds = c(ftrs$Wv_ubounds[ftr]),
                          Gauss_amp = c(pars[1]), Gauss_mu = c(pars[2]),
                          Gauss_sig = c(pars[3]))
      fixedftrs = rbind(fixedftrs, tempdf)
      fixed = c(fixed, ftr)
      w = which((wvl >= ftrs$Wv_lbounds[ftr]-2) & (wvl <= ftrs$Wv_ubounds[ftr]+2))
      newfit = gauss1func(wvl[w], pars[1],pars[2],pars[3])
      residspec[w] = rep(1, length(w)) + residspec[w] - newfit
    }
  }

  for(i in 1:(dim(fixedftrs)[1])){
    w = which((wvl >= fixedftrs$Wv_lbounds[i]-2) & (wvl <= fixedftrs$Wv_ubounds[i]+2))
    goodfitted[w] = goodfitted[w] - fixedftrs$Gauss_amp[i]*gaussfunc(wvl[w],
                                                                     fixedftrs$Gauss_mu[i],
                                                                     fixedftrs$Gauss_sig[i])
  }

  msefunc = function(i){
    keep = which((wvl >= ftrs$Wv_lbounds[i]) &
                   (wvl <= ftrs$Wv_ubounds[i]))
    return(mean((flx[keep] - goodfitted[keep])^2))
  }
  if(cores > 1){
    mse = unlist(parallel::mclapply(c(1:(dim(ftrs)[1])), msefunc, mc.cores = cores))
  }else{
    mse = sapply(c(1:length(ftrs$Wv_lbounds)), msefunc)
  }

  fittype = rep(0, length(ftrs$Wv_lbounds))
  fittype[which(!(c(1:length(fittype)) %in% gdfits))] = 2
  ftrs$FitType = fittype
  ftrs = ftrs[ftrs$FitType == 0,]
  fixedftrs$FitType = rep(1, dim(fixedftrs)[1])
  GoodFeatures = rbind(ftrs, fixedftrs)
  rownames(GoodFeatures) = 1:(dim(GoodFeatures)[1])
  return(list(parameters = GoodFeatures, fitted = goodfitted,
              goodfits = sort(c(fixed, gdfits)), mse = mse))
}

