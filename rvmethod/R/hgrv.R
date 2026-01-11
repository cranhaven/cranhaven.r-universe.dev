#' Evaluate the First-Degree Generalized Hermite-Gaussian Function
#'
#' This function evaluates the first-degree Hermite-Gaussian function with a
#' general center and spread.
#'
#' @param x the vector of values at which to evaluate the function
#' @param mu the center parameter of the function
#' @param sig the spread parameter of the function
#' @return vector of values of the specified first-degree generalized Hermite-Gaussian function
#' @examples x = seq(50, 60, length.out=100)
#' y = HG1(x, 55, 1)
#' plot(x, y)
#'
#' @export
HG1 = function(x, mu, sig){
  return(2*((x-mu)/sig)*exp(-((x - mu)^2)/(2*sig^2))/sqrt(sig*2*sqrt(pi)))
}

#' Apply the Hermite-Gaussian Radial Velocity (HGRV) Estimation Method
#'
#' This function applies the HGRV method as given in 
#' \href{https://arxiv.org/abs/2005.14083}{Holzer et al. (2020)} to a given observed spectrum, using
#' the estimated template from the \code{estimate_template} function and the
#' \code{parameters} component of the output from the \code{Gaussfit} function.
#' The result is an estimate of the relative radial velocity present in the
#' observed spectrum in units of m/s.
#'
#' @param obs_wvl the vector of wavelengths of the observed spectrum
#' @param obs_flx the vector of normalized flux of the observed spectrum
#' @param tmp_wvl the vector of wavelengths of the template spectrum
#' @param tmp_flx the vector of normalized flux of the template spectrum
#' @param Features a dataframe with the wavelength bounds and fitted Gaussian parameters for each absorption feature. The \code{parameters} component of the output from the \code{Gaussfit} function provides this.
#' @param obs_err the vector of uncertainties in the normalized flux of the observed spectrum (must be the same length as \code{obs_wvl} and \code{obs_flx})
#' @param cntm the vector of continuum values used to normalize the flux of the observed spectrum (must be the same length as \code{obs_wvl} and \code{obs_flx})
#' @return a list with the following components
#' \item{rv}{the estimated radial velocity in units of m/s}
#' \item{rv_err}{the standard error of the estimated radial velocity in units of m/s}
#' \item{n}{the number of data points used in the weighted linear regression}
#' \item{data}{a list with the observed wavelengths (\code{wvl}), the difference
#' flux (\code{diff_flux}), the explanatory variable constructed as a sum of
#' first-degree generalized Hermite-Gaussian functions (\code{hgvar}), and the
#' weights (\code{weights}) used in the regression.}
#' @examples data(template)
#' ftrs = findabsorptionfeatures(template$Wavelength,
#'                               template$Flux,
#'                               pix_range = 8, gamma = 0.05,
#'                               alpha = 0.07, minlinedepth = 0.015)
#' gapp = Gaussfit(template$Wavelength, template$Flux, ftrs)
#' data(observed_spec)
#' hgrv_output = hgrv(observed_spec$Wavelength, observed_spec$Flux,
#'                    template$Wavelength, template$Flux, gapp$parameters,
#'                    obs_err = observed_spec$Uncertainty)
#' plot(hgrv_output$data$hgvar, hgrv_output$data$diff_flux)
#' abline(a=0, b=hgrv_output$rv)
#' abline(a=0, b=hgrv_output$rv - 3*hgrv_output$rv_err, lty=2)
#' abline(a=0, b=hgrv_output$rv + 3*hgrv_output$rv_err, lty=2)
#'
#' @export
hgrv = function(obs_wvl, obs_flx, tmp_wvl, tmp_flx, Features, obs_err = NULL,
                cntm = NULL){
  if(is.null(obs_err) & is.null(cntm)){
    obs_unc = rep(1, length(obs_wvl))
    warning("Treating noise as homoskedastic.\n To avoid, provide either 'obs_err' or 'cntm' with vector of values.")
  }else if(!is.null(obs_err)){
    obs_unc = obs_err
  }else{
    obs_unc = rep(1, length(obs_wvl))
  }
  pxlspc = tmp_wvl[2:length(tmp_wvl)] - tmp_wvl[1:(length(tmp_wvl)-1)]
  jmps = which(pxlspc > quantile(pxlspc, 0.75) + 12*IQR(pxlspc))
  lbnds = c(tmp_wvl[1], tmp_wvl[jmps])
  ubnds = c(tmp_wvl[jmps+1], tmp_wvl[length(tmp_wvl)])
  owvl <- oflx <- ounc <- ocntm <- c()
  for(i in 1:length(lbnds)){
    w = which((obs_wvl >= lbnds[i]) & (obs_wvl <= ubnds[i]) & !is.na(obs_flx) &
                !is.na(obs_unc))
    owvl = c(owvl, obs_wvl[w])
    oflx = c(oflx, obs_flx[w])
    ounc = c(ounc, obs_unc[w])
    if(!is.null(cntm)){
      ocntm = c(ocntm, cntm[w])
    }
  }
  keep = unique(unlist(lapply(c(1:length(Features$Wv_lbounds)),
                              function(i) which((owvl >= Features$Wv_lbounds[i]) &
                                                  (owvl <= Features$Wv_ubounds[i])))))
  owvl = owvl[keep]
  oflx = oflx[keep]
  ounc = ounc[keep]
  if(!is.null(cntm)){
    ocntm = ocntm[keep]
  }

  w = which(!is.na(tmp_flx))
  tflx = wave_match(tmp_wvl[w], tmp_flx[w], owvl)
  assertthat::are_equal(length(tflx), length(oflx))

  if(is.null(obs_err) & !is.null(cntm)){
    ounc = sqrt(tflx/ocntm)
  }

  hgflux = rep(0, length(owvl))
  mFeatures = Features[(Features$Wv_lbounds >= owvl[1]) &
                         (Features$Wv_ubounds <= owvl[length(owvl)]),]
  for(i in 1:length(mFeatures$Wv_lbounds)){
    #only update the values corresponding to wavelengths within 2 angstroms of the wavelength bounds
    w = which((owvl >= mFeatures$Wv_lbounds[i] - 2) &
                (owvl <= mFeatures$Wv_ubounds[i] + 2))
    coef = sqrt(sqrt(pi))*mFeatures$Gauss_amp[i]*mFeatures$Gauss_mu[i]/(299792458*sqrt(2*mFeatures$Gauss_sig[i]))
    hgflux[w] = hgflux[w] + coef*HG1(owvl[w], mFeatures$Gauss_mu[i], mFeatures$Gauss_sig[i])
  }

  dflx = tflx - oflx
  upper = quantile(dflx, 0.75) + 5*IQR(dflx)
  lower = quantile(dflx, 0.25) - 5*IQR(dflx)
  if(sum(((dflx < lower) | (dflx > upper)) & (abs(hgflux) > sd(hgflux))) > 0){
    warning("Regression may be influenced by large outliers.")
  }
  if(sum(abs(hgflux) > 10*sd(hgflux)) > 0){
    warning("Regression may be influenced by high leverage points.")
  }

  wdiff = dflx/ounc
  whgflux = hgflux/ounc
  rv_hat = sum(wdiff*whgflux)/sum(whgflux^2)
  resid = wdiff - rv_hat*whgflux
  var_hat = sum(resid*resid)/(length(resid)-1)
  rv_hat_sd = sqrt(var_hat/sum(whgflux^2))
  return(list(rv = rv_hat, rv_err = rv_hat_sd, n = length(wdiff),
              data = list(wvl = owvl, diff_flux = dflx, hgvar = hgflux,
                          weights = 1/ounc^2)))
}
