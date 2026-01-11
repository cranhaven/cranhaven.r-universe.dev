#' Estimate the Template Spectrum
#'
#' This function uses local quadratic regression to estimate the template
#' spectrum from a collection of observed spectra from a star as described in
#' \href{https://arxiv.org/abs/2005.14083}{Holzer et al. (2020)}. All observed
#' spectra are assumed to be normalized. The bandwidth is chosen locally through
#' generalized cross-validation. We \strong{strongly} recommend using parallel
#' computing for this function. Therefore, the \code{cores} argument has the
#' default value of 19.
#'
#' @param SPECTRA a list of all observed spectra to use in estimating the template.
#' Each observed spectrum should have the format of being a list with the following
#' names (or a dataframe with the following columns): ``Wavelength" and ``Flux".
#' @param min_wvl a number that indicates the minimum wavelength for the estimated
#' template
#' @param max_wvl a number that indicates the maximum wavelength for the estimated
#' template
#' @param bandwidth_bnds a vector of length 2 that gives the interval of bandwidth
#' values (in the same units as the wavelength of the spectra) to be considered in
#' the generalized cross-validation
#' @param min_count the minimum number of data points required for local regression
#' to be done on a given wavelength chunk
#' @param cores the number of cores to parallelize over (if set to 1, no
#' parallelizing is done)
#' @return a list with the following elements:
#' \item{Wavelength}{the wavelength axis of the estimated template}
#' \item{Flux}{the normalized flux of the estimated template}
#' \item{Chunk_bounds}{a list of length 2 vectors that give
#' the wavelength bounds for each chunk for which the smoothing was done on}
#' \item{Bandwidths}{the bandwidths chosen for each of the chunks}
#' \item{Std_err}{the standard errors of the estimated normalized flux that can
#' be used for prediction confidence intervals}
#' @examples data(spectra)
#' plot(spectra[[1]]$Wavelength, spectra[[1]]$Flux, col='gray', type='l')
#' for(spec in spectra){
#'  lines(spec$Wavelength, spec$Flux, col='gray')
#' }
#' tempest = estimate_template(spectra, cores = 1)
#' lines(tempest$Wavelength, tempest$Flux, col='red')
#' @export
estimate_template = function(SPECTRA, min_wvl = NULL, max_wvl = NULL,
                             bandwidth_bnds = c(0.017,0.05),
                             min_count = 100, cores=19){

  #stack all observed spectra
  if(cores > 1){
    wvl = unlist(parallel::mclapply(SPECTRA, function(spec) spec$Wavelength, mc.cores = cores))
    flx = unlist(parallel::mclapply(SPECTRA, function(spec) spec$Flux, mc.cores = cores))
  }else{
    wvl = unlist(lapply(SPECTRA, function(spec) spec$Wavelength))
    flx = unlist(lapply(SPECTRA, function(spec) spec$Flux))
  }

  srt = order(wvl)
  flx = flx[srt]
  wvl = wvl[srt]
  srt = NULL

  wvl2 = sort(SPECTRA[[1]]$Wavelength)
  pxl_spc = quantile(wvl2[2:length(wvl2)] - wvl2[1:(length(wvl2)-1)],
                     probs = c(0.25, 0.75))
  pxl_spc = pxl_spc[2] + 5*(pxl_spc[2] - pxl_spc[1])
  wvl2 = NULL

  keep = which((flx > 0) & (!is.na(flx)))
  wvl = wvl[keep]
  flx = flx[keep]
  if(!is.null(min_wvl)){
    keep = which(wvl >= min_wvl)
    wvl = wvl[keep]
    flx = flx[keep]
  }
  if(!is.null(max_wvl)){
    keep = which(wvl <= max_wvl)
    wvl = wvl[keep]
    flx = flx[keep]
  }
  keep = NULL

  #Separate the stacked spectrum into chunks to parallelize the process over
  jumps = which(wvl[2:length(wvl)] - wvl[1:(length(wvl)-1)] > pxl_spc)

  lbs = c(wvl[1], wvl[jumps+1])
  ubs = c(wvl[jumps], wvl[length(wvl)]) + rep_len(1e-8,length(jumps)+1)
  lbs2 = c()
  for(i in 1:length(lbs)){
    if(ubs[i] - lbs[i] > 8){
      brks = seq(lbs[i], ubs[i], 4)
      if(ubs[i] - brks[length(brks)] < 3){
        brks = brks[1:(length(brks)-1)]
      }
      lbs2 = c(lbs2, brks[2:length(brks)])
    }else if(ubs[i] - lbs[i] >= 6){
      lbs2 = c(lbs2, lbs[i] + (ubs[i] - lbs[i])/2)
    }
  }
  lbs = sort(c(lbs, lbs2))
  ubs = sort(c(ubs, lbs2))
  if(cores > 1){
    bnds = parallel::mclapply(1:length(lbs), function(i) c(lbs[i], ubs[i]), mc.cores = cores)
  }else{
    bnds = lapply(1:length(lbs), function(i) c(lbs[i], ubs[i]))
  }

  #function that estimates the template for a chunk
  smoothspec = function(bd){
    keep = which((wvl >= bd[1]) & (wvl < bd[2]))
    predwvl = seq(wvl[keep][1], wvl[keep][length(keep)],
                  length.out = as.integer(3*length(keep)/length(SPECTRA)))
    if(length(keep) < min_count){
      return(list(predwvl, rep_len(NA, length(predwvl))))
    }
    #use generalized cross-validation to pick an optimal bandwidth
    if(bandwidth_bnds[1] > bandwidth_bnds[2]){stop("bandwidth bounds out of order")}

    amin = length(which(wvl[keep] <= wvl[keep][1] + bandwidth_bnds[1]))/length(keep)
    amax = length(which(wvl[keep] <= wvl[keep][1] + bandwidth_bnds[2]))/length(keep)
    alphas = seq(amin, amax, length.out = 20)
    gcvs = tryCatch(locfit::gcvplot(flx[keep] ~ wvl[keep],
                            deg=2, alpha=alphas,
                            kern='gauss'), error = function(e){list(alpha = alphas, values = c(1,1,0,rep(1,length(alphas)-3)))})
    bestalpha = gcvs$alpha[which.min(gcvs$values)]
    #use the best bandwidth and estimate template
    mdl = tryCatch(locfit::locfit(flx[keep] ~ wvl[keep], deg=2,
                          alpha=bestalpha, kern='gauss'), error = function(e){rep(NA,1)})

    if(class(mdl)=="logical"){
      return(list(predwvl, rep(NA, length(predwvl)), NA))
    }else{
      bandwidth = seq(bandwidth_bnds[1], bandwidth_bnds[2], length.out = 20)[which.min(gcvs$values)]
      pmdl = predict(mdl, predwvl, band='local')
      return(list(predwvl, pmdl$fit, bandwidth, pmdl$se.fit))
    }
  }

  #Parallelize the smoothing
  if(cores>1){
    fittedflx = parallel::mclapply(bnds, smoothspec, mc.cores=19)
    wvl = unlist(parallel::mclapply(fittedflx, function(lst) lst[[1]], mc.cores = cores))
    flx = unlist(parallel::mclapply(fittedflx, function(lst) lst[[2]], mc.cores = cores))
    bdwidth = unlist(parallel::mclapply(fittedflx, function(lst) lst[[3]], mc.cores = cores))
    se = unlist(parallel::mclapply(fittedflx, function(lst) lst[[4]], mc.cores=cores))
  }else{
    fittedflx = lapply(bnds, smoothspec)
    wvl = unlist(lapply(fittedflx, function(lst) lst[[1]]))
    flx = unlist(lapply(fittedflx, function(lst) lst[[2]]))
    bdwidth = unlist(lapply(fittedflx, function(lst) lst[[3]]))
    se = unlist(lapply(fittedflx, function(lst) lst[[4]]))
  }

  keep = which(!is.na(as.numeric(wvl)))
  smoothtemp = list(Wavelength = wvl[keep], Flux = flx[keep],
                    Chunk_bounds = bnds, Bandwidths = bdwidth,
                    Std_err = se[keep])
  return(smoothtemp)
}
