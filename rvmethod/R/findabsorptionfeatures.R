#' Find Absorption Features in a Spectrum
#'
#' This function applies the Absorption Feature Finder algorithm (Algorithm 1 in
#' \href{https://arxiv.org/abs/2005.14083}{Holzer et. al 2020}) to find absorption 
#' features in a high signal-to-noise,
#' normalized, spectrum. For a spectrum that covers more than 100 Angstroms, it is
#' recommended to parallelize it by setting the \code{cores} argument to be greater
#' than 1.
#'
#' @param wvl vector of wavelengths in the spectrum
#' @param flux vector of normalized flux in the spectrum (must have the same length as \code{wvl})
#' @param pix_range integer that specifies the window size in units of pixels to use in the moving linear regression
#' @param gamma significance level used in finding local minima
#' @param alpha significance level used in estimating wavelength bounds of features (\strong{Note:} this must be larger than \code{gamma})
#' @param minlinedepth minimum depth required for found absorption features to be returned
#' @param cores number of cores to parallelize over (if set to 1, no parallelizing is done)
#' @return a list with the following components:
#' \item{wvbounds}{a list of length 2 vectors that each give the lower and upper bounds of found absorption features}
#' \item{min_wvl}{a vector of the wavelengths at which the minimum flux is achieved for each found absorption feature}
#' \item{min_flx}{a vector of the minimum flux for each found absorption feature}
#' \item{max_flx}{a vector of the maximum flux for each found absorption feature}
#' @examples
#' data(template)
#' ftrs = findabsorptionfeatures(template$Wavelength,
#'                               template$Flux,
#'                               pix_range = 8, gamma = 0.05,
#'                               alpha = 0.07, minlinedepth = 0.015)
#' plot(template$Wavelength, template$Flux,
#'      type='l', xlab = "Wavelength", ylab = "Flux")
#' for(i in 1:length(ftrs$wvbounds)){
#'   lines(ftrs$wvbounds[[i]],
#'         c(1,1) - 0.01*rep(i%%2,2), col=3)
#' }
#' @export
findabsorptionfeatures = function(wvl, flux, pix_range = 7, gamma = 0.01,
                                  alpha = 0.05, minlinedepth=0, cores = 1){

  #Sort the spectrum
  srt = order(wvl)
  wvl = wvl[srt]
  flux = flux[srt]

  #Add zeros on both ends to prevent a common error from occurring
  flux = c(rep(0,pix_range), flux, rep(0,pix_range))
  for(m in 1:pix_range){
    #wvl = c(wvl[1] - (m+1)*(wvl[2]-wvl[1]), wvl,
    #        wvl[length(wvl)] + (m+1)*(wvl[length(wvl)]-wvl[length(wvl)-1]))
    wvl = c(wvl[1] - (wvl[2]-wvl[1]), wvl,
            wvl[length(wvl)] + (wvl[length(wvl)]-wvl[length(wvl)-1]))
  }

  #Find local minima
  minwvs = c()
  minfluxs = c()
  for(i in (pix_range+1):(length(wvl) - pix_range)){
    minimum = flux[i] < flux[i-1] & flux[i] < flux[i+1]
    if(!minimum){
      next
    }
    for(j in 0:pix_range){
      minimum = minimum & !((i+j) %in% minwvs) & !((i-j) %in% minwvs)
      if(!minimum){break}
    }


    if(!minimum){next}

    left = lm(flux[(i-pix_range):(i-1)] ~ wvl[(i-pix_range):(i-1)])
    right = lm(flux[i:(i+pix_range-1)] ~ wvl[i:(i+pix_range-1)])
    minimum = minimum & as.numeric(left$coefficients[2]) < 0 &
      summary(left)$coefficients[2,4] < gamma & as.numeric(right$coefficients[2]) > 0 &
      summary(right)$coefficients[2,4] < gamma
    if(minimum){
      minwvs = c(minwvs, i)
      minfluxs = c(minfluxs, flux[i])
    }
  }

  #Find the absorption feature bounds
  findbounds = function(i){
    feature = T
    j = 0
    while(feature){
      ml = lm(flux[(i-pix_range-j):(i-j)] ~ wvl[(i-pix_range-j):(i-j)])
      if(as.numeric(ml$coefficients[2]) > 0 | summary(ml)$coefficients[2,4] > alpha){
        if(pix_range%%2 == 0){
          lowerbound = wvl[as.integer(i - j - pix_range/2)]
          mfl = flux[as.integer(i - j - pix_range/2)]
        }else{
          lowerbound = wvl[as.integer(i - j - pix_range/2 +0.5)]
          mfl = flux[as.integer(i - j - pix_range/2 + 0.5)]
        }
        feature = F
      }
      j = j+1
    }
    feature = T
    j=0
    while(feature){
      ml = lm(flux[(i+j):(i+pix_range+j)] ~ wvl[(i+j):(i+pix_range+j)])
      if(as.numeric(ml$coefficients[2]) < 0 | summary(ml)$coefficients[2,4] > alpha){
        if(pix_range%%2 == 0){
          upperbound = wvl[as.integer(i + j + pix_range/2)]
          mfu = flux[as.integer(i + j + pix_range/2)]
        }else{
          upperbound = wvl[as.integer(i + j + pix_range/2 -0.5)]
          mfu = flux[as.integer(i + j + pix_range/2 - 0.5)]
        }
        feature = F
      }
      j = j+1
    }
    return(list(wvbnds = c(lowerbound,upperbound), mxflx = max(c(mfl, mfu))))
  }

  if(cores > 1){
    tmpoutput = parallel::mclapply(minwvs, findbounds, mc.cores = cores)
    wvlbounds = parallel::mclapply(tmpoutput, function(lst) lst$wvbnds, mc.cores=cores)
    maxfluxs = parallel::mclapply(tmpoutput, function(lst) lst$mxflx, mc.cores=cores)
  }else{
    tmpoutput = lapply(minwvs, findbounds)
    wvlbounds = lapply(tmpoutput, function(lst) lst$wvbnds)
    maxfluxs = lapply(tmpoutput, function(lst) lst$mxflx)
  }


  #remove features that are below a minimum line depth
  keep = which(unlist(maxfluxs) -  minfluxs >= minlinedepth)
  return(list(wvbounds = wvlbounds[keep], min_wvl = wvl[minwvs][keep],
              min_flx = minfluxs[keep], max_flx = unlist(maxfluxs)))
}
