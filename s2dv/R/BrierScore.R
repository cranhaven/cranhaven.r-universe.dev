#'Compute Brier score, its decomposition, and Brier skill score
#'
#'Compute the Brier score (BS) and the components of its standard decompostion
#'with the two within-bin components described in Stephenson et al., (2008). It
#'also returns the bias-corrected decomposition of the BS (Ferro and Fricker, 
#'2012). BSS has the climatology as the reference forecast. 
#'
#'@param exp A vector or a numeric array with named dimensions. It should be 
#'  the predicted probabilities which are within the range [0, 1] if memb_dim
#'  doesn't exist. If it has memb_dim, the value should be 0 or 1, and the 
#'  predicted probabilities will be computed by ensemble mean. The dimensions 
#'  must at least have 'time_dim'. 
#'  range [0, 1].
#'@param obs A numeric array with named dimensions of the binary observations 
#'  (0 or 1). The dimension must be the same as 'exp' except memb_dim, which is
#'  optional. If it has 'memb_dim', then the length must be 1. The length of 
#'  'dat_dim' can be different from 'exp' if it has.
#'@param thresholds A numeric vector used to bin the forecasts. The default 
#' value is \code{seq(0.1, 0.9, 0.1)}, which means that the bins are 
#'  \code{[0, 0.1), [0.1, 0.2), ... [0.9, 1]}.
#'@param time_dim A character string indicating the name of dimension along  
#'  which Brier score is computed. The default value is 'sdate'.
#'@param dat_dim A character string indicating the name of dataset dimension in
#'  'exp' and 'obs'. The length of this dimension can be different between 
#'  'exp' and 'obs'. The default value is NULL.
#'@param memb_dim A character string of the name of the member dimension in 
#'  'exp' (and 'obs', optional). The function will do the ensemble mean 
#'  over this dimension. If there is no member dimension, set NULL. The default
#'  value is NULL.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list that contains:
#'\item{$rel}{standard reliability}
#'\item{$res}{standard resolution}
#'\item{$unc}{standard uncertainty}  
#'\item{$bs}{Brier score}
#'\item{$bs_check_res}{rel - res + unc}
#'\item{$bss_res}{res - rel / unc}
#'\item{$gres}{generalized resolution}
#'\item{$bs_check_gres}{rel - gres + unc}
#'\item{$bss_gres}{gres - rel / unc}
#'\item{$rel_bias_corrected}{bias - corrected rel}
#'\item{$gres_bias_corrected}{bias - corrected gres}
#'\item{$unc_bias_corrected}{bias - corrected unc}
#'\item{$bss_bias_corrected}{gres_bias_corrected - rel_bias_corrected / unc_bias_corrected}
#'\item{$nk}{number of forecast in each bin}
#'\item{$fkbar}{average probability of each bin}
#'\item{$okbar}{relative frequency that the observed event occurred}
#'The data type and dimensions of the items depend on if the input 'exp' and 
#''obs' are:\cr
#'(a) Vectors\cr
#'(b) Arrays with 'dat_dim' specified\cr
#'(c) Arrays with no 'dat_dim' specified\cr
#'Items 'rel', 'res', 'unc', 'bs', 'bs_check_res', 'bss_res', 'gres', 
#''bs_check_gres', 'bss_gres', 'rel_bias_corrected', 'gres_bias_corrected', 
#''unc_bias_corrected', and 'bss_bias_corrected' are (a) a number (b) an array
#'with dimensions c(nexp, nobs, all the rest dimensions in 'exp' and 'obs' 
#'except 'time_dim' and 'memb_dim') (c) an array with dimensions of
#''exp' and 'obs' except 'time_dim' and 'memb_dim'\cr
#'Items 'nk', 'fkbar', and 'okbar' are (a) a vector of length of bin number 
#'determined by 'threshold' (b) an array with dimensions c(nexp, nobs, 
#'no. of bins, all the rest dimensions in 'exp' and 'obs' except 'time_dim' and
#''memb_dim') (c) an array with dimensions c(no. of bin, all the rest dimensions
#'in 'exp' and 'obs' except 'time_dim' and 'memb_dim')
#' 
#'@references
#'Wilks (2006) Statistical Methods in the Atmospheric Sciences.\cr
#'Stephenson et al. (2008). Two extra components in the Brier score decomposition. 
#'  Weather and Forecasting, 23: 752-757.\cr
#'Ferro and Fricker (2012). A bias-corrected decomposition of the BS. 
#'  Quarterly Journal of the Royal Meteorological Society, DOI: 10.1002/qj.1924.
#'
#'@examples
#'# Inputs are vectors
#'exp <- runif(10)
#'obs <- round(exp)
#'x <- BrierScore(exp, obs)
#'
#'# Inputs are arrays
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'bins_ano_exp <- ProbBins(ano_exp, thr = c(1/3, 2/3))
#'bins_ano_obs <- ProbBins(ano_obs, thr = c(1/3, 2/3))
#'res <- BrierScore(bins_ano_exp, MeanDims(bins_ano_obs, 'member'), memb_dim = 'member') 
#'
#'@import multiApply
#'@export
BrierScore <- function(exp, obs, thresholds = seq(0.1, 0.9, 0.1), time_dim = 'sdate',
                       dat_dim = NULL, memb_dim = NULL, ncores = NULL) {

  # Check inputs
  ## exp and obs (1)
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a numeric vector or a numeric array.")
  }
  if (is.null(dim(exp))) {  #is vector
    dim(exp) <- c(length(exp))
    names(dim(exp)) <- time_dim
  }
  if (is.null(dim(obs))) {  #is vector
    dim(obs) <- c(length(obs))
    names(dim(obs)) <- time_dim
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  if (!all(obs %in% c(0, 1))) {
    stop("Parameter 'obs' must be binary events (0 or 1).")
  }
  ## thresholds
  if (!is.numeric(thresholds) | !is.vector(thresholds)) {
    stop("Parameter 'thresholds' must be a numeric vector.")
  }
  if (any(thresholds <= 0 | thresholds >= 1)) {
    stop("Parameter 'thresholds' must be between 0 and 1 as the bin-breaks.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' and 'obs' dimension.")
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' and 'obs' dimension.")
    }
  }
  ## memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp))) {
      stop("Parameter 'memb_dim' is not found in 'exp' dimension.")
    }
    if (memb_dim %in% names(dim(obs)) && dim(obs)[memb_dim] != 1) {
      stop("The length of parameter 'memb_dim' in 'obs' must be 1.")
    }
  }
  ## exp and obs (2)
  if (is.null(memb_dim)) {
    if (max(exp) > 1 | min(exp) < 0) {
      stop("Parameter 'exp' must be within [0, 1] range.")
    }
  } else {
    if (!all(exp %in% c(0, 1))) {
      stop("Parameter 'exp' must be 0 or 1 if it has memb_dim.")
    }
  }
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  if (!is.null(memb_dim)) {
    name_exp <- name_exp[-which(name_exp == memb_dim)]
    if (memb_dim %in% name_obs) {
    name_obs <- name_obs[-which(name_obs == memb_dim)]
    }
  }
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!all(name_exp %in% name_obs) | !all(name_obs %in% name_exp)) {
    stop("Parameter 'exp' and 'obs' must have the same names and lengths ",
         "of all the dimensions except 'dat_dim' and 'memb_dim'.")
  }
  if (!all(dim(exp)[name_exp] == dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have the same names and lengths ",
         "of all the dimensions except 'dat_dim' and 'memb_dim'.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 

  ###############################
  # Calculate Brier score

  ## ensemble mean
  if (!is.null(memb_dim)) {
    exp <- MeanDims(exp, memb_dim)
    if (memb_dim %in% names(dim(obs))) {
      obs <- MeanDims(obs, memb_dim)
    }
  }

  if (is.null(dat_dim)) {
    res <- Apply(list(exp, obs), 
                 target_dims = list(c(time_dim), 
                                    c(time_dim)),
                 fun = .BrierScore, 
                 thresholds = thresholds,
                 ncores = ncores)
  } else {
    res <- Apply(list(exp, obs),
                 target_dims = list(c(time_dim, dat_dim),
                                    c(time_dim, dat_dim)),
                 fun = .BrierScore,
                 thresholds = thresholds,
                 ncores = ncores)
  }

 return(res)
}

.BrierScore <- function(exp, obs, thresholds = seq(0.1, 0.9, 0.1)) {

  # exp: [sdate] or [sdate, nexp]
  # obs: [sdate] or [sdate, nobs]
  if (length(dim(exp)) == 2) {
    nexp <- as.numeric(dim(exp)[2])
    nobs <- as.numeric(dim(obs)[2])
    exp_ori <- exp
    obs_ori <- obs
    # Create empty arrays
    arr_rel <- arr_res <- arr_unc <- arr_bs <- arr_bs_check_res <- arr_bss_res <- 
      arr_gres <- arr_bs_check_gres <- arr_bss_gres <- arr_rel_bias_corrected <- 
      arr_gres_bias_corrected  <- arr_unc_bias_corrected <- arr_bss_bias_corrected <- 
      array(dim = c(nexp = nexp, nobs = nobs))
    arr_nk <- arr_fkbar <- arr_okbar <- 
      array(dim = c(nexp = nexp, nobs = nobs, bin = length(thresholds) + 1))

  } else {
    nexp <- 1
    nobs <- 1
  }

  for (n_exp in 1:nexp) {
    for (n_obs in 1:nobs) {
      if (exists('exp_ori')) {
        exp <- exp_ori[, n_exp]
        obs <- obs_ori[, n_obs]
      }
      n <- length(exp)
      nbins <- length(thresholds) + 1  # Number of bins
      bins <- vector('list', nbins) #as.list(paste("bin", 1:nbins, sep = ""))
      for (i in 1:nbins) {
        if (i == 1) {
          bins[[i]] <- list(which(exp >= 0 & exp < thresholds[i]))
        } else if (i == nbins) {
          bins[[i]] <- list(which(exp >= thresholds[i - 1] & exp <= 1))
        } else {
          bins[[i]] <- list(which(exp >= thresholds[i - 1] & exp < thresholds[i]))
        }
      }
    
      fkbar <- okbar <- nk <- array(0, dim = nbins)
      for (i in 1:nbins) {
        nk[i] <- length(bins[[i]][[1]])
        fkbar[i] <- sum(exp[bins[[i]][[1]]]) / nk[i]
        okbar[i] <- sum(obs[bins[[i]][[1]]]) / nk[i]
      }
        
    #-----in old .BrierScore()---------
    #    fkbar[fkbar == Inf] <- 0 
    #    okbar[is.nan(okbar)] <- 0
    #----------------------------------
    
      obar <- sum(obs) / length(obs)
      relsum <- ressum <- term1 <- term2 <- 0
      for (i in 1:nbins) {
        if (nk[i] > 0) {
          relsum <- relsum + nk[i] * (fkbar[i] - okbar[i])^2
          ressum <- ressum + nk[i] * (okbar[i] - obar)^2
          for (j in 1:nk[i]) {
            term1 <- term1 + (exp[bins[[i]][[1]][j]] - fkbar[i])^2
            term2 <- term2 + (exp[bins[[i]][[1]][j]] -
                              fkbar[i]) * (obs[bins[[i]][[1]][j]] - okbar[i])
          }
        }
      }
      rel <- relsum / n
      res <- ressum / n
      unc <- obar * (1 - obar)
      bs <- sum((exp - obs)^2) / n
      bs_check_res <- rel - res + unc
      bss_res <- (res - rel) / unc
      gres <- res - term1 * (1 / n) + term2 * (2 / n)   # Generalized resolution
      bs_check_gres <- rel - gres + unc                 # BS using GRES
      bss_gres <- (gres - rel) / unc                    # BSS using GRES
        
      
      # Estimating the bias-corrected components of the BS 
      term3 <- array(0, nbins)
      for (i in 1:nbins) {
        term3[i] <- (nk[i] / (nk[i] - 1)) * okbar[i] * (1 - okbar[i])
      }
      term_a <- sum(term3,  na.rm = T) / n
      term_b <- (obar * (1 - obar)) / (n - 1)
      rel_bias_corrected <- rel - term_a
      gres_bias_corrected <- gres - term_a + term_b
      if (rel_bias_corrected < 0 || gres_bias_corrected < 0) {
        rel_bias_corrected2 <- max(rel_bias_corrected, 
                                   rel_bias_corrected - gres_bias_corrected, 0)
        gres_bias_corrected2 <- max(gres_bias_corrected, 
                                    gres_bias_corrected - rel_bias_corrected, 0)
        rel_bias_corrected <- rel_bias_corrected2
        gres_bias_corrected <- gres_bias_corrected2
      }
      unc_bias_corrected <- unc + term_b
      bss_bias_corrected <- (gres_bias_corrected - rel_bias_corrected) / unc_bias_corrected
       
      # Add name for nk, fkbar, okbar
      names(dim(nk)) <- 'bin'
      names(dim(fkbar)) <- 'bin'
      names(dim(okbar)) <- 'bin'
    
      if (exists('exp_ori')) {
        arr_rel[n_exp, n_obs] <- rel
        arr_res[n_exp, n_obs] <- res
        arr_unc[n_exp, n_obs] <- unc
        arr_bs[n_exp, n_obs] <- bs
        arr_bs_check_res[n_exp, n_obs] <- bs_check_res
        arr_bss_res[n_exp, n_obs] <- bss_res
        arr_gres[n_exp, n_obs] <- gres
        arr_bs_check_gres[n_exp, n_obs] <- bs_check_gres
        arr_bss_gres[n_exp, n_obs] <- bss_gres
        arr_rel_bias_corrected[n_exp, n_obs] <- rel_bias_corrected
        arr_gres_bias_corrected[n_exp, n_obs] <- gres_bias_corrected
        arr_unc_bias_corrected[n_exp, n_obs] <- unc_bias_corrected
        arr_bss_bias_corrected[n_exp, n_obs] <- bss_bias_corrected
        arr_nk[n_exp, n_obs, ] <- nk
        arr_fkbar[n_exp, n_obs, ] <- fkbar
        arr_okbar[n_exp, n_obs, ] <- okbar
      }

    }
  }

  if (exists('exp_ori')) {
    res_list <- list(rel = arr_rel, res = arr_res, unc = arr_unc, bs = arr_bs, 
                     bs_check_res = arr_bs_check_res, bss_res = arr_bss_res, 
                     gres = arr_gres, bs_check_gres = arr_bs_check_gres,
                     bss_gres = arr_bss_gres, rel_bias_corrected = arr_rel_bias_corrected,
                     gres_bias_corrected = arr_gres_bias_corrected,
                     unc_bias_corrected = arr_unc_bias_corrected,
                     bss_bias_corrected = arr_bss_bias_corrected, nk = arr_nk, 
                     fkbar = arr_fkbar, okbar = arr_okbar) #bins = list(bins), 
  } else {

    res_list <- list(rel = rel, res = res, unc = unc, bs = bs, bs_check_res = bs_check_res,
                     bss_res = bss_res, gres = gres, bs_check_gres = bs_check_gres,
                     bss_gres = bss_gres, rel_bias_corrected = rel_bias_corrected,
                     gres_bias_corrected = gres_bias_corrected,
                     unc_bias_corrected = unc_bias_corrected,
                     bss_bias_corrected = bss_bias_corrected, nk = nk, fkbar = fkbar,
                     okbar = okbar) #bins = list(bins), 
  }

  return(invisible(res_list))
}
