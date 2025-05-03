#'Compute interquartile range, maximum-minimum, standard deviation and median
#'absolute deviation
#'
#'Compute interquartile range, maximum-minimum, standard deviation and median
#'absolute deviation along the list of dimensions provided by the compute_dim 
#'argument (typically along the ensemble member and start date dimension).
#'The confidence interval is computed by bootstrapping by 100 times. The input
#'data can be the output of \code{Load()}, \code{Ano()}, or 
#'\code{Ano_CrossValid()}, for example.
#'
#'@param data A numeric vector or array with named dimensions to compute the 
#'  statistics. The dimensions should at least include 'compute_dim'.
#'@param compute_dim A vector of character strings of the dimension names along 
#'  which to compute the statistics. The default value is 'member'.
#'@param na.rm A logical value indicating if NAs should be removed (TRUE) or
#'  kept (FALSE) for computation. The default value is TRUE.
#'@param conf A logical value indicating whether to compute the confidence 
#'  intervals or not. The default value is TRUE.
#'@param alpha A numeric of the significance level to be used in the 
#'  statistical significance test. The default value is 0.05.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list of numeric arrays with the same dimensions as 'data' but without
#''compute_dim' and with the first dimension 'stats'. If 'conf' is TRUE, the 
#'length of 'stats' is 3 corresponding to the lower limit of the confidence 
#'interval, the spread, and the upper limit of the confidence interval. If 
#''conf' is FALSE, the length of 'stats' is 1 corresponding to the spread.
#'\item{$iqr}{
#'  InterQuartile Range.
#'}
#'\item{$maxmin}{
#'  Maximum - Minimum.
#'}
#'\item{$sd}{
#'  Standard Deviation.
#'}
#'\item{$mad}{
#'  Median Absolute Deviation.
#'} 
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'runmean_months <- 12
#'smooth_ano_exp <- Smoothing(ano_exp, runmeanlen = runmean_months)
#'smooth_ano_exp_m_sub <- smooth_ano_exp - InsertDim(MeanDims(smooth_ano_exp, 'member', 
#'                                                            na.rm = TRUE), 
#'                                                   posdim = 3, 
#'                                                   lendim = dim(smooth_ano_exp)['member'], 
#'                                                   name = 'member')
#'suppressWarnings({
#'spread <- Spread(smooth_ano_exp_m_sub, compute_dim = c('member', 'sdate'))
#'})
#'
#'\dontrun{
#'PlotVsLTime(Reorder(spread$iqr, c('dataset', 'stats', 'ftime')), 
#'            toptitle = "Inter-Quartile Range between ensemble members",
#'            ytitle = "K", monini = 11, limits = NULL, 
#'            listexp = c('CMIP5 IC3'), listobs = c('ERSST'), biglab = FALSE, 
#'            hlines = c(0))
#'PlotVsLTime(Reorder(spread$maxmin, c('dataset', 'stats', 'ftime')), 
#'            toptitle = "Maximum minus minimum of the members", 
#'            ytitle = "K", monini = 11, limits = NULL, 
#'            listexp = c('CMIP5 IC3'), listobs = c('ERSST'), biglab = FALSE, 
#'            hlines = c(0))
#'PlotVsLTime(Reorder(spread$sd, c('dataset', 'stats', 'ftime')), 
#'            toptitle = "Standard deviation of the members", 
#'            ytitle = "K", monini = 11, limits = NULL, 
#'            listexp = c('CMIP5 IC3'), listobs = c('ERSST'), biglab = FALSE, 
#'            hlines = c(0))
#'PlotVsLTime(Reorder(spread$mad, c('dataset', 'stats', 'ftime')), 
#'            toptitle = "Median Absolute Deviation of the members",
#'            ytitle = "K", monini = 11, limits = NULL, 
#'            listexp = c('CMIP5 IC3'), listobs = c('ERSST'), biglab = FALSE, 
#'            hlines = c(0))
#'}
#'
#'@import multiApply
#'@importFrom stats IQR sd mad runif quantile
#'@export
Spread <- function(data, compute_dim = 'member', na.rm = TRUE, 
                   conf = TRUE, alpha = 0.05, ncores = NULL) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {  #is vector
    dim(data) <- c(length(data))
    names(dim(data)) <- compute_dim[1]
  }
  if (any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## compute_dim
  if (!is.character(compute_dim)) {
    stop("Parameter 'compute_dim' must be a character vector.")
  }
  if (!all(compute_dim %in% names(dim(data)))) {
    stop("Parameter 'compute_dim' has some element not in 'data' dimension names.")
  }
  ## na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  ## conf
  if (!is.logical(conf) | length(conf) > 1) {
    stop("Parameter 'conf' must be one logical value.")
  }
  ## alpha
  if (any(!is.numeric(alpha) | alpha < 0 | alpha > 1) | length(alpha) > 1) {
    stop("Parameter 'alpha' must be a numeric number between 0 and 1.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | any(ncores %% 1 != 0) | any(ncores <= 0) |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 

  ###############################
  # Calculate Spread

  output <- Apply(list(data),
                  target_dims = compute_dim,
                  fun = .Spread,
                  output_dims = list(iqr = 'stats', maxmin = 'stats', 
                                     sd = 'stats', mad = 'stats'),
                  na.rm = na.rm,
                  conf = conf, alpha = alpha,
                  ncores = ncores)

  return(output)
}

.Spread <- function(data, compute_dim = 'member', na.rm = TRUE,
                    conf = TRUE, alpha = 0.05) {

  # data: compute_dim. [member] or [member, sdate] for example

  # Compute spread
  res_iqr <- IQR(data, na.rm = na.rm)
  res_maxmin <- max(data, na.rm = na.rm) - min(data, na.rm = na.rm)
  res_sd <- sd(data, na.rm = na.rm)
  res_mad <- mad(data, na.rm = na.rm)

  # Compute conf (bootstrapping)
  if (conf) {
    # The output length is 3, [conf.low, spread, conf.high]
    res_iqr <- rep(res_iqr, 3)
    res_maxmin <- rep(res_maxmin, 3)
    res_sd <- rep(res_sd, 3)
    res_mad <- rep(res_mad, 3)

    conf_low <- alpha / 2
    conf_high <- 1 - conf_low
  
    # Create vector for saving bootstrap result
    iqr_bs <- rep(NA, 100)
    maxmin_bs <- rep(NA, 100)
    sd_bs <- rep(NA, 100)
    mad_bs <- rep(NA, 100)

    # bootstrapping for 100 times
    num <- length(data)
    for (jmix in 1:100) {
      drawings <- round(runif(num, 0.5, num + 0.5))
      iqr_bs[jmix] <- IQR(data[drawings], na.rm = na.rm)
      maxmin_bs[jmix] <- max(data[drawings], na.rm = na.rm) - 
                           min(data[drawings], na.rm = na.rm)
      sd_bs[jmix] <- sd(data[drawings], na.rm = na.rm)
      mad_bs[jmix] <- mad(data[drawings], na.rm = na.rm)
    }
    
    # Calculate confidence interval with the bootstrapping results
    res_iqr[1] <- quantile(iqr_bs, conf_low, na.rm = na.rm)
    res_iqr[3] <- quantile(iqr_bs, conf_high, na.rm = na.rm)
    res_maxmin[1] <- res_maxmin[2] + (quantile(maxmin_bs, conf_low, na.rm = na.rm) - 
                                 quantile(maxmin_bs, conf_high, na.rm = na.rm)) / 2
    res_maxmin[3] <- res_maxmin[2] - (quantile(maxmin_bs, conf_low, na.rm = na.rm) - 
                                 quantile(maxmin_bs, conf_high, na.rm = na.rm)) / 2
    res_sd[1] <- quantile(sd_bs, conf_low, na.rm = na.rm)
    res_sd[3] <- quantile(sd_bs, conf_high, na.rm = na.rm)
    res_mad[1] <- res_mad[2] + (quantile(mad_bs, conf_low, na.rm = na.rm) - 
                                quantile(mad_bs, conf_high, na.rm = na.rm))
    res_mad[3] <- res_mad[2] - (quantile(mad_bs, conf_low, na.rm = na.rm) - 
                                quantile(mad_bs, conf_high, na.rm = na.rm))

  }

  # Turn infinite to NA
  res_maxmin[which(is.infinite(res_maxmin))] <- NA

  return(invisible(list(iqr = as.array(res_iqr), maxmin = as.array(res_maxmin), 
                        sd = as.array(res_sd), mad = as.array(res_mad))))
}
