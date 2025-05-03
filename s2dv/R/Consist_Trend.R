#'Compute trend using only model data for which observations are available
#'
#'Compute the linear trend for a time series by least square fitting together
#'with the associated error interval for both the observational and model data.
#'The 95\% confidence interval and detrended observational and model data are 
#'also provided.\cr
#'The function doesn't do the ensemble mean, so if the input data have the 
#'member dimension, ensemble mean needs to be computed beforehand.
#'
#'@param exp A named numeric array of experimental data, with at least two 
#'  dimensions 'time_dim' and 'dat_dim'.
#'@param obs A named numeric array of observational data, same dimensions as  
#'  parameter 'exp' except along 'dat_dim'.
#'@param dat_dim A character string indicating the name of the dataset 
#'  dimensions. If data at some point of 'time_dim' are not complete along 
#'  'dat_dim' in both 'exp' and 'obs', this point in all 'dat_dim' will be 
#'  discarded. The default value is 'dataset'.
#'@param time_dim A character string indicating the name of dimension along  
#'  which the trend is computed. The default value is 'sdate'.
#'@param interval A positive numeric indicating the unit length between two 
#' points along 'time_dim' dimension. The default value is 1.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A list containing:
#'\item{$trend}{
#'  A numeric array of the trend coefficients of model and observational data 
#'  with dimensions c(stats = 2, nexp + nobs, the rest dimensions of 'exp' and 
#'  'obs' except time_dim), where 'nexp' is the length of 'dat_dim' in 'exp' 
#'  and 'nobs' is the length of 'dat_dim' in 'obs. The 'stats' dimension 
#'  contains the intercept and the slope.
#'}
#'\item{$conf.lower}{
#'  A numeric array of the lower limit of 95\% confidence interval with 
#'  dimensions same as $trend. The 'stats' dimension contains the lower 
#'  confidence level of the intercept and the slope.
#'}
#'\item{$conf.upper}{
#'  A numeric array of the upper limit of 95\% confidence interval with 
#'  dimensions same as $trend. The 'stats' dimension contains the upper 
#'  confidence level of the intercept and the slope.
#'}
#'\item{$detrended_exp}{
#'  A numeric array of the detrended model data with the same dimensions as 
#'  'exp'.
#'}
#'\item{$detrended_obs}{
#'  A numeric array of the detrended observational data with the same 
#'  dimensions as 'obs'.
#'}
#'
#'@examples
#'#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'runmean_months <- 12
#'smooth_ano_exp <- Smoothing(ano_exp, runmeanlen = runmean_months)
#'smooth_ano_obs <- Smoothing(ano_obs, runmeanlen = runmean_months)
#'dim_to_mean <- 'member'  # average along members
#'years_between_startdates <- 5
#'trend <- Consist_Trend(MeanDims(smooth_ano_exp, dim_to_mean, na.rm = TRUE), 
#'                       MeanDims(smooth_ano_obs, dim_to_mean, na.rm = TRUE), 
#'                       interval = years_between_startdates)
#'#Bind data for plotting
#'trend_bind <- abind::abind(trend$conf.lower[2, , ], trend$trend[2, , ], 
#'                           trend$conf.upper[2, , ], trend$trend[1, , ], along = 0)
#'trend_bind <- Reorder(trend_bind, c(2, 1, 3))
#'\donttest{
#'PlotVsLTime(trend_bind, toptitle = "trend", ytitle = "K/(5 years)", 
#'            monini = 11, limits = c(-0.8, 0.8), listexp = c('CMIP5 IC3'), 
#'            listobs = c('ERSST'), biglab = FALSE, hlines = c(0)) 
#'PlotAno(InsertDim(trend$detrended_exp, 2, 1), InsertDim(trend$detrended_obs, 2, 1), 
#'        startDates, "Detrended tos anomalies", ytitle = 'K', 
#'        legends = 'ERSST', biglab = FALSE)
#'}
#'
#'@import multiApply
#'@export
Consist_Trend <- function(exp, obs, dat_dim = 'dataset', time_dim = 'sdate', interval = 1,
                          ncores = NULL) {
  # Check inputs 
  ## exp and obs
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a numeric array.")
  }
  if (is.null(dim(exp)) | is.null(dim(obs))) {
    stop("Parameter 'exp' and 'obs' must be at least two dimensions ",
         "containing time_dim and dat_dim.")
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  if (!all(names(dim(exp)) %in% names(dim(obs))) | 
      !all(names(dim(obs)) %in% names(dim(exp)))) {
    stop("Parameter 'exp' and 'obs' must have the same dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## dat_dim
  if (!is.character(dat_dim)) {
    stop("Parameter 'dat_dim' must be a character string.")
  }
  if (!all(dat_dim %in% names(dim(exp))) | !all(dat_dim %in% names(dim(obs)))) {
    stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## exp and obs (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  for (i in seq_along(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim[i])]
    name_obs <- name_obs[-which(name_obs == dat_dim[i])]
  }
  if (!all(dim(exp)[name_exp] == dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of ",
         "all dimension except 'dat_dim'.")
  }
  ## interval
  if (!is.numeric(interval) | interval <= 0 | length(interval) > 1) {
    stop("Parameter 'interval' must be a positive number.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate Consist_Trend

  output <- Apply(list(exp, obs),
                  target_dims = list(c(dat_dim, time_dim),
                                     c(dat_dim, time_dim)),
                  fun = .Consist_Trend,
                  output_dims = list(trend = c('stats', dat_dim),
                                     conf.lower = c('stats', dat_dim),
                                     conf.upper = c('stats', dat_dim),
                                     detrended_exp = c(dat_dim, time_dim),
                                     detrended_obs = c(dat_dim, time_dim)),
                  interval = interval, 
                  ncores = ncores)

  return(output)
}

.Consist_Trend <- function(exp, obs, interval = 1) {
  # exp: [nexp, sdate]
  # obs: [nobs, sdate]
  
  #  Find common points  
  nan <- colMeans(exp, na.rm = FALSE) + colMeans(obs, na.rm = FALSE)  # [sdate]
  exp[, is.na(nan)] <- NA
  obs[, is.na(nan)] <- NA  

  #  Compute trends 
  res_exp <- apply(exp, 1, .Trend, interval = interval, polydeg = 1)
  res_obs <- apply(obs, 1, .Trend, interval = interval, polydeg = 1)
  exp_trend <- lapply(res_exp, '[[', 'trend')
  exp_trend <- do.call(abind::abind, c(exp_trend, along = 2)) # [stats = 2, dat]
  obs_trend <- lapply(res_obs, '[[', 'trend')
  obs_trend <- do.call(abind::abind, c(obs_trend, along = 2))
  # bind along 'dat'
  res_trend <- abind::abind(exp_trend, obs_trend, along = 2)  # [stats = 2, dat = (nexp + nobs)]

  # Compute conf.lower
  exp_conf.lower <- lapply(res_exp, '[[', 'conf.lower')
  exp_conf.lower <- do.call(abind::abind, c(exp_conf.lower, along = 2)) # [stats = 2, dat]
  obs_conf.lower <- lapply(res_obs, '[[', 'conf.lower')
  obs_conf.lower <- do.call(abind::abind, c(obs_conf.lower, along = 2))
  res_conf.lower <- abind::abind(exp_conf.lower, obs_conf.lower, along = 2)

  # Compute conf.upper
  exp_conf.upper <- lapply(res_exp, '[[', 'conf.upper')
  exp_conf.upper <- do.call(abind::abind, c(exp_conf.upper, along = 2)) # [stats = 2, dat]
  obs_conf.upper <- lapply(res_obs, '[[', 'conf.upper')
  obs_conf.upper <- do.call(abind::abind, c(obs_conf.upper, along = 2))
  res_conf.upper <- abind::abind(exp_conf.upper, obs_conf.upper, along = 2)

  # Compute detrended
  exp_detrended <- lapply(res_exp, '[[', 'detrended')
  exp_detrended <- do.call(abind::abind, c(exp_detrended, along = 0))
  obs_detrended <- lapply(res_obs, '[[', 'detrended')
  obs_detrended <- do.call(abind::abind, c(obs_detrended, along = 0))

  return(invisible(list(trend = res_trend,
                        conf.lower = res_conf.lower, conf.upper = res_conf.upper,
                        detrended_exp = exp_detrended, detrended_obs = obs_detrended)))
}
