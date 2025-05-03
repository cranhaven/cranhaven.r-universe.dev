#'Compute the ratio between the RMSE of two experiments
#'
#'Calculate the ratio of the RMSE for two forecasts with the same observation,
#'that is, RMSE(ens, obs) / RMSE(ens.ref, obs). The p-value is provided by a 
#'two-sided Fischer test.
#'
#'@param exp1 A numeric array with named dimensions of the first experimental 
#'  data. It must have at least 'time_dim' and have the same dimensions as
#'  'exp2' and 'obs'.
#'@param exp2 A numeric array with named dimensions of the second experimental 
#'  data. It must have at least 'time_dim' and have the same dimensions as 
#'  'exp1' and 'obs'.
#'@param obs A numeric array with named dimensions of the observational data.
#'  It must have at least 'time_dim' and have the same dimensions as 'exp1' and
#'  'exp2'.
#'@param time_dim A character string of the dimension name along which RMS is
#'  computed. The default value is 'sdate'.
#'@param pval A logical value indicating whether to compute the p-value of Ho:
#'  RMSE1/RMSE2 = 1 or not. The default value is TRUE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A list containing the numeric arrays with dimensions identical with
#'  'exp1', 'exp2', and 'obs', except 'time_dim':
#'\item{$ratiorms}{
#'  The ratio between the RMSE (i.e., RMSE1/RMSE2).
#'}
#'\item{$p.val}{
#'  The p-value of the two-sided Fisher test with Ho: RMSE1/RMSE2 = 1. Only 
#'  exists if 'pval' is TRUE.
#'}
#'
#'@examples
#'\dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                     c('observation'), startDates,
#'                                     output = 'lonlat',
#'                                     latmin = 27, latmax = 48,
#'                                     lonmin = -12, lonmax = 40)
#'}
#'# Compute DJF seasonal means and anomalies.
#'initial_month <- 11
#'mean_start_month <- 12
#'mean_stop_month <- 2                                
#'sampleData$mod <- Season(sampleData$mod, monini = initial_month, 
#'                         moninf = mean_start_month, monsup = mean_stop_month)
#'sampleData$obs <- Season(sampleData$obs, monini = initial_month, 
#'                         moninf = mean_start_month, monsup = mean_stop_month)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'# Generate two experiments with 2 and 1 members from the only experiment 
#'# available in the sample data. Take only data values for a single forecast
#'# time step.
#'ano_exp_1 <- ClimProjDiags::Subset(ano_exp, 'member', c(1, 2))
#'ano_exp_2 <- ClimProjDiags::Subset(ano_exp, 'member', c(3))
#'ano_exp_1 <- ClimProjDiags::Subset(ano_exp_1, c('dataset', 'ftime'),
#'                                   list(1, 1), drop = 'selected')
#'ano_exp_2 <- ClimProjDiags::Subset(ano_exp_2, c('dataset', 'ftime'),
#'                                   list(1, 1), drop = 'selected')
#'ano_obs <- ClimProjDiags::Subset(ano_obs, c('dataset', 'ftime'), list(1, 1), drop = 'selected')
#'# Compute ensemble mean and provide as inputs to RatioRMS.
#'rrms <- RatioRMS(MeanDims(ano_exp_1, 'member'), 
#'                 MeanDims(ano_exp_2, 'member'), 
#'                 MeanDims(ano_obs, 'member'))
#'# Plot the RatioRMS for the first forecast time step.
#'\donttest{
#'PlotEquiMap(rrms$ratiorms, sampleData$lon, sampleData$lat, 
#'            toptitle = 'Ratio RMSE')
#'}
#'
#'@import multiApply
#'@export
RatioRMS <- function(exp1, exp2, obs, time_dim = 'sdate', pval = TRUE, ncores = NULL) {

  # Check inputs 
  ## exp1, exp2, obs
  if (is.null(exp1) | is.null(exp2) | is.null(obs)) {
    stop("Parameter 'exp1', 'exp2', and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp1) | !is.numeric(exp2) | !is.numeric(obs)) {
    stop("Parameter 'exp1', 'exp2', and 'obs' must be a numeric array.")
  }
  if (is.null(dim(exp1))) {  #is vector
    dim(exp1) <- c(length(exp1))
    names(dim(exp1)) <- time_dim
  }
  if (is.null(dim(exp2))) {  #is vector
    dim(exp2) <- c(length(exp2))
    names(dim(exp2)) <- time_dim
  }
  if (is.null(dim(obs))) {  #is vector
    dim(obs) <- c(length(obs))
    names(dim(obs)) <- time_dim
  }
  if (any(is.null(names(dim(exp1)))) | any(nchar(names(dim(exp1))) == 0) |
      any(is.null(names(dim(exp2)))) | any(nchar(names(dim(exp2))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp1', 'exp2', and 'obs' must have dimension names.")
  }
  if (!all(names(dim(exp1)) %in% names(dim(exp2))) | 
      !all(names(dim(exp2)) %in% names(dim(obs))) | 
      !all(names(dim(obs)) %in% names(dim(exp1)))) {
    stop("Parameter 'exp1', 'exp2', and 'obs' must have same dimension names.")
  }
  name_1 <- sort(names(dim(exp1)))
  name_2 <- sort(names(dim(exp2)))
  name_3 <- sort(names(dim(obs)))
  if (!all(dim(exp1)[name_1] == dim(exp2)[name_2]) |
      !all(dim(exp1)[name_1] == dim(obs)[name_3])) {
    stop("Parameter 'exp1', 'exp2', and 'obs' must have the same length of ",
         "all the dimensions.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp1))) {
    stop("Parameter 'time_dim' is not found in 'exp1', 'exp2', and 'obs' dimensions.")
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 

  ###############################
  # Calculate RatioRMS
  if (is.null(ncores)) {
    use_Apply <- FALSE
  } else if (ncores == 1) {
    use_Apply <- FALSE
  } else {
    use_Apply <- TRUE
  }

  if (use_Apply) {
    res <- Apply(list(exp1, exp2, obs), 
                 target_dims = list(c(names(dim(exp1))), 
                                    c(names(dim(exp1))),
                                    c(names(dim(exp1)))),
                 fun = .RatioRMS, 
                 time_dim = time_dim, pval = pval,
                 ncores = ncores)
  } else {
    res <- .RatioRMS(exp1, exp2, obs, time_dim = time_dim, pval = pval)
  }

  return(res)
}

.RatioRMS <- function(exp1, exp2, obs, time_dim = 'sdate', pval = TRUE) {

  # exp1, exp2, obs: [all_dim]
  dif1 <- exp1 - obs
  dif2 <- exp2 - obs
  rms1 <- MeanDims(dif1^2, time_dim, na.rm = TRUE)^0.5
  rms2 <- MeanDims(dif2^2, time_dim, na.rm = TRUE)^0.5
  rms2[which(abs(rms2) <= (max(abs(rms2), na.rm = TRUE) / 1000))] <- 
    max(abs(rms2), na.rm = TRUE) / 1000
  ratiorms <- rms1 / rms2

  if (pval) {
    eno1 <- Eno(dif1, time_dim)
    eno2 <- Eno(dif2, time_dim)
    F <- (eno1 * (rms1) ** 2 / (eno1 - 1)) / (eno2 * (rms2) ** 2 / (eno2 - 1))
    F[which(F < 1)] <- 1 / F[which(F < 1)]
    
    if (is.null(dim(ratiorms))) {
      p.val <- NULL
    } else {
      p.val <- array(dim = dim(ratiorms))
    }
    avail_ind <- which(!is.na(eno1) & !is.na(eno2) & eno1 > 2 & eno2 > 2)
    p.val[avail_ind] <- (1 - pf(F, eno1[avail_ind] - 1, eno2[avail_ind] - 1)) * 2
    ratiorms[-avail_ind] <- NA
  }

  if (pval) {
    return(invisible(list(ratiorms = ratiorms, p.val = p.val)))
  } else {
    return(invisible(list(ratiorms = ratiorms)))
  }
}
