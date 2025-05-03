#'Chunk long simulations for comparison with hindcasts
#'
#'Reorganize a long run (historical typically) with only one start date into 
#'chunks corresponding to a set of start dates. The time frequency of the data
#'should be monthly.
#'
#'@param data A numeric array of model or observational data with dimensions
#'  at least sdate_dim and ftime_dim.
#'@param sdatesin A character string of the start date of 'data'. The format
#'  should be 'YYYYMMDD' or 'YYYYMM'.
#'@param sdatesout A vector of character string indicating the expected start 
#'  dates of the output. The format should be 'YYYYMMDD' or 'YYYYMM'.
#'@param nleadtimesout A positive integer indicating the length of leadtimes of
#'  the output.
#'@param sdate_dim A character string indicating the name of the sdate date 
#'  dimension of 'data'. The default value is 'sdate'. 
#'@param ftime_dim A character string indicating the name of the lead time 
#'  dimension of 'data'. The default value is 'ftime'. 
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A numeric array with the same dimensions as data, except the length
#'  of sdate_dim is 'sdatesout' and the length of ftime_dim is nleadtimesout. 
#'
#'@examples
#'  \dontshow{
#'startDates <- c('19901101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                     c('observation'), startDates,
#'                                     leadtimemin = 1,
#'                                     leadtimemax = 60,
#'                                     output = 'areave',
#'                                     latmin = 27, latmax = 48,
#'                                     lonmin = -12, lonmax = 40)
#'  }
#'
#'sdates_out <- c('19901101', '19911101', '19921101', '19931101', '19941101')
#'leadtimes_per_startdate <- 12	
#'exp_data <- Histo2Hindcast(sampleData$mod, startDates, 
#'                           sdates_out, leadtimes_per_startdate)
#'obs_data <- Histo2Hindcast(sampleData$obs, startDates, 
#'                           sdates_out, leadtimes_per_startdate)
#'  \dontrun{
#'exp_data <- Reorder(exp_data, c(3, 4, 1, 2))
#'obs_data <- Reorder(obs_data, c(3, 4, 1, 2))
#'PlotAno(exp_data, obs_data, sdates_out, 
#'        toptitle = paste('Anomalies reorganized into shorter chunks'), 
#'        ytitle = 'K', fileout = NULL)
#'  }
#'
#'@import multiApply
#'@export
Histo2Hindcast <- function(data, sdatesin, sdatesout, nleadtimesout, 
                           sdate_dim = 'sdate', ftime_dim = 'ftime',
                           ncores = NULL) {

  ## Input Checks
  # data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  # sdatesin
  if (is.null(sdatesin)) {
    stop("Parameter 'sdatesin' cannot be NULL.")
  }
  if (!is.character(sdatesin) || length(sdatesin) > 1) {
    stop("Parameter 'sdatesin' must be a character string in the format",
         " 'YYYYMMDD' or 'YYYYMM'.")
  } else if (!nchar(sdatesin) %in% c(6, 8) | is.na(as.numeric(sdatesin))) {
    stop("Parameter 'sdatesin' must be a character string in the format",
         " 'YYYYMMDD' or 'YYYYMM'.")
  }
  # sdatesout
  if (is.null(sdatesout)) {
    stop("Parameter 'sdatesout' cannot be NULL.")
  }
  if (!is.character(sdatesout) | !is.vector(sdatesout)) {
    stop("Parameter 'sdatesout' must be a vector of character in the ",
         "format 'YYYYMMDD' or 'YYYYMM'.")
  } else if (!all(nchar(sdatesout) %in% c(6, 8)) | anyNA(as.numeric(sdatesin))) {
    stop("Parameter 'sdatesout' must be a vector of character in the ",
         "format 'YYYYMMDD' or 'YYYYMM'.")
  }
  # nleadtimesout
  if (is.null(nleadtimesout)) {
    stop("Parameter 'nleadtimesout' cannot be NULL.")
  }
  if (!is.numeric(nleadtimesout) | any(nleadtimesout %% 1 != 0) | 
      any(nleadtimesout < 0) | length(nleadtimesout) > 1) {
    stop("Parameter 'nleadtimesout' must be a positive integer.")
  }
  # sdate_dim
  if (!is.character(sdate_dim) || length(sdate_dim) > 1) {
    stop("Parameter 'sdate_dim' must be a character string.")
  }
  if (!sdate_dim %in% names(dim(data))) {
    stop("Parameter 'sdate_dim' is not found in 'data' dimension.")
  }
  if (dim(data)[sdate_dim] > 1) {
    stop("The dimension length of sdate_dim of 'data' must be 1.")
  }
  # ftime_dim
  if (!is.character(ftime_dim) || length(ftime_dim) > 1) {
    stop("Parameter 'ftime_dim' must be a character string.")
  }
  if (!ftime_dim %in% names(dim(data))) {
    stop("Parameter 'ftime_dim' is not found in 'data' dimension.")
  }
  # ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) || ncores %% 1 != 0 || ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }


  yrin <- as.numeric(substr(sdatesin, 1, 4))
  yrout <- as.numeric(substr(sdatesout, 1, 4))
  mthin <- as.numeric(substr(sdatesin, 5, 6))
  if (mthin > 12) {
    stop("Parameter 'sdatesin' must be in the format 'YYYYMMDD' or ",
         "'YYYYMM'. Found the month is over 12.")
  }
  mthout <- as.numeric(substr(sdatesout, 5, 6))
  if (any(mthout > 12)) {
    stop("Parameter 'sdatesout' must be a vector of character in the ",
         "format 'YYYYMMDD' or 'YYYYMM'. Found certain month is over 12.")
  }
  if (any((yrout - yrin) * 12 + (mthout - mthin) < 0)) {
    warning("Some of the start dates requested in 'sdatesout' are ",
            "earlier than the original start date 'sdatesin'. These ",
            "sdates will be filled with NA values")
  }

  res <- Apply(data, 
               target_dims = c(sdate_dim, ftime_dim), 
               output_dims = c(sdate_dim, ftime_dim),
               fun = .Histo2Hindcast, 
               yrin = yrin, yrout = yrout,
               mthin = mthin, mthout = mthout,
               nleadtimesout = nleadtimesout,
               ncores = ncores)$output1

  return(res)

}

.Histo2Hindcast <- function(data, yrin, yrout, mthin, mthout, nleadtimesout) {
  # data: [sdate = 1, ftime]

  res <- array(dim = c(sdate = length(yrout), ftime = nleadtimesout))

  diff_mth <- (yrout - yrin) * 12 + (mthout - mthin)
  for (i in seq_along(diff_mth)) {
    ftime_ind <- max(1 + diff_mth[i], 1):min(nleadtimesout + diff_mth[i], dim(data)[2])
    if (diff_mth[i] < 0) {
      # Fill with NA values if the requested date is earlier than available data
      res[i, seq_along(ftime_ind)] <- rep(NA, length(seq_along(ftime_ind)))
    } else if (diff_mth[i] < dim(data)[2]) {
      res[i, seq_along(ftime_ind)] <- data[1, ftime_ind]
    }
  }
  return(res)
}
