#'Calculate Signal-to-noise ratio
#'
#'This function computes the signal-to-noise ratio, where the signal is the 
#'ensemble mean variance and the noise is the variance of the ensemble members 
#'about the ensemble mean (Eade et al., 2014; Scaife and Smith, 2018).
#'
#'@param data A numerical array with, at least, 'time_dim' and 'member_dim' 
#'  dimensions.
#'@param time_dim A character string indicating the name of the time dimension
#'  in 'data'. The default value is 'year'.
#'@param member_dim A character string indicating the name of the member 
#'  dimension in 'data'. The default value is 'member'.
#'@param na.rm A logical value indicating whether to remove NA values during the
#'  computation. The default value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return An array with of the signal-to-noise ratio. It has the same dimensions
#'  as 'data' except 'time_dim' and 'member_dim' dimensions.
#'
#'@examples
#' exp <- array(data = runif(600), dim = c(year = 15, member = 10, lat = 2, lon = 2))
#' SignalNoiseRatio(exp)
#'
#'@import multiApply
#'@importFrom stats var
#'@export
SignalNoiseRatio <- function(data, time_dim = 'year', member_dim = 'member', 
                             na.rm = FALSE, ncores = NULL) {
  
  ## Input Check
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (!(is.character(time_dim) & length(time_dim) == 1)) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!(is.character(member_dim) & length(member_dim) == 1)) {
    stop("Parameter 'member_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("'data' must have 'time_dim' dimension.")
  }
  if (!member_dim %in% names(dim(data))) {
    stop("'data' must have 'member_dim' dimension.")
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be TRUE or FALSE.")
  }
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 | length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  
  SNR <- multiApply::Apply(data = data, 
                           target_dims = c(time_dim, member_dim), 
                           output_dims = NULL,
                           fun = .SignalNoiseRatio,
                           na.rm = na.rm,
                           ncores = ncores)$output1
  return(SNR)
}

.SignalNoiseRatio <- function(data, na.rm = na.rm) {
  # data: [time, member]

  ## Ensemble mean and spread
  ens_mean <- rowMeans(data, na.rm = na.rm)
  ens_spread <- apply(data, 2, "-", ens_mean)

  ## Ensemble mean variance -> signal
  var_signal <- stats::var(ens_mean, na.rm = na.rm)
  ## Variance of ensemble members about ensemble mean (= spread) -> noise
  var_noise <- stats::var(as.vector(ens_spread), na.rm = na.rm)

  ## Signal to noise ratio
  SNR <- var_signal / var_noise

  return(SNR)
}
