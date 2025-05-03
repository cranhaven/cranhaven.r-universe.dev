#'Filter frequency peaks from an array
#'
#'Filter out the selected frequency from a time series. The filtering is 
#'performed by dichotomy, seeking for a frequency around the parameter 'freq'
#'and the phase that maximizes the signal to subtract from the time series.
#'The maximization of the signal to subtract relies on a minimization of the 
#'mean square differences between the time series ('data') and the cosine of 
#'the specified frequency and phase.
#'
#'@param data A numeric vector or array of the data to be filtered. 
#'  If it's a vector, it should be a time series. If it's an array,
#'  the  dimensions must have at least 'time_dim'.
#'@param freq A number of the frequency to filter.
#'@param time_dim A character string indicating the dimension along which to 
#'  compute the filtering. The default value is 'ftime'.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A numeric vector or array of the filtered data with the dimensions
#'  the same as 'data'.
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'ensmod <- MeanDims(sampleData$mod, 2)
#'spectrum <- Spectrum(ensmod)
#'
#'for (jsdate in 1:dim(spectrum)['sdate']) {
#'  for (jlen in 1:dim(spectrum)['ftime']) {
#'    if (spectrum[jlen, 2, 1, jsdate] > spectrum[jlen, 3, 1, jsdate]) {
#'      ensmod[1, jsdate, ] <- Filter(ensmod[1, jsdate, ], spectrum[jlen, 1, 1, jsdate])
#'    }
#'  }
#'}
#'  \donttest{
#'PlotAno(InsertDim(ensmod, 2, 1), sdates = startDates)
#'  }
#'
#'@import multiApply
#'@importFrom stats lm
#'@export
Filter <- function(data, freq, time_dim = 'ftime', ncores = NULL) {

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
    names(dim(data)) <- time_dim
  }
  if (any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## freq
  if (is.null(freq)) {
    stop("Parameter 'freq' cannot be NULL.")
  }
  if (!is.numeric(freq) | length(freq) != 1) {
    stop("Parameter 'freq' must be a number.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate Filter
  output <- Apply(list(data),
                  target_dims = time_dim,
                  fun = .Filter,
                  freq = freq,
                  output_dims = time_dim,
                  ncores = ncores)$output1

  return(output)
}

.Filter <- function(data, freq) {
  # data: [ftime]

  fac1 <- 1
  fac2 <- 1
  ndat <- length(data)
  ndat2 <- length(which(!is.na(data)))
  maxi <- 0
  endphase <- 0

  for (jfreq in seq(freq - 0.5 / ndat2, freq + 0.5 / ndat2, 0.1 / (ndat2 * fac1))) {
    for (phase in seq(0, pi, (pi / (10 * fac2)))) {
      xtest <- cos(phase + 1:ndat * jfreq * 2 * pi)
      test <- lm(data[!is.na(data)] ~ xtest[!is.na(data)])$fitted.values
      if (sum(test^2) > maxi) { 
        endphase <- phase
        endfreq <- jfreq
      }
      maxi <- max(sum(test^2), maxi)
    }
  }
  xend <- cos(endphase + 1:ndat * endfreq * 2 * pi)
  data[!is.na(data)] <- data[!is.na(data)] - lm(
                              data[!is.na(data)] ~ xend[!is.na(data)]
                              )$fitted.values

  return(invisible(data))
}
