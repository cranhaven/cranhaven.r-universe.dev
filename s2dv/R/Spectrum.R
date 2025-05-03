#'Estimate frequency spectrum
#'
#'Estimate the frequency spectrum of the data array together with a 
#'user-specified confidence level. The output is provided as an array with 
#'dimensions c(number of frequencies, stats = 3, other margin dimensions of 
#'data). The 'stats' dimension contains the frequencies at which the spectral 
#'density is estimated, the estimates of the spectral density, and the 
#'significance level.\cr
#'The spectrum estimation relies on an R built-in function \code{spectrum()} 
#'and the confidence interval is estimated by the Monte-Carlo method.
#'
#'@param data A vector or numeric array of which the frequency spectrum is 
#'  required. If it's a vector, it should be a time series. If it's an array,
#'  the  dimensions must have at least 'time_dim'. The data is assumed to be  
#'  evenly spaced in time.
#'@param time_dim A character string indicating the dimension along which to 
#'  compute the frequency spectrum. The default value is 'ftime'.
#'@param alpha A numeric indicating the significance level for the Monte-Carlo
#'  significance test. The default value is 0.05.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A numeric array of the frequency spectrum with dimensions 
#'  c(<time_dim> = number of frequencies, stats = 3, the rest of the 
#'  dimensions of 'data'). The 'stats' dimension contains the frequency values,
#'  the spectral density, and the confidence interval.
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
#'@importFrom stats spectrum cor rnorm sd quantile
#'@export
Spectrum <- function(data, time_dim = 'ftime', alpha = 0.05, ncores = NULL) {

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
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  ## alpha
  if (!is.numeric(alpha) | alpha < 0 | alpha > 1 | length(alpha) > 1) {
    stop("Parameter 'alpha' must be a numeric number between 0 and 1.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate Spectrum

  output <- Apply(list(data),
                  target_dims = time_dim,
                  fun = .Spectrum,
                  output_dims = c(time_dim, 'stats'),
                  alpha = alpha,
                  ncores = ncores)$output1

  return(output)
}

.Spectrum <- function(data, alpha = 0.05) {
  # data: [time]

  data <- data[!is.na(data)]
  ndat <- length(data)

  if (ndat >= 3) {
    tmp <- spectrum(data, plot = FALSE)
    output <- array(dim = c(length(tmp$spec), 3))
    output[, 1] <- tmp$freq
    output[, 2] <- tmp$spec
    ntir <- 100
    store <- array(dim = c(ntir, length(tmp$spec)))
    for (jt in 1:ntir) {
      toto <- mean(data)
      alpha1 <- cor(data[2:ndat], data[1:(ndat - 1)])
      for (ind in 2:ndat) { 
        b <- rnorm(1, mean(data) * (1 - alpha1), sd(data) * sqrt(1 - 
                   alpha1 ^ 2))
        toto <- c(toto, toto[ind - 1] * alpha1 + b)
      }
      toto2 <- spectrum(toto, plot = FALSE)
      store[jt, ] <- toto2$spec
    }
    for (jx in seq_along(tmp$spec)) {
      output[jx, 3] <- quantile(store[, jx], 1 - alpha)
    }
  } else {
    output <- NA
  }

  return(invisible(output))
}
