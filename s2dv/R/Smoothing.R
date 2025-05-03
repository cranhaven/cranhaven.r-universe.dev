#'Smooth an array along one dimension
#'
#'Smooth an array of any number of dimensions along one dimension.
#'
#'@param data A numerical array to be smoothed along one of its dimension 
#'  (typically the forecast time dimension).
#'@param runmeanlen An integer indicating the running mean length of sampling 
#'  units (typically months). The default value is 12.
#'@param time_dim A character string indicating the name of the dimension to be 
#'  smoothed along. The default value is 'ftime'.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A numerical array with the same dimensions as parameter 'data' but 
#'  the 'time_dim' dimension is moved to the first. The head and tail part which
#'  do not have enough neighboring data for smoothing is assigned as NA. 
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'smooth_ano_exp <- Smoothing(ano_exp, time_dim = 'ftime', runmeanlen = 12)
#'smooth_ano_obs <- Smoothing(ano_obs, time_dim = 'ftime', runmeanlen = 12)
#'smooth_ano_exp <- Reorder(smooth_ano_exp, c(2, 3, 4, 1))
#'smooth_ano_obs <- Reorder(smooth_ano_obs, c(2, 3, 4, 1))
#'  \dontrun{
#'PlotAno(smooth_ano_exp, smooth_ano_obs, startDates, 
#'        toptitle = "Smoothed Mediterranean mean SST", ytitle = "K")
#'  }
#'@import plyr multiApply
#'@export
Smoothing <- function(data, time_dim = 'ftime', runmeanlen = 12, ncores = NULL) {

  # Check data
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
  ## runmeanlen
  if (any(!is.numeric(runmeanlen) | runmeanlen %% 1 != 0 | 
          runmeanlen <= 0 | length(runmeanlen) > 1)) {
    stop("Parameter 'runmeanlen' must be a positive integer.")
  }
  time_dim_length <- dim(data)[which(names(dim(data)) == time_dim)]
  if (runmeanlen >= time_dim_length & time_dim_length %% 2 == 0) {
    stop("Parameter 'runmeanlen' must be within [1, ", time_dim_length - 1, 
         "].")
  }
  if (runmeanlen > time_dim_length & time_dim_length %% 2 != 0) {
    stop("Parameter 'runmeanlen' must be within [1, ", time_dim_length,
         "].")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate Smoothing

  if (runmeanlen == 1) {
    output <- Reorder(data, c(time_dim, 
                              names(dim(data))[-which(names(dim(data)) == time_dim)]))
  } else {

    output <- Apply(list(data),
                    target_dims = time_dim,
                    fun = .Smoothing,
                    output_dims = time_dim,
                    time_dim = time_dim, runmeanlen = runmeanlen, 
                    ncores = ncores)$output1
  }
  return(output)
}

.Smoothing <- function(data, runmeanlen = 12, time_dim = 'ftime') {
  # data: [time_dim]

  nmr1 <- floor(runmeanlen / 2)
  nltime <- length(data)
  smoothed <- rep(NA, length(data))

  # We do a loop for all values which have the complete window. 
  # Other values are left to NA.
  # If the window length is even, need to weight the half to the values
  # at both ends.
  if ((runmeanlen %% 2) == 0) {
    for (jtime in (1 + nmr1):(nltime - nmr1)) {
      # calculate the two edges
      edge <- (data[jtime - nmr1] + data[jtime + nmr1]) / (runmeanlen * 2)
      # calculate the complete window
      smoothed[jtime] <- (sum(data[(jtime - nmr1 + 1):(jtime + nmr1 - 1)]) / runmeanlen) + edge
    }
  } else {
    for (jtime in (1 + nmr1):(nltime - nmr1)) {
      # calculate the complete window
      smoothed[jtime] <- sum(data[(jtime - nmr1):(jtime + nmr1)]) / runmeanlen
    }
  }

  return(smoothed)
}
