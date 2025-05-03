#'Calculate ratio of predictable components (RPC)
#'
#'This function computes the ratio of predictable components (RPC; Eade et al., 2014).
#'
#'@param exp A numerical array with, at least, 'time_dim' and 'memb_dim' 
#'  dimensions.
#'@param obs A numerical array with the same dimensions than 'exp' except the
#'  'memb_dim' dimension.
#'@param time_dim A character string indicating the name of the time dimension.
#'  The default value is 'year'.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. The default value is 'member'.
#'@param na.rm A logical value indicating whether to remove NA values during
#'  the computation. The default value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return An array of the ratio of the predictable components. it has the same
#'  dimensions as 'exp' except 'time_dim' and 'memb_dim' dimensions.
#'
#'@examples
#'exp <- array(data = runif(600), dim = c(year = 15, member = 10, lat = 2, lon = 2))
#'obs <- array(data = runif(60), dim = c(year = 15, lat = 2, lon = 2))
#'RatioPredictableComponents(exp, obs)
#'
#'@import multiApply stats
#'@export
RatioPredictableComponents <- function(exp, obs, time_dim = 'year', 
                                       memb_dim = 'member', na.rm = FALSE, 
                                       ncores = NULL) {

  ## Checkings
  if (is.null(exp)) {
    stop("Parameter 'exp' cannot be NULL.")
  }
  if (!is.numeric(exp)) {
    stop("Parameter 'exp' must be a numeric array.")
  }
  if (is.null(obs)) {
    stop("Parameter 'obs' cannot be NULL.")
  }
  if (!is.numeric(obs)) {
    stop("Parameter 'obs' must be a numeric array.")
  }
  if (!(is.character(time_dim) & length(time_dim) == 1)) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!(is.character(memb_dim) & length(memb_dim) == 1)) {
    stop("Parameter 'memb_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp))) {
    stop("'exp' must have 'time_dim' dimension.")
  }
  if (!memb_dim %in% names(dim(exp))) {
    stop("'exp' must have 'memb_dim' dimension.")
  }
  if (!time_dim %in% names(dim(obs))) {
    stop("'obs' must have 'time_dim' dimension.")
  }
  if (!identical(dim(exp)[time_dim], dim(obs)[time_dim])) {
    stop("'exp' and 'obs' must have the same length for 'time_dim'.")
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be TRUE or FALSE.")
  }
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 |  ncores <= 0 | length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  
  RPC <- multiApply::Apply(data = list(exp, obs), 
                           target_dims = list(exp = c(time_dim, memb_dim),
                                              obs = time_dim), 
                           output_dims = NULL,
                           fun = .RatioPredictableComponents,
                           na.rm = na.rm,
                           ncores = ncores)$output1
  return(RPC)
}

.RatioPredictableComponents <- function(exp, obs, na.rm = na.rm) {
  # exp: [time, member]
  # obs: [time]

  ## Ensemble mean and spread
  ens_mean <- rowMeans(exp, na.rm = na.rm)
  #ens_spread <- apply(exp, 2, "-", ens_mean)

  ## Ensemble mean variance -> signal
  var_signal <- var(ens_mean, na.rm = na.rm)
  ## Variance of ensemble members about ensemble mean (= spread) -> noise
  # var_noise <- var(as.vector(ens_spread), na.rm = na.rm)

  ## Total variance
  # var_total <- var_signal + var_noise
  var_total <- mean(apply(exp, 2, var, na.rm = na.rm), na.rm = na.rm)

  ## Correlation between observations and the ensemble mean
  r <- cor(ens_mean, obs, method = 'pearson')

  ## Ratio of predictable components
  RPC <- r / sqrt(var_signal / var_total)
  
  return(RPC)
}

