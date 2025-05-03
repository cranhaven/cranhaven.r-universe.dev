#'Compute the Continuous Ranked Probability Score
#'
#'The Continuous Ranked Probability Score (CRPS; Wilks, 2011) is the continuous 
#'version of the Ranked Probability Score (RPS; Wilks, 2011). It is a skill 
#'metric to evaluate the full distribution of probabilistic forecasts. It has a 
#'negative orientation (i.e., the higher-quality forecast the smaller CRPS) and 
#'it rewards the forecast that has probability concentration around the observed
#'value. In case of a deterministic forecast, the CRPS is reduced to the mean 
#'absolute error. It has the same units as the data. The function is based on 
#'enscrps_cpp from SpecsVerification. If there is more than one dataset, CRPS 
#'will be computed for each pair of exp and obs data.
#'
#'@param exp A named numerical array of the forecast with at least time 
#'  dimension.  
#'@param obs A named numerical array of the observation with at least time 
#'  dimension. The dimensions must be the same as 'exp' except 'memb_dim' and 
#'  'dat_dim'.
#'@param time_dim A character string indicating the name of the time dimension.
#'  The default value is 'sdate'.
#'@param memb_dim A character string indicating the name of the member dimension
#'  to compute the probabilities of the forecast. The default value is 'member'.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  The length of this dimension can be different between 'exp' and 'obs'. The
#'  default value is NULL.
#'@param Fair A logical indicating whether to compute the FairCRPS (the 
#'  potential CRPS that the forecast would have with an infinite ensemble size).
#'  The default value is FALSE.
#'@param return_mean A logical indicating whether to return the temporal mean
#'  of the CRPS or not. If TRUE, the temporal mean is calculated along time_dim,
#'  if FALSE the time dimension is not aggregated. The default is TRUE. 
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A numerical array of CRPS with dimensions c(nexp, nobs, the rest dimensions of
#''exp' except 'time_dim' and 'memb_dim' dimensions). nexp is the number of 
#'experiment (i.e., dat_dim in exp), and nobs is the number of observation
#'(i.e., dat_dim in obs). If dat_dim is NULL, nexp and nobs are omitted.
#'
#'@references 
#'Wilks, 2011; https://doi.org/10.1016/B978-0-12-385022-5.00008-7
#'
#'@examples
#'exp <- array(rnorm(1000), dim = c(lat = 3, lon = 2, member = 10, sdate = 50))
#'obs <- array(rnorm(1000), dim = c(lat = 3, lon = 2, sdate = 50))
#'res <- CRPS(exp = exp, obs = obs)
#'
#'@import multiApply
#'@importFrom SpecsVerification enscrps_cpp
#'@importFrom ClimProjDiags Subset
#'@export
CRPS <- function(exp, obs, time_dim = 'sdate', memb_dim = 'member', dat_dim = NULL,
                 Fair = FALSE, return_mean = TRUE, ncores = NULL) {
  # Check inputs
  ## exp and obs (1)
  if (!is.array(exp) | !is.numeric(exp))
    stop("Parameter 'exp' must be a numeric array.")
  if (!is.array(obs) | !is.numeric(obs))
    stop("Parameter 'obs' must be a numeric array.")
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1)
    stop("Parameter 'time_dim' must be a character string.")
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## memb_dim
  if (!is.character(memb_dim) | length(memb_dim) > 1) {
    stop("Parameter 'memb_dim' must be a character string.")
  }
  if (!memb_dim %in% names(dim(exp))) {
    stop("Parameter 'memb_dim' is not found in 'exp' dimension.")
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.",
           " Set it as NULL if there is no dataset dimension.")
    }
  }
  ## exp and obs (2)
  if (memb_dim %in% names(dim(obs))) {
    if (identical(as.numeric(dim(obs)[memb_dim]), 1)) {
      obs <- ClimProjDiags::Subset(x = obs, along = memb_dim, indices = 1, drop = 'selected')
    } else {
      stop("Not implemented for observations with members ", 
           "('obs' can have 'memb_dim', but it should be of length = 1).")
    }
  }
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  name_exp <- name_exp[-which(name_exp == memb_dim)]
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!identical(length(name_exp), length(name_obs)) |
      !identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of ",
         "all dimensions except 'memb_dim' and 'dat_dim'.")
  }
  ## Fair
  if (!is.logical(Fair) | length(Fair) > 1) {
    stop("Parameter 'Fair' must be either TRUE or FALSE.")
  }
  ## return_mean
  if (!is.logical(return_mean) | length(return_mean) > 1) {
    stop("Parameter 'return_mean' must be either TRUE or FALSE.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 | length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    }
  }
  
  ###############################
  
  # Compute CRPS
  crps <- Apply(data = list(exp = exp, obs = obs), 
                target_dims = list(exp = c(time_dim, memb_dim, dat_dim), 
                                   obs = c(time_dim, dat_dim)),
                fun = .CRPS, 
                time_dim = time_dim, memb_dim = memb_dim, dat_dim = dat_dim,
                Fair = Fair,
                ncores = ncores)$output1
  
  if (return_mean) {
    crps <- MeanDims(crps, time_dim, na.rm = FALSE) 
  } else {
    crps <- crps
  }
  
  return(crps)
}

.CRPS <- function(exp, obs, time_dim = 'sdate', memb_dim = 'member', dat_dim = NULL,
                  Fair = FALSE) {
  
  # exp: [sdate, memb, (dat_dim)]
  # obs: [sdate, (dat_dim)]
  
  # Adjust dimensions if needed
  if (is.null(dat_dim)) {
    nexp <- 1
    nobs <- 1
    dim(exp) <- c(dim(exp), nexp = nexp)
    dim(obs) <- c(dim(obs), nobs = nobs)
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
  }
  
  # for FairCRPS
  R_new <- ifelse(Fair, Inf, NA)
  
  CRPS <- array(dim = c(dim(exp)[time_dim], nexp = nexp, nobs = nobs))
  
  for (i in 1:nexp) {
    for (j in 1:nobs) {
      exp_data <- exp[, , i]
      obs_data <- obs[, j]
      
      if (is.null(dim(exp_data))) dim(exp_data) <- c(dim(exp)[1:2])
      if (is.null(dim(obs_data))) dim(obs_data) <- c(dim(obs)[1])
      
      crps <- SpecsVerification::enscrps_cpp(ens = exp_data, obs = obs_data, R_new = R_new)
      CRPS[, i, j] <- crps
    }
  }
  
  if (is.null(dat_dim)) {
    dim(CRPS) <- c(dim(CRPS)[time_dim])
  }
  
  return(CRPS)
}
