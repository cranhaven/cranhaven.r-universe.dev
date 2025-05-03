#'Compute mean square error
#'
#'Compute the mean square error for an array of forecasts and an array of
#'observations. The MSEs are computed along time_dim, the dimension which 
#'corresponds to the start date dimension. If comp_dim is given, the MSEs are 
#'computed only if obs along the comp_dim dimension are complete between 
#'limits[1] and limits[2], i.e. there are no NAs between limits[1] and 
#'limits[2]. This option can be activated if the user wants to account only 
#'for the forecasts for which the corresponding observations are available at 
#'all leadtimes.\cr
#'The confidence interval is computed by the chi2 distribution.\cr
#'
#'@param exp A named numeric array of experimental data, with at least 
#' 'time_dim' dimension. It can also be a vector with the same length as 'obs'.
#'@param obs A named numeric array of observational data, same dimensions as  
#'  parameter 'exp' except along 'dat_dim' and 'memb_dim'. It can also be a 
#'  vector with the same length as 'exp'.
#'@param time_dim A character string indicating the name of dimension along  
#'  which the correlations are computed. The default value is 'sdate'.
#'@param memb_dim A character string indicating the name of the member dimension
#'  to compute the ensemble mean; it should be set to NULL if the input data are
#'  already the ensemble mean. The default value is NULL.
#'@param dat_dim A character string indicating the name of dataset or member 
#'  (nobs/nexp) dimension. The datasets of exp and obs will be paired and 
#'  computed MSE for each pair. The default value is NULL.
#'@param comp_dim A character string indicating the name of dimension along which
#'  obs is taken into account only if it is complete. The default value
#'  is NULL.
#'@param limits A vector of two integers indicating the range along comp_dim to 
#'  be completed. The default value is c(1, length(comp_dim dimension)).
#'@param conf A logical value indicating whether to retrieve the confidence 
#'  intervals or not. The default value is TRUE.
#'@param alpha A numeric indicating the significance level for the statistical
#'  significance test. The default value is 0.05.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list containing the numeric arrays with dimension:\cr 
#'  c(nexp, nobs, all other dimensions of exp except time_dim).\cr
#'nexp is the number of experiment (i.e., dat_dim in exp), and nobs is the 
#'number of observation (i.e., dat_dim in obs).\cr
#'\item{$mse}{
#'  The mean square error. 
#'}
#'\item{$conf.lower}{
#'  The lower confidence interval. Only present if \code{conf = TRUE}.
#'}
#'\item{$conf.upper}{
#'  The upper confidence interval. Only present if \code{conf = TRUE}.
#'}
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'smooth_ano_exp <- Smoothing(ano_exp, runmeanlen = 12, time_dim = 'ftime')
#'smooth_ano_obs <- Smoothing(ano_obs, runmeanlen = 12, time_dim = 'ftime')
#'res <- MSE(smooth_ano_exp, smooth_ano_obs, memb_dim = 'member', 
#'           comp_dim = 'ftime', limits = c(7, 54))
#'
#'# Synthetic data:
#'exp1 <- array(rnorm(120), dim = c(dat = 3, sdate = 10, ftime = 4))
#'obs1 <- array(rnorm(80),  dim = c(dat = 2, sdate = 10, ftime = 4))
#'res1 <- MSE(exp1, obs1, comp_dim = 'ftime', dat_dim = 'dat')
#'  
#'exp2 <- array(rnorm(20), dim = c(sdate = 5, member = 4))
#'obs2 <- array(rnorm(10),  dim = c(sdate = 5, member = 2))
#'res2 <- MSE(exp2, obs2, memb_dim = 'member')
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@importFrom stats qchisq
#'@export
MSE <- function(exp, obs, time_dim = 'sdate', dat_dim = NULL, memb_dim = NULL,
                comp_dim = NULL, limits = NULL, conf = TRUE, alpha = 0.05, ncores = NULL) {

  # Check inputs 
  ## exp and obs (1)
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a numeric array.")
  }
  if (is.null(dim(exp)) & is.null(dim(obs))) {  #is vector
    if (length(exp) == length(obs)) {
      exp <- array(exp, dim = c(length(exp)))
      names(dim(exp)) <- c(time_dim)
      obs <- array(obs, dim = c(length(obs)))
      names(dim(obs)) <- c(time_dim)
    } else {
      stop("Parameter 'exp' and 'obs' must be array with as least two ",
           "dimensions time_dim and dat_dim, or vector of same length.")
    }
  } else if (is.null(dim(exp)) | is.null(dim(obs))) {
    stop("Parameter 'exp' and 'obs' must be array with as least two ",
         "dimensions time_dim and dat_dim, or vector of same length.")
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp)) & !memb_dim %in% names(dim(obs))) {
      stop("Parameter 'memb_dim' is not found in 'exp' dimension.")
    }
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string or NULL.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.",
           "Set it as NULL if there is no dataset dimension.")
    }
  }
  ## comp_dim
  if (!is.null(comp_dim)) {
    if (!is.character(comp_dim) | length(comp_dim) > 1) {
      stop("Parameter 'comp_dim' must be a character string.")
    }
    if (!comp_dim %in% names(dim(exp)) | !comp_dim %in% names(dim(obs))) {
      stop("Parameter 'comp_dim' is not found in 'exp' or 'obs' dimension.")
    }
  }
  ## limits
  if (!is.null(limits)) {
    if (is.null(comp_dim)) {
      stop("Paramter 'comp_dim' cannot be NULL if 'limits' is assigned.")
    }
    if (!is.numeric(limits) | any(limits %% 1 != 0) | any(limits < 0) | 
        length(limits) != 2 | any(limits > dim(exp)[comp_dim])) {
      stop("Parameter 'limits' must be a vector of two positive ",
           "integers smaller than the length of paramter 'comp_dim'.")
    }
  }
  ## conf
  if (!is.logical(conf) | length(conf) > 1) {
    stop("Parameter 'conf' must be one logical value.")
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
  ## exp and obs (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  if (!is.null(memb_dim)) {
    if (memb_dim %in% name_exp) {
      name_exp <- name_exp[-which(name_exp == memb_dim)]
    }
    if (memb_dim %in% name_obs) {
      name_obs <- name_obs[-which(name_obs == memb_dim)]
    }
  }
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!all(name_exp == name_obs)) {
    stop("Parameter 'exp' and 'obs' must have the same dimension names.")
  }
  if (!all(dim(exp)[name_exp] == dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of ",
         "all dimensions except 'dat_dim' and 'memb_dim'.")
  }
  if (dim(exp)[time_dim] < 2) {
    stop("The length of time_dim must be at least 2 to compute MSE.")
  }

  ###############################
  ## Ensemble mean
  if (!is.null(memb_dim)) {
    if (memb_dim %in% names(dim(exp))) {
      exp <- MeanDims(exp, memb_dim, na.rm = T)
    }
    if (memb_dim %in% names(dim(obs))) {
      obs <- MeanDims(obs, memb_dim, na.rm = T)
    }
  }

  ###############################
  # Sort dimension
  name_exp <- names(dim(exp))
  name_obs <- names(dim(obs))
  order_obs <- match(name_exp, name_obs)
  obs <- Reorder(obs, order_obs)
  
  ###############################
  # Calculate MSE
  
  #  Remove data along comp_dim dim if there is at least one NA between limits
  if (!is.null(comp_dim)) {
    if (is.null(limits)) {
      limits <- c(1, dim(obs)[comp_dim])
    }
    pos <- which(names(dim(obs)) == comp_dim)
    obs_sub <- Subset(obs, pos, list(limits[1]:limits[2]))
    outrows <- is.na(MeanDims(obs_sub, pos, na.rm = FALSE))
    outrows <- InsertDim(outrows, pos, dim(obs)[comp_dim])
    obs[which(outrows)] <- NA
  }
  
  res <- Apply(list(exp, obs), 
               target_dims = list(c(time_dim, dat_dim), 
                                  c(time_dim, dat_dim)),
               fun = .MSE, 
               time_dim = time_dim, dat_dim = dat_dim,
               conf = conf, alpha = alpha,
               ncores = ncores)
  return(res)
}

.MSE <- function(exp, obs, time_dim = 'sdate', dat_dim = NULL, conf = TRUE, alpha = 0.05) {

  if (is.null(dat_dim)) {
    # exp: [sdate]
    # obs: [sdate]
    nexp <- 1
    nobs <- 1
    ini_dims <- dim(exp)
    dim(exp) <- c(ini_dims, dat_dim = 1)
    dim(obs) <- c(ini_dims, dat_dim = 1)
  } else {
    # exp: [sdate, dat_exp]
    # obs: [sdate, dat_obs]
    nexp <- as.numeric(dim(exp)[2])
    nobs <- as.numeric(dim(obs)[2])
  }
  
  dif <- array(dim = c(dim(exp)[1], nexp = nexp, nobs = nobs))
  chi <- array(dim = c(nexp = nexp, nobs = nobs))
  
  if (conf) {
    conflow <- alpha / 2
    confhigh <- 1 - conflow
    conf.lower <- array(dim = c(nexp = nexp, nobs = nobs))
    conf.upper <- array(dim = c(nexp = nexp, nobs = nobs))
  }
  
  # dif
  for (i in 1:nobs) {
    dif[, , i] <- sapply(1:nexp, function(x) {
                                   exp[, x] - obs[, i]
                                   })
  }
  
  mse <- colMeans(dif^2, na.rm = TRUE) # array(dim = c(nexp, nobs))
  
  if (conf) {
    #count effective sample along sdate. eno: c(nexp, nobs)
#    eno <- Eno(dif, time_dim)  # slower than for loop below?
    eno <- array(dim = c(nexp = nexp, nobs = nobs))
    for (n_obs in 1:nobs) {
      for (n_exp in 1:nexp) {
        eno[n_exp, n_obs] <- .Eno(dif[, n_exp, n_obs], na.action = na.pass)
      }
    }
    
    # conf.lower
    chi <- sapply(1:nobs, function(i) {
                            qchisq(confhigh, eno[, i] - 1)
                          })
    conf.lower <- (eno * mse ** 2 / chi) ** 0.5
    
    # conf.upper
    chi <- sapply(1:nobs, function(i) {
                            qchisq(conflow, eno[, i] - 1)
                          })
    conf.upper <- (eno * mse ** 2 / chi) ** 0.5
  }
  
  ###################################
  # Remove nexp and nobs if dat_dim = NULL
  if (is.null(dat_dim)) {
    dim(mse) <- NULL
    if (conf) {
      dim(conf.lower) <- NULL
      dim(conf.upper) <- NULL
    }
  }
  
  ###################################
 
  res <- list(mse = mse)
  if (conf) res <- c(res, list(conf.lower = conf.lower, conf.upper = conf.upper))
 
  return(res)
  
}
