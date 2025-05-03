#'Compute the Mean Bias
#'
#'The Mean Bias or Mean Error (Wilks, 2011) is defined as the mean difference 
#'between the ensemble mean forecast and the observations. It is a deterministic
#'metric. Positive values indicate that the forecasts are on average too high
#'and negative values indicate that the forecasts are on average too low.
#'It also allows to compute the Absolute Mean Bias or bias without temporal 
#'mean. If there is more than one dataset, the result will be computed for each
#'pair of exp and obs data.
#'
#'@param exp A named numerical array of the forecast with at least time 
#'  dimension.  
#'@param obs A named numerical array of the observation with at least time 
#'  dimension. The dimensions must be the same as 'exp' except 'memb_dim' and 
#'  'dat_dim'.
#'@param time_dim A character string indicating the name of the time dimension.
#'  The default value is 'sdate'.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  The length of this dimension can be different between 'exp' and 'obs'. 
#'  The default value is NULL.
#'@param memb_dim A character string indicating the name of the member dimension
#'  to compute the ensemble mean; it should be set to NULL if the parameter 
#'  'exp' is already the ensemble mean. The default value is NULL.
#'@param na.rm A logical value indicating if NAs should be removed (TRUE) or
#'  kept (FALSE) for computation. The default value is FALSE.
#'@param absolute A logical value indicating whether to compute the absolute 
#'  bias. The default value is FALSE.
#'@param time_mean A logical value indicating whether to compute the temporal 
#'  mean of the bias. The default value is TRUE.
#'@param alpha A numeric or NULL (default) to indicate the significance level 
#'  using Welch's t-test. Only available when absolute is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A numerical array of bias with dimensions c(nexp, nobs, the rest dimensions of
#''exp' except 'time_dim' (if time_mean = T) and 'memb_dim'). nexp is the number
#'of experiment (i.e., 'dat_dim' in exp), and nobs is the number of observation 
#'(i.e., 'dat_dim' in obs). If dat_dim is NULL, nexp and nobs are omitted. If 
#'alpha is specified, and absolute is FALSE, the result is a list with two 
#'elements: the bias as described above and the significance as a logical array
#'with the same dimensions.
#'
#'@references 
#'Wilks, 2011; https://doi.org/10.1016/B978-0-12-385022-5.00008-7
#'
#'@examples
#'exp <- array(rnorm(1000), dim = c(dat = 1, lat = 3, lon = 5, member = 10, sdate = 50))
#'obs <- array(rnorm(1000), dim = c(dat = 1, lat = 3, lon = 5, sdate = 50))
#'bias <- Bias(exp = exp, obs = obs, memb_dim = 'member')
#'bias2 <- Bias(exp = exp, obs = obs, memb_dim = 'member', alpha = 0.01)
#'abs_bias <- Bias(exp = exp, obs = obs, memb_dim = 'member', absolute = TRUE, alpha = NULL)
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
Bias <- function(exp, obs, time_dim = 'sdate', memb_dim = NULL, dat_dim = NULL,
                 na.rm = FALSE,  absolute = FALSE, time_mean = TRUE,
                 alpha = 0.05, ncores = NULL) {
  
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
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp))) {
      stop("Parameter 'memb_dim' is not found in 'exp' dimension.")
    }
    if (memb_dim %in% names(dim(obs))) {
      if (identical(as.numeric(dim(obs)[memb_dim]), 1)) {
        obs <- ClimProjDiags::Subset(x = obs, along = memb_dim, indices = 1, drop = 'selected')
      } else {
        stop("Not implemented for observations with members ('obs' can have 'memb_dim', ",
             "but it should be of length = 1).")
      }
    }
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
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  if (!is.null(memb_dim)) {
    name_exp <- name_exp[-which(name_exp == memb_dim)]
  }
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!identical(length(name_exp), length(name_obs)) |
      !identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of ",
         "all dimensions except 'memb_dim' and 'dat_dim'.")
  }
  ## na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  ## absolute
  if (!is.logical(absolute) | length(absolute) > 1) {
    stop("Parameter 'absolute' must be one logical value.")
  }
  ## time_mean
  if (!is.logical(time_mean) | length(time_mean) > 1) {
    stop("Parameter 'time_mean' must be one logical value.")
  }
  ## alpha
  if (!is.null(alpha)) {
    if (any(!is.numeric(alpha) | alpha <= 0 | alpha >= 1 | length(alpha) > 1)) {
      stop("Parameter 'alpha' must be null or a numeric value.")
    }
    if (absolute) {
      alpha <- NULL
      .warning("Parameter 'absolute' is TRUE, so 'alpha' has been set to",
               "false and significance will not be returned.")
    }
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
        length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    }
  }

  ###############################

  ## Ensemble mean
  if (!is.null(memb_dim)) {
    exp <- MeanDims(exp, memb_dim, na.rm = na.rm)
  }
  
  ## (Mean) Bias
  bias <- Apply(data = list(exp, obs),
                target_dims = c(time_dim, dat_dim),
                fun = .Bias, 
                time_dim = time_dim,
                dat_dim = dat_dim, 
                na.rm = na.rm,
                absolute = absolute,
                time_mean = time_mean,
                alpha = alpha,
                ncores = ncores)

  if (is.null(alpha)) {
    bias <- bias$output1
  }
  return(bias)
}


.Bias <- function(exp, obs, time_dim = 'sdate', dat_dim = NULL, na.rm = FALSE, 
                  absolute = FALSE, time_mean = TRUE, alpha = NULL) {
  # exp and obs: [sdate, (dat)]
  if (is.null(dat_dim)) {
    bias <- exp - obs
    
    if (isTRUE(absolute)) {
      bias <- abs(bias)
    } 
    
    if (isTRUE(time_mean)) {
      bias <- mean(bias, na.rm = na.rm)
    }
    
    if (!is.null(alpha)) {
      if (!absolute) {
        if (all(is.na(bias))) {
          sign <- NA
        } else {
          pval <- t.test(x = obs, y = exp, alternative = "two.sided")$p.value
          sign <- pval <= alpha
        }
      }
    }
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
    bias <- array(dim = c(dim(exp)[time_dim], nexp = nexp, nobs = nobs))
    pval <- array(dim = c(nexp = nexp, nobs = nobs))
    sign <- array(dim = c(nexp = nexp, nobs = nobs))
    for (i in 1:nexp) {
      for (j in 1:nobs) {
        bias[, i, j] <-  exp[, i] - obs[, j]
        if (!is.null(alpha)) {
          if (!absolute) {
            pval[i, j] <- t.test(x = obs[, j], y = exp[, i],
                                 alternative = "two.sided")$p.value
            sign[i, j] <- pval[i, j] <= alpha
          }
        }
      }
    }

    if (isTRUE(absolute)) {
      bias <- abs(bias)
    }

    if (isTRUE(time_mean)) {
      bias <- MeanDims(bias, time_dim, na.rm = na.rm)
      if (!is.null(sign)) {
        sign[which(is.na(bias))] <- NA
      }
    }
  } 
  if (!is.null(alpha) && !absolute) {
    return(list(bias = bias, sign = sign))
  } else {
    return(bias)
  }
}
