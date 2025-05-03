#'Compute the Absolute Mean Bias Skill Score
#'
#'The Absolute Mean Bias Skill Score is based on the Absolute Mean Error (Wilks,
#' 2011) between the ensemble mean forecast and the observations. It measures 
#'the accuracy of the forecast in comparison with a reference forecast to assess
#'whether the forecast presents an improvement or a worsening with respect to 
#'that reference. The Mean Bias Skill Score ranges between minus infinite and 1.
#'Positive values indicate that the forecast has higher skill than the reference
#'forecast, while negative values indicate that it has a lower skill. Examples
#'of reference forecasts are the climatological forecast (average of the 
#'observations), a previous model version, or another model. It is computed as
#'\code{AbsBiasSS = 1 - AbsBias_exp / AbsBias_ref}. The statistical significance
#'is obtained based on a Random Walk test at the confidence level specified
#'(DelSole and Tippett, 2016). If there is more than one dataset, the result 
#'will be computed for each pair of exp and obs data.
#'
#'@param exp A named numerical array of the forecast with at least time 
#'  dimension.  
#'@param obs A named numerical array of the observation with at least time 
#'  dimension. The dimensions must be the same as 'exp' except 'memb_dim' and 
#'  'dat_dim'.
#'@param ref A named numerical array of the reference forecast data with at 
#'  least time dimension. The dimensions must be the same as 'exp' except
#'  'memb_dim' and 'dat_dim'. If there is only one reference dataset, it should
#'  not have dataset dimension. If there is corresponding reference for each
#'  experiement, the dataset dimension must have the same length as in 'exp'. If
#'  'ref' is NULL, the climatological forecast is used as reference forecast.
#'  The default value is NULL.
#'@param time_dim A character string indicating the name of the time dimension.
#'  The default value is 'sdate'.
#'@param memb_dim A character string indicating the name of the member dimension
#'  to compute the ensemble mean; it should be set to NULL if the parameter 'exp'
#'  and 'ref' are already the ensemble mean. The default value is NULL.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  The length of this dimension can be different between 'exp' and 'obs'. 
#'  The default value is NULL.
#'@param na.rm A logical value indicating if NAs should be removed (TRUE) or
#'  kept (FALSE) for computation. The default value is FALSE.
#'@param sig_method.type A character string indicating the test type of the
#'  significance method. Check \code{RandomWalkTest()} parameter 
#'  \code{test.type} for details. The default is 'two.sided.approx', which is 
#'  the default of \code{RandomWalkTest()}.
#'@param alpha A numeric of the significance level to be used in the statistical
#'  significance test. The default value is 0.05.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'\item{$biasSS}{
#'  A numerical array of BiasSS with dimensions nexp, nobs and the rest 
#'  dimensions of 'exp' except 'time_dim' and 'memb_dim'.
#'}
#'\item{$sign}{
#'  A logical array of the statistical significance of the BiasSS
#'  with the same dimensions as $biasSS. nexp is the number of 
#'  experiment (i.e., 'dat_dim' in exp), and nobs is the number of observation 
#'  (i.e., 'dat_dim' in obs). If dat_dim is NULL, nexp and nobs are omitted.
#'}
#'
#'@references 
#'Wilks, 2011; https://doi.org/10.1016/B978-0-12-385022-5.00008-7
#'DelSole and Tippett, 2016; https://doi.org/10.1175/MWR-D-15-0218.1
#'
#'@examples
#'exp <- array(rnorm(1000), dim = c(dat = 1, lat = 3, lon = 5, member = 10, sdate = 50))
#'ref <- array(rnorm(1000), dim = c(dat = 1, lat = 3, lon = 5, member = 10, sdate = 50))
#'obs <- array(rnorm(1000), dim = c(dat = 1, lat = 3, lon = 5, sdate = 50))
#'biasSS1 <- AbsBiasSS(exp = exp, obs = obs, ref = ref, memb_dim = 'member')
#'biasSS2 <- AbsBiasSS(exp = exp, obs = obs, ref = NULL, memb_dim = 'member')
#'
#'@import multiApply
#'@export
AbsBiasSS <- function(exp, obs, ref = NULL, time_dim = 'sdate', memb_dim = NULL, 
                      dat_dim = NULL, na.rm = FALSE, sig_method.type = 'two.sided.approx', 
                      alpha = 0.05, ncores = NULL) {
  
  # Check inputs
  ## exp, obs, and ref (1)
  if (!is.array(exp) | !is.numeric(exp)) {
    stop("Parameter 'exp' must be a numeric array.")
  }
  if (!is.array(obs) | !is.numeric(obs)) {
    stop("Parameter 'obs' must be a numeric array.")
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  if (!is.null(ref)) {
    if (!is.array(ref) | !is.numeric(ref))
      stop("Parameter 'ref' must be a numeric array.")
    if (any(is.null(names(dim(ref)))) | any(nchar(names(dim(ref))) == 0)) {
      stop("Parameter 'ref' must have dimension names.")
    }
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' or 'obs' dimension.")
  }
  if (!is.null(ref) & !time_dim %in% names(dim(ref))) {
    stop("Parameter 'time_dim' is not found in 'ref' dimension.")
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
  ## exp, obs, and ref (2)
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
  if (!is.null(ref)) {
    name_ref <- sort(names(dim(ref)))
    if (!is.null(memb_dim) && memb_dim %in% name_ref) {
      name_ref <- name_ref[-which(name_ref == memb_dim)]
    }
    if (!is.null(dat_dim)) {
      if (dat_dim %in% name_ref) {
        if (!identical(dim(exp)[dat_dim], dim(ref)[dat_dim])) {
          stop("If parameter 'ref' has dataset dimension, it must be", 
               " equal to dataset dimension of 'exp'.")
        }
        name_ref <- name_ref[-which(name_ref == dat_dim)]
      }
    }
    if (!identical(length(name_exp), length(name_ref)) |
        !identical(dim(exp)[name_exp], dim(ref)[name_ref])) {
      stop("Parameter 'exp' and 'ref' must have the same length of ",
           "all dimensions except 'memb_dim' and 'dat_dim' if there is ",
           "only one reference dataset.")
    }
  }
  ## na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  ## alpha
  if (any(!is.numeric(alpha) | alpha <= 0 | alpha >= 1 | length(alpha) > 1)) {
    stop("Parameter 'alpha' must be a number between 0 and 1.")
  }
  ## sig_method.type
  #NOTE: These are the types of RandomWalkTest()
  if (!sig_method.type %in% c('two.sided.approx', 'two.sided', 'greater', 'less')) {
    stop("Parameter 'sig_method.type' must be 'two.sided.approx', 'two.sided', ",
         "'greater', or 'less'.")
  }
  if (sig_method.type == 'two.sided.approx' && alpha != 0.05) {
    .warning("DelSole and Tippett (2016) aproximation is valid for alpha ",
             "= 0.05 only. Returning the significance at the 0.05 significance level.")
  }

  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
        length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    }
  }  
 
 ############################
 
  ## Ensemble mean
  if (!is.null(memb_dim)) {
    exp <- MeanDims(exp, memb_dim, na.rm = na.rm)
    if (!is.null(ref) & memb_dim %in% names(dim(ref))) {
      ref <- MeanDims(ref, memb_dim, na.rm = na.rm)
    }
  }
  
  ## Mean bias skill score
  if (!is.null(ref)) { # use "ref" as reference forecast
    if (!is.null(dat_dim) && dat_dim %in% names(dim(ref))) {
      target_dims_ref <- c(time_dim, dat_dim)
    } else {
      target_dims_ref <- c(time_dim)
    }
    data <- list(exp = exp, obs = obs, ref = ref)
    target_dims = list(exp = c(time_dim, dat_dim),
                       obs = c(time_dim, dat_dim),
                       ref = target_dims_ref)
  } else {
    data <- list(exp = exp, obs = obs)
    target_dims = list(exp = c(time_dim, dat_dim),
                       obs = c(time_dim, dat_dim))
  }

  output <- Apply(data,
                  target_dims = target_dims,
                  fun = .AbsBiasSS,
                  dat_dim = dat_dim, 
                  na.rm = na.rm, alpha = alpha, sig_method.type = sig_method.type, 
                  ncores = ncores)

  return(output)
}

.AbsBiasSS <- function(exp, obs, ref = NULL, dat_dim = NULL, na.rm = FALSE,
                       sig_method.type = 'two.sided.approx', alpha = 0.05) {
  # exp and obs: [sdate, (dat_dim)]
  # ref: [sdate, (dat_dim)] or NULL

  # Adjust exp, obs, ref to have dat_dim temporarily
  if (is.null(dat_dim)) {
    nexp <- 1
    nobs <- 1
    exp <- InsertDim(exp, posdim = 2, lendim = 1, name = 'dataset')
    obs <- InsertDim(obs, posdim = 2, lendim = 1, name = 'dataset')
    if (!is.null(ref)) {
      ref <- InsertDim(ref, posdim = 2, lendim = 1, name = 'dataset')
    }
    ref_dat_dim <- FALSE
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
    if (length(dim(ref)) == 1) { # ref: [sdate]
      ref_dat_dim <- FALSE
    } else {
      ref_dat_dim <- TRUE
    }
  }

  biasSS <- array(dim = c(nexp = nexp, nobs = nobs))
  sign <- array(dim = c(nexp = nexp, nobs = nobs))

  for (i in 1:nexp) {
    exp_data <- exp[, i]
    if (isTRUE(ref_dat_dim)) {
      ref_data <- ref[, i]
    } else {
      ref_data <- ref
    }
    for (j in 1:nobs) {
      obs_data <- obs[, j]

      if (isTRUE(na.rm)) {
        if (is.null(ref)) {
          good_values <- !is.na(exp_data) & !is.na(obs_data)
          exp_data <- exp_data[good_values]
          obs_data <- obs_data[good_values]
        } else {
          good_values <- !is.na(exp_data) & !is.na(ref_data) & !is.na(obs_data)
          exp_data <- exp_data[good_values]
          ref_data <- ref_data[good_values]
          obs_data <- obs_data[good_values]
        }
      }
      
      ## Bias of the exp
      bias_exp <- .Bias(exp = exp_data, obs = obs_data, na.rm = na.rm, 
                        absolute = TRUE, time_mean = FALSE)
      ## Bias of the ref
      if (is.null(ref)) { ## Climatological forecast
        ref_data <- rep(mean(obs_data, na.rm = na.rm), length(obs_data))
      }
      bias_ref <- .Bias(exp = ref_data, obs = obs_data, na.rm = na.rm, 
                        absolute = TRUE, time_mean = FALSE)
      ## Skill score and significance
      biasSS[i, j] <- 1 - mean(bias_exp) / mean(bias_ref)
      sign[i, j] <- .RandomWalkTest(skill_A = bias_exp, skill_B = bias_ref, 
                                    test.type = sig_method.type, alpha = alpha,
                                    sign = T, pval = F)$sign
    }
  }

  if (is.null(dat_dim)) {
    dim(biasSS) <- NULL
    dim(sign) <- NULL
  }
  
  
  return(list(biasSS = biasSS, sign = sign))
}
