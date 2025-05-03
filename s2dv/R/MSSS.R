#'Compute mean square error skill score
#'
#'Compute the mean square error skill score (MSSS) between an array of forecast 
#''exp' and an array of observation 'obs'. The two arrays should have the same
#'dimensions except along 'dat_dim' and 'memb_dim'. The MSSSs are computed along
#''time_dim', the dimension which corresponds to the start date dimension.
#'MSSS computes the mean square error skill score of each exp in 1:nexp 
#'against each obs in 1:nobs which gives nexp * nobs MSSS for each grid point 
#'of the array.\cr
#'The p-value and significance test are optionally provided by an one-sided 
#'Fisher test or Random Walk test.\cr
#'
#'@param exp A named numeric array of experimental data which contains at least
#'  time dimension (time_dim). It can also be a vector with the same length as
#'  'obs', then the vector will automatically be 'time_dim'.
#'@param obs A named numeric array of observational data which contains at least
#'  time dimension (time_dim). The dimensions should be the same as parameter
#'  'exp' except the length of 'dat_dim' and 'memb_dim' dimension. It can also 
#'  be a vector with the same length as 'exp', then the vector will 
#'  automatically be 'time_dim'.
#'@param ref A named numerical array of the reference forecast data with at 
#'  least time dimension, or 0 (typical climatological forecast) or 1 
#'  (normalized climatological forecast). If it is an array, the dimensions must
#'  be the same as 'exp' except 'memb_dim' and 'dat_dim'. If there is only one
#'  reference dataset, it should not have dataset dimension. If there is 
#'  corresponding reference for each experiment, the dataset dimension must 
#'  have the same length as in 'exp'. If 'ref' is NULL, the typical 
#'  climatological forecast is used as reference forecast (equivalent to 0.)
#'  The default value is NULL.
#'@param dat_dim A character string indicating the name of dataset (nobs/nexp) 
#'  dimension. The default value is NULL.
#'@param time_dim A character string indicating the name of dimension along  
#'  which the MSSS are computed. The default value is 'sdate'.
#'@param memb_dim A character string indicating the name of the member dimension
#'  to compute the ensemble mean; it should be set to NULL if the data are
#'  already the ensemble mean. The default value is NULL.
#'@param pval A logical value indicating whether to compute or not the p-value 
#'  of the test Ho: MSSS = 0. The default value is TRUE.
#'@param sign A logical value indicating whether to compute or not the 
#'  statistical significance of the test Ho: MSSS = 0. The default value is 
#'  FALSE.
#'@param alpha A numeric of the significance level to be used in the 
#'  statistical significance test. The default value is 0.05.
#'@param N.eff Effective sample size to be used in the statistical significance
#'  test with the Random Walk. It can be NA (and it will be computed with the 
#'  s2dv:::.Eno), FALSE (and it will use the length of "obs" along "time_dim", so the 
#'  autocorrelation is not taken into account), a numeric (which is used for 
#'  all cases), or an array with the same dimensions as "obs" except "time_dim"
#'  (for a particular N.eff to be used for each case). The default value is NA.
#'@param sig_method A character string indicating the significance method. The
#'  options are "one-sided Fisher" (default) and "Random Walk". 
#'@param sig_method.type A character string indicating the test type of the
#'  significance method. Check \code{RandomWalkTest()} parameter 
#'  \code{test.type} for details if parameter "sig_method" is "Random Walk". The
#'  default is NULL (since "one-sided Fisher" doesn't have different test 
#'  types.)
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A list containing the numeric arrays with dimension:\cr 
#'  c(nexp, nobs, all other dimensions of exp except time_dim).\cr
#'nexp is the number of experiment (i.e., dat_dim in exp), and nobs is the 
#'number of observation (i.e., dat_dim in obs). If dat_dim is NULL, nexp and 
#'nobs are omitted.\cr
#'\item{$msss}{
#'  A numerical array of the mean square error skill score. 
#'}
#'\item{$p.val}{
#'  A numerical array of the p-value with the same dimensions as $msss.
#'  Only present if \code{pval = TRUE}.
#'}
#'\item{sign}{
#'  A logical array of the statistical significance of the MSSS with the same
#'  dimensions as $msss. Only present if \code{sign = TRUE}.
#'}
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'rmsss <- MSSS(ano_exp, ano_obs, dat_dim = 'dataset', memb_dim = 'member')
#'
#'# Synthetic data:
#'exp <- array(rnorm(30), dim = c(dataset = 2, time = 3, memb = 5))
#'obs <- array(rnorm(15), dim = c(time = 3, dataset = 1))
#'res <- MSSS(exp, obs, time_dim = 'time', dat_dim = 'dataset', memb_dim = 'memb')
#'
#'@rdname MSSS
#'@import multiApply
#'@importFrom stats pf
#'@export
MSSS <- function(exp, obs, ref = NULL, time_dim = 'sdate', dat_dim = NULL,
                 memb_dim = NULL, pval = TRUE, sign = FALSE, alpha = 0.05, N.eff = NA,
                 sig_method = 'one-sided Fisher', sig_method.type = NULL, ncores = NULL) {
  
  # Check inputs 
  ## exp, obs, and ref (1)
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
  if (!is.null(ref)) {
    if (!is.numeric(ref)) {
      stop("Parameter 'ref' must be numeric.")
    }
    if (is.array(ref)) {
      if (any(is.null(names(dim(ref)))) | any(nchar(names(dim(ref))) == 0)) {
        stop("Parameter 'ref' must have dimension names.")
      }
    } else if (length(ref) != 1 | !all(ref %in% c(0, 1))) {
      stop("Parameter 'ref' must be a numeric array or number 0 or 1.")
    }
  } else {
    ref <- 0
    .warning("If a reference dataset is not provided (ref = NULL), the default ", 
            "value for the climatology is 0 and MSSS results will only be ",
            "correct if 'exp' and 'obs' are anomalies. Provide a non-null ",
            "'ref' for full-field data.")
  }
  if (!is.array(ref)) { # 0 or 1
    ref <- array(data = ref, dim = dim(exp))
  }
  
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(exp)) | !time_dim %in% names(dim(obs))) {
    stop("Parameter 'time_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string or NULL.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.",
           " Set it as NULL if there is no dataset dimension.")
    }
  }
  ## memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp))) {
      stop("Parameter 'memb_dim' is not found in 'exp' dimension.")
    }
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## sign
  if (!is.logical(sign) | length(sign) > 1) {
    stop("Parameter 'sign' must be one logical value.")
  }
  ## alpha
  if (!is.numeric(alpha) | length(alpha) > 1) {
    stop("Parameter 'alpha' must be one numeric value.")
  }
  ## N.eff
  if (is.array(N.eff)) {
    if (!is.numeric(N.eff)) stop("Parameter 'N.eff' must be numeric.")
    if (!all(names(dim(N.eff)) %in% names(dim(obs))) |
        any(dim(obs)[match(names(dim(N.eff)), names(dim(obs)))] != dim(N.eff))) {
      stop('If parameter "N.eff" is provided with an array, it must ',
           'have the same dimensions as "obs" except "time_dim".')
    }
  } else if (any((!is.na(N.eff) & !isFALSE(N.eff) & 
                  !is.numeric(N.eff)) | length(N.eff) != 1)) {
    stop('Parameter "N.eff" must be NA, FALSE, a numeric, or an array with ',
         'the same dimensions as "obs" except "time_dim".')
  }
  ## sig_method
  if (length(sig_method) != 1 | !any(sig_method %in% c('one-sided Fisher', 'Random Walk'))) {
    stop("Parameter 'sig_method' must be one of 'one-sided Fisher' or 'Random Walk'.")
  }
  ## sig_method.type
  if (sig_method == 'Random Walk') {
    if (is.null(sig_method.type)) {
      .warning("Parameter 'sig_method.type' must be specified if 'sig_method' is ",
               "Random Walk. Assign it as 'two.sided'.")
      sig_method.type <- "two.sided"
    }
    if (!any(sig_method.type %in% c('two.sided.approx', 'two.sided', 'greater', 'less'))) {
      stop("Parameter 'sig_method.type' must be a method accepted by RandomWalkTest() ",
           "parameter 'test.type'.")
    }
    if (sig_method.type == 'two.sided.approx' & pval == T) {
      .warning("p-value cannot be calculated by Random Walk 'two.sided.approx' method.")
      pval <- FALSE
      if (alpha != 0.05) {
        .warning("DelSole and Tippett (2016) aproximation is valid for alpha ",
                 "= 0.05 only. Returning the significance at the 0.05 significance level.")
        alpha <- 0.05
      }
    }
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 | length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 
  ## exp, obs, and ref (2)
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
  
  name_ref <- sort(names(dim(ref)))
  if (!is.null(memb_dim) && memb_dim %in% name_ref) {
    name_ref <- name_ref[-which(name_ref == memb_dim)]
  }
  if (!is.null(dat_dim)) {
    if (dat_dim %in% name_ref) {
      if (!identical(dim(exp)[dat_dim], dim(ref)[dat_dim])) {
        stop("If parameter 'ref' has dataset dimension, it must be ",
             "equal to dataset dimension of 'exp'.")
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

  if (dim(exp)[time_dim] <= 2) {
    stop("The length of time_dim must be more than 2 to compute MSSS.")
  }

  ###############################
  #  # Sort dimension
  #  name_exp <- names(dim(exp))
  #  name_obs <- names(dim(obs))
  #  order_obs <- match(name_exp, name_obs)
  #  obs <- Reorder(obs, order_obs)
  
  ###############################
  ## Ensemble mean
  if (!is.null(memb_dim)) {
    if (memb_dim %in% names(dim(exp))) {
      exp <- MeanDims(exp, memb_dim, na.rm = T)
    }
    if (memb_dim %in% names(dim(obs))) {
      obs <- MeanDims(obs, memb_dim, na.rm = T)
    }
    if (memb_dim %in% names(dim(ref))) {
      ref <- MeanDims(ref, memb_dim, na.rm = T)
    }
  }
  
  ###############################
  # Calculate MSSS
  
  data <- list(exp = exp, obs = obs, ref = ref)
  if (!is.null(dat_dim)) {
    if (dat_dim %in% names(dim(ref))) {
      target_dims <- list(exp = c(time_dim, dat_dim),
                          obs = c(time_dim, dat_dim),
                          ref = c(time_dim, dat_dim))
    } else {
      target_dims <- list(exp = c(time_dim, dat_dim),
                          obs = c(time_dim, dat_dim),
                          ref = c(time_dim))
    }
  } else {
    target_dims <- list(exp = time_dim, obs = time_dim, ref = time_dim)
  }
  
  if (is.array(N.eff)) {
    data$N.eff <- N.eff
    target_dims[length(target_dims)+1] <- list(NULL)
    res <- Apply(data, 
                 target_dims = target_dims,
                 fun = .MSSS, 
                 time_dim = time_dim, dat_dim = dat_dim,
                 pval = pval, sign = sign, alpha = alpha,
                 sig_method = sig_method, sig_method.type = sig_method.type,
                 ncores = ncores)
  } else {
    res <- Apply(data, 
                 target_dims = target_dims,
                 fun = .MSSS, 
                 time_dim = time_dim, dat_dim = dat_dim,
                 pval = pval, sign = sign, alpha = alpha, N.eff = N.eff,
                 sig_method = sig_method, sig_method.type = sig_method.type,
                 ncores = ncores)
  }
  
  return(res)
}

.MSSS <- function(exp, obs, ref = NULL, time_dim = 'sdate', dat_dim = NULL, pval = TRUE, 
                  sign = FALSE, alpha = 0.05, N.eff = NA, 
                  sig_method = 'one-sided Fisher', sig_method.type = NULL) {
  # exp: [sdate, (dat)]
  # obs: [sdate, (dat)]
  # ref: [sdate, (dat)] or NULL
  
  if (is.null(ref)) {
    ref <- array(data = 0, dim = dim(obs))
  } else if (identical(ref, 0) | identical(ref, 1)) {
    ref <- array(ref, dim = dim(exp))
  }
  
  if (is.null(dat_dim)) {
    # exp: [sdate]
    # obs: [sdate]
    nexp <- 1
    nobs <- 1
    nref <- 1
    # Add dat dim back temporarily
    dim(exp) <- c(dim(exp), dat = 1)
    dim(obs) <- c(dim(obs), dat = 1)
    dim(ref) <- c(dim(ref), dat = 1)
    
  } else {
    # exp: [sdate, dat_exp]
    # obs: [sdate, dat_obs]
    nexp <- as.numeric(dim(exp)[2])
    nobs <- as.numeric(dim(obs)[2])
    if (dat_dim %in% names(dim(ref))) {
      nref <- as.numeric(dim(ref)[2])
    } else {
      dim(ref) <- c(dim(ref), dat = 1)
      nref <- 1
    }
  }
  
  nsdate <- as.numeric(dim(exp)[1])
  
  # MSE of forecast
  dif1 <- array(dim = c(nsdate, nexp, nobs))
  names(dim(dif1)) <- c(time_dim, 'nexp', 'nobs')
  
  for (i in 1:nobs) {
    dif1[, , i] <- sapply(1:nexp, function(x) { 
                                    exp[, x] - obs[, i]
                                    })
  }
  
  mse_exp <- colMeans(dif1^2, na.rm = TRUE)  # [nexp, nobs]
  
  # MSE of reference
  dif2 <- array(dim = c(nsdate, nref, nobs))
  names(dim(dif2)) <- c(time_dim, 'nexp', 'nobs')
  for (i in 1:nobs) {
    dif2[, , i] <- sapply(1:nref, function(x) {
                                    ref[, x] - obs[, i]
                                    })
  }
  mse_ref <- colMeans(dif2^2, na.rm = TRUE)  # [nref, nobs]
  if (nexp != nref) {
    # expand mse_ref to nexp (nref is 1)
    mse_ref <- array(mse_ref, dim = c(nobs = nobs, nexp = nexp))
    mse_ref <- Reorder(mse_ref, c(2, 1))
  }

  msss <- 1 - mse_exp / mse_ref
  
  #################################################
  
  if (sig_method == 'one-sided Fisher') {
    p_val <- array(dim = c(nexp = nexp, nobs = nobs))
    ## pval and sign 
    if (pval || sign) {
      eno1 <- Eno(dif1, time_dim)
      if (is.null(ref)) {
        eno2 <- Eno(obs, time_dim) 
        eno2 <- array(eno2, dim = c(nobs = nobs, nexp = nexp))
        eno2 <- Reorder(eno2, c(2, 1))
      } else {
        eno2 <- Eno(dif2, time_dim)
        if (nref != nexp) {
          eno2 <- array(eno2, dim = c(nobs = nobs, nexp = nexp))
          eno2 <- Reorder(eno2, c(2, 1))
        }
      }
      
      F.stat <- (eno2 * mse_ref^2 / (eno2 - 1)) / ((eno1 * mse_exp^2 / (eno1 - 1)))
      tmp <- !is.na(eno1) & !is.na(eno2) & eno1 > 2 & eno2 > 2
      p_val <- 1 - pf(F.stat, eno1 - 1, eno2 - 1)
      if (sign) signif <- p_val <= alpha 
      # If there isn't enough valid data, return NA
      p_val[which(!tmp)] <- NA
      if (sign) signif[which(!tmp)] <- NA
      
      # change not enough valid data msss to NA
      msss[which(!tmp)] <- NA
    }
    
  } else if (sig_method == "Random Walk") {

    if (sign) signif <- array(dim = c(nexp = nexp, nobs = nobs))
    if (pval) p_val <- array(dim = c(nexp = nexp, nobs = nobs))

    for (i in 1:nexp) {
      for (j in 1:nobs) {        
        error_exp <- array(data = abs(exp[, i] - obs[, j]), dim = c(time = nsdate))
        if (nref == nexp) {
          error_ref <- array(data = abs(ref[, i] - obs[, j]), dim = c(time = nsdate))
        } else {
          # nref = 1
          error_ref <- array(data = abs(ref - obs[, j]), dim = c(time = nsdate))
        }
        if (is.na(N.eff)) {
          N.eff <- .Eno(x = obs[, j], na.action = na.pass) ## effective degrees of freedom
        }
        aux <- .RandomWalkTest(skill_A = error_exp, skill_B = error_ref, 
                               test.type = sig_method.type, N.eff = N.eff,
                               pval = pval, sign = sign, alpha = alpha)
        if (sign) signif[i, j] <- aux$sign
        if (pval) p_val[i, j] <- aux$p.val
      }
    }
  }
  
  ###################################
  # Remove extra dimensions if dat_dim = NULL
  if (is.null(dat_dim)) {
    dim(msss) <- NULL
    if (pval) dim(p_val) <- NULL
    if (sign) dim(signif) <- NULL
  }
  ###################################
  
  # output  
  res <- list(msss = msss)
  if (pval) res <- c(res, list(p.val = p_val))
  if (sign) res <- c(res, list(sign = signif))
  
  return(res)
}
