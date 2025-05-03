#'Bias Correction based on the mean and standard deviation adjustment
#'
#'@author Verónica Torralba, \email{veronica.torralba@bsc.es}
#'@description This function applies the simple bias adjustment technique 
#'described in Torralba et al. (2017). The adjusted forecasts have an equivalent 
#'standard deviation and mean to that of the reference dataset.
#'
#'@param exp An object of class \code{s2dv_cube} as returned by \code{CST_Start} 
#'  function, containing the seasonal forecast experiment data in the element 
#'  named \code{$data} with at least time and member dimensions.
#'@param obs An object of class \code{s2dv_cube} as returned by \code{CST_Start} 
#'  function, containing the observed data in the element named \code{$data} 
#'  with at least time dimension.
#'@param exp_cor An object of class \code{s2dv_cube} as returned by 
#'  \code{CST_Start} function, containing the seasonal forecast experiment to be 
#'  corrected with at least time dimension. If it is NULL, the 'exp' forecast 
#'  will be corrected. If there is only one corrected dataset, it should not  
#'  have dataset dimension. If there is a corresponding corrected dataset for  
#'  each 'exp' forecast, the dataset dimension must have the same length as in 
#'  'exp'. The default value is NULL.
#'@param na.rm A logical value indicating whether missing values should be 
#'  stripped before the computation proceeds, by default it is set to FALSE.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. By default, it is set to 'member'.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  The length of this dimension can be different between 'exp' and 'obs'. 
#'  The default value is NULL.
#'@param ncores An integer that indicates the number of cores for parallel 
#'  computations using multiApply function. The default value is NULL.
#'@return An object of class \code{s2dv_cube} containing the bias corrected 
#'forecasts with the dimensions nexp, nobs and same dimensions as in the 'exp' 
#'object. nexp is the number of experiment (i.e., 'dat_dim' in exp), and nobs is 
#'the number of observation (i.e., 'dat_dim' in obs). If dat_dim is NULL, nexp 
#'and nobs are omitted. If 'exp_cor' is provided the returned array will be with 
#'the same dimensions as 'exp_cor'.
#' 
#'@references Torralba, V., F.J. Doblas-Reyes, D. MacLeod, I. Christel and M. 
#'Davis (2017). Seasonal climate prediction: a new source of information for 
#'the management of wind energy resources. Journal of Applied Meteorology and 
#'Climatology, 56, 1231-1247, \doi{10.1175/JAMC-D-16-0204.1}. (CLIM4ENERGY, 
#'EUPORIAS, NEWA, RESILIENCE, SPECS)
#'
#'@examples
#'mod1 <- 1 : (1 * 3 * 4 * 5 * 6 * 7)
#'dim(mod1) <- c(dataset = 1, member = 3, sdate = 4, time = 5, lat = 6, lon = 7)
#'obs1 <- 1 : (1 * 1 * 4 * 5 * 6 * 7)
#'dim(obs1) <- c(dataset = 1, member = 1, sdate = 4, time = 5, lat = 6, lon = 7)
#'lon <- seq(0, 30, 5)
#'lat <- seq(0, 25, 5)
#'coords <- list(lat = lat, lon = lon)
#'exp <- list(data = mod1, coords = coords)
#'obs <- list(data = obs1, coords = coords)
#'attr(exp, 'class') <- 's2dv_cube'
#'attr(obs, 'class') <- 's2dv_cube'
#'a <- CST_BiasCorrection(exp = exp, obs = obs)
#'@import multiApply
#'@export
CST_BiasCorrection <- function(exp, obs, exp_cor = NULL, na.rm = FALSE, 
                               memb_dim = 'member', sdate_dim = 'sdate', 
                               dat_dim = NULL, ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(exp, 's2dv_cube') || !inherits(obs, 's2dv_cube')) {
    stop("Parameter 'exp' and 'obs' must be of the class 's2dv_cube'.")
  }
  if (!is.null(exp_cor)) {
    if (!inherits(exp_cor, 's2dv_cube')) {
      stop("Parameter 'exp_cor' must be of the class 's2dv_cube'.")
    }
  }

  BiasCorrected <- BiasCorrection(exp = exp$data, obs = obs$data, exp_cor = exp_cor$data, 
                                  memb_dim = memb_dim, sdate_dim = sdate_dim, dat_dim = dat_dim, 
                                  na.rm = na.rm, ncores = ncores)

  if (is.null(exp_cor)) {
    exp$data <- BiasCorrected
    exp$attrs$Datasets <- c(exp$attrs$Datasets, obs$attrs$Datasets)
    exp$attrs$source_files <- c(exp$attrs$source_files, obs$attrs$source_files)
    
    return(exp)

  } else {
    exp_cor$data <- BiasCorrected
    exp_cor$attrs$Datasets <- c(exp_cor$attrs$Datasets, exp$attrs$Datasets, obs$attrs$Datasets)
    exp_cor$attrs$source_files <- c(exp_cor$attrs$source_files, exp$attrs$source_files, obs$attrs$source_files)

    return(exp_cor)
  }
}

#'Bias Correction based on the mean and standard deviation adjustment
#'
#'@author Verónica Torralba, \email{veronica.torralba@bsc.es}
#'@description This function applies the simple bias adjustment technique 
#'described in Torralba et al. (2017). The adjusted forecasts have an equivalent 
#'standard deviation and mean to that of the reference dataset.
#'
#'@param exp A multidimensional array with named dimensions containing the 
#'  seasonal forecast experiment data with at least time and member dimensions.
#'@param obs A multidimensional array with named dimensions containing the 
#'  observed data with at least time dimension.
#'@param exp_cor A multidimensional array with named dimensions containing the 
#'  seasonal forecast experiment to be corrected with at least time and member 
#'  dimension. If it is NULL, the 'exp' forecast will be corrected. If there is 
#'  only one corrected dataset, it should not have dataset dimension. If there 
#'  is a corresponding corrected dataset for each 'exp' forecast, the dataset 
#'  dimension must have the same length as in 'exp'. The default value is NULL.
#'@param na.rm A logical value indicating whether missing values should be 
#'  stripped before the computation proceeds, by default it is set to FALSE.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. By default, it is set to 'member'.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  The length of this dimension can be different between 'exp' and 'obs'. 
#'  The default value is NULL.
#'@param ncores An integer that indicates the number of cores for parallel 
#'  computations using multiApply function. The default value is NULL.
#'
#'@return An array containing the bias corrected forecasts with the dimensions 
#'nexp, nobs and same dimensions as in the 'exp' object. nexp is the number of 
#'experiment (i.e., 'dat_dim' in exp), and nobs is the number of observation 
#'(i.e., 'dat_dim' in obs). If dat_dim is NULL, nexp and nobs are omitted. If 
#''exp_cor' is provided the returned array will be with the same dimensions as 
#''exp_cor'.
#'
#'@references Torralba, V., F.J. Doblas-Reyes, D. MacLeod, I. Christel and M. 
#'Davis (2017). Seasonal climate prediction: a new source of information for the 
#'management of wind energy resources. Journal of Applied Meteorology and 
#'Climatology, 56, 1231-1247, \doi{10.1175/JAMC-D-16-0204.1}. (CLIM4ENERGY, 
#'EUPORIAS, NEWA, RESILIENCE, SPECS)
#'
#'@examples
#'mod1 <- 1 : (1 * 3 * 4 * 5 * 6 * 7)
#'dim(mod1) <- c(dataset = 1, member = 3, sdate = 4, time = 5, lat = 6, lon = 7)
#'obs1 <- 1 : (1 * 1 * 4 * 5 * 6 * 7)
#'dim(obs1) <- c(dataset = 1, member = 1, sdate = 4, time = 5, lat = 6, lon = 7)
#'a <- BiasCorrection(exp = mod1, obs = obs1)
#'@import multiApply
#'@export
BiasCorrection <- function(exp, obs, exp_cor = NULL, na.rm = FALSE,
                           memb_dim = 'member', sdate_dim = 'sdate',
                           dat_dim = NULL, ncores = NULL) {
  # Check inputs
  ## exp, obs
  if (!is.array(exp) || !is.numeric(exp)) {
    stop("Parameter 'exp' must be a numeric array.")
  }
  if (!is.array(obs) || !is.numeric(obs)) {
    stop("Parameter 'obs' must be a numeric array.")
  }
  obsdims <- names(dim(obs))
  expdims <- names(dim(exp))
  if (is.null(expdims)) {
    stop("Parameter 'exp' must have dimension names.")
  }
  if (is.null(obsdims)) {
    stop("Parameter 'obs' must have dimension names.")
  }
  if (any(is.na(exp)))  {
    warning("Parameter 'exp' contains NA values.")
  }
  if (any(is.na(obs)))  {
    warning("Parameter 'obs' contains NA values.")
  }
  ## exp_cor
  if (!is.null(exp_cor)) {
    exp_cordims <- names(dim(exp_cor))
    if (is.null(exp_cordims)) {
      stop("Parameter 'exp_cor' must have dimension names.")
    }
  }
  ## sdate_dim, memb_dim
  if (!is.character(sdate_dim) || length(sdate_dim) != 1) {
    stop("Parameter 'sdate_dim' must be a character string.")
  }
  if (!sdate_dim %in% expdims || !sdate_dim %in% obsdims) {
    stop("Parameter 'sdate_dim' is not found in 'exp' or 'obs' dimension.")
  }
  if (dim(exp)[sdate_dim] == 1) {
    stop("Parameter 'exp' must have dimension length of 'sdate_dim' bigger than 1.")
  }
  if (!all(c(memb_dim, sdate_dim) %in% expdims)) {
    stop("Parameter 'exp' requires 'sdate_dim' and 'memb_dim' dimensions.")  
  }
  if (memb_dim %in% obsdims) {
    if (dim(obs)[memb_dim] != 1) {
      stop("If parameter 'obs' has dimension 'memb_dim' its length must be equal to 1.")
    }
  } else {
    obs <- InsertDim(obs, posdim = 1, lendim = 1, name = memb_dim)
  }
  if (!is.null(exp_cor)) {
    if (!memb_dim %in% names(dim(exp_cor))) {
      exp_cor <- InsertDim(exp_cor, posdim = 1, lendim = 1, name = memb_dim)
      exp_cor_remove_memb <- TRUE
    } else {
      exp_cor_remove_memb <- FALSE
    }
  } else {
    exp_cor_remove_memb <- FALSE
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
  ## exp, obs, and exp_cor (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  name_exp <- name_exp[-which(name_exp == memb_dim)]
  name_obs <- name_obs[-which(name_obs == memb_dim)]
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!identical(length(name_exp), length(name_obs)) |
      !identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of all dimensions", 
         " except 'memb_dim' and 'dat_dim'.")
  }
  if (!is.null(exp_cor)) {
    name_exp_cor <- sort(names(dim(exp_cor)))
    name_exp <- sort(names(dim(exp)))
    if (!is.null(dat_dim)) {
      if (dat_dim %in% exp_cordims) {
        if (!identical(dim(exp)[dat_dim], dim(exp_cor)[dat_dim])) {
          stop("If parameter 'exp_cor' has dataset dimension, it must be", 
               " equal to dataset dimension of 'exp'.")
        }
        name_exp_cor <- name_exp_cor[-which(name_exp_cor == dat_dim)]
        target_dims_cor <- c(memb_dim, sdate_dim, dat_dim)
      } else {
        target_dims_cor <- c(memb_dim, sdate_dim)
      }
    } else {
      target_dims_cor <- c(memb_dim, sdate_dim)
    }
    name_exp <- name_exp[-which(name_exp %in% c(memb_dim, sdate_dim, dat_dim))]
    name_exp_cor <- name_exp_cor[-which(name_exp_cor %in% target_dims_cor)]
    if (!identical(length(name_exp), length(name_exp_cor)) |
        !identical(dim(exp)[name_exp], dim(exp_cor)[name_exp_cor])) {
      stop("Parameter 'exp' and 'exp_cor' must have the same length of ",
           "all common dimensions except 'dat_dim', 'sdate_dim' and 'memb_dim'.")
    }
  }
  ## na.rm
  if (!is.logical(na.rm))  {
    na.rm <- FALSE
    warning("Paramater 'na.rm' must be a logical, it has been set to FALSE.")
  }
  if (length(na.rm) > 1) {
    na.rm <- na.rm[1]
    warning("Paramter 'na.rm' has length greater than 1, and only the fist element is used.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    }
  }
  
  if (is.null(exp_cor)) {
    BiasCorrected <- Apply(data = list(var_obs = obs, var_exp = exp),
                           target_dims = list(c(memb_dim, sdate_dim, dat_dim),
                                              c(memb_dim, sdate_dim, dat_dim)),
                           fun = .sbc, dat_dim = dat_dim, 
                           na.rm = na.rm, ncores = ncores)$output1
  } else {
    BiasCorrected <- Apply(data = list(var_obs = obs,
                                       var_exp = exp,
                                       var_cor = exp_cor),
                           target_dims = list(c(memb_dim, sdate_dim, dat_dim),
                                              c(memb_dim, sdate_dim, dat_dim),
                                              target_dims_cor),
                           fun = .sbc, dat_dim = dat_dim, 
                           na.rm = na.rm, ncores = ncores)$output1
  }
  if (!is.null(dat_dim)) {
    pos <- match(c(names(dim(exp))[-which(names(dim(exp)) == dat_dim)], 'nexp', 'nobs'), 
                   names(dim(BiasCorrected)))
    BiasCorrected <- aperm(BiasCorrected, pos)
  } else {
    pos <- match(c(names(dim(exp))), names(dim(BiasCorrected)))
    BiasCorrected <- aperm(BiasCorrected, pos)
  }

  if (exp_cor_remove_memb) {
    dim(BiasCorrected) <- dim(BiasCorrected)[-which(names(dim(BiasCorrected)) == memb_dim)]
  }

  return(BiasCorrected)
}

.sbc <- function(var_obs, var_exp, var_cor = NULL, dat_dim = NULL, na.rm = FALSE) {

  # exp: [memb, sdate, (dat)]
  # obs: [memb, sdate, (dat)]
  # ref: [memb, sdate, (dat)] or NULL

  if (is.null(dat_dim)) {
    nexp <- 1
    nobs <- 1
    var_exp <- InsertDim(var_exp, posdim = 3, lendim = 1, name = 'dataset')
    var_obs <- InsertDim(var_obs, posdim = 3, lendim = 1, name = 'dataset')
    if (!is.null(var_cor)) {
      var_cor <- InsertDim(var_cor, posdim = 3, lendim = 1, name = 'dataset')
    }
  } else {
    nexp <- as.numeric(dim(var_exp)[dat_dim])
    nobs <- as.numeric(dim(var_obs)[dat_dim])
  }

  if (!is.null(var_cor)) {
    if (length(dim(var_cor)) == 2) { # ref: [memb, sdate]
      cor_dat_dim <- FALSE
    } else {                         # ref: [memb, sdate, dat]
      cor_dat_dim <- TRUE
    }
    corrected <- array(dim = c(dim(var_cor)[1:2], nexp = nexp, nobs = nobs))
  } else {
    ntime <- dim(var_exp)[2]
    corrected <- array(dim = c(dim(var_exp)[1:2], nexp = nexp, nobs = nobs))
  }

  for (i in 1:nexp) {
    for (j in 1:nobs) {
      if (is.null(var_cor)) {
        for (t in 1:ntime) {
          # parameters
          sd_obs <- sd(var_obs[, -t, j], na.rm = na.rm)
          sd_exp <- sd(var_exp[, -t, i], na.rm = na.rm)
          clim_exp <- mean(var_exp[, -t, i], na.rm = na.rm)
          clim_obs <- mean(var_obs[, -t, j], na.rm = na.rm)
          
          # bias corrected forecast
          corrected[, t, i, j] <- ((var_exp[, t, i] - clim_exp) * (sd_obs / sd_exp)) + clim_obs
        }
      } else {
        # parameters
        sd_obs <- sd(var_obs[, , j], na.rm = na.rm)
        sd_exp <- sd(var_exp[, , i], na.rm = na.rm)
        clim_exp <- mean(var_exp[, , i], na.rm = na.rm)
        clim_obs <- mean(var_obs[, , j], na.rm = na.rm)
        
        # bias corrected forecast
        if (cor_dat_dim) {
          corrected[, , i, j] <- ((var_cor[, , i] - clim_exp) * (sd_obs / sd_exp)) + clim_obs
        } else {
          corrected[, , i, j] <- ((var_cor - clim_exp) * (sd_obs / sd_exp)) + clim_obs
        }
      }
    }
  }

  if (is.null(dat_dim)) {
    if (!is.null(var_cor)) {
      dim(corrected) <- dim(var_cor)[1:2]
    } else {
      dim(corrected) <- dim(var_exp)[1:2]
    }
  }

  return(corrected)
}
