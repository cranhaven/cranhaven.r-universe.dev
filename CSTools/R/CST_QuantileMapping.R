#'Quantile Mapping for seasonal or decadal forecast data
#'
#'@description This function is a wrapper of fitQmap and doQmap from package
#''qmap' to be applied on the object of class 's2dv_cube'. The quantile mapping
#'adjustment between an experiment, typically a hindcast, and observation is 
#'applied to the experiment itself or to a provided forecast.

#'@author Nuria Perez-Zanon, \email{nuria.perez@bsc.es}
#'@param exp An object of class \code{s2dv_cube}.
#'@param obs An object of class \code{s2dv_cube}.
#'@param exp_cor An object of class \code{s2dv_cube} in which the quantile 
#'  mapping correction should be applied. If it is not specified, the correction
#'  is applied in object 'exp'.
#'@param sdate_dim A character string indicating the dimension name in which 
#'  cross-validation would be applied when exp_cor is not provided. 'sdate' by 
#'  default.
#'@param memb_dim A character string indicating the dimension name where
#'  ensemble members are stored in the experimental arrays. It can be NULL if 
#'  there is no ensemble member dimension. It is set as 'member' by default.
#'@param window_dim A character string indicating the dimension name where 
#'  samples have been stored. It can be NULL (default) in case all samples are 
#'  used. 
#'@param method A character string indicating the method to be used:'PTF', 
#'  'DIST', 'RQUANT', 'QUANT', 'SSPLIN'. By default, the empirical quantile 
#'  mapping 'QUANT' is used.
#'@param na.rm A logical value indicating if missing values should be removed   
#'  (FALSE by default).
#'@param ncores An integer indicating the number of cores for parallel 
#'  computation using multiApply function. The default value is NULL (1). 
#'@param ... Additional parameters to be used by the method choosen. See qmap 
#'  package for details.
#'
#'@return An object of class \code{s2dv_cube} containing the experimental data
#'after applying the quantile mapping correction.
#'
#'@seealso \code{\link[qmap]{fitQmap}} and \code{\link[qmap]{doQmap}} 
#'@examples
#'# Use synthetic data
#'exp <- NULL
#'exp$data <- 1 : c(1 * 3 * 5 * 4 * 3 * 2)
#'dim(exp$data) <- c(dataset = 1, member = 3, sdate = 5, ftime = 4,
#'                   lat = 3, lon = 2)
#'class(exp) <- 's2dv_cube'
#'obs <- NULL
#'obs$data <- 101 : c(100 + 1 * 1 * 5 * 4 * 3 * 2)
#'dim(obs$data) <- c(dataset = 1, member = 1, sdate = 5, ftime = 4,
#'                   lat = 3, lon = 2)
#'class(obs) <- 's2dv_cube'
#'res <- CST_QuantileMapping(exp, obs)
#'
#'@import qmap 
#'@import multiApply 
#'@import s2dv
#'@export
CST_QuantileMapping <- function(exp, obs, exp_cor = NULL, sdate_dim = 'sdate',
                                memb_dim = 'member', window_dim = NULL, 
                                method = 'QUANT', na.rm = FALSE, 
                                ncores = NULL, ...) {
  # Check 's2dv_cube'
  if (!inherits(exp, 's2dv_cube') || !inherits(obs, 's2dv_cube')) {
    stop("Parameter 'exp' and 'obs' must be of the class 's2dv_cube'.")
  }
  if (!is.null(exp_cor)) {
    if (!inherits(exp_cor, 's2dv_cube')) {
      stop("Parameter 'exp_cor' must be of the class 's2dv_cube'.")
    }
  }

  QMapped <- QuantileMapping(exp = exp$data, obs = obs$data, 
                             exp_cor = exp_cor$data,
                             sdate_dim = sdate_dim, memb_dim = memb_dim,
                             window_dim = window_dim, method = method,
                             na.rm = na.rm, ncores = ncores, ...)
  if (is.null(exp_cor)) {
    exp$data <- QMapped
    exp$attrs$Datasets <- c(exp$attrs$Datasets, obs$attrs$Datasets)
    exp$attrs$source_files <- c(exp$attrs$source_files, obs$attrs$source_files)
    return(exp)
  } else {
    exp_cor$data <- QMapped
    exp_cor$attrs$Datasets <- c(exp_cor$attrs$Datasets, exp$attrs$Datasets, 
                                obs$attrs$Datasets)
    exp_cor$attrs$source_files <- c(exp_cor$attrs$source_files, exp$attrs$source_files, 
                                    obs$attrs$source_files)
    return(exp_cor)
  }
}

#'Quantile Mapping for seasonal or decadal forecast data
#'
#'@description This function is a wrapper of fitQmap and doQmap from package
#''qmap' to be applied on multi-dimensional arrays. The quantile mapping 
#'adjustment between an experiment, typically a hindcast, and observation is 
#'applied to the experiment itself or to a provided forecast.
#'
#'@author Nuria Perez-Zanon, \email{nuria.perez@bsc.es}
#'@param exp A multidimensional array with named dimensions containing the 
#'  hindcast. 
#'@param obs A multidimensional array with named dimensions containing the 
#'  reference dataset.
#'@param exp_cor A multidimensional array with named dimensions in which the
#'  quantile mapping correction should be applied. If it is not specified, the 
#'  correction is applied on object 'exp'.
#'@param sdate_dim A character string indicating the dimension name in which
#'  cross-validation would be applied when exp_cor is not provided. 'sdate' by
#'  default.
#'@param memb_dim A character string indicating the dimension name where
#'  ensemble members are stored in the experimental arrays. It can be NULL if 
#'  there is no ensemble member dimension. It is set as 'member' by default.
#'@param window_dim A character string indicating the dimension name where 
#'  samples have been stored. It can be NULL (default) in case all samples are
#'  used. 
#'@param method A character string indicating the method to be used: 'PTF',
#'  'DIST', 'RQUANT', 'QUANT', 'SSPLIN'. By default, the empirical quantile 
#'  mapping 'QUANT' is used. 
#'@param na.rm A logical value indicating if missing values should be removed  
#'  (FALSE by default). 
#'@param ncores An integer indicating the number of cores for parallel 
#'  computation using multiApply function. The default value is NULL (1). 
#'@param ... Additional parameters to be used by the method choosen. See qmap 
#'  package for details.
#'
#'@return An array containing the experimental data after applying the quantile
#'mapping correction.
#' 
#'@seealso \code{\link[qmap]{fitQmap}} and \code{\link[qmap]{doQmap}} 
#'@examples
#'# Use synthetic data
#'exp <- 1 : c(1 * 3 * 5 * 4 * 3 * 2)
#'dim(exp) <- c(dataset = 1, member = 3, sdate = 5, ftime = 4, 
#'              lat = 3, lon = 2)
#'obs <- 101 : c(100 + 1 * 1 * 5 * 4 * 3 * 2)
#'dim(obs) <- c(dataset = 1, member = 1, sdate = 5, ftime = 4,
#'              lat = 3, lon = 2)
#'res <- QuantileMapping(exp, obs)
#'
#'@import qmap 
#'@import multiApply 
#'@import s2dv
#'@export
QuantileMapping <- function(exp, obs, exp_cor = NULL, sdate_dim = 'sdate',
                            memb_dim = 'member', window_dim = NULL, 
                            method = 'QUANT', na.rm = FALSE, 
                            ncores = NULL, ...) {
  # exp and obs
  obsdims <- names(dim(obs))
  expdims <- names(dim(exp))
  if (!is.array(exp) || !is.numeric(exp)) {
    stop("Parameter 'exp' must be a numeric array.")
  }
  if (!is.array(obs) || !is.numeric(obs)) {
    stop("Parameter 'obs' must be a numeric array.")
  }
  if (is.null(expdims)) {
    stop("Parameter 'exp' must have dimension names.")
  }
  if (is.null(obsdims)) {
    stop("Parameter 'obs' must have dimension names.")
  }
  # sdate_dim
  if (!is.character(sdate_dim) | length(sdate_dim) != 1) {
    stop("Parameter 'sdate_dim' must be a character string.")
  }
  if (!sdate_dim %in% expdims | !sdate_dim %in% obsdims) {
    stop("Parameter 'sdate_dim' is not found in 'exp' or 'obs' dimension.")
  }
  if (dim(exp)[sdate_dim] == 1 || dim(obs)[sdate_dim] == 1) {
    stop("Parameter 'exp' and 'obs' must have dimension length of 'sdate_dim' bigger than 1.")
  }
  # exp_cor
  if (!is.null(exp_cor)) {
    if (is.null(names(dim(exp_cor)))) {
      stop("Parameter 'exp_cor' must have dimension names.")
    }
    if (!sdate_dim %in% names(dim(exp_cor))) {
      stop("Parameter 'sdate_dim' is not found in 'exp_cor' dimension.")
    }
  }
  # method
  if (!(method %in% c('PTF', 'DIST', 'RQUANT', 'QUANT', 'SSPLIN')) | length(method) != 1) {
    stop("Parameter 'method' must be one of the following methods: ",
         "'PTF', 'DIST', 'RQUANT', 'QUANT', 'SSPLIN'.")
  }
  # memb_dim
  if (is.null(memb_dim)) {
    remove_member <- TRUE
    memb_dim <- "temp_memb_dim"
    exp <- InsertDim(exp, posdim = 1, lendim = 1, name = "temp_memb_dim")
    obs <- InsertDim(obs, posdim = 1, lendim = 1, name = "temp_memb_dim")
    obsdims <- names(dim(obs))
    expdims <- names(dim(exp))
    if (!is.null(exp_cor)) {
      exp_cor <- InsertDim(exp_cor, posdim = 1, lendim = 1, name = "temp_memb_dim")
    }
  } else {
    remove_member <- FALSE
    if (!all(memb_dim %in% obsdims)) {
      obs <- InsertDim(obs, posdim = 1, lendim = 1,
                       name = memb_dim[!(memb_dim %in% obsdims)])
      obsdims <- names(dim(obs))
    }
    if (any(!memb_dim %in% expdims)) {
      stop(paste0("Parameter 'memb_dim' is not found in 'exp' dimensions. ", 
                  "Set it as NULL if there is no member dimension."))
    }
  }
  sample_dims <- c(memb_dim, sdate_dim)
  # window_dim
  if (!is.null(window_dim)) {
    if (!(window_dim %in% obsdims)) {
      stop("Parameter 'window_dim' is not found in 'obs'.")
    }
    obs <- CSTools::MergeDims(obs, c(memb_dim, window_dim))
    if (window_dim %in% expdims) {
      exp <- CSTools::MergeDims(exp, c(memb_dim, window_dim))
      warning("Parameter 'window_dim' is found in exp and is merged to 'memb_dim'.")
    }
  }
  # na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  # ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    }
  }

  ###############################
  if (!is.null(exp_cor)) {
    qmaped <- Apply(list(exp, obs, exp_cor), target_dims = sample_dims, 
                    fun = .qmapcor, method = method, sdate_dim = sdate_dim,
                    na.rm = na.rm, ..., 
                    ncores = ncores)$output1
  } else {
    qmaped <- Apply(list(exp, obs), target_dims = sample_dims,
                    fun = .qmapcor, exp_cor = NULL, method = method,
                    sdate_dim = sdate_dim, na.rm = na.rm, ...,                
                    ncores = ncores)$output1
  }
  # remove added 'temp_memb_dim'
  if (remove_member) {
    dim(qmaped) <- dim(qmaped)[-which(names(dim(qmaped)) == "temp_memb_dim")]
  }

  return(qmaped)
}

.qmapcor <- function(exp, obs, exp_cor = NULL, sdate_dim = 'sdate', 
                     method = 'QUANT', na.rm = FALSE, ...) {

  # exp: [memb (+ window), sdate]
  # obs: [memb (+ window), sdate] 
  # exp_cor: NULL or [memb, sdate]

  if (is.null(exp_cor)) {
    applied <- exp * NA
    for (sd in 1:dim(exp)[sdate_dim]) {
      if (na.rm) {
        # select start date for cross-val
        nas_pos <- which(!is.na(exp[, sd]))
        obs2 <- as.vector(obs[, -sd])
        exp2 <- as.vector(exp[, -sd])
        exp_cor2 <- as.vector(exp[, sd])
        # remove NAs
        obs2 <- obs2[!is.na(obs2)]
        exp2 <- exp2[!is.na(exp2)]   
        exp_cor2 <- exp_cor2[!is.na(exp_cor2)]
        tryCatch({
          adjust <- fitQmap(obs2, exp2, method = method, ...)
          applied[nas_pos, sd] <- doQmap(exp_cor2, adjust, ...)
          },
          error = function(error_message) {
            return(applied[, sd])
          })
      } else {
        # na.rm = FALSE shouldn't fail, just return NA
        if (anyNA(obs[, -sd]) | anyNA(exp[, -sd])) {
          applied[, sd] <- NA
        } else {
          adjust <- fitQmap(as.vector(obs[, -sd]), as.vector(exp[, -sd]),
                            method = method, ...)
          exp2 <- exp[, sd]
          if (sum(is.na(exp2)) >= 1) {
            app <- rep(NA, length(exp2))
            nas_pos <- which(is.na(exp2))
            exp2 <- exp2[!is.na(exp2)]
            app[-nas_pos] <- doQmap(as.vector(exp2), adjust, ...)
          } else {
            app <- doQmap(as.vector(exp2), adjust, ...)
          }
          applied[, sd] <- app
        }
      }
    }
  } else {
    applied <- exp_cor * NA
    if (na.rm) {
      tryCatch({
        adjust <- fitQmap(obs[!is.na(obs)], exp[!is.na(exp)],
                          method = method, ...)
        applied[!is.na(exp_cor)] <- doQmap(exp_cor[!is.na(exp_cor)],
                                           adjust, ...)
        },
        error = function(error_message) {
          return(applied)
        })
    } else {
      adjust <- fitQmap(as.vector(obs), as.vector(exp), method = method, ...)
      applied <- doQmap(as.vector(exp_cor), adjust, ...)
    }
    dim(applied) <- dim(exp_cor)
  }
  return(applied)
}
