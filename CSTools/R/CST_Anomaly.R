#'Anomalies relative to a climatology along selected dimension with or without 
#'cross-validation
#'
#'@author Perez-Zanon Nuria, \email{nuria.perez@bsc.es}
#'@author Pena Jesus, \email{jesus.pena@bsc.es}
#'@description This function computes the anomalies relative to a climatology 
#'computed along the selected dimension (usually starting dates or forecast 
#'time) allowing the application or not of crossvalidated climatologies. The 
#'computation is carried out independently for experimental and observational 
#'data products.
#'
#'@param exp An object of class \code{s2dv_cube} as returned by \code{CST_Start} 
#'  function, containing the seasonal forecast experiment data in the element 
#'  named \code{$data}.
#'@param obs An object of class \code{s2dv_cube} as returned by \code{CST_Start} 
#'  function, containing the observed data in the element named \code{$data}.
#'@param dim_anom A character string indicating the name of the dimension 
#'  along which the climatology will be computed. The default value is 'sdate'.
#'@param cross A logical value indicating whether cross-validation should be 
#'  applied or not. Default = FALSE.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. It must be one dimension in 'exp' and 'obs'. If there is no 
#'  member dimension, set NULL. The default value is 'member'.
#'@param memb A logical value indicating whether to subtract the climatology 
#'  based on the individual members (TRUE) or the ensemble mean over all
#'  members (FALSE) when calculating the anomalies. The default value is TRUE.
#'@param dat_dim A character vector indicating the name of the dataset and 
#'  member dimensions. If there is no dataset dimension, it can be NULL.
#'  The default value is "c('dataset', 'member')".
#'@param filter_span A numeric value indicating the degree of smoothing. This 
#'  option is only available if parameter \code{cross} is set to FALSE.
#'@param ftime_dim A character string indicating the name of the temporal 
#'  dimension where the smoothing with 'filter_span' will be applied. It cannot
#'  be NULL if 'filter_span' is provided. The default value is 'ftime'.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL. It will be used only when 
#'  'filter_span' is not NULL.
#'
#'@return A list with two S3 objects, 'exp' and 'obs', of the class 
#''s2dv_cube', containing experimental and date-corresponding observational 
#'anomalies, respectively. These 's2dv_cube's can be ingested by other functions 
#'in CSTools.
#'
#'@examples
#'mod <- 1 : (2 * 3 * 4 * 5 * 6 * 7)
#'dim(mod) <- c(dataset = 2, member = 3, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'obs <- 1 : (1 * 1 * 4 * 5 * 6 * 7)
#'dim(obs) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'lon <- seq(0, 30, 5)
#'lat <- seq(0, 25, 5)
#'coords <- list(lon = lon, lat = lat)
#'exp <- list(data = mod, coords = coords)
#'obs <- list(data = obs, coords = coords)
#'attr(exp, 'class') <- 's2dv_cube'
#'attr(obs, 'class') <- 's2dv_cube'
#'
#'anom <- CST_Anomaly(exp = exp, obs = obs, cross = FALSE, memb = TRUE)
#'
#'@seealso \code{\link[s2dv]{Ano_CrossValid}}, \code{\link[s2dv]{Clim}} and 
#'\code{\link{CST_Start}}
#'
#'@import multiApply
#'@importFrom s2dv InsertDim Clim Ano_CrossValid Reorder
#'@export
CST_Anomaly <- function(exp = NULL, obs = NULL, dim_anom = 'sdate', 
                        cross = FALSE, memb_dim = 'member', memb = TRUE, 
                        dat_dim = c('dataset', 'member'), filter_span = NULL, 
                        ftime_dim = 'ftime', ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(exp, 's2dv_cube') & !is.null(exp) || 
      !inherits(obs, 's2dv_cube') & !is.null(obs)) {
    stop("Parameter 'exp' and 'obs' must be of the class 's2dv_cube'.")
  }
  # exp and obs
  if (is.null(exp$data) & is.null(obs$data)) {
    stop("One of the parameter 'exp' or 'obs' cannot be NULL.")
  }
  case_exp = case_obs = 0
  if (is.null(exp)) {
    exp <- obs 
    case_obs = 1
    warning("Parameter 'exp' is not provided and 'obs' will be used instead.")
  }  
  if (is.null(obs)) {
    obs <- exp 
    case_exp = 1
    warning("Parameter 'obs' is not provided and 'exp' will be used instead.")
  }
  if (any(is.null(names(dim(exp$data))))| any(nchar(names(dim(exp$data))) == 0) |
      any(is.null(names(dim(obs$data))))| any(nchar(names(dim(obs$data))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names in element 'data'.")
  }
  dim_exp <- dim(exp$data)
  dim_obs <- dim(obs$data)
  dimnames_exp <- names(dim_exp)
  dimnames_obs <- names(dim_obs)
  # dim_anom
  if (!is.character(dim_anom)) {
    stop("Parameter 'dim_anom' must be a character string.")
  }
  if (!dim_anom %in% names(dim_exp) | !dim_anom %in% names(dim_obs)) {
    stop("Parameter 'dim_anom' is not found in 'exp' or in 'obs' dimension in element 'data'.")
  }
  if (dim_exp[dim_anom] <= 1 | dim_obs[dim_anom] <= 1) {
    stop("The length of dimension 'dim_anom' in label 'data' of the parameter ",
         "'exp' and 'obs' must be greater than 1.")
  }
  # cross
  if (!is.logical(cross) | !is.logical(memb)) {
    stop("Parameters 'cross' and 'memb' must be logical.")
  }
  if (length(cross) > 1 | length(memb) > 1) {
    cross <- cross[1]
    warning("Parameter 'cross' has length greater than 1 and only the first element ",
            "will be used.")
  }
  # memb
  if (length(memb) > 1) {
     memb <- memb[1]
     warning("Parameter 'memb' has length greater than 1 and only the first element ",
             "will be used.")
  }
  # memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
  }
  # dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim)) {
      stop("Parameter 'dat_dim' must be a character vector.")
    }
  }
  # filter_span
  if (!is.null(filter_span)) {
    if (!is.numeric(filter_span)) {
      warning("Paramater 'filter_span' is not numeric and any filter ",
              "is being applied.")
      filter_span <- NULL
    }
    # ncores
    if (!is.null(ncores)) {
      if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
        length(ncores) > 1) {
        stop("Parameter 'ncores' must be a positive integer.")
      }
    }
    # ftime_dim
    if (!is.character(ftime_dim)) {
      stop("Parameter 'ftime_dim' must be a character string.")
    }
    if (!ftime_dim %in% names(dim_exp) | !ftime_dim %in% names(dim_obs)) {
      stop("Parameter 'ftime_dim' is not found in 'exp' or in 'obs' dimension in element 'data'.")
    }
  }
  
  # Computating anomalies
  #----------------------
  
  # With cross-validation
  if (cross) {
    ano <- Ano_CrossValid(exp = exp$data, obs = obs$data, time_dim = dim_anom,
                          memb_dim = memb_dim, memb = memb, dat_dim = dat_dim,
                          ncores = ncores)
   
  # Without cross-validation 
  } else {
    tmp <- Clim(exp = exp$data, obs = obs$data, time_dim = dim_anom,
		            memb_dim = memb_dim, memb = memb, dat_dim = dat_dim, 
                ncores = ncores)
    if (!is.null(filter_span)) {
      tmp$clim_exp <- Apply(tmp$clim_exp, 
                            target_dims = c(ftime_dim),
                            output_dims = c(ftime_dim),
                            fun = .Loess,
                            loess_span = filter_span, 
                            ncores = ncores)$output1
      tmp$clim_obs <- Apply(tmp$clim_obs, 
                            target_dims = c(ftime_dim),
                            output_dims = c(ftime_dim),
                            fun = .Loess,
                            loess_span = filter_span, 
                            ncores = ncores)$output1
    }
    if (memb) { 
      clim_exp <- tmp$clim_exp
      clim_obs <- tmp$clim_obs
    } else {
      clim_exp <- InsertDim(tmp$clim_exp, 1, dim_exp[memb_dim]) 
      clim_obs <- InsertDim(tmp$clim_obs, 1, dim_obs[memb_dim]) 
    }
    clim_exp <- InsertDim(clim_exp, 1, dim_exp[dim_anom]) 
    clim_obs <- InsertDim(clim_obs, 1, dim_obs[dim_anom])
    ano <- NULL

    # Permuting back dimensions to original order
    clim_exp <- Reorder(clim_exp, dimnames_exp)
    clim_obs <- Reorder(clim_obs, dimnames_obs)

    ano$exp <- exp$data - clim_exp
    ano$obs <- obs$data - clim_obs 
  }

  exp$data <- ano$exp
  exp$dims <- dim(ano$exp)
  obs$data <- ano$obs
  obs$dims <- dim(ano$obs)
  
  #  Outputs
  # ~~~~~~~~~
    if (case_obs == 1) {
      return(obs)
    } 
    else if (case_exp == 1) {
      return(exp)
    }
    else {
    return(list(exp = exp, obs = obs)) 
  }
}

.Loess <- function(clim, loess_span) {
  data <- data.frame(ensmean = clim, day = 1 : length(clim))
  loess_filt <- loess(ensmean ~ day, data, span = loess_span)
  output <- predict(loess_filt)
  return(output)
}

