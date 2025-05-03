#'Multiple Metrics applied in Multiple Model Anomalies
#'
#'@author Mishra Niti, \email{niti.mishra@bsc.es}
#'@author Perez-Zanon Nuria, \email{nuria.perez@bsc.es}
#'@description This function calculates correlation (Anomaly Correlation 
#'Coefficient; ACC), root mean square error (RMS) and the root mean square error 
#'skill score (RMSSS) of individual anomaly models and multi-models mean (if 
#'desired) with the observations.
#'
#'@param exp An object of class \code{s2dv_cube} as returned by 
#'  \code{CST_Anomaly} function, containing the anomaly of the seasonal forecast 
#'  experiments data in the element named \code{$data}.
#'@param obs An object of class \code{s2dv_cube} as returned by 
#'  \code{CST_Anomaly} function, containing the anomaly of observed data in the 
#'  element named \code{$data}.
#'@param metric A character string giving the metric for computing the maximum 
#'  skill. This must be one of the strings 'correlation', 'rms', 'rmsss' and 
#'  'rpss'. If 'rpss' is chossen the terciles probabilities are evaluated.
#'@param multimodel A logical value indicating whether a Multi-Model Mean should 
#'  be computed.
#'@param time_dim Name of the temporal dimension where a mean will be applied. 
#'  It can be NULL, the default value is 'ftime'.
#'@param memb_dim Name of the member dimension. It can be NULL, the default 
#'  value is 'member'.
#'@param sdate_dim Name of the start date dimension or a dimension name 
#'  identifiying the different forecast. It can be NULL, the default value is 
#'  'sdate'.
#'@return An object of class \code{s2dv_cube} containing the statistics of the 
#'selected metric in the element \code{$data} which is a list of arrays: for the
#'metric requested and others for statistics about its signeificance. The arrays 
#'have two dataset dimensions equal to the 'dataset' dimension in the 
#'\code{exp$data} and \code{obs$data} inputs. If \code{multimodel} is TRUE, the 
#'first position in the first 'nexp' dimension correspons to the Multi-Model Mean. 
#'@seealso \code{\link[s2dv]{Corr}}, \code{\link[s2dv]{RMS}}, 
#'\code{\link[s2dv]{RMSSS}} and \code{\link{CST_Load}}
#'@references Mishra, N., Prodhomme, C., & Guemas, V. (n.d.). Multi-Model Skill 
#'Assessment of Seasonal Temperature and Precipitation Forecasts over Europe, 
#'29-31. \doi{10.1007/s00382-018-4404-z}
#' 
#'@importFrom s2dv MeanDims Reorder Corr RMS RMSSS InsertDim
#'@import abind
#'@importFrom easyVerification climFairRpss veriApply
#'@import stats
#'@import multiApply
#'@examples
#'mod <- rnorm(2*2*4*5*2*2)
#'dim(mod) <- c(dataset = 2, member = 2, sdate = 4, ftime = 5, lat = 2, lon = 2)
#'obs <- rnorm(1*1*4*5*2*2)
#'dim(obs) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 2, lon = 2)
#'lon <- seq(0, 30, 5)
#'lat <- seq(0, 25, 5)
#'coords <- list(lat = lat, lon = lon)
#'exp <- list(data = mod, coords = coords)
#'obs <- list(data = obs, coords = coords)
#'attr(exp, 'class') <- 's2dv_cube'
#'attr(obs, 'class') <- 's2dv_cube'
#'a <- CST_MultiMetric(exp = exp, obs = obs)
#'@export
CST_MultiMetric <- function(exp, obs, metric = "correlation", multimodel = TRUE,
                            time_dim = 'ftime', memb_dim = 'member',
                            sdate_dim = 'sdate') {
  # Check 's2dv_cube'
  if (!inherits(exp, 's2dv_cube') || !inherits(obs, 's2dv_cube')) {
    stop("Parameter 'exp' and 'obs' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  result <- MultiMetric(exp$data, obs$data, metric = metric, multimodel = multimodel,
                        time_dim = time_dim, memb_dim = memb_dim, sdate_dim = sdate_dim)
  exp$data <- result
  exp$attrs$Datasets <- c(exp$attrs$Datasets, obs$attrs$Datasets)
  exp$attrs$source_files <- c(exp$attrs$source_files, obs$attrs$source_files)

  return(exp)
}

#'Multiple Metrics applied in Multiple Model Anomalies
#'
#'@author Mishra Niti, \email{niti.mishra@bsc.es}
#'@author Perez-Zanon Nuria, \email{nuria.perez@bsc.es}
#'@description This function calculates correlation (Anomaly Correlation 
#'Coefficient; ACC), root mean square error (RMS) and the root mean square error 
#'skill score (RMSSS) of individual anomaly models and multi-models mean (if 
#'desired) with the observations on arrays with named dimensions.
#'
#'@param exp A multidimensional array with named dimensions.
#'@param obs A multidimensional array with named dimensions.
#'@param metric A character string giving the metric for computing the maximum 
#'  skill. This must be one of the strings 'correlation', 'rms' or 'rmsss.
#'@param multimodel A logical value indicating whether a Multi-Model Mean should 
#'  be computed.
#'@param time_dim Name of the temporal dimension where a mean will be applied. 
#'  It can be NULL, the default value is 'ftime'.
#'@param memb_dim Name of the member dimension. It can be NULL, the default 
#'  value is 'member'.
#'@param sdate_dim Name of the start date dimension or a dimension name 
#'  identifiying the different forecast. It can be NULL, the default value is 
#'  'sdate'.
#'@return A list of arrays containing the statistics of the selected metric in 
#'the element \code{$data} which is a list of arrays: for the metric requested 
#'and others for statistics about its signeificance. The arrays have two dataset 
#'dimensions equal to the 'dataset' dimension in the \code{exp$data} and 
#'\code{obs$data} inputs. If \code{multimodel} is TRUE, the greatest position in 
#'the first dimension correspons to the Multi-Model Mean. 
#'@seealso \code{\link[s2dv]{Corr}}, \code{\link[s2dv]{RMS}}, 
#'\code{\link[s2dv]{RMSSS}} and \code{\link{CST_Load}}
#'@references Mishra, N., Prodhomme, C., & Guemas, V. (n.d.). Multi-Model Skill 
#'Assessment of Seasonal Temperature and Precipitation Forecasts over Europe, 
#'29-31. \doi{10.1007/s00382-018-4404-z}
#' 
#'@importFrom s2dv MeanDims Reorder Corr RMS RMSSS InsertDim
#'@import abind
#'@importFrom easyVerification climFairRpss veriApply
#'@import stats
#'@import multiApply
#'@examples
#'exp <- array(rnorm(2*2*4*5*2*2), 
#'             dim = c(dataset = 2, member = 2, sdate = 4, ftime = 5, lat = 2, 
#'                     lon = 2))
#'obs <- array(rnorm(1*1*4*5*2*2),
#'             dim = c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 2, 
#'                     lon = 2))
#'res <- MultiMetric(exp = exp, obs = obs)
#'@export
MultiMetric <- function(exp, obs, metric = "correlation", multimodel = TRUE,
                        time_dim = 'ftime', memb_dim = 'member', 
                        sdate_dim = 'sdate') {
                          
  if (!is.null(names(dim(exp))) & !is.null(names(dim(obs)))) {
    if (all(names(dim(exp)) %in% names(dim(obs)))) {
      dimnames <- names(dim(exp))
    } else {
      stop("Dimension names of element 'data' from parameters 'exp'",
           " and 'obs' should have the same name dimmension.")
    }
  } else {
    stop("Element 'data' from parameters 'exp' and 'obs'",
         " should have dimension names.")
  }
  if (!is.logical(multimodel)) {
    stop("Parameter 'multimodel' must be a logical value.")
  }
  if (length(multimodel) > 1) {
    multimodel <- multimodel[1]
    warning("Parameter 'multimodel' has length > 1 and only the first ",
            "element will be used.")
  }
  if (length(metric) > 1) {
    metric <- metric[1]
    warning("Parameter 'multimodel' has length > 1 and only the first ",
            "element will be used.")
  }
  if (is.null(time_dim) | !is.character(time_dim)) {
    time_dim <- 'time'
  }
  if (is.null(memb_dim) | !is.character(memb_dim)) {
    memb_dim <- 'memb'
  }
  if( is.null(sdate_dim) | !is.character(sdate_dim)) {
    sdate_dim <- 'sdate'
  }
  exp_dims <- dim(exp)
  obs_dims <- dim(obs) 
  if (!is.null(names(exp_dims)) & !is.null(names(obs_dims))) {
    if (all(names(exp_dims) == names(obs_dims))) {
      if (!(time_dim %in% names(exp_dims))) {
        warning("Parameter 'time_dim' does not match with a dimension name in 'exp'",
                " and 'obs'. A 'time_dim' of length 1 is added.")
        dim(exp) <- c(exp_dims, time_dim = 1)
        names(dim(exp))[length(dim(exp))] <- time_dim
        dim(obs) <- c(obs_dims, time_dim = 1)
        names(dim(obs))[length(dim(obs))] <- time_dim
        exp_dims <- dim(exp)
        obs_dims <- dim(obs) 
      }
      if (!(memb_dim %in% names(exp_dims))) {
        warning("Parameter 'memb_dim' does not match with a dimension name in ",
                "'exp' and 'obs'. A 'memb_dim' of length 1 is added.")
        dim(exp) <- c(exp_dims, memb_dim = 1)
        names(dim(exp))[length(dim(exp))] <- memb_dim
        dim(obs) <- c(obs_dims, memb_dim = 1)
        names(dim(obs))[length(dim(obs))] <- memb_dim
        exp_dims <- dim(exp)
        obs_dims <- dim(obs)
      }
      if (!(sdate_dim %in% names(exp_dims))) {
        warning("Parameter 'sdate_dim' does not match with a dimension name in ",
                "'exp' and 'obs'. A 'sdate_dim' of length 1 is added.")
        dim(exp) <- c(exp_dims, sdate_dim = 1)
        names(dim(exp))[length(dim(exp))] <- sdate_dim
        dim(obs) <- c(obs_dims, sdate_dim = 1)
        names(dim(obs))[length(dim(obs))] <- sdate_dim
        exp_dims <- dim(exp)
        obs_dims <- dim(obs)
      }
    } else {
      stop("Dimension names of element 'data' from parameters 'exp'",
           " and 'obs' should be the same and in the same order.")
    }
  } else {
    stop("Element 'data' from parameters 'exp' and 'obs'",
         " should have dimmension names.")
  }
  if (metric == 'rpss') {
    if (multimodel == TRUE) {
      warning("A probabilistic metric cannot be use to evaluate a multimodel mean.")
    }
    AvgExp <- MeanDims(exp, time_dim, na.rm = TRUE)
    AvgObs <- MeanDims(obs, time_dim, na.rm = TRUE)
    dif_dims <- which(dim(AvgExp) != dim(AvgObs))
    dif_dims <- names(dif_dims[-which(names(dif_dims) == memb_dim)])
    lapply(dif_dims, function(x) {
      names(dim(AvgExp))[which(names(dim(AvgExp)) == x)] <<- paste0(dif_dims, '_exp')})

    pos_memb <- which(names(dim(AvgExp)) == memb_dim)
    dim(AvgObs) <- dim(AvgObs)[-pos_memb]
    AvgExp <- Reorder(AvgExp, c(names(dim(AvgExp))[-pos_memb], memb_dim))
    pos_memb <- which(names(dim(AvgExp)) == memb_dim)
    pos_sdate <- which(names(dim(AvgExp)) == sdate_dim)
    corr <- Apply(list(AvgExp, AvgObs),
       target_dims = list(c(sdate_dim, 'lat', 'lon', memb_dim), c(sdate_dim, 'lat', 'lon')),
                  fun = function(x, y) {
                    veriApply('FairRpss', fcst = x, obs = y,
                              ensdim = which(names(dim(x)) ==  'member'),
                              tdim = which(names(dim(x)) == 'sdate'),
                              prob = c(1/3, 2/3))})
    
  } else if (metric %in% c('correlation', 'rms', 'rmsss')) {
    AvgExp <- MeanDims(exp, c(memb_dim, time_dim), na.rm = TRUE)
    AvgObs <- MeanDims(obs, c(memb_dim, time_dim), na.rm = TRUE)
    dataset_dim <- c('data', 'dataset', 'datsets', 'models')
    if (any(dataset_dim %in% names(exp_dims))) {
        dataset_dim <- dataset_dim[dataset_dim %in% names(dim(AvgExp))]
    } else {
        warning("Parameter 'exp' and 'obs' does not have a dimension 'dataset'.")
    }
    if (multimodel == TRUE) {
        # seasonal avg of anomalies for multi-model
        AvgExp_MMM <- MeanDims(AvgExp, c(dataset_dim), na.rm = TRUE)
        pos_dataset <- which(names(dim(AvgExp)) == dataset_dim)
        AvgExp_MMM <- s2dv::InsertDim(AvgExp_MMM, posdim = pos_dataset, lendim = 1,
                                        name = dataset_dim)
        AvgExp <- abind(AvgExp_MMM, AvgExp, along = pos_dataset)
        names(dim(AvgExp)) <- names(dim(AvgExp_MMM))
    }
    if (metric == 'correlation') {
      corr <- s2dv::Corr(AvgExp, AvgObs, dat_dim = dataset_dim, time_dim = sdate_dim)
    } else if (metric == 'rms') {
      corr <- s2dv::RMS(AvgExp, AvgObs, dat_dim = dataset_dim, time_dim = sdate_dim)
    } else if (metric == 'rmsss') {
      corr <- s2dv::RMSSS(AvgExp, AvgObs, dat_dim = dataset_dim, time_dim = sdate_dim)
    }
  } else {
    stop("Parameter 'metric' must be a character string indicating ",
         "one of the options: 'correlation', 'rms', 'rmsss' or 'rpss'.")
  }
  
  return(corr)
}
