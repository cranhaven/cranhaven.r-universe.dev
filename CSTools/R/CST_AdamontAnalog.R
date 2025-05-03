#'CST_AdamontAnalog finds analogous data in the reference dataset to experiment
#'data based on weather types
#'
#'@description This function searches for analogs in a reference dataset for 
#'experiment data, based on corresponding weather types. The experiment data is
#'typically a hindcast, observations are typically provided by reanalysis data.
#'@author Paola Marson, \email{paola.marson@meteo.fr} for PROSNOW version
#'@author Lauriane Batté, \email{lauriane.batte@meteo.fr} for CSTools adaptation
#'
#'@param exp Experiment data an object of class \code{s2dv_cube}, can be output 
#'  from quantile correction using CST_AdamontQQCorr.
#'@param wt_exp Corresponding weather types (same dimensions as \code{exp$data} 
#'  but lat/lon).
#'@param obs Reference data, also of class \code{s2dv_cube}. Note that lat/lon 
#'  dimensions need to be the same as \code{exp}.
#'@param wt_obs Corresponding weather types (same dimensions as \code{obs$data}
#'  but lat/lon)
#'@param nanalogs Integer defining the number of analog values to return
#'  (default: 5).
#'@param method A character string indicating the method used for analog
#'  definition. It can be:
#'  \itemize{
#'    \item{'pattcorr': pattern correlation.}
#'    \item{'rain1' (for precip patterns): rain occurrence consistency.}
#'    \item{'rain01' (for precip patterns): rain occurrence/non occurrence 
#'    consistency}
#'  }        
#'@param thres Real number indicating the threshold to define rain 
#'  occurrence/non occurrence in rain (0)1.
#'@param search_obsdims List of dimensions in \code{obs} along which analogs are
#'  searched for.
#'@param londim Name of longitude dimension.
#'@param latdim Name of latitude dimension.
#'@return analog_vals An object of class \code{s2dv_cube} containing 
#'  nanalogs analog values for each value of \code{exp} input data.
#'@examples
#'wt_exp <- sample(1:3, 15*6*3, replace = TRUE)
#'dim(wt_exp) <- c(dataset = 1, member = 15, sdate = 6, ftime = 3)
#'wt_obs <- sample(1:3, 6*3, replace = TRUE)
#'dim(wt_obs) <- c(dataset = 1, member = 1, sdate = 6, ftime = 3)
#'exp <- NULL
#'exp$data <- 1 : c(1 * 15 * 6 * 3 * 8 * 8)
#'dim(exp$data) <- c(dataset = 1, member = 15, sdate = 6, ftime = 3,
#'                   lat = 8, lon = 8)
#'class(exp) <- 's2dv_cube'
#'obs <- NULL
#'obs$data <- 101 : c(100 + 1 * 1 * 6 * 3 * 8 * 8)
#'dim(obs$data) <- c(dataset = 1, member = 1, sdate = 6, ftime = 3,
#'                   lat = 8, lon = 8)
#'class(obs) <- 's2dv_cube'
#'analog_vals <- CST_AdamontAnalog(exp = exp, obs = obs, wt_exp = wt_exp, 
#'                                 wt_obs = wt_obs, nanalogs = 2)
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
CST_AdamontAnalog <- function(exp, obs, wt_exp, wt_obs, nanalogs, 
			                        method = 'pattcorr', thres = NULL, 
			                        search_obsdims = c('member', 'sdate', 'ftime'), 
			                        londim = 'lon', latdim = 'lat') {
  dimnames <- names(dim(obs$data))
  dimnamesexp <- names(dim(exp$data))

  if (!inherits(exp, 's2dv_cube') || !inherits(obs, 's2dv_cube')) {
    stop("Inputs 'exp' and 'obs' must be of class 's2dv_cube', ",
	       "as output by CSTools::CST_Load.")
  }
  if (!(method %in% c('pattcorr','rain1','rain01'))) {
    stop("Input parameter 'method' must be 'pattcorr', 'rain1', or 'rain01'")
  }
  if (is.null(nanalogs)) {
     nanalogs <- 5
  }
  if (!(latdim %in% dimnames) || !(londim %in% dimnames)){
    stop("'londim' or 'latdim' input doesn't match with 'obs$data' dimension",
	       " names")
  }
  if (!(latdim %in% dimnamesexp) || !(londim %in% dimnamesexp)){
    stop("'londim' or 'latdim' input doesn't match with 'exp$data' dimension",
	       " names")
  }
  if (!all(search_obsdims %in% dimnames)) {
    stop("Names in parameter 'search_obsdims' should match 'obs$data' ",
	       "dimension names.")
  }
  if (!all(dim(wt_exp) %in% dim(exp$data))) {
    stop("Dimensions for 'wt_exp' should match 'exp$data' except lat/lon")
  }
  if (!all(dim(wt_obs) %in% dim(obs$data))) {
    stop("Dimensions for 'wt_obs' should match 'obs$data' except lat/lon")
  }
  plat_exp <- which(dimnamesexp == latdim)
  plon_exp <- which(dimnamesexp == londim)
  plat_obs <- which(dimnames == latdim)
  plon_obs <- which(dimnames == londim)
  if ((dim(obs$data)[plon_obs] != dim(exp$data)[plon_exp]) ||
      (dim(obs$data)[plat_obs] != dim(exp$data)[plat_exp])){
     stop("Element 'data' from parameters 'obs' and 'exp' should have",
          "same lon / lat dimensions if working with regular grids.")
  }
  # End of sanity checks; call AdamontAnalog function
  analog_vals <- AdamontAnalog(exp = exp$data, obs = obs$data, wt_exp = wt_exp,
			                         wt_obs = wt_obs, nanalogs = nanalogs,
			                         method = method, thres = thres,
			                         search_obsdims = search_obsdims, londim = londim,
			                         latdim = latdim )
  return(analog_vals)
}
#'AdamontAnalog finds analogous data in the reference dataset to experiment
#'data based on weather types
#'
#'@description This function searches for analogs in a reference dataset for 
#'experiment data, based on corresponding weather types. The experiment data is
#'typically a hindcast, observations are typically provided by reanalysis data.
#'@author Paola Marson, \email{paola.marson@meteo.fr} for PROSNOW version
#'@author Lauriane Batté, \email{lauriane.batte@meteo.fr} for CSTools adaptation
#' 
#' 
#'@param exp A multidimensional array with named dimensions containing the 
#'  experiment data.
#'@param wt_exp Corresponding weather types (same dimensions as \code{exp$data} 
#'  but lat/lon).
#'@param obs A multidimensional array with named dimensions containing the 
#'  reference data. Note that lat/lon dimensions need to be the same as 
#'  \code{exp}.
#'@param wt_obs Corresponding weather types (same dimensions as \code{obs$data}
#'  but lat/lon).
#'@param nanalogs Integer defining the number of analog values to return
#'  (default: 5).
#'@param method A character string indicating the method used for analog
#'  definition. It can be:
#'  \itemize{
#'    \item{'pattcorr': pattern correlation.}
#'    \item{'rain1' (for precip patterns): rain occurrence consistency.}
#'    \item{'rain01' (for precip patterns): rain occurrence/non occurrence 
#'    consistency}
#'  }        
#'@param thres Real number indicating the threshold to define rain 
#'  occurrence/non occurrence in rain (0)1.
#'@param search_obsdims List of dimensions in \code{obs} along which analogs are
#'  searched for.
#'@param londim Name of longitude dimension.
#'@param latdim Name of latitude dimension.
#'@return analog_vals An array containing nanalogs analog values.
#'@examples
#'wt_exp <- sample(1:3, 15*6*3, replace = TRUE)
#'dim(wt_exp) <- c(dataset = 1, member = 15, sdate = 6, ftime = 3)
#'wt_obs <- sample(1:3, 6*3, replace = TRUE)
#'dim(wt_obs) <- c(dataset = 1, member = 1, sdate = 6, ftime = 3)
#'exp <- 1 : c(1 * 15 * 6 * 3 * 8 * 8)
#'dim(exp) <- c(dataset = 1, member = 15, sdate = 6, ftime = 3, lat = 8, lon = 8)
#'obs <- 101 : c(100 + 1 * 1 * 6 * 3 * 8 * 8)
#'dim(obs) <- c(dataset = 1, member = 1, sdate = 6, ftime = 3, lat = 8, lon = 8)
#'analog_vals <- AdamontAnalog(exp = exp, obs = obs, wt_exp = wt_exp, 
#'                             wt_obs = wt_obs, nanalogs = 2)
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@rdname CST_AdamontAnalog
#'@export
AdamontAnalog <- function(exp, obs, wt_exp, wt_obs, nanalogs = 5, 
			                    method = 'pattcorr', thres = NULL,
			                    search_obsdims = c('member', 'sdate', 'ftime'), 
			                    londim = 'lon', latdim = 'lat') { 
  # exp: lat, lon, sdate, ftime, member
  # obs: lat, lon, dims for searching 'sdate' 'ftime'...
  # wt_exp: sdate, ftime, member 
  # wt_obs: the dims for searching
  dimnames <- names(dim(obs))
  dimnamesexp <- names(dim(exp))
  if (method %in% c('rain1','rain01') & is.null(thres)) {
    stop("Threshold 'thres' must be defined with methods 'rain1' and 'rain01'")
  }
  if (method == 'pattcorr' & !is.null(thres)) {
    warning("Parameter 'thres' is not used with method 'pattcorr'.")
  }
  if (!(latdim %in% dimnamesexp) || !(londim %in% dimnamesexp)) {
    stop("'londim' or 'latdim' input doesn't match with 'exp' dimension names")
  }
  # Position of lat/lon dimensions in exp data
  poslatexp <- which(dimnamesexp == latdim)
  poslonexp <- which(dimnamesexp == londim)
  poslatobs <- which(dimnames == latdim)
  poslonobs <- which(dimnames == londim)
  if (!all(search_obsdims %in% dimnames)) {
    stop("Names in parameter 'search_obsdims' should match 'obs' ",
	      "dimension names.")
  }
  if (!all(dim(wt_exp) %in% dim(exp))){
    stop("Dimensions for 'wt_exp' should match 'exp' except lat/lon")
  }
  if (!all(dim(wt_obs) %in% dim(obs))){
    stop("Dimensions for 'wt_obs' should match 'obs' except lat/lon")
  }
  if ((dim(obs)[poslonobs]!=dim(exp)[poslonexp]) ||
      (dim(obs)[poslatobs]!=dim(exp)[poslatexp])){
    stop("Parameters 'obs' and 'exp' should have same lon / lat dimensions.")
  }

  ## Reshaping obs:
  ## The dimensions where to search in a single dim
  if (length(search_obsdims) > 1) {
    for (i in 1:(length(search_obsdims) - 1)) {
      obs <- MergeDims(obs, search_obsdims[i:(i + 1)], 
		                 rename_dim = search_obsdims[i + 1])
      wt_obs <- MergeDims(wt_obs, search_obsdims[i:(i + 1)], 
			                 rename_dim = search_obsdims[i + 1])
    }
  }
  names(dim(obs))[which(names(dim(obs)) == search_obsdims[length(search_obsdims)])] <- 'time'  
  names(dim(wt_obs))[which(names(dim(wt_obs)) == search_obsdims[length(search_obsdims)])] <- 'time' 
  # Split 'time' dim in weather types
  obs <- SplitDim(obs, split_dim = 'time', indices = as.vector(wt_obs),
		  new_dim_name='type')

  analog_vals <- Apply(list(exp, obs, wt_exp), 
		       target_dims = list(c(londim, latdim),
					  c(londim, latdim, 'time', 'type'), 
					  NULL),
		       .aanalogs, method = method, thres = thres)$output1
  
  # Reshaping output:
  analog_vals <- Subset(analog_vals, along = 'type', indices = 1, drop = 'selected')
  poslat <- which(names(dim(analog_vals)) == latdim)
  poslon <- which(names(dim(analog_vals)) == londim)
  postime <- which(names(dim(analog_vals)) == 'time') # Dimension with N analogs
  pos <- 1:length(dim(analog_vals))
  if (poslatexp > poslonexp){
      analog_vals <- aperm(analog_vals,c(pos[-c(poslon,poslat,postime)],
					            postime,poslon,poslat))
  } else {
      analog_vals <- aperm(analog_vals,c(pos[-c(poslon,poslat,postime)],
					            postime,poslat,poslon))
  }
  # Renaming 'time' dim to 'analog'
  names(dim(analog_vals))[which(names(dim(analog_vals)) == 'time')] <- 'analog' 
  return(analog_vals)
}


.aanalogs <- function(exp, obs, wt_exp, nanalogs = 5, method = 'pattcorr', 
		               thres = NULL, londimexp = 'lon', latdimexp = 'lat', 
		               londimobs = 'lon', latdimobs = 'lat') {
  # exp: lon, lat
  # obs: lon, lat, time, wt
  # wt_exp: wt single scalar
  	
  search_analog <- switch(method, 'rain1' = .rain1, 'rain01' = .rain01, 
			                    'pattcorr' = .pattcor,
                          stop(paste0("Adamont Analog function only supports ",
				                  "methods 'rain1', 'rain01', 'pattcorr'")))

  obs <- Subset(obs, along = 'type', indices = wt_exp)
  accuracy <- Apply(list(exp, obs), target_dims = list(c(londimexp, latdimexp),
                                                       c(londimobs, latdimobs)),
                    search_analog, thres = thres)$output1
  obs <- Subset(obs, along = 'time', 
                indices = order(accuracy, decreasing = TRUE)[1:nanalogs])
  return(obs)
}

.rain1 <- function(exp_day, obs_day, thres) {
  accuracy <- sum((obs_day >= thres) * (exp_day >= thres))
  return(accuracy)
}
.rain01 <- function(exp_day, obs_day, thres) {
  accuracy <- sum(diag(table((obs_day >= thres),(exp_day >= thres))))
  return(accuracy)
}
.pattcor <- function(exp_day, obs_day, thres = NULL) {
  accuracy <- cor(as.vector(obs_day),as.vector(exp_day))
  return(accuracy)
}


