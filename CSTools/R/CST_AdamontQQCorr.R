#'CST_AdamontQQCorr computes quantile-quantile correction of seasonal or 
#'decadal forecast data using weather types
#'
#'@description This function computes a quantile mapping based on weather types 
#'for experiment data (typically a hindcast) onto reference \code{obs},
#'typically provided by reanalysis data.
#'@author Lauriane Batté, \email{lauriane.batte@meteo.fr}
#'@author Paola Marson, \email{paola.marson@meteo.fr}
#'@author Gildas Dayon, \email{gildas.dayon@meteo.fr}
#'
#'@param exp Experiment data an object of class \code{s2dv_cube}.
#'@param wt_exp Corresponding weather types (same dimensions as \code{exp$data}
#'  but lat/lon).
#'@param obs Reference data, also of class \code{s2dv_cube}. lat/lon dimensions
#'  can differ from \code{exp} if non rectilinear latlon grids are used, 
#'  in which case regrid should be set to TRUE and .NearestNeighbors \code{NN}
#'  output should be provided.
#'@param wt_obs Corresponding weather types (same dimensions as \code{obs} but
#'  lat/lon).
#'@param corrdims List of dimensions in \code{exp} for which quantile mapping
#'  correction is applied.
#'@param londim Character name of longitude dimension in \code{exp} and 
#'  \code{obs}.
#'@param latdim Character name of latitude dimension in \code{exp} and 
#'  \code{obs}.
#'
#'@return An object of class \code{s2dv_cube} containing experiment data on the
#'lat/lon grid of \code{obs} input data, corrected by quantile mapping 
#'depending on the weather types \code{wt_exp}.
#'
#'@examples
#'wt_exp <- c(1,1,2,3,3,2,2,1,1,2,2,3)
#'dim(wt_exp) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3)
#'wt_obs <- c(3,3,1,2,2,2,2,1,3,1,1,2) 
#'dim(wt_obs) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3)
#'exp <- NULL
#'exp$data <-  1 : c(1 * 1 * 4 * 3 * 4 * 4)
#'dim(exp$data) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3,
#'                   lat = 4, lon = 4)
#'class(exp) <- 's2dv_cube'
#'obs <- NULL
#'obs$data <-  101 : c(100 + 1 * 1 * 4 * 3 * 4 * 4)
#'dim(obs$data) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3,
#'                   lat = 4, lon = 4)
#'class(obs) <- 's2dv_cube'
#'exp_corr <- CST_AdamontQQCorr(exp = exp, wt_exp = wt_exp, 
#'                              obs = obs, wt_obs = wt_obs, 
#'                              corrdims = c('dataset','member','sdate','ftime'))
#'@import qmap
#'@importFrom ClimProjDiags Subset
#'@import multiApply
#'@import abind
#'@export
CST_AdamontQQCorr <- function(exp, wt_exp, obs, wt_obs, 
			                        corrdims = c('member', 'sdate', 'ftime'),
			                        londim = 'lon', latdim = 'lat') {

  if (!inherits(exp, 's2dv_cube') || !inherits(obs, 's2dv_cube')){
    stop("Inputs 'exp' and 'obs' must be of class 's2dv_cube', ",
	       "as output by CSTools::CST_Load.")
  }
  dimnames <- names(dim(obs$data))
  dimnamesexp <- names(dim(exp$data))
  if (!(latdim %in% dimnames) || !(londim %in% dimnames)) {
    stop("'londim' or 'latdim' input doesn't match with 'obs$data' dimension",
        " names")
  }
  if (!(latdim %in% dimnamesexp) || !(londim %in% dimnamesexp)) {
    stop("'londim' or 'latdim' input doesn't match with 'exp$data' dimension",
        " names")
  }
  if (!(('time' %in% corrdims) || ('ftime' %in% corrdims))) {
    warning("Forecast time should be one of the dimensions for the correction ",
          "specified in corrdims input list")
  }
  if (!all(corrdims %in% dimnamesexp)) {
    stop("Names in parameter 'corrdims' should match input dimension names.")
  }
  if (!all(dim(wt_exp) %in% dim(exp$data))) {
    stop("Dimensions for 'wt_exp' should match 'exp$data' except lat/lon")
  }
  if (!all(dim(wt_obs) %in% dim(obs$data))) {
    stop("Dimensions for 'wt_obs' should match 'obs$data' except lat/lon")
  }
  if ((length(dim(exp$coords[[londim]])) == 2) || 
      (length(dim(obs$coords[[londim]])) == 2)) {
    myNN <- .NearestNeighbors(exp = exp, obs = obs, method = 'ADA')
    exp_corr <- AdamontQQCorr(exp = exp$data, wt_exp = wt_exp, obs = obs$data,
                              wt_obs = wt_obs, corrdims = corrdims,
                              londim = londim, latdim = latdim, regrid = TRUE, 
                              NN = myNN) 
  } else {
    ## If not (standard case)
    ## exp$data lat/lon dimensions should match obs$data
    plat_exp <- which(dimnamesexp == latdim)
    plon_exp <- which(dimnamesexp == londim)
    plat_obs <- which(dimnames == latdim)
    plon_obs <- which(dimnames == londim)
    if ((dim(obs$data)[plon_obs] != dim(exp$data)[plon_exp]) ||
        (dim(obs$data)[plat_obs] != dim(exp$data)[plat_exp])) {
      stop("Element 'data' from parameters 'obs' and 'exp' should have ",
          "same lon / lat dimensions if working with regular grids.")
    }
    exp_corr <- AdamontQQCorr(exp = exp$data, wt_exp = wt_exp, obs = obs$data,
                              wt_obs = wt_obs, corrdims = corrdims,
                              londim = londim, latdim = latdim, 
                              regrid = FALSE)
  }
  exp$data <- exp_corr
  return(exp)
}


#'AdamontQQCorr computes quantile-quantile correction of seasonal or decadal
#'forecast data using weather types
#'
#'@description This function computes a quantile mapping based on weather types
#'for experiment data (typically a hindcast) onto reference \code{obs},
#'typically provided by reanalysis data.
#'@author Paola Marson, \email{paola.marson@meteo.fr} for PROSNOW version
#'@author Lauriane Batté, \email{lauriane.batte@meteo.fr} for CSTools adaptation
#'
#'@param exp Array with named dimensions (such as \code{$data} array of 
#'  experiment data from an object of class \code{s2dv_cube}).
#'@param wt_exp Corresponding weather types (same dimensions as \code{exp} but
#'  lat/lon).
#'@param obs Array with named dimensions with reference data (can also be 
#'  \code{$data} array of class \code{s2dv_cube}). lat/lon dimensions can differ 
#'  from \code{exp} if non rectilinear latlon grids are used, in which case 
#'  regrid should be set to TRUE and .NearestNeighbors \code{NN} output should 
#'  be provided.
#'@param wt_obs Corresponding weather types (same dimensions as \code{obs} but
#'  lat/lon).
#'@param corrdims List of dimensions in \code{exp} for which quantile mapping 
#'  correction is applied.
#'@param londim Character name of longitude dimension in \code{exp} and 
#'  \code{obs}.
#'@param latdim Character name of latitude dimension in \code{exp} and 
#'  \code{obs}.
#'@param regrid (optional) Boolean indicating whether .NearestNeighbors
#'  regridding is needed.
#'@param NN (optional, if regrid = TRUE) List (output from .NearestNeighbors)
#'  maps (nlat, nlon) onto (nlat_o, nlon_o).
#'
#'@return An array (such as \code{$data} array from an object of class 
#'\code{s2dv_cube}) with named dimensions, containing experiment data on the 
#'lat/lon grid of \code{obs} array, corrected by quantile mapping depending on 
#'the weather types \code{wt_exp}
#'
#'@examples
#'wt_exp <- c(1,1,2,3,3,2,2,1,1,2,2,3)
#'dim(wt_exp) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3)
#'wt_obs <- c(3,3,1,2,2,2,2,1,3,1,1,2) 
#'dim(wt_obs) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3)
#'exp <- 1 : c(1 * 1 * 4 * 3 * 4 * 4)
#'dim(exp) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3,
#'              lat = 4, lon = 4)
#'obs <- 101 : c(100 + 1 * 1 * 4 * 3 * 4 * 4)
#'dim(obs) <- c(dataset = 1, member = 1, sdate = 4, ftime = 3,
#'              lat = 4, lon = 4)
#'exp_corr <- AdamontQQCorr(exp = exp, wt_exp = wt_exp, 
#'                          obs = obs, wt_obs = wt_obs, 
#'                          corrdims = c('dataset', 'member', 'sdate', 'ftime'))
#'@import qmap
#'@importFrom ClimProjDiags Subset
#'@import multiApply
#'@import abind
#'@export 
AdamontQQCorr <- function(exp, wt_exp, obs, wt_obs, 
			                    corrdims = c('member', 'sdate', 'ftime'), 
			                    londim = 'lon', latdim = 'lat', regrid = FALSE, 
                          NN = NULL) {

  dimnames <- names(dim(obs))
  dimnamesexp <- names(dim(exp))

  if (!(latdim %in% dimnames) || !(londim %in% dimnames)) {
    stop("'londim' or 'latdim' input doesn't match with 'obs' dimension names")
  }
  if (!(('time' %in% corrdims) || ('ftime' %in% corrdims))) {
    warning("Forecast time should be one of the dimensions for the correction",
            " specified in corrdims input list")
  }
  if (!all(corrdims %in% dimnamesexp)) {
    stop("Names in parameter 'corrdims' should match input dimension names.")
  }
  if (!all(dim(wt_exp) %in% dim(exp))) {
    stop("Dimensions for 'wt_exp' should match 'exp' except lat/lon")
  }
  if (!all(dim(wt_obs) %in% dim(obs))) {
    stop("Dimensions for 'wt_obs' should match 'obs' except lat/lon")
  }
  if ((regrid == 'TRUE') & is.null(NN)) {
    stop("regrid set to TRUE: provide nearest neighbors input NN")
  }
  # The regridding part should only be done if lat/lon dimensions of obs and
  # exp differ.
  if (regrid == 'TRUE') {
    obsdims <- names(dim(obs))
    poslat <- which(obsdims == latdim)
    poslon <- which(obsdims == londim)
    nlat_o <- dim(obs)[poslat]
    nlon_o <- dim(obs)[poslon]
    ilat_o <- array(c(1:nlat_o))
    names(dim(ilat_o))[1] <- latdim
    ilon_o <- array(c(1:nlon_o))
    names(dim(ilon_o))[1] <- londim
    ## First step if obs data is higher resolution than exp data is to use
    ## nearest neighbor to compute downscaling of exp data	
    exp_corr <- Apply(list(exp, ilat_o, ilon_o),
    target_dims = list(c(latdim,londim), latdim, londim), .getNN, NN = NN)$output1
    ## Reorder exp_corr dimensions to match exp dimensions
    dexpc <- match(names(dim(exp)), names(dim(exp_corr)))
    exp_corr <- aperm(exp_corr, dexpc)
    dimnames(exp_corr) <- dimnames(exp)[dexpc]
    ## Keep original wt_exp for remapping data
    wt_exp2 <- wt_exp
    ## Both exp and obs data are now on the same grid
  } else {
    ## exp lat/lon dimensions should match obs
    plat_exp <- which(dimnamesexp == latdim)
    plon_exp <- which(dimnamesexp == londim)
    plat_obs <- which(dimnames == latdim)
    plon_obs <- which(dimnames == londim)
    if ((dim(obs)[plon_obs] != dim(exp)[plon_exp]) ||
        (dim(obs)[plat_obs] != dim(exp)[plat_exp])) {
      stop("Parameters 'obs' and 'exp' should have same lon / lat ",
            "dimensions if regrid set to 'FALSE' (regular grid case).")
    }
    exp_corr <- exp
    ## Keep original wt_exp for remapping data
    wt_exp2 <- wt_exp
  }
   
  ## Use CST_QuantileMapping function for quantile mapping
  ## depending on weather type
  for (i in 1:(length(corrdims) - 1)) {
    obs <- MergeDims(obs, corrdims[i:(i+1)], rename_dim = corrdims[i+1])
    wt_obs <- MergeDims(wt_obs, corrdims[i:(i+1)], rename_dim = corrdims[i+1])
    exp_corr <- MergeDims(exp_corr, corrdims[i:(i+1)], rename_dim = corrdims[i+1])
    wt_exp2 <- MergeDims(wt_exp2, corrdims[i:(i+1)], rename_dim = corrdims[i+1])
  }

  names(dim(obs))[which(names(dim(obs)) == corrdims[length(corrdims)])] <- 'time'
  names(dim(wt_obs))[which(names(dim(wt_obs)) == corrdims[length(corrdims)])] <- 'time'
  names(dim(exp_corr))[which(names(dim(exp_corr)) == corrdims[length(corrdims)])] <- 'time'
  names(dim(wt_exp2))[which(names(dim(wt_exp2)) == corrdims[length(corrdims)])] <- 'time'

  # Split 'time' dim in weather types
  obs <- SplitDim(obs, split_dim = 'time', indices = as.vector(wt_obs), 
                  new_dim_name = 'type')
  exp_corr <- SplitDim(exp_corr, split_dim = 'time', indices = as.vector(wt_exp2), 
                       new_dim_name = 'type')
  ## Add NAs to exp_corr if needed to have compatible sample dimensions
  numtobs <- dim(obs)[which(names(dim(obs)) == 'time')]
  numtexp <- dim(exp_corr)[which(names(dim(exp_corr)) == 'time')]

  if (numtexp%%numtobs > 0) {
    ## Create extra dimension and include NAs
    ndimexp <- names(dim(exp_corr))
    ndimobs <- names(dim(obs))
    postime <- which(ndimexp == 'time')
    dimadd <- dim(exp_corr)
    dimadd[postime] <- ceiling(numtexp/numtobs) * numtobs - numtexp
    exp_corr <- abind::abind(exp_corr, array(NA, dimadd), along = postime)
    names(dim(exp_corr)) <- ndimexp
    exp_corr <- SplitDim(exp_corr, 'time', freq = numtobs, indices = NULL)
    dimobs <- c(dim(obs), 1)
    dim(obs) <- dimobs
    names(dim(obs)) <- c(ndimobs, 'index')
    res <- QuantileMapping(exp = exp_corr, obs = obs, memb_dim = 'index', 
                           sdate_dim = 'time', method = 'RQUANT', na.rm = TRUE)
    res <- MergeDims(res, c('time','index'))
    ## Remove the extra NA values added previously
    res <- Subset(res, along = 'time', indices = 1:numtexp)
  } else {
    ## Apply QuantileMapping to exp_corr depending on weather type
    exp_corr <- InsertDim(exp_corr, posdim = 1, lendim = 1, name = 'member')
    res <- QuantileMapping(exp = exp_corr, obs = obs, sdate_dim = 'time', 
                           samplemethod = 'RQUANT', na.rm = TRUE)
    dim(res) <- dim(res)[-which(names(dim(res)) == 'member')]
  }
  rm(exp_corr) # Save space in memory
  ## Reshape exp_corr data onto time dimension before 'Split'
  rep_pos <- array(NA, c(time = length(wt_exp2)))
  pos_time <- which(names(dim(res)) == 'time')
  pos_type <- which(names(dim(res)) == 'type')
  for (x in unique(wt_exp2)) {
    rep_pos[which(wt_exp2 == x)] <- 1:length(which(wt_exp2 == x))
  }
  exp_corr <- .unsplit_wtype(exp = res, wt_exp = wt_exp2, rep_pos = rep_pos,
                             pos_time = pos_time) 
  # Now reshape exp_corr data onto original dimensions
  dim(exp_corr) <- c(dim(wt_exp), dim(exp_corr)[-c(pos_time,pos_type)])
  return(exp_corr)
}

.getNN <- function(exp, ilat, ilon, NN) {
   return(exp[NN$imin_lat[ilat, ilon], NN$imin_lon[ilat, ilon]])
}

.unsplit_wtype <- function(exp = exp,dim_wt = 'type', wt_exp = wt_exp,
			                     dim_time = 'time', rep_pos = rep_pos, pos_time = 1) {
  # Initiate output
  new <- Subset(Subset(exp, along = dim_wt, indices = wt_exp[1]), 
                       along = dim_time, indices = rep_pos[1])
  dimnames <- names(dim(new))
  for (x in 2:length(wt_exp)) {
    dat <- Subset(Subset(exp, along = dim_wt, indices = wt_exp[x]), 
                         along = dim_time, indices = rep_pos[x])
    new <- abind::abind(new, dat, along = pos_time)
  }
  names(dim(new)) <- dimnames
  return(new)
}

#'ADAMONT Nearest Neighbors computes the distance between reference data grid 
#'centroid and SF data grid
#'
#'@author Paola Marson, \email{paola.marson@meteo.fr} for PROSNOW version
#'@author Lauriane Batté, \email{lauriane.batte@meteo.fr} for CSTools adaptation 
#'@description This function computes the nearest neighbor for each reference 
#'data (lon, lat) point in the experiment dataset by computing the distance 
#'between the reference dataset grid and the experiment data. This is the first 
#'step in the ADAMONT method adapted from Verfaillie et al. (2018).
#'
#'@param method A string among three options ('ADA': standard ADAMONT distance, 
#'  'simple': lon/lat straight euclidian distance, 'radius': distance on the 
#'  sphere).
#'@param exp An object of class \code{s2dv_cube} as returned by \code{CST_Load} 
#'  function, containing the seasonal forecast experiment longitudes in 
#'  \code{$lon} and latitudes in \code{$lat}.
#'@param obs An object of class \code{s2dv_cube} as returned by \code{CST_Load} 
#'  function, containing the reference data on a different grid, with longitudes 
#'  in \code{$lon} and latitudes in \code{$lat}.
#'
#'@return NN a list, containing the following:
#'\itemize{
#'  \item{'min_lon': array of dimensions \code{obs$lon} giving the longitude of 
#'        closest gridpoint in exp.}
#'  \item{'min_lat': array of dimensions \code{obs$lat} giving the latitude of 
#'        closest gridpoint in exp.}
#'  \item{'imin_lon': array of dimensions \code{obs$lon} giving the longitude 
#'        index of closest gridpoint in exp.}
#'  \item{'imin_lat': array of dimensions \code{obs$lat} giving the latitude 
#'        index of closest gridpoint in exp.}
#'}
#' 
#'@importFrom ClimProjDiags Subset
#'@import ncdf4
#'@noRd
.NearestNeighbors <- function (exp, obs, method = 'ADA') {
  # Check 's2dv_cube'
  if (!inherits(exp, 's2dv_cube') || !inherits(obs, 's2dv_cube')) {
    stop("Inputs 'exp' and 'obs' must be of class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  # Check 'exp' and 'obs' object structure
  if (!all(c('data', 'coords') %in% names(exp))) {
    stop("Parameter 'exp' must have 'data' and 'coords' elements ",
         "within the 's2dv_cube' structure.")
  }
  
  if (!any(names(exp$coords) %in% .KnownLonNames()) | 
      !any(names(exp$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names of parameter 'exp' do not match any ",
         "of the names accepted by the package.")
  }
  if (!all(names(exp$coords) %in% names(obs$coords))) {
    stop("Coordinates names must be equal in 'exp' and in 'obs'.")
  }

  lon_name <- names(exp$coords)[[which(names(exp$coords) %in% .KnownLonNames())]]
  lat_name <- names(exp$coords)[[which(names(exp$coords) %in% .KnownLatNames())]]
  exp_lon <- exp$coords[[lon_name]]
  exp_lat <- exp$coords[[lat_name]]
  obs_lon <- obs$coords[[lon_name]]
  obs_lat <- obs$coords[[lat_name]]
  dim_exp_lon <- dim(exp_lon)
  dim_exp_lat <- dim(exp_lat)
  dim_obs_lon <- dim(obs_lon)
  dim_obs_lat <- dim(obs_lat)

  # Check if one of the grids is non-regular:
  if ((length(dim_exp_lon) == 2) || (length(dim_obs_lon) == 2)) {
    # Flatten longitudes and latitudes in case of 2-D longitudes and latitudes (Lambert grids, etc.)
    if ((length(dim_exp_lon) == 2) & (length(dim_exp_lat) == 2)) {
      dim(exp_lon) <- c(dim_exp_lon[1] * dim_exp_lon[2])
      dim(exp_lat) <- c(dim_exp_lat[1] * dim_exp_lat[2])
    }
    if ((length(dim_obs_lon) == 2) & (length(dim_obs_lat) == 2)) {
      dim(obs_lon) <- c(dim_obs_lon[1] * dim_obs_lon[2])
      dim(obs_lat) <- c(dim_obs_lat[1] * dim_obs_lat[2])
    }
    # Now lat and lon arrays have 1 dimension, length npt (= nlat*nlon)
    OBS_grid <- cbind(obs_lon, obs_lat)
    EXP_grid <- cbind(exp_lon, exp_lat)
    dist_min <- min_lon <- min_lat <- imin_lon <- imin_lat <- array(dim = nrow(OBS_grid))
    if (method == 'ADA') {
      C <- cos(OBS_grid[,2] * pi/180)^2
      for (i in 1:nrow(OBS_grid)) {
        dist <- (OBS_grid[i, 2] - EXP_grid[, 2])^2 + 
                 C[i] * (OBS_grid[i, 1] - EXP_grid[, 1])^2
        dist_min[i] < -min(dist)
        min_lon[i] <- EXP_grid[which.min(dist), 1]
        min_lat[i] <- EXP_grid[which.min(dist), 2]
        imin_lon[i] <- which(exp_lon == min_lon[i])
        imin_lat[i] <- which(exp_lat == min_lat[i])
      }
    } else if (method == 'simple') {
      for (i in 1:nrow(OBS_grid)) {
        dist <- (OBS_grid[i, 2] - EXP_grid[, 2])^2 + (OBS_grid[i, 1] - EXP_grid[, 1])^2
        dist_min[i] <- min(dist)
        min_lon[i] <- EXP_grid[which.min(dist), 1]
        min_lat[i] <- EXP_grid[which.min(dist), 2]
        imin_lon[i] < -which(exp_lon == min_lon[i]) 
        imin_lat[i] <- which(exp_lat == min_lat[i])
      }
    } else if (method == 'radius') {
      R <- 6371e3 # metres, Earth radius
      EXP_gridr <- EXP_grid * pi/180
      OBS_gridr <- OBS_grid * pi/180
      for (i in 1:nrow(OBS_grid)) {
        a <- sin((OBS_gridr[i,2] - EXP_gridr[,2])/2)^2 + cos(OBS_gridr[i, 2]) * 
             cos(EXP_gridr[, 2]) * sin((OBS_gridr[i, 1] - EXP_gridr[, 1])/2)^2
        c <- 2*atan2(sqrt(a), sqrt(1 - a))
        dist <- R*c
        dist_min[i] <- min(dist)
        min_lon[i] <- EXP_grid[which.min(dist), 1]
        min_lat[i] <- EXP_grid[which.min(dist), 2]
        imin_lon[i] <- which(exp_lon == min_lon[i])
        imin_lat[i] <- which(exp_lat == min_lat[i])
      }
    } else {
      stop("AdamontNearestNeighbors supports method = 'ADA', 'simple' or 'radius' only.")
    }
  
    # Reshape outputs to original grid
    dim(min_lon)=dim_obs_lon
    dim(min_lat)=dim_obs_lat
    dim(imin_lon)=dim_obs_lon
    dim(imin_lat)=dim_obs_lat

  } else {
  # Regular lon/lat grid case: has been handled by CST_Load()
    stop(paste0("AdamontNearestNeighbors is meant for non-regular lat/lon ", 
                "grids; use e.g. CST_Load to interpolate exp onto obs grid"))
  }

  NN = list(min_lon = min_lon, min_lat = min_lat, imin_lon = imin_lon, 
            imin_lat = imin_lat)

  return(NN)
}
