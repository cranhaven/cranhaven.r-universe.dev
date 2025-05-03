#'Compute climatological weights for RainFARM stochastic precipitation downscaling
#'
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'
#'@description Compute climatological ("orographic") weights from a fine-scale 
#'precipitation climatology file.
#'@references Terzago, S., Palazzi, E., & von Hardenberg, J. (2018).
#'Stochastic downscaling of precipitation in complex orography: 
#'A simple method to reproduce a realistic fine-scale climatology.
#'Natural Hazards and Earth System Sciences, 18(11),
#'2825-2840. \doi{10.5194/nhess-18-2825-2018}.
#'@param climfile Filename of a fine-scale precipitation climatology. The file 
#'  is expected to be in NetCDF format and should contain at least one 
#'  precipitation field. If several fields at different times are provided,
#'  a climatology is derived by time averaging. Suitable climatology files could 
#'  be for example a fine-scale precipitation climatology from a high-resolution 
#'  regional climate model (see e.g. Terzago et al. 2018), a local 
#'  high-resolution gridded climatology from observations, or a reconstruction 
#'  such as those which can be downloaded from the WORLDCLIM 
#'  (\url{https://www.worldclim.org}) or CHELSA (\url{https://chelsa-climate.org/}) 
#'  websites. The latter data will need to be converted to NetCDF format before 
#'  being used (see for example the GDAL tools (\url{https://gdal.org/}). It  
#'  could also be an 's2dv_cube' object.
#'@param nf Refinement factor for downscaling (the output resolution is 
#'  increased by this factor).
#'@param lon Vector of longitudes. 
#'@param lat Vector of latitudes. The number of longitudes and latitudes is 
#'  expected to be even and the same. If not the function will perform a 
#'  subsetting to ensure this condition.
#'@param varname Name of the variable to be read from \code{climfile}.
#'@param fsmooth Logical to use smooth conservation (default) or large-scale 
#'  box-average conservation. 
#'@param lonname A character string indicating the name of the longitudinal 
#'  dimension set as 'lon' by default.
#'@param latname A character string indicating the name of the latitudinal 
#'  dimension set as 'lat' by default.
#'@param ncores An integer that indicates the number of cores for parallel 
#'  computations using multiApply function. The default value is one.
#'
#'@return An object of class 's2dv_cube' containing in matrix \code{data} the 
#'weights with dimensions (lon, lat).
#'@examples
#'# Create weights to be used with the CST_RainFARM() or RainFARM() functions
#'# using an external random data in the form of 's2dv_cube'.
#'obs <- rnorm(2 * 3 * 4 * 8 * 8)
#'dim(obs) <- c(dataset = 1, member = 2, sdate = 3, ftime = 4, lat = 8, lon = 8)
#'lon <- seq(10, 13.5, 0.5)
#'lat <- seq(40, 43.5, 0.5)
#'coords <- list(lon = lon, lat = lat)
#'data <- list(data = obs, coords = coords)
#'class(data) <- "s2dv_cube"
#'res <- CST_RFWeights(climfile = data, nf = 3, lon, lat, lonname = 'lon', 
#'                     latname = 'lat', fsmooth = TRUE)
#'@import ncdf4
#'@import rainfarmr
#'@import multiApply
#'@importFrom utils tail
#'@importFrom utils head
#'@export
CST_RFWeights <- function(climfile, nf, lon, lat, varname = NULL, fsmooth = TRUE, 
                          lonname = 'lon', latname = 'lat', ncores = NULL) {
  if (!inherits(climfile, "s2dv_cube")) {
    if (!is.null(varname) & !is.character(varname)) {
      stop("Parameter 'varname' must be a character string indicating the name",
           " of the variable to be read from the file.")
    }
  }
  # Ensure input  grid is square and with even dimensions
  if ((length(lat) != length(lon)) | (length(lon) %% 2 == 1)) {
    warning("Input data are expected to be on a square grid",
            " with an even number of pixels per side.")
    nmin <- min(length(lon), length(lat))
    nmin <- floor(nmin / 2) * 2
    lon <- lon[1:nmin]
    lat <- lat[1:nmin]
    warning("The input data have been cut to the range.")
    warning(paste0("lon: [", lon[1], ", ", lon[length(lon)], "] ",
                 " lat: [", lat[1], ", ", lat[length(lat)], "]"))
  }

  if (is.character(climfile)) {  
    ncin <- nc_open(climfile)
    latin <- ncvar_get(ncin, grep(latname, attributes(ncin$dim)$names,
                                  value = TRUE))
    lonin <- ncvar_get(ncin, grep(lonname, attributes(ncin$dim)$names,
                                  value = TRUE))
    if (varname == "") {
      varname <- grep("bnds", attributes(ncin$var)$names,
                      invert = TRUE, value = TRUE)[1]
    }
    zclim <- ncvar_get(ncin, varname)
    nc_close(ncin)
  } else if (inherits(climfile, "s2dv_cube")) {
    # Check object structure
    if (!all(c('data', 'coords') %in% names(climfile))) {
      stop("Parameter 'climfile' must have 'data' and 'coords' elements ",
           "within the 's2dv_cube' structure.")
    }
    # Check coordinates
    if (!any(names(climfile$coords) %in% .KnownLonNames()) | 
        !any(names(climfile$coords) %in% .KnownLatNames())) {
      stop("Spatial coordinate names do not match any of the names accepted by ",
          "the package.")
    }
    loncoordname <- names(climfile$coords)[[which(names(climfile$coords) %in% .KnownLonNames())]]
    latcoordname <- names(climfile$coords)[[which(names(climfile$coords) %in% .KnownLatNames())]]

    zclim <- climfile$data
    latin <-  as.vector(climfile$coords[[latcoordname]])
    lonin <- as.vector(climfile$coords[[loncoordname]])
  } else {
    stop("Parameter 'climfile' is expected to be a character string indicating",
         " the path to the files or an object of class 's2dv_cube'.")
  }
  # Check dim names and order
  if (length(names(dim(zclim))) < 1) {
    stop("The dataset provided in 'climfile' requires dimension names.")
  }

  result <- RF_Weights(zclim, latin, lonin, nf, lat, lon, fsmooth = fsmooth,
                       lonname = lonname, latname = latname, ncores = ncores) 
  if (inherits(climfile, "s2dv_cube")) {
    climfile$data <- result$data
    climfile$coords[[loncoordname]] <- result[[lonname]]
    climfile$coords[[latcoordname]] <- result[[latname]]
  } else { 
    climfile <- NULL
    climfile$data <- result
    climfile$coords[[lonname]] <- result[[lonname]]
    climfile$coords[[latname]] <- result[[latname]]
  }
  return(climfile)
}
#'Compute climatological weights for RainFARM stochastic precipitation downscaling
#'
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'
#'@description Compute climatological ("orographic") weights from a fine-scale 
#'precipitation climatology file.
#'@references Terzago, S., Palazzi, E., & von Hardenberg, J. (2018).
#'Stochastic downscaling of precipitation in complex orography: 
#'A simple method to reproduce a realistic fine-scale climatology.
#'Natural Hazards and Earth System Sciences, 18(11),
#'2825-2840. \doi{10.5194/nhess-18-2825-2018}.
#'@param zclim A multi-dimensional array with named dimension containing at 
#'  least one precipiation field with spatial dimensions. 
#'@param lonin A vector indicating the longitudinal coordinates corresponding to 
#'  the \code{zclim} parameter.
#'@param latin A vector indicating the latitudinal coordinates corresponding to 
#'  the \code{zclim} parameter.
#'@param nf Refinement factor for downscaling (the output resolution is 
#'  increased by this factor).
#'@param lon Vector of longitudes. 
#'@param lat Vector of latitudes. The number of longitudes and latitudes is 
#'  expected to be even and the same. If not the function will perform a 
#'  subsetting to ensure this condition.
#'@param fsmooth Logical to use smooth conservation (default) or large-scale 
#'  box-average conservation. 
#'@param lonname A character string indicating the name of the longitudinal 
#'  dimension set as 'lon' by default.
#'@param latname A character string indicating the name of the latitudinal 
#'  dimension set as 'lat' by default.
#'@param ncores An integer that indicates the number of cores for parallel 
#'  computations using multiApply function. The default value is one.
#'
#'@return An object of class 's2dv_cube' containing in matrix \code{data} the 
#'weights with dimensions (lon, lat).
#'@examples
#'a <- array(1:2500, c(lat = 50, lon = 50))
#'res <- RF_Weights(a, seq(0.1 ,5, 0.1), seq(0.1 ,5, 0.1), 
#'                  nf = 5, lat = 1:5, lon = 1:5)
#'@import ncdf4
#'@import rainfarmr
#'@import multiApply
#'@importFrom utils tail
#'@importFrom utils head
#'@export
RF_Weights <- function(zclim, latin, lonin, nf, lat, lon, fsmooth = TRUE,
                       lonname = 'lon', latname = 'lat', ncores = NULL) {
  x <- Apply(list(zclim), target_dims = c(lonname, latname), fun = rf_weights,
             latin = latin, lonin = lonin, nf = nf, lat = lat, lon = lon,
             lonname = lonname, latname = latname, 
             fsmooth = fsmooth, ncores = ncores)$output1
  grid <- lon_lat_fine(lon, lat, nf)
  res <- NULL
  res$data <- x
  res[[lonname]] <- grid$lon
  res[[latname]] <- grid$lon
  return(res)
}

rf_weights <- function(zclim, latin, lonin, nf, lat, lon, lonname = 'lon', 
                       latname = 'lat', fsmooth = TRUE) {
  # Check if lon and lat need to be reversed
  if (lat[1] > lat[2]) {
    lat <- rev(lat)
    frev <- TRUE
  } else {
    frev <- FALSE
  }
  if (latin[1] > latin[2]) {
    latin <- rev(latin)
    zclim <- zclim[, seq(dim(zclim)[2], 1)]
  }
  # If lon is not monotonic make it so
  if (lon[1] > tail(lon, 1)) {
    lon <- (lon - 360) * (lon > tail(lon, 1)) + lon * (lon <= tail(lon, 1))
  }
  # Is the reference climatology global in the zonal direction ?
  # Test if the the first and the last longitude are close
  dx <- abs((tail(lonin, 1) %% 360) - (lonin[1] %% 360))
  # Shortest distance on a torus
  if (dx > 180) {
        dx <- 360 - dx
  }
  # If this distance is smaller than twice the grid spacing we are global
  if (dx <= (lonin[2] - lonin[1]) * 2) {
    # Is the target area not fully inside the reference climatology area ?
    if (lon[1] < lonin[1]) {
      # Shift lonin to the west by 180 degrees
      nn <- length(lonin)
      zclim0 <- zclim
      zclim[1:(nn / 2), ] <- zclim0[(nn / 2 + 1):nn, ]
      zclim[(nn / 2 + 1):nn, ] <- zclim0[1:(nn / 2), ]
      lonin <- lonin - 180
    } else if (tail(lon, 1) > tail(lonin, 1)) {
      # Shift lonin to the east by 180 degrees
      nn <- length(lonin)
      zclim0 <- zclim
      zclim[1:(nn / 2), ] <- zclim0[(nn / 2 + 1):nn, ]
      zclim[(nn / 2 + 1):nn, ] <- zclim0[1:(nn / 2), ]
      lonin <- lonin + 180
    }
  }
  ww <- rfweights(zclim, lonin, latin, lon, lat, nf, fsmooth = fsmooth)
  if (frev) {
    ww <- ww[, seq(dim(ww)[2], 1)]
  }
  attributes(dim(ww))$names <- c(lonname, latname)
  return(ww)
}
