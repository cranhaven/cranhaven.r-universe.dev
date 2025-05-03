#'@rdname CST_RFSlope
#'@title RainFARM spectral slopes from a CSTools object
#'
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'
#'@description This function computes spatial spectral slopes from a CSTools 
#'object to be used for RainFARM stochastic precipitation downscaling method and 
#'accepts a CSTools object (of the class 's2dv_cube') as input.
#'
#'@param data An object of the class 's2dv_cube', containing the spatial 
#'  precipitation fields to downscale. The data object is expected to have an 
#'  element named \code{$data} with at least two spatial dimensions named "lon" 
#'  and "lat" and one or more dimensions over which to average these slopes, 
#'  which can be specified by parameter \code{time_dim}.
#'@param kmin First wavenumber for spectral slope (default \code{kmin=1}).
#'@param time_dim String or character array with name(s) of dimension(s) (e.g. 
#'  "ftime", "sdate", "member" ...) over which to compute spectral slopes. If a 
#'  character array of dimension names is provided, the spectral slopes will be 
#'  computed as an average over all elements belonging to those dimensions. If 
#'  omitted one of c("ftime", "sdate", "time")  is searched and the first one 
#'  with more than one element is chosen.
#'@param ncores Is an integer that indicates the number of cores for parallel 
#'  computations using multiApply function. The default value is one.
#'@return CST_RFSlope() returns spectral slopes using the RainFARM convention
#'  (the logarithmic slope of k*|A(k)|^2 where A(k) are the spectral amplitudes).
#'  The returned array has the same dimensions as the \code{exp} element of the 
#'  input object, minus the dimensions specified by \code{lon_dim}, 
#'  \code{lat_dim} and \code{time_dim}.
#'@examples
#'exp <- 1 : (2 * 3 * 4 * 8 * 8)
#'dim(exp) <- c(dataset = 1, member = 2, sdate = 3, ftime = 4, lat = 8, lon = 8)
#'lon <- seq(10, 13.5, 0.5)
#'lat <- seq(40, 43.5, 0.5)
#'coords <- list(lon = lon, lat = lat)
#'data <- list(data = exp, coords = coords)
#'class(data) <- 's2dv_cube'
#'slopes <- CST_RFSlope(data)
#'@import multiApply
#'@import rainfarmr
#'@importFrom ClimProjDiags Subset
#'@export
CST_RFSlope <- function(data, kmin = 1, time_dim = NULL, ncores = NULL) {

  # Check 's2dv_cube'
  if (!inherits(data, "s2dv_cube")) {
    stop("Parameter 'data' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  
  # Check dimensions
  if (!any(names(dim(data$data)) %in% .KnownLonNames()) | 
      !any(names(dim(data$data)) %in% .KnownLatNames())) {
    stop("Spatial dimension names do not match any of the names accepted by ",
         "the package.")
  }

  lon_name <- names(dim(data$data))[[which(names(dim(data$data)) %in% .KnownLonNames())]]
  lat_name <- names(dim(data$data))[[which(names(dim(data$data)) %in% .KnownLatNames())]]

  slopes <- RFSlope(data$data, kmin, time_dim,
                    lon_dim = lon_name, lat_dim = lat_name)

  return(slopes)
}

#'@rdname RFSlope
#'@title RainFARM spectral slopes from an array (reduced version)
#'
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'
#'@description This function computes spatial spectral slopes from an array,
#'to be used for RainFARM stochastic precipitation downscaling method.
#'
#'@param data Array containing the spatial precipitation fields to downscale.
#'  The input array is expected to have at least two dimensions named "lon" and 
#'  "lat" by default (these default names can be changed with the \code{lon_dim} 
#'  and \code{lat_dim} parameters) and one or more dimensions over which to 
#'  average the slopes, which can be specified by parameter \code{time_dim}.
#'@param kmin First wavenumber for spectral slope (default \code{kmin=1}).
#'@param time_dim String or character array with name(s) of dimension(s)
#'  (e.g. "ftime", "sdate", "member" ...) over which to compute spectral slopes.
#'  If a character array of dimension names is provided, the spectral slopes
#'  will be computed as an average over all elements belonging to those dimensions.
#'  If omitted one of c("ftime", "sdate", "time")  is searched and the first one
#'  with more than one element is chosen.
#'@param lon_dim Name of lon dimension ("lon" by default).
#'@param lat_dim Name of lat dimension ("lat" by default).
#'@param ncores is an integer that indicates the number of cores for parallel 
#'  computations using multiApply function. The default value is one.
#'
#'@return RFSlope() returns spectral slopes using the RainFARM convention
#'(the logarithmic slope of k*|A(k)|^2 where A(k) are the spectral amplitudes).
#'The returned array has the same dimensions as the input array,
#'minus the dimensions specified by \code{lon_dim}, \code{lat_dim} and \code{time_dim}.
#'@examples
#'# Example for the 'reduced' RFSlope function 
#'# Create a test array with dimension 8x8 and 20 timesteps, 
#'# 3 starting dates and 20 ensemble members.
#'pr <- 1:(4*3*8*8*20)
#'dim(pr) <- c(ensemble = 4, sdate = 3, lon = 8, lat = 8, ftime = 20)
#'# Compute the spectral slopes ignoring the wavenumber
#'# corresponding to the largest scale (the box)
#'slopes <- RFSlope(pr, kmin = 2, time_dim = 'ftime')
#'@import multiApply
#'@import rainfarmr
#'@importFrom ClimProjDiags Subset
#'@export
RFSlope <- function(data, kmin = 1, time_dim = NULL,
                    lon_dim = "lon", lat_dim = "lat", ncores = NULL) {
  # Know spatial coordinates names
  if (!all(c(lon_dim, lat_dim) %in% names(dim(data)))) {
    stop("Spatial coordinate names do not match data dimension names.")
  }

  if (length(ncores) > 1) {
    ncores = ncores[1]
    warning("Parameter 'ncores' has length > 1 and only the first element will be used.")
  }
  if (!is.null(ncores)) {
    ncores <- round(ncores)
    if (ncores == 0) {
      ncores = NULL
    }
  }
  # Ensure input  grid is square and with even dimensions
  if ( (dim(data)[lon_dim] != dim(data)[lat_dim]) |
       (dim(data)[lon_dim] %% 2 == 1)) {
    warning("Input data are expected to be on a square grid",
            " with an even number of pixels per side.")
    nmin <- min(dim(data)[lon_dim], dim(data)[lat_dim])
    nmin <- floor(nmin / 2) * 2
    data <- .subset(data, lat_dim, 1:nmin)
    data <- .subset(data, lon_dim, 1:nmin)
    warning(paste("The input data have been cut to a square of",
            nmin, "pixels on each side."))
  }

  # Check/detect time_dim
  if (is.null(time_dim)) {
    time_dim_names <- c("ftime", "sdate", "time")
    time_dim_num <- which(time_dim_names %in% names(dim(data)))
    if (length(time_dim_num) > 0) {
    # Find time dimension with length > 1
      ilong <- which(dim(data)[time_dim_names[time_dim_num]] > 1)
      if (length(ilong) > 0) {
        time_dim <- time_dim_names[time_dim_num[ilong[1]]]
      } else {
        stop("No time dimension longer than one found.")
      }
    } else {
      stop("Could not automatically detect a target time dimension ",
           "in the provided data in 'data'.")
    }
    warning(paste("Selected time dim: ", time_dim))
  }

  # reorder and group time_dim together at the end
  cdim0 <- dim(data)
  imask <- names(cdim0) %in% time_dim
  data <- .aperm2(data, c(which(!imask), which(imask)))
  cdim <- dim(data)
  ind <- 1 : length(which(!imask))
  # compact (multiply) time_dim dimensions
  dim(data) <- c(cdim[ind], rainfarm_samples = prod(cdim[-ind]))

  # Repeatedly apply .RFSlope
  result <- Apply(data, c(lon_dim, lat_dim, "rainfarm_samples"),
                  .RFSlope, kmin, ncores = ncores)$output1

  return(slopes = result)
}

#'Atomic RFSlope
#'@param pr precipitation array to downscale with dims (lon, lat, time).
#'@param kmin first wavenumber for spectral slope (default kmin=1).
#'@return .RFSlope returns a scalar spectral slope using the RainFARM convention
#'(the logarithmic slope of k*|A(k)|^2 where A(k) is the spectral amplitude).
#'@noRd
.RFSlope <- function(pr, kmin) {
  if (any(is.na(pr))) {
    posna <- unlist(lapply(1:dim(pr)['rainfarm_samples'],
                           function(x){!is.na(pr[1, 1, x])}))
    pr <- Subset(pr, 'rainfarm_samples', posna)
  }
  fxp <- fft2d(pr)
  sx <- fitslope(fxp, kmin = kmin)
  return(sx)
}

# Function to generalize through do.call() n-dimensional array subsetting 
# and array indexing. Derived from Stack Overflow issue
# https://stackoverflow.com/questions/14500707/select-along-one-of-n-dimensions-in-array
.subset <- function(field, dim_name, range, drop = FALSE) {

  idim  <- which(names(dim(field)) %in% dim_name)
  # Create list representing arguments supplied to [
  # bquote() creates an object corresponding to a missing argument
  indices <- rep(list(bquote()), length(dim(field)))
  indices[[idim]] <- range

  # do.call on the indices
  field <- do.call("[",c(list(field), indices, list(drop = drop)))

  return(field)
}
