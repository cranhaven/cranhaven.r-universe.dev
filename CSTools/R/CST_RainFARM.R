#'@rdname CST_RainFARM
#'@title RainFARM stochastic precipitation downscaling of a CSTools object
#'
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'
#'@description This function implements the RainFARM stochastic precipitation
#'downscaling method and accepts a CSTools object (an object of the class 
#''s2dv_cube' as provided by `CST_Load`) as input.
#'Adapted for climate downscaling and including orographic correction
#'as described in Terzago et al. 2018.
#'@references Terzago, S. et al. (2018). NHESS 18(11), 2825-2840.
#'\doi{10.5194/nhess-18-2825-2018};
#'D'Onofrio et al. (2014), J of Hydrometeorology 15, 830-843; Rebora et. al. 
#'(2006), JHM 7, 724.
#'@param data An object of the class 's2dv_cube' as returned by `CST_Load`, 
#'  containing the spatial precipitation fields to downscale.
#'  The data object is expected to have an element named \code{$data} with at 
#'  least two spatial dimensions named "lon" and "lat" and one or more 
#'  dimensions over which to compute average spectral slopes (unless specified 
#'  with parameter \code{slope}), which can be specified by parameter 
#'  \code{time_dim}. The number of longitudes and latitudes in the input data is 
#'  expected to be even and the same. If not the function will perform a 
#'  subsetting to ensure this condition.
#'@param weights Matrix with climatological weights which can be obtained using
#'  the \code{CST_RFWeights} function. If \code{weights = 1.} (default) no  
#'  weights are used. The names of these dimensions must be at least 'lon' and 
#'  'lat'. 
#'@param nf Refinement factor for downscaling (the output resolution is 
#'  increased by this factor).
#'@param slope Prescribed spectral slope. The default is \code{slope = 0.}
#'  meaning that the slope is determined automatically over the dimensions 
#'  specified by \code{time_dim}. A 1D array with named dimension can be 
#'  provided (see details and examples).
#'@param kmin First wavenumber for spectral slope (default: \code{kmin = 1}).
#'@param nens Number of ensemble members to produce (default: \code{nens = 1}).
#'@param fglob Logical to conserve global precipitation over the domain 
#'  (default: FALSE).
#'@param fsmooth Logical to conserve precipitation with a smoothing kernel 
#'  (default: TRUE).
#'@param time_dim String or character array with name(s) of dimension(s)
#'  (e.g. "ftime", "sdate", "member" ...) over which to compute spectral slopes.
#'  If a character array of dimension names is provided, the spectral slopes
#'  will be computed as an average over all elements belonging to those 
#'  dimensions. If omitted one of c("ftime", "sdate", "time") is searched and 
#'  the first one with more than one element is chosen.
#'@param verbose Logical for verbose output (default: FALSE).
#'@param drop_realization_dim Logical to remove the "realization" stochastic 
#'  ensemble dimension, needed for saving data through function CST_SaveData 
#'  (default: FALSE) with the following behaviour if set to TRUE:
#'  \enumerate{
#'    \item{if \code{nens == 1}: the dimension is dropped;}
#'    \item{if \code{nens > 1} and a "member" dimension exists: the "realization" 
#'    and "member" dimensions are compacted (multiplied) and the resulting 
#'    dimension is named "member";}
#'    \item{if \code{nens > 1} and a "member" dimension does not exist: the 
#'    "realization" dimension is renamed to "member".}
#'  }
#'@param nprocs The number of parallel processes to spawn for the use for 
#'  parallel computation in multiple cores. (default: 1) 
#'
#'@return CST_RainFARM() returns a downscaled CSTools object (i.e., of the 
#'class 's2dv_cube'). If \code{nens > 1} an additional dimension named 
#'"realization" is added to the \code{$data} array after the "member" dimension 
#'(unless \code{drop_realization_dim = TRUE} is specified). The ordering of the 
#'remaining dimensions in the \code{$data} element of the input object is 
#'maintained.
#'@details Wether parameter 'slope' and 'weights' presents seasonality 
#'dependency, a dimension name should match between these parameters and the 
#'input data in parameter 'data'. See example 2 below where weights and slope 
#'vary with 'sdate' dimension.
#'@examples
#'# Example 1: using CST_RainFARM for a CSTools object
#'nf <- 8   # Choose a downscaling by factor 8
#'exp <- 1 : (2 * 3 * 4 * 8 * 8)
#'dim(exp) <- c(dataset = 1, member = 2, sdate = 3, ftime = 4, lat = 8, lon = 8)
#'lon <- seq(10, 13.5, 0.5)
#'lat <- seq(40, 43.5, 0.5)
#'coords <- list(lon = lon, lat = lat)
#'data <- list(data = exp, coords = coords)
#'class(data) <- 's2dv_cube'
#'# Create a test array of weights
#'ww <- array(1., dim = c(lon = 8 * nf, lat = 8 * nf))
#'res <- CST_RainFARM(data, nf = nf, weights = ww, nens = 3, time_dim = 'ftime')
#'@import multiApply
#'@import rainfarmr
#'@importFrom ClimProjDiags Subset
#'@importFrom abind abind
#'@export
CST_RainFARM <- function(data, weights = 1., slope = 0, nf, kmin = 1,
                         nens = 1, fglob = FALSE, fsmooth = TRUE,
                         nprocs = 1, time_dim = NULL, verbose = FALSE,
                         drop_realization_dim = FALSE) {
  # Check 's2dv_cube'
  if (!inherits(data, "s2dv_cube")) {
    stop("Parameter 'data' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  # Check 'exp' object structure
  if (!all(c('data', 'coords') %in% names(data))) {
    stop("Parameter 'data' must have 'data' and 'coords' elements ",
         "within the 's2dv_cube' structure.")
  }
  # Check coordinates
  if (!any(names(data$coords) %in% .KnownLonNames()) | 
      !any(names(data$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names do not match any of the names accepted by ",
         "the package.")
  }
  # Check dimensions
  if (!any(names(dim(data$data)) %in% .KnownLonNames()) | 
      !any(names(dim(data$data)) %in% .KnownLatNames())) {
    stop("Spatial dimension names do not match any of the names accepted by ",
         "the package.")
  }

  lon <- names(data$coords)[[which(names(data$coords) %in% .KnownLonNames())]]
  lat <- names(data$coords)[[which(names(data$coords) %in% .KnownLatNames())]]

  lon_name <- names(dim(data$data))[[which(names(dim(data$data)) %in% .KnownLonNames())]]
  lat_name <- names(dim(data$data))[[which(names(dim(data$data)) %in% .KnownLatNames())]]

  res <- RainFARM(data = data$data, 
                  lon = as.vector(data$coords[[lon]]), 
                  lat = as.vector(data$coords[[lat]]),
                  nf = nf, weights = weights, nens, slope, kmin, 
                  fglob, fsmooth, nprocs, time_dim, 
                  lon_dim = lon_name, lat_dim = lat_name,
                  drop_realization_dim, verbose)

  data$data <- res$data
  data$coords[[lon]] <- res[[lon_name]]
  data$coords[[lat]] <- res[[lat_name]]

  return(data)
}

#'@rdname RainFARM
#'@title RainFARM stochastic precipitation downscaling (reduced version)
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'@description This function implements the RainFARM stochastic precipitation downscaling method
#'and accepts in input an array with named dims ("lon", "lat")
#'and one or more dimension (such as "ftime", "sdate" or "time")
#'over which to average automatically determined spectral slopes.
#'Adapted for climate downscaling and including orographic correction.
#'References:
#'Terzago, S. et al. (2018). NHESS 18(11), 2825-2840. \doi{10.5194/nhess-18-2825-2018},
#'D'Onofrio et al. (2014), J of Hydrometeorology 15, 830-843; Rebora et. al. 
#'(2006), JHM 7, 724.
#'@param data Precipitation array to downscale. The input array is expected to 
#'  have at least two dimensions named "lon" and "lat" by default (these default 
#'  names can be changed with the \code{lon_dim} and \code{lat_dim} parameters)
#'  and one or more dimensions over which to average these slopes, which can be 
#'  specified by parameter \code{time_dim}. The number of longitudes and 
#'  latitudes in the input data is expected to be even and the same. If not
#'  the function will perform a subsetting to ensure this condition.
#'@param lon Vector or array of longitudes.
#'@param lat Vector or array of latitudes.
#'@param weights Multi-dimensional array with climatological weights which can 
#'  be obtained using the \code{CST_RFWeights} function. If \code{weights = 1.} 
#'  (default) no weights are used. The names of these dimensions must be at 
#'  least the same longitudinal and latitudinal dimension names as data.
#'@param nf Refinement factor for downscaling (the output resolution is 
#'  increased by this factor). 
#'@param slope Prescribed spectral slope. The default is \code{slope = 0.}
#'  meaning that the slope is determined automatically over the dimensions 
#'  specified by \code{time_dim}. A 1D array with named dimension can be 
#'  provided (see details and examples).
#'@param kmin First wavenumber for spectral slope (default: \code{kmin = 1}).
#'@param nens Number of ensemble members to produce (default: \code{nens = 1}).
#'@param fglob Logical to conseve global precipitation over the domain 
#'  (default: FALSE).
#'@param fsmooth Logical to conserve precipitation with a smoothing kernel 
#'  (default: TRUE).
#'@param time_dim String or character array with name(s) of time dimension(s)
#'  (e.g. "ftime", "sdate", "time" ...) over which to compute spectral slopes.
#'  If a character array of dimension names is provided, the spectral slopes
#'  will be computed over all elements belonging to those dimensions.
#'  If omitted one of c("ftime", "sdate", "time") is searched and the first one 
#'  with more than one element is chosen.
#'@param lon_dim Name of lon dimension ("lon" by default).
#'@param lat_dim Name of lat dimension ("lat" by default).
#'@param verbose logical for verbose output (default: FALSE).
#'@param drop_realization_dim Logical to remove the "realization" stochastic 
#'  ensemble dimension (default: FALSE) with the following behaviour if set to 
#'  TRUE:
#'  \enumerate{
#'    \item{if \code{nens == 1}: the dimension is dropped;}
#'    \item{if \code{nens > 1} and a "member" dimension exists: the "realization" 
#'    and "member" dimensions are compacted (multiplied) and the resulting 
#'    dimension is named "member";}
#'    \item{if \code{nens > 1} and a "member" dimension does not exist: the 
#'    "realization" dimension is renamed to "member".}
#'  }
#'@param nprocs The number of parallel processes to spawn for the use for 
#'  parallel computation in multiple cores. (default: 1)
#'@return RainFARM() Returns a list containing the fine-scale longitudes, 
#'  latitudes and the sequence of \code{nens} downscaled fields. If 
#'  \code{nens > 1} an additional dimension named "realization" is added to the 
#'  output array after the "member" dimension (if it exists and unless 
#'  \code{drop_realization_dim = TRUE} is specified). The ordering of the 
#'  remaining dimensions in the \code{exp} element of the input object is 
#'  maintained.
#'@details Wether parameter 'slope' and 'weights' presents seasonality 
#'dependency, a dimension name should match between these parameters and the 
#'input data in parameter 'data'. See example 2 below where weights and slope 
#'vary with 'sdate' dimension.
#'@examples
#'# Example for the 'reduced' RainFARM function 
#'nf <- 8   # Choose a downscaling by factor 8
#'exp <- 1 : (2 * 3 * 4 * 8 * 8)
#'dim(exp) <- c(dataset = 1, member = 2, sdate = 3, ftime = 4, lat = 8, lon = 8)
#'lon <- seq(10, 13.5, 0.5)
#'lat <- seq(40, 43.5, 0.5)
#'# Create a test array of weights
#'ww <- array(1., dim = c(lon = 8 * nf, lat = 8 * nf))
#'res <- RainFARM(data = exp, lon = lon, lat = lat, nf = nf, 
#'                weights = ww, nens = 3, time_dim = 'ftime')
#'@import multiApply
#'@import rainfarmr
#'@importFrom ClimProjDiags Subset
#'@importFrom abind abind
#'@export
RainFARM <- function(data, lon, lat, nf, weights = 1., nens = 1, slope = 0, 
                     kmin = 1, fglob = FALSE, fsmooth = TRUE, nprocs = 1, 
                     time_dim = NULL, lon_dim = "lon", lat_dim = "lat",
                     drop_realization_dim = FALSE, verbose = FALSE) {

  # Check 'lon_dim' and 'lat_dim' parameters
  if (!all(c(lon_dim, lat_dim) %in% names(dim(data)))) {
    stop("Parameters 'lon_dim' and 'lat_dim' do not match with 'data' ", 
         "dimension names.")
  }
  if (length(dim(weights)) > 0) {
    if (!all(c(lon_dim, lat_dim) %in% names(dim(weights)))) {
      stop("Parameters 'lon_dim' and 'lat_dim' do not match with 'weights' ", 
           "dimension names.")
    }
  }

  # Ensure input  grid is square and with even dimensions
  if ( (dim(data)[lon_dim] != dim(data)[lat_dim]) |
       (dim(data)[lon_dim] %% 2 == 1)) {
    warning("Warning: input data are expected to be on a square grid",
            " with an even number of pixels per side.")
    nmin <- min(dim(data)[lon_dim], dim(data)[lat_dim])
    nmin <- floor(nmin / 2) * 2
    data <- .subset(data, lat_dim, 1:nmin)
    data <- .subset(data, lon_dim, 1:nmin)
    if (length(dim(lon)) == 2) {
      lon <- lon[1:nmin, 1:nmin]
      lat <- lat[1:nmin, 1:nmin]
    } else {
      lon <- lon[1:nmin]
      lat <- lat[1:nmin]
    }
    warning("The input data have been cut to the range.")
    warning(paste0("lon: [", lon[1], ", ", lon[length(lon)], "] ",
                   " lat: [", lat[1], ", ", lat[length(lat)], "]"))
  }
  if (length(dim(weights)) > 0) {
    if (length(names(dim(weights))) == 0) {
      stop("Parameter 'weights' must have dimension names when it is not a scalar.")
    } else {
      if (length(which(names(dim(weights)) == lon_dim)) > 0 &
          length(which(names(dim(weights)) == lat_dim)) > 0) { 
        lonposw <- which(names(dim(weights)) == lon_dim)
        latposw <- which(names(dim(weights)) == lat_dim)
      } else {
        stop("Parameter 'weights' must have dimension names equal to latitudinal",
             " and longitudinal dimension names as 'data' when it is not a scalar.")
      }
    }
  }
  if (!(length(dim(weights)) == 0)) { 
    if (!(dim(weights)[lonposw] == dim(data)[lon_dim] * nf) &
        !(dim(weights)[latposw] == dim(data)[lat_dim] * nf)) {
    stop(paste("The dimensions of the weights matrix (", dim(weights)[1],
               "x", dim(weights)[2] ,
               ") are not consistent with the size of the data (",
               dim(data)[lon_dim], ") and the refinement factor (", nf, ")"))
    }
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
    warning(paste("Selected time dim:", time_dim))
  }
  # Check if slope is an array
  #if (length(slope) > 1) {
  #  warning("Parameter 'slope' has length > 1 and only the first ",
  #          "element will be used.")
  #  slope <- as.numeric(slope[1])
  #}

  # Perform common calls
  r <- lon_lat_fine(lon, lat, nf)
  lon_f <- r[['lon']]
  lat_f <- r[['lat']]

  # reorder and group time_dim together at the end
  cdim0 <- dim(data)
  imask <- names(cdim0) %in% time_dim
  data <- .aperm2(data, c(which(!imask), which(imask)))
  cdim <- dim(data)
  ind <- 1:length(which(!imask))
  # compact (multiply) time_dim dimensions
  dim(data) <- c(cdim[ind], rainfarm_samples = prod(cdim[-ind]))

  # Repeatedly apply .RainFARM
  if (length(weights) == 1 & length(slope) == 1) {
    result <- Apply(data, c(lon_dim, lat_dim, "rainfarm_samples"), .RainFARM,
                    weights, slope, nf, nens, kmin,
                    fglob, fsmooth, ncores = nprocs, verbose,
                    split_factor = "greatest")$output1
  } else if (length(slope) == 1 & length(weights) > 1 ) {
     result <- Apply(list(data, weights),
                     list(c(lon_dim, lat_dim, "rainfarm_samples"),
                          c(lonposw, latposw)), 
                    .RainFARM, slope = slope,
                    nf = nf, nens = nens, kmin = kmin,
                    fglob = fglob, fsmooth = fsmooth, ncores = nprocs,
                    verbose = verbose,
                    split_factor = "greatest")$output1
  } else {
     result <- Apply(list(data, weights, slope),  
                     list(c(lon_dim, lat_dim, "rainfarm_samples"), 
                          c(lonposw, latposw), NULL), 
                     fun = .RainFARM,
                     nf = nf, nens = nens, kmin = kmin,
                     fglob = fglob, fsmooth = fsmooth, ncores = nprocs,
                     verbose = verbose,
                     split_factor = "greatest")$output1
  }
  # result has dims: lon, lat, rainfarm_samples, realization, other dims
  # Expand back rainfarm_samples to compacted dims
  dim(result) <- c(dim(result)[1:2], cdim[-ind], dim(result)[-(1:3)])
  # Reorder as it was in original data
  # + realization dim after member if it exists
  ienspos <- which(names(cdim0) == "member")
  if (length(ienspos) == 0) ienspos <- length(names(cdim0))
  iorder <- sapply(c(names(cdim0)[1:ienspos], "realization",
                     names(cdim0)[-(1:ienspos)]),
                   grep, names(dim(result)))
  ndim <- names(dim(result))
  result <- aperm(result, iorder)
  # R < 3.2.3 compatibility fix
  names(dim(result)) <- ndim[iorder]

  if (drop_realization_dim) {
    cdim <- dim(result)
    if (nens == 1) {
      dim(result) <- cdim[-which(names(cdim) == "realization")[1]]
    } else if ("member" %in% names(cdim)) {
      # compact member and realization dimension if member dim exists,
      # else rename realization to member
      ind <- which(names(cdim) %in% c("member", "realization"))
      dim(result) <- c(cdim[1:(ind[1] - 1)], cdim[ind[1]] * cdim[ind[2]],
                       cdim[(ind[2] + 1):length(cdim)])
    } else {
      ind <- which(names(cdim) %in% "realization")
      names(dim(result))[ind] <- "member"
    }
  }

  res <- NULL
  res[['data']] <- result
  res[[lon_dim]] <- lon_f
  res[[lat_dim]] <- lat_f

  return(res)
}

#'Atomic RainFARM
#'@param pr Precipitation array to downscale with dimensions (lon, lat, time).
#'@param weights Matrix with climatological weights which can be obtained using
#'  the \code{CST_RFWeights} function (default: \code{weights = 1.} i.e. no 
#'  weights).
#'@param slope Prescribed spectral slope (default: \code{slope = 0.}
#'@param nf Refinement factor for downscaling (the output resolution is 
#'  increased by this factor). Meaning that the slope is determined 
#'  automatically over the dimensions specified by \code{time_dim}.
#'@param kmin First wavenumber for spectral slope (default: \code{kmin = 1}).
#'@param nens Number of ensemble members to produce (default: \code{nens = 1}).
#'@param fglob Logical to conseve global precipitation over the domain 
#'  (default: FALSE).
#'@param fsmooth Logical to conserve precipitation with a smoothing kernel 
#'  (default: TRUE).
#'@param verbose Logical for verbose output (default: FALSE).
#'@return .RainFARM returns a downscaled array with dimensions (lon, lat, time, 
#'  realization)
#'@noRd
.RainFARM <- function(pr, weights, slope, nf, nens, kmin,
                      fglob, fsmooth, verbose) {
  posna <- NULL
  if (any(is.na(pr))) {
    posna <- unlist(lapply(1:dim(pr)['rainfarm_samples'],
                         function(x){!is.na(pr[1, 1, x])}))
    pr <- Subset(pr, 'rainfarm_samples', posna)
  }
  if (slope == 0) {
    fxp <- fft2d(pr)
    sx <- fitslope(fxp, kmin = kmin)
  } else {
    sx <- slope
  }
  result_dims <- c(dim(pr)[1] * nf, dim(pr)[2] * nf, dim(pr)[3],
                   realization = nens)
  r <- array(dim = result_dims)
  for (i in 1:nens) {
    r[, , , i] <- rainfarm(pr, sx, nf, weights, fglob = fglob,
                           fsmooth = fsmooth, verbose = verbose)
  }
  # restoring NA values in their position:
  if (!is.null(posna)) {
    pos <- which(posna == FALSE)
    dimdata <- dim(r)
    xdim <- which(names(dimdata) == 'rainfarm_samples')
    dimdata[xdim] <- dimdata[xdim] + length(pos)
    new <- array(NA, dimdata)
    posT <- which(posna == TRUE)
    i = 1
    invisible(lapply(posT, function(x) {
                             new[,,x,] <<- r[,,i,]
                             i <<- i + 1
                    }))
    #names(dim(r)) <- names(result_dims)
    warning("Missing values found in the samples.")
    r <- new
  }
  return(r)
}

# Function to generalize through do.call() n-dimensional array subsetting 
# and array indexing. Derived from Stack Overflow issue
# https://stackoverflow.com/questions/14500707/select-along-one-of-n-dimensions-in-array
.subset <- function(field, dim_name, range, drop = FALSE) {
  
  ndim <- names(dim(field))
  idim  <- which(ndim %in% dim_name )
  # Create list representing arguments supplied to [
  # bquote() creates an object corresponding to a missing argument
  indices <- rep(list(bquote()), length(dim(field)))
  indices[[idim]] <- range
  # do.call on the indices
  field <- do.call("[", c(list(field), indices, list(drop = drop)))
  # Needed for R <=3.2
  names(dim(field)) <- ndim
  
  return(field)
}
