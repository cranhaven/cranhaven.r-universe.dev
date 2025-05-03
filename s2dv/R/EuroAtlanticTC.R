#'Teleconnection indices in European Atlantic Ocean region 
#'
#'Calculate the four main teleconnection indices in European Atlantic Ocean 
#'region: North Atlantic oscillation (NAO), East Atlantic Pattern (EA), East
#'Atlantic/Western Russia (EAWR), and Scandinavian pattern (SCA). The function
#'\code{REOF()} is used for the calculation, and the first four modes are 
#'returned.
#'
#'@param ano A numerical array of anomalies with named dimensions to calculate
#'  REOF then the four teleconnections. The dimensions must have at least 
#'  'time_dim' and 'space_dim', and the data should cover the European Atlantic
#'  Ocean area (20N-80N, 90W-60E).
#'@param lat A vector of the latitudes of 'ano'. It should be 20N-80N.
#'@param lon A vector of the longitudes of 'ano'. It should be 90W-60E.
#'@param time_dim A character string indicating the name of the time dimension
#' of 'ano'. The default value is 'sdate'. 
#'@param space_dim A vector of two character strings. The first is the dimension
#'  name of latitude of 'ano' and the second is the dimension name of longitude
#'  of 'ano'. The default value is c('lat', 'lon').
#'@param ntrunc A positive integer of the modes to be kept. The default value 
#'  is 30. If time length or the product of latitude length and longitude 
#'  length is less than ntrunc, ntrunc is equal to the minimum of the three 
#'  values.
#'@param corr A logical value indicating whether to base on a correlation (TRUE)
#'  or on a covariance matrix (FALSE). The default value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A list containing:
#'\item{patterns}{
#'  An array of the first four REOF patterns normalized to 1 (unitless) with 
#'  dimensions (modes = 4, the rest of the dimensions of 'ano' except 
#'  'time_dim'). The modes represent NAO, EA, EAWR, and SCA, of which the order
#'  and sign changes depending on the dataset and period employed, so manual
#'  reordering  may be needed. Multiplying 'patterns' by 'indices' gives the 
#'  original reconstructed field.
#'}
#'\item{indices}{
#'  An array of the first four principal components with the units of the 
#'  original field to the power of 2, with dimensions (time_dim, modes = 4, the
#'  rest of the dimensions of 'ano' except 'space_dim'). 
#'} 
#'\item{var}{
#'  An array of the percentage (%) of variance fraction of total variance 
#'  explained by each mode. The dimensions are (modes = ntrunc, the rest of the
#'  dimensions of 'ano' except 'time_dim' and 'space_dim').
#'}
#'\item{wght}{
#'  An array of the area weighting with dimensions 'space_dim'. It is calculated
#'  by the square root of cosine of 'lat' and used to compute the fraction of 
#'  variance explained by each REOFs.
#'}
#'@examples
#'# Use synthetic data
#'set.seed(1)
#'dat <- array(rnorm(800), dim = c(dat = 2, sdate = 5, lat = 8, lon = 15))
#'lat <- seq(10, 90, length.out = 8)
#'lon <- seq(-100, 70, length.out = 15)
#'res <- EuroAtlanticTC(dat, lat = lat, lon = lon)
#'
#'@seealso REOF NAO
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
EuroAtlanticTC <- function(ano, lat, lon, ntrunc = 30, time_dim = 'sdate', 
                           space_dim = c('lat', 'lon'), corr = FALSE,
                           ncores = NULL) {

  # Check inputs 
  ## ano
  if (is.null(ano)) {
    stop("Parameter 'ano' cannot be NULL.")
  }
  if (!is.numeric(ano)) {
    stop("Parameter 'ano' must be a numeric array.")
  }
  if (any(is.null(names(dim(ano)))) | any(nchar(names(dim(ano))) == 0)) {
    stop("Parameter 'ano' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(ano))) {
    stop("Parameter 'time_dim' is not found in 'ano' dimension.")
  }
  ## space_dim
  if (!is.character(space_dim) | length(space_dim) != 2) {
    stop("Parameter 'space_dim' must be a character vector of 2.")
  }
  if (!all(space_dim %in% names(dim(ano)))) {
    stop("Parameter 'space_dim' is not found in 'ano' dimension.")
  }
  ## lat and lon
  if (!is.numeric(lat) | length(lat) != dim(ano)[space_dim[1]]) {
    stop("Parameter 'lat' must be a numeric vector with the same ",
         "length as the latitude dimension of 'ano'.")
  }
  if (any(lat > 90 | lat < -90)) {
    stop("Parameter 'lat' must contain values within the range [-90, 90].")
  }
  if (!is.numeric(lon) | length(lon) != dim(ano)[space_dim[2]]) {
    stop("Parameter 'lon' must be a numeric vector with the same ",
         "length as the longitude dimension of 'ano'.")
  }
  if (all(lon >= 0)) {
    if (any(lon > 360 | lon < 0)) {
      stop("Parameter 'lon' must be within the range [-180, 180] or [0, 360].")
    }
  } else {
    if (any(lon < -180 | lon > 180)) {
      stop("Parameter 'lon' must be within the range [-180, 180] or [0, 360].")
    }
  }
  stop_needed <- FALSE
  # A preset region for computing EuroAtlantic teleconnections
  lat.min <- 20
  lat.max <- 80
  lon.min <- -90 # Write this as a negative number please!
  lon.max <- 60

  # Choose lats and lons inside the Euroatlantic region. 
  # Change lon to [-180, 180] if it isn't
  lon <- ifelse(lon < 180, lon, lon - 360)
  ind_lat <- which(lat >= lat.min & lat <= lat.max)
  ind_lon <- which(lon >= lon.min & lon <= lon.max)

  # Subset 
  lat <- lat[ind_lat]
  lon <- lon[ind_lon]

  # Lat should be [20, 80] (5deg tolerance)
  if (max(lat) < (lat.max - 5) | min(lat) > (lat.min + 5)) {
    stop_needed <- TRUE
  }
  # Lon should be [-90, 60] (5deg tolerance)
  if (!(min(lon) < (lon.min + 5) & max(lon) > (lon.max - 5))) {
    stop_needed <- TRUE
  }
  if (stop_needed) {
    stop("The provided data does not cover the EuroAtlantic region (20N-80N, 90W-60E).")
  }
  ## ntrunc
  if (!is.numeric(ntrunc) | ntrunc %% 1 != 0 | ntrunc <= 0 | length(ntrunc) > 1) {
    stop("Parameter 'ntrunc' must be a positive integer.")
  }
  ## corr
  if (!is.logical(corr) | length(corr) > 1) {
    stop("Parameter 'corr' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }


  ###############################
  # Calculate indices

  ano <- ClimProjDiags::Subset(ano, space_dim, list(ind_lat, ind_lon), drop = FALSE)

  # ntrunc is bounded
  if (ntrunc != min(dim(ano)[time_dim], prod(dim(ano)[space_dim]), ntrunc)) {
    ntrunc <- min(dim(ano)[time_dim], prod(dim(ano)[space_dim]), ntrunc)
    .warning(paste0("Parameter 'ntrunc' is changed to ", ntrunc, ", the minimum among ",
                    "the length of time_dim, the production of the length of space_dim, ",
                    "and ntrunc."))
  }
  if (ntrunc < 4) {
    .warning(paste0("Parameter 'ntrunc' is ", ntrunc, " so only the first ", ntrunc,
                    " modes will be calculated."))
  }

  # Area weighting is needed to compute the fraction of variance explained by 
  # each mode
  space_ind <- sapply(space_dim, function(a) which(names(dim(ano)) == a))
  wght <- array(cos(lat * pi / 180), dim = dim(ano)[space_ind])

  # We want the covariance matrix to be weigthed by the grid
  # cell area so the anoaly field is weighted by its square
  # root since the covariance matrix equals transpose(ano)
  # times ano.
  wght <- sqrt(wght)

  reofs <- Apply(ano,
               target_dims = c(time_dim, space_dim),
               output_dims = list(REOFs = c('mode', space_dim),
                                  RPCs = c(time_dim, 'mode'),
                                  var = 'mode'),
               fun = .REOF,
               corr = corr, ntrunc = ntrunc, wght = wght,
               ncores = ncores)

  if (ntrunc >= 4) {
    TCP <- ClimProjDiags::Subset(reofs$REOFs, 'mode', 1:4, drop = FALSE)
    TCI <- ClimProjDiags::Subset(reofs$RPCs, 'mode', 1:4, drop = FALSE)
  } else {
    TCP <- reofs$REOFs
    TCI <- reofs$RPCs
  }

  return(list(patterns = TCP, indices = TCI,  var = reofs$var, wght = wght))
}

