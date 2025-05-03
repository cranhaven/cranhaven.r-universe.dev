#'Area-weighted empirical orthogonal function analysis with varimax rotation using SVD
#'
#'Perform an area-weighted EOF analysis with varimax rotation using single 
#'value decomposition (SVD) based on a covariance matrix or a correlation matrix if 
#'parameter 'corr' is set to TRUE. The internal s2dv function \code{.EOF()} is used 
#'internally.
#'
#'@param ano A numerical array of anomalies with named dimensions to calculate
#'  REOF. The dimensions must have at least 'time_dim' and 'space_dim'.
#'@param lat A vector of the latitudes of 'ano'.
#'@param lon A vector of the longitudes of 'ano'.
#'@param time_dim A character string indicating the name of the time dimension
#' of 'ano'. The default value is 'sdate'. 
#'@param space_dim A vector of two character strings. The first is the dimension
#'  name of latitude of 'ano' and the second is the dimension name of longitude
#'  of 'ano'. The default value is c('lat', 'lon').
#'@param ntrunc A positive integer of the number of eofs to be kept for varimax 
#'  rotation. This function uses this value as 'neof' too, which is the number
#'  of eofs to return by \code{.EOF()}. The default value is 15. If time length
#'  or the product of latitude length and longitude length is less than 
#'  'ntrunc', 'ntrunc' is equal to the minimum of the three values.
#'@param corr A logical value indicating whether to base on a correlation (TRUE)
#'  or on a covariance matrix (FALSE). The default value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list containing:
#'\item{REOFs}{
#'  An array of REOF patterns normalized to 1 (unitless) with dimensions 
#'  (number of modes, the rest of the dimensions of 'ano' except 
#'  'time_dim'). Multiplying 'REOFs' by 'RPCs' gives the original 
#'  reconstructed field.
#'}
#'\item{RPCs}{
#'  An array of principal components with the units of the original field to 
#'  the power of 2, with dimensions (time_dim, number of modes, the rest of the
#'  dimensions of 'ano' except 'space_dim'). 
#'} 
#'\item{var}{
#'  An array of the percentage (%) of variance fraction of total variance 
#'  explained by each mode. The dimensions are (number of modes, the rest of 
#'  the dimension except 'time_dim' and 'space_dim').
#'}
#'\item{wght}{
#'  An array of the area weighting with dimensions 'space_dim'. It is calculated
#'  by the square root of cosine of 'lat' and used to compute the fraction of 
#'  variance explained by each REOFs.
#'}
#'
#'@seealso EOF
#'@examples
#'# This example computes the REOFs along forecast horizons and plots the one 
#'# that explains the greatest amount of variability. The example data has low  
#'# resolution so the result may not be explanatory, but it displays how to 
#'# use this function.
#'\dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                     c('observation'), startDates,
#'                                     leadtimemin = 1,
#'                                     leadtimemax = 4,
#'                                     output = 'lonlat',
#'                                     latmin = 27, latmax = 48,
#'                                     lonmin = -12, lonmax = 40)
#'}
#'ano <- Ano_CrossValid(sampleData$mod, sampleData$obs)
#'ano <- MeanDims(ano$exp, c('dataset', 'member'))
#'res <- REOF(ano, lat = sampleData$lat, lon = sampleData$lon, ntrunc = 5)
#'\dontrun{
#'PlotEquiMap(eof$EOFs[1, , , 1], sampleData$lat, sampleData$lon)
#'}
#'
#'@import multiApply
#'@importFrom stats varimax
#'@export
REOF <- function(ano, lat, lon, ntrunc = 15, time_dim = 'sdate', 
                 space_dim = c('lat', 'lon'), corr = FALSE, ncores = NULL) {

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
  ## lat
  if (!is.numeric(lat) | length(lat) != dim(ano)[space_dim[1]]) {
    stop("Parameter 'lat' must be a numeric vector with the same ",
         "length as the latitude dimension of 'ano'.")
  }
  if (any(lat > 90 | lat < -90)) {
    stop("Parameter 'lat' must contain values within the range [-90, 90].")
  }
  ## lon
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
  # Calculate REOF

  # ntrunc is bounded
  if (ntrunc != min(dim(ano)[time_dim], prod(dim(ano)[space_dim]), ntrunc)) {
    ntrunc <- min(dim(ano)[time_dim], prod(dim(ano)[space_dim]), ntrunc)
    .warning(paste0("Parameter 'ntrunc' is changed to ", ntrunc, ", the minimum among ",
                    "the length of time_dim, the production of the length of space_dim, ",
                    "and ntrunc."))
  }

  # Area weighting is needed to compute the fraction of variance explained by 
  # each mode
  space_ind <- sapply(space_dim, function(a) which(names(dim(ano)) == a))
  wght <- array(cos(lat * pi / 180), dim = dim(ano)[space_ind])
  
  # We want the covariance matrix to be weigthed by the grid
  # cell area so the anomaly field is weighted by its square
  # root since the covariance matrix equals transpose(ano)
  # times ano.
  wght <- sqrt(wght)

  res <- Apply(ano, 
               target_dims = c(time_dim, space_dim), 
               output_dims = list(REOFs = c('mode', space_dim),
                                  RPCs = c(time_dim, 'mode'),
                                  var = 'mode'),
               fun = .REOF, 
               corr = corr, ntrunc = ntrunc, wght = wght,
               ncores = ncores)

  return(c(res, wght = list(wght)))

}


.REOF <- function(ano, ntrunc, corr = FALSE, wght = wght) {
  # ano: [sdate, lat, lon]

  # Dimensions
  ny <- dim(ano)[2]
  nx <- dim(ano)[3]
  
  # Get the first ntrunc EOFs:
  eofs <- .EOF(ano = ano, neofs = ntrunc, corr = corr, wght = wght) 
  #list(EOFs = EOF, PCs = PC, var = var.eof, mask = mask)

  # Recover loadings (with norm 1), weight the EOFs by the weigths
  # eofs$EOFs: [mode, lat, lon]
  Loadings <- apply(eofs$EOFs, 1, '*', wght)  # [lat*lon, mode]

  # Rotate the loadings:
  varim <- varimax(Loadings)

  # Weight back the rotated loadings (REOFs):
  if (is.list(varim)) {
    varim_loadings <- varim$loadings  # [lat*lon, mode]
  } else { # if mode = 1, varim is an array
    varim_loadings <- varim
  }
  REOFs <- apply(varim_loadings, 2, '/', wght)
  dim(REOFs) <- c(ny, nx, ntrunc)

  # Reorder dimensions to match EOF conventions: [mode, lat, lon]
  REOFs <- aperm(REOFs, c(3, 1, 2))

  # Compute the rotated PCs (RPCs): multiply the weigthed anomalies by the loading patterns.
  ano.wght <- apply(ano, 1, '*', wght) # [lat*lon, sdate]
  RPCs <- t(ano.wght) %*% varim_loadings # [sdate, mode]

  ## Alternative methods suggested here:
  ##https://stats.stackexchange.com/questions/59213/
  ##how-to-compute-varimax-rotated-principal-components-in-r/137003#137003
  ##gives same results as pinv is just transpose in this case, as loadings are ortonormal!
  # invLoadings <- t(pracma::pinv(varim$loadings)) 
  ## invert and traspose the rotated loadings. pinv uses a SVD again (!)
  # RPCs <- ano.wght %*% invLoadings

  # Compute explained variance fraction:
  var <- apply(RPCs, 2, function(x) {
                          sum(x * x)
                          }) * 100 / eofs$tot_var  # [mode]
  dim(var) <- c(mode = length(var))

  return(invisible(list(REOFs = REOFs, RPCs = RPCs, var = var)))
}
