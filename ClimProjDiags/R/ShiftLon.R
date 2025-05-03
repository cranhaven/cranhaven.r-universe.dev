#'Shift longitudes of a data array
#'
#'@description Shift the longitudes of a data array. Only reasonable for global
#'  longitude shifting. It is useful for map plotting or aligning datasets.
#'
#'@param data A named multidimensional array with at least 'lon_dim' dimension.
#'@param lon A numeric vector of longitudes. The values are expected to be 
#'  monotonic increasing.
#'@param westB A number indicating the west boundary of the new longitudes.
#'@param lon_dim A character string indicating the name of the longitude 
#'  dimension in 'data'. The default value is 'lon'.
#'@param ncores An integer indicating the number of cores used for computation.
#'  The default value is NULL (use only one core).
#'
#'@return 
#'A list of 2:
#'\item{data}{
#'  Array of the shifted data with the same dimensions as parameter 'data'.
#'}
#'\item{lon}{
#'  The monotonic increasing new longitudes with the same length as parameter 
#'  'lon' and start at 'westB'.
#'}
#'
#'@examples
#'data <- array(data = 1:50, dim = c(lon = 360, lat = 181))
#'lon <- array(data = 0:359, dim = c(lon = 360))
#'lat <- -90:90 ## lat does not change
#'shifted <- ShiftLon(data = data, lon = lon, westB = -180, ncores = 1)
#'
#'  \dontrun{
#'s2dv::PlotEquiMap(var = data, lon = lon, lat = lat, filled.continents = FALSE)
#'s2dv::PlotEquiMap(var = shifted$data, lon = shifted$lon, lat = lat, filled.continents = FALSE)
#'  }
#'  
#'@import multiApply
#'@export
ShiftLon <- function(data, lon, westB, lon_dim = 'lon', ncores = NULL) {
  
  # Check inputs
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (is.vector(data)) {
    data <- as.array(data)
    names(dim(data)) <- lon_dim
    warning("Parameter 'data' is a vector. Transfer it to an array and assign ", lon_dim, " as dimension name.")
  }
  if (!is.array(data)) {
    stop("Parameter 'data' must be an array.")
  }
  if(any(is.null(names(dim(data))))| any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## lon_dim
  if (!is.character(lon_dim) | length(lon_dim) > 1) {
    stop("Parameter 'lon_dim' must be a character string.")
  }
  if (!(lon_dim %in% names(dim(data)))) {
    stop("Parameter 'lon_dim' is not found in 'data' dimensions.")
  }
  ## lon
  if (!is.numeric(lon)) {
    stop("Parameter 'lon' must be numeric.")
  }
  if (!(length(lon) == as.numeric(dim(data)[lon_dim]))) {
    stop("The length of 'lon' must be the same as the length of 'lon_dim' in 'data'.")
  }
  if (any(diff(lon) < 0)) {
    stop("Parameter 'lon' must be monotonic increasing.")
  }
  ## westB
  if (!is.numeric(westB)) {
    stop("Parameter 'westB' must be numeric.")
  }
  if (length(westB) != 1) {
    westB <- westB[1]
    warning("Parameter 'westB' should be a number. Use the first element only.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores)) {
      stop("Parameter 'ncores' must be a positive integer.")
    } else if (any(ncores %% 1 != 0) | any(ncores <= 0) | length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  
  #############################################
  
  # Shifting the longitudes (only needed to do it once)
  ## Adjust western boundary of lon vector if necessary
  ## If westB is not within lon, adjust it to find 'first' below
  shft_westB_back <- 0
  if (westB < min(lon)) {
    westB <- westB + 360
    shft_westB_back <- -360
  } else if (westB > max(lon)) {
    westB <- westB - 360
    shft_westB_back <- 360
  }
  if (westB < min(lon) | westB > (min(lon) + 360)) {
    stop("westB is too far away from the 'lon' range.")
  }
  
  ## Find closest index in lon vector
  first <- which.min(abs(westB - lon))[1]
  if (first == 1) {
    new.lon <- lon
  } else {
    new.lon <- c(lon[first:length(lon)],lon[1:(first-1)])
  }
  ## Order to monotonically increasing
  if (!all(diff(new.lon) > 0)) {
    new.lon[(which(diff(new.lon) < 0) + 1):length(new.lon)] <- new.lon[(which(diff(new.lon) < 0) + 1):length(new.lon)] + 360
    
  }
  
  # Shifting the data
  ori_dim <- dim(data)

  output <- Apply(data = data,
                  target_dims = lon_dim,
                  fun = .ShiftLon,
                  lon = lon,
                  new.lon = new.lon,
                  ncores = ncores)$output1
  
  # Shift new.lon back to start at westB
  new.lon <- new.lon + shft_westB_back

  # Change dimension order back
  output <- aperm(output, match(names(ori_dim), names(dim(output))))

  return(list(data = output, lon = new.lon))
}

.ShiftLon <- function(data, lon, new.lon) {
  # data: [lon]
  new.data <- data
  new.data[new.lon %in% lon] <- data[lon %in% new.lon, drop = F]
  new.data[!new.lon %in% lon] <- data[!lon %in% new.lon, drop = F]
  
  return(new.data)
}
