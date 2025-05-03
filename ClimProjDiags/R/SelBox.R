#'Select spatial region from multidimensional arrays
#'
#'@description Subset a spatial region from spatial data giving a vector with 
#'the maximum and minimum of latitudes and longitudes of the selected region.
#'
#'@param data An array with minimum two dimensions of latitude and longitude.
#'@param lon Numeric vector of longitude locations of the cell centers of the
#'  grid of \code{data}'. 
#'@param lat Numeric vector of latitude locations of the cell centers of the
#'  grid of \code{data}'.
#'@param region A vector of length four indicating the minimum longitude, the
#'  maximum longitude, the minimum latitude and the maximum latitude.
#'@param londim A character string indicating the name of the longitudinal
#'  dimension. The default value is 'lon'.
#'@param latdim A character string indicating the name of the latitudinal
#'  dimension. The default value is 'lat'.
#'@param mask A matrix with the same spatial dimensions of \code{data}.
#'
#'@return A list of length 4:
#'\itemize{
#'  \item{\code{$data}, an array with the same dimensions as the input 
#'        \code{data} array, but with spatial dimension reduced to the selected 
#'        \code{region}.}
#'  \item{\code{$lat}, a vector with the new corresponding latitudes for the 
#'        selected \code{region}.}
#'  \item{\code{$lon}, a vector with the new corresponding longitudes for the 
#'        selected \code{region}.}
#'  \item{\code{$mask}, if parameter \code{mask} is supplied, an array with 
#'        reduced length of the dimensions to the selected \code{region}. 
#'        Otherwise, a NULL element is returned.}
#'}
#'
#'@examples 
#'# Example with synthetic data:
#'data <- 1:(20 * 3 * 2 * 4)
#'dim(data) <- c(lon = 20, lat = 3, time = 2, model = 4)
#'lon <- seq(2, 40, 2)
#'lat <- c(1, 5, 10)
#'
#'a <- SelBox(data = data, lon = lon, lat = lat, region = c(2, 20, 1, 5), 
#'            londim = "lon", latdim = "lat", mask = NULL)
#'@export
SelBox <- function(data, lon, lat, region, londim = 'lon', latdim = 'lat', 
                   mask = NULL) {
  # Check inputs
  # data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.array(data) && !is.matrix(data)) {
    stop("Parameter 'data' must be a numeric array or matrix.")
  }
  dim_names <- names(dim(data))
  if (is.null(dim_names)) {
    stop("Parameter 'data' must have dimension names.")
  }

  # lon, lat
  if (is.null(lon) | is.null(lat)) {
    stop("Parameters 'lon' and 'lat' cannot be NULL.")
  }
  if (!is.numeric(lon) | !is.numeric(lat)) {
    stop("Parameters 'lon' and 'lat' must be numeric.")
  }
  if (!is.null(dim(lon)) | !is.null(dim(lat))) {
    if (length(dim(lon)) == 1 & length(dim(lat)) == 1) {
      lon <- as.vector(lon)
      lat <- as.vector(lat)
    } else {
      stop("Parameters 'lon' and 'lat' need to be a vector.")
    }
  }
  # region
  if (is.null(region)) {
    stop("Parameter 'region' cannot be NULL.")
  }
  if (!is.numeric(region)) {
    stop("Parameter 'region' must be numeric.")
  }
  if (length(region) != 4) {
    stop("The region argument has to be a vector of length four indicating the ",
         "minimum longitude, the maximum longitude, the minimum latitude and ", 
         "the maximum latitude.")
  }
  # londim
  if (is.numeric(londim)) {
    warning("Numeric 'londim' is deprecated, use dimension names instead. The ", 
            "corresponding dimension name will be assigned.")
    londim <- dim_names[londim]
  }
  if (!is.character(londim)) {
    stop("Parameter 'londim' must be a character string.")
  }
  if (length(londim) > 1) {
    warning("Parameter 'londim' must be of length 1. Only the first value ", 
            "will be used.")
    londim <- londim[1]
  }
  if (!londim %in% names(dim(data))) {
    stop("Parameter 'londim' is not found in 'data'.")
  }
  if (dim(data)[londim] != length(lon)) {
    stop(paste0("The longitudinal dimension of parameter 'data' must be of the ", 
                "same length as parameter 'lon'."))
  }
  # latdim
  if (is.numeric(latdim)) {
    warning("Numeric 'latdim' is deprecated, use dimension names instead. The ", 
            "corresponding dimension name will be assigned.")
    latdim <- dim_names[latdim]
  }
  if (!is.character(latdim)) {
    stop("Parameter 'latdim' must be a character string.")
  }
  if (length(latdim) > 1) {
    warning("Parameter 'latdim' must be of length 1. Only the first value ", 
            "will be used.")
    latdim <- latdim[1]
  }
  if (!latdim %in% names(dim(data))) {
    stop("Parameter 'latdim' is not found in 'data'.")
  }
  if (dim(data)[latdim] != length(lat)) {
    stop(paste0("The latitudinal dimension of parameter 'data' must be of the ", 
                "same length as parameter 'lat'."))
  }
  # mask
  if (!is.null(mask)) {
    if (!all(dim(data)[which(names(dim(data)) %in% c(londim, latdim))] %in% 
             dim(mask)[which(names(dim(mask)) %in% c(londim, latdim))])) {
      stop("Parameter 'mask' must have the same spatial dimensions of data.")
    }
  }

  if (region[3] <= region[4]) {
    LatIdx <- which(lat >= region[3] & lat <= region[4])
  } else {
    LatIdx <- which(lat <= region[3] | lat >= region[4])
  }
  # if (region[1] <= region[2]) {
  #   LonIdx <- which(lon >= region[1] & lon <= region[2])
  # } else {
  #   LonIdx <- which(lon >= region[1] | lon <= region[2])
  # }
  LonIdx <- Lon2Index(lon, lonmin = region[1], lonmax = region[2])

  data <- Subset(data, along = c(londim, latdim), indices = list(LonIdx, LatIdx), drop = "none")

  if (!is.null(mask)) {
    mask <- Subset(mask, along = c(londim, latdim), indices = list(LonIdx, LatIdx), drop = "none")
  } else {
    mask <- NULL
  }
  return(list(data = data, lon = lon[LonIdx], lat = lat[LatIdx], mask = mask))
}
