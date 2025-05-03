#'Compute the square-root of the cosine of the latitude weighting on the given 
#'array.
#'
#'This function performs square-root of the cosine of the latitude weighting on 
#'the given array.
#'
#'@param data A numeric array with named dimensions, representing the data to be
#'  applied the weights. It should have at least the latitude dimension and it 
#'  can have more other dimensions.
#'@param lat A numeric vector or array with one dimension containing the 
#'  latitudes (in degrees).
#'@param lat_dim A character string indicating the name of the latitudinal
#'  dimension. The default value is 'lat'.
#'@param method A character string indicating the type of weighting applied: 
#'  'cos' (cosine of the latitude) or 'sqrtcos' (square-root of the 
#'  cosine of the latitude). The default value is 'cos'.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return An array containing the latitude weighted data with same dimensions as
#'parameter 'data'.
#'
#'@examples 
#'exp <- array(rnorm(1:30), dim = c(lat = 3, lon = 5, sdate = 2))
#'lat <- c(10, 15, 20)
#'res <- WeightedCells(data = exp, lat = lat)
#'@import multiApply
#'@export
WeightedCells <- function(data, lat, lat_dim = 'lat', method = 'cos', ncores = NULL) {
  
  # Check inputs
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {
    stop("Parameter 'data' must be at least latitude dimension.")
  }
  if(any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }

  ## lat_dim
  if (!is.character(lat_dim) | length(lat_dim) > 1) {
    stop("Parameter 'lat_dim' must be a character string.")
  }
  if (!lat_dim %in% names(dim(data))) {
    stop("Parameter 'lat_dim' is not found in 'data'.")
  }

  ## lat
  if (is.null(lat)) {
    stop("Parameter 'lat' cannot be NULL.")
  }
  if (!is.numeric(lat)) {
    stop("Parameter 'lat' must be a numeric vector or array.")
  }
  if (dim(data)[lat_dim] != length(lat)) {
    stop("Length of parameter 'lat' doesn't match the length of ",
         "latitudinal dimension in parameter 'data'.")
  }

  ## method
  if (!method %in% c('cos', 'sqrtcos')) {
    stop("Parameter 'method' must be 'cos' or 'sqrtcos'.")
  }

  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    }
  }

  namedims <- names(dim(data))
  lat <- as.vector(lat)

  if (method == 'cos') {
    wt <- cos(lat * pi / 180)
  } else {
    wt <- sqrt(cos(lat * pi / 180))
  }
  
  res <- Apply(data = data, 
               target_dims = c(lat_dim),  
               fun = .WeightedCells, 
               wt = wt,
               ncores = ncores)$output1
               
  order <- match(namedims, names(dim(res)))
  res <- aperm(res, order)

  return(res)
}

.WeightedCells <- function(data, wt) {
  data <- wt * data
  return(data)
}
