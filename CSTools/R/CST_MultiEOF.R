#'@rdname CST_MultiEOF
#'@title EOF analysis of multiple variables
#'
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'@author Paolo Davini - ISAC-CNR, \email{p.davini@isac.cnr.it}
#'
#'@description This function performs EOF analysis over multiple variables,
#'accepting in input a list of CSTools objects. Based on Singular Value 
#'Decomposition. For each field the EOFs are computed and the corresponding PCs 
#'are standardized (unit variance, zero mean); the minimum number of principal 
#'components needed to reach the user-defined variance is retained. The function 
#'weights the input data for the latitude cosine square root.
#'
#'@param datalist A list of objects of the class 's2dv_cube', containing the 
#'  variables to be analysed. Each data object in the list is expected to have 
#'  an element named \code{$data} with at least two spatial dimensions named 
#'  "lon" and "lat", a dimension "ftime" and a dimension "sdate". Latitudinal 
#'  dimension accepted names: 'lat', 'lats', 'latitude', 'y', 'j', 'nav_lat'. 
#'  Longitudinal dimension accepted names: 'lon', 'lons','longitude', 'x', 'i', 
#'  'nav_lon'. NAs can exist but it should be consistent along 'time_dim'. That 
#'  is, if one grid point has NAs for each variable, all the time steps at this 
#'  point should be NAs. 
#'@param lon_dim A character string indicating the name of the longitudinal 
#'  dimension. By default, it is set to 'lon'. 
#'@param lat_dim A character string indicating the name of the latitudinal 
#'  dimension. By default, it is set to 'lat'. 
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. 
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'. 
#'@param var_dim A character string indicating the name of the variable 
#'  dimension. By default, it is set to 'var'. 
#'@param neof_max Maximum number of single eofs considered in the first 
#'  decomposition.
#'@param neof_composed Number of composed eofs to return in output.
#'@param minvar Minimum variance fraction to be explained in first decomposition.
#'@param lon_lim Vector with longitudinal range limits for the EOF calculation 
#'  for all input variables.
#'@param lat_lim Vector with latitudinal range limits for the EOF calculation 
#'  for all input variables.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'@return 
#'A list containing:
#'\item{coeff}{
#'  An 's2dv_cube' with the data element being an array of principal components 
#'  with dimensions 'time_dim', 'sdate_dim', number of eof, rest of the 
#'  dimensions of 'data' except 'lon_dim' and 'lat_dim'. 
#'}
#'\item{variance}{
#'  An 's2dv_cube' with the data element being an array of explained variances 
#'  with dimensions 'eof' and the rest of the dimensions of 'data' except 
#'  'time_dim', 'sdate_dim', 'lon_dim' and 'lat_dim'.
#'}
#'\item{eof_pattern}{
#'  An 's2dv_cube' with the data element being an array of EOF patterns obtained 
#'  by regression with dimensions: 'eof' and the rest of the dimensions of 
#'  'data' except 'time_dim' and 'sdate_dim'.
#'}
#'\item{mask}{
#'  An 's2dv_cube' with the data element being an array of the mask with 
#'  dimensions ('lon_dim', 'lat_dim', rest of the dimensions of 'data' except 
#'  'time_dim'). It is made from 'data', 1 for the positions that 'data' has 
#'  value and NA for the positions that 'data' has NA. It is used to replace NAs 
#'  with 0s for EOF calculation and mask the result with NAs again after the 
#'  calculation.
#'}
#'\item{coordinates}{
#'  Longitudinal and latitudinal coordinates vectors.
#'}
#'@examples
#'seq <- 1 : (2 * 3 * 4 * 5 * 6 * 8)
#'mod1 <- sin( 0.7 + seq )^2 + cos( seq ^ 2 * 1.22  )
#'dim(mod1) <- c(dataset = 2, member = 3, sdate = 4, ftime = 5, lat = 6, 
#'               lon = 8)
#'mod2 <- sin( seq * 2 ) ^ 3 + cos( seq ^ 2 )
#'dim(mod2) <- c(dataset = 2, member = 3, sdate = 4, ftime = 5, lat = 6, 
#'               lon = 8)
#'lon <- seq(0, 35, 5)
#'lat <- seq(0, 25, 5)
#'exp1 <- list(data = mod1, coords = list(lat = lat, lon = lon))
#'exp2 <- list(data = mod2, coords = list(lat = lat, lon = lon))
#'attr(exp1, 'class') <- 's2dv_cube'
#'attr(exp2, 'class') <- 's2dv_cube'
#'d = as.POSIXct(c("2017/01/01", "2017/01/02", "2017/01/03", "2017/01/04", 
#'                 "2017/01/05", "2018/01/01", "2018/01/02", "2018/01/03",
#'                 "2018/01/04", "2018/01/05", "2019/01/01", "2019/01/02", 
#'                 "2019/01/03", "2019/01/04", "2019/01/05", "2020/01/01", 
#'                 "2020/01/02", "2020/01/03", "2020/01/04", "2020/01/05"))
#'exp1$attrs$Dates = d
#'exp2$attrs$Dates = d
#'
#'cal <- CST_MultiEOF(datalist = list(exp1, exp2), neof_composed = 2)
#'@import abind
#'@export
CST_MultiEOF <- function(datalist, lon_dim = "lon", lat_dim = "lat", 
                         time_dim = 'ftime', sdate_dim = 'sdate', 
                         var_dim = 'var', neof_max = 40, neof_composed = 5, 
                         minvar = 0.6, lon_lim = NULL, lat_lim = NULL, 
                         ncores = NULL) {
  # Check 's2dv_cube'
  if (!(all(sapply(datalist, inherits, 's2dv_cube')))) {
     stop("Elements of the list in parameter 'datalist' must be of the ", 
          "class 's2dv_cube'.")
  }
  if (!all(c('data', 'coords', 'attrs') %in% names(datalist[[1]]))) {
    stop("Parameter 'datalist' must have 'data', 'coords' and 'attrs' elements ",
         "within the 's2dv_cube' structure.")
  }
  # Dates
  dates <- datalist[[1]]$attrs$Dates
  if (is.null(dates)) {
    stop("Element 'Dates' is not found in 'attrs' list of the first array.")
  }
  # coordinates
  if (!any(names(datalist[[1]]$coords) %in% .KnownLonNames()) | 
      !any(names(datalist[[1]]$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names do not match any of the names accepted by the ",
         "package. Latitudes accepted names: 'lat', 'lats', 'latitude', 'y', 'j', ", 
         "'nav_lat'. Longitudes accepted names: 'lon', 'lons', 'longitude', 'x',", 
         " 'i', 'nav_lon'.")
  }

  # Check if all dims equal
  adims = lapply(lapply(datalist, function(x) x$data), dim)
  if (!all(apply(apply(abind(adims, along = 0), 2, duplicated), 2, sum) == 
           (length(adims)-1))) {
    stop("Input data fields must all have the same dimensions.")
  }

  exp <- abind(lapply(datalist, '[[', 'data'), along = 0)
  dim(exp) <- c(length(datalist), dim(datalist[[1]]$data))
  names(dim(exp)) <- c(var_dim, names(dim(datalist[[1]]$data)))

  lon_name <- names(datalist[[1]]$coords)[[which(names(datalist[[1]]$coords) %in% .KnownLonNames())]]
  lat_name <- names(datalist[[1]]$coords)[[which(names(datalist[[1]]$coords) %in% .KnownLatNames())]]
  lon <- as.vector(datalist[[1]]$coords[[lon_name]])
  lat <- as.vector(datalist[[1]]$coords[[lat_name]])

  result <- MultiEOF(data = exp, lon = lon, lat = lat,
                     lon_dim = lon_dim, lat_dim = lat_dim, time_dim = time_dim, 
                     sdate_dim = sdate_dim, var_dim = var_dim, 
                     dates = dates, minvar = minvar,
                     neof_max = neof_max, neof_composed = neof_composed,
                     lon_lim = lon_lim, lat_lim = lat_lim, ncores = ncores)
  names_res <- names(result[1:4])
  res <- lapply(seq_along(result)[1:4], function(i) {
    coords = list(lon, lat)
    names(coords) <- c(lon_dim, lat_dim)
    dates <- dates
    varName <- names(result)[[i]]
    metadata <- lapply(datalist, function(x) x$attrs$Variable$metadata)
    metadata <- unlist(metadata, recursive=FALSE)
    metadata <- metadata[unique(names(metadata))]
    suppressWarnings(
      cube <- s2dv_cube(data = result[[i]], coords = coords, varName = varName, Dates = dates, 
                        source_files = unlist(sapply(datalist, function(x) x$attrs$source_files)), 
                        metadata = metadata, when = Sys.time())
    )
    return(cube)
  })
  names(res) <- names_res
  return(c(res, result[5:6]))
}
#'@rdname MultiEOF
#'@title EOF analysis of multiple variables starting from an array (reduced 
#'version)
#'
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'@author Paolo Davini - ISAC-CNR, \email{p.davini@isac.cnr.it}
#'
#'@description This function performs EOF analysis over multiple variables, 
#'accepting in input an array with a dimension \code{"var"} for each variable to 
#'analyse. Based on Singular Value Decomposition. For each field the EOFs are 
#'computed and the corresponding PCs are standardized (unit variance, zero mean); 
#'the minimum number of principal components needed to reach the user-defined 
#'variance is retained. The function weights the input data for the latitude 
#'cosine square root.
#'
#'@param data A multidimensional array with dimension \code{"var"}, containing 
#'  the variables to be analysed. The other diemnsions follow the same structure
#'  as the \code{"exp"} element of a 's2dv_cube' object. Latitudinal 
#'  dimension accepted names: 'lat', 'lats', 'latitude', 'y', 'j', 'nav_lat'. 
#'  Longitudinal dimension accepted names: 'lon', 'lons','longitude', 'x', 'i', 
#'  'nav_lon'. NAs can exist but it should be consistent along 'time_dim'. That 
#'  is, if one grid point has NAs for each variable, all the time steps at this 
#'  point should be NAs. 
#'@param lon Vector of longitudes.
#'@param lat Vector of latitudes.
#'@param dates Vector or matrix of dates in POSIXct format.
#'@param time Deprecated parameter, it has been substituted by 'dates'. It will 
#'  be removed in the next release.
#'@param lon_dim A character string indicating the name of the longitudinal 
#'  dimension. By default, it is set to 'lon'. 
#'@param lat_dim A character string indicating the name of the latitudinal 
#'  dimension. By default, it is set to 'lat'. 
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. 
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'. 
#'@param var_dim A character string indicating the name of the variable 
#'  dimension. By default, it is set to 'var'. 
#'@param neof_max Maximum number of single eofs considered in the first 
#'  decomposition.
#'@param neof_composed Number of composed eofs to return in output.
#'@param minvar Minimum variance fraction to be explained in first decomposition.
#'@param lon_lim Vector with longitudinal range limits for the calculation for 
#'  all input variables.
#'@param lat_lim Vector with latitudinal range limits for the calculation for 
#'  all input variables.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'@return 
#'A list containing:
#'\item{coeff}{
#'  An array of principal components with dimensions 'time_dim', 'sdate_dim', 
#'  number of eof, rest of the dimensions of 'data' except 'lon_dim' and 
#'  'lat_dim'. 
#'}
#'\item{variance}{
#'  An array of explained variances with dimensions 'eof' and the rest of the 
#'  dimensions of 'data' except 'time_dim', 'sdate_dim', 'lon_dim' and 
#'  'lat_dim'.
#'}
#'\item{eof_pattern}{
#'  An array of EOF patterns obtained by regression with dimensions: 'eof' and 
#'  the rest of the dimensions of 'data' except 'time_dim' and 'sdate_dim'.
#'}
#'\item{mask}{
#'  An array of the mask with dimensions ('lon_dim', 'lat_dim', rest of the 
#'  dimensions of 'data' except 'time_dim'). It is made from 'data', 1 for the 
#'  positions that 'data' has value and NA for the positions that 'data' has NA. 
#'  It is used to replace NAs with 0s for EOF calculation and mask the result 
#'  with NAs again after the calculation.
#'}
#'\item{coordinates}{
#'  Longitudinal and latitudinal coordinates vectors.
#'}
#'
#'@examples
#'exp <- array(runif(1280)*280, dim = c(dataset = 2, member = 2, sdate = 3, 
#'                                      ftime = 3, lat = 4, lon = 4, var = 1))
#'lon <- seq(0, 3)
#'lat <- seq(47, 44)
#'dates <- c("2000-11-01", "2000-12-01", "2001-01-01", "2001-11-01", 
#'           "2001-12-01", "2002-01-01", "2002-11-01", "2002-12-01", "2003-01-01")
#'Dates <- as.POSIXct(dates, format = "%Y-%m-%d")
#'dim(Dates) <- c(ftime = 3, sdate = 3)
#'cal <- MultiEOF(data = exp, lon = lon, lat = lat, dates = Dates)
#'@import multiApply
#'@export
MultiEOF <- function(data, lon, lat, dates, time = NULL,
                     lon_dim = "lon", lat_dim = "lat",
                     time_dim = 'ftime', sdate_dim = 'sdate', var_dim = 'var',
                     neof_max = 40, neof_composed = 5, minvar = 0.6,
                     lon_lim = NULL, lat_lim = NULL, ncores = NULL) {
  # Check inputs 
  # data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (any(is.null(names(dim(data))))| any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  # dates
  if (!is.null(time)) {
    warning("The parameter 'time' is deprecated, use 'dates' instead.")
    dates <- time
  }
  # lon_dim
  if (!is.character(lon_dim) | length(lon_dim) != 1) {
    stop("Parameter 'lon_dim' must be a character string.")
  }
  if (!lon_dim %in% names(dim(data))) {
    stop("Parameter 'lon_dim' is not found in 'data' dimension.")
  }
  # lat_dim
  if (!is.character(lat_dim) | length(lat_dim) != 1) {
    stop("Parameter 'lat_dim' must be a character string.")
  }
  if (!lat_dim %in% names(dim(data))) {
    stop("Parameter 'lat_dim' is not found in 'data' dimension.")
  }
  # lon
  if (!is.numeric(lon) | length(lon) != dim(data)[lon_dim]) {
    stop(paste0("Parameter 'lon' must be a numeric vector with the same ",
                "length as the longitude dimension of 'data'."))
  }
  if (any(lon > 360 | lon < -360)) {
    warning("Some 'lon' is out of the range [-360, 360].")
  }
  # lat
  if (!is.numeric(lat) | length(lat) != dim(data)[lat_dim]) {
    stop(paste0("Parameter 'lat' must be a numeric vector with the same ",
                "length as the latitude dimension of 'data'."))
  }
  if (any(lat > 90 | lat < -90)) {
    stop("Parameter 'lat' must contain values within the range [-90, 90].")
  }
  # time_dim
  if (!is.character(time_dim) | length(time_dim) != 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  # sdate_dim
  if (!is.character(sdate_dim) | length(sdate_dim) != 1) {
    stop("Parameter 'sdate_dim' must be a character string.")
  }
  if (!sdate_dim %in% names(dim(data))) {
    stop("Parameter 'sdate_dim' is not found in 'data' dimension.")
  }
  # var_dim
  if (!is.character(var_dim) | length(var_dim) != 1) {
    stop("Parameter 'var_dim' must be a character string.")
  }
  if (!var_dim %in% names(dim(data))) {
    stop("Parameter 'var_dim' is not found in 'data' dimension.")
  }
  # neof_max
  if (!is.numeric(neof_max)) {
    stop("Parameter 'neof_max' must be a positive integer.")
  }
  # neof_composed
  if (!is.numeric(neof_composed)) {
    stop("Parameter 'neof_composed' must be a positive integer.")
  }
  # minvar
  if (!is.numeric(minvar)) {
    stop("Parameter 'minvar' must be a positive number between 0 and 1.")
  }
  # lon_lim
  if (!is.null(lon_lim)) {
    if (!is.numeric(lon_lim)) {
      stop("Parameter 'lon_lim' must be numeric.")
    }
  }
  # lat_lim
  if (!is.null(lat_lim)) {
    if (!is.numeric(lat_lim)) {
      stop("Parameter 'lat_lim' must be numeric.")
    }
  }
  # ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  # Reorder and group ftime and sdate together at the end in that order
  imaskt <- names(dim(data)) %in% time_dim
  imasks <- names(dim(data)) %in% sdate_dim
  data <- .aperm2(data, c(which(!(imasks | imaskt)),
                          which(imaskt), which(imasks)))
  dims <- dim(data)
  ind <- 1:length(which(!(imaskt | imasks)))
  # compact (multiply) time_dim dimensions
  dim(data) <- c(dims[ind], samples = prod(dims[-ind]))

  # Repeatedly apply .multi.eofs
  result <- Apply(data = data, 
                  target_dims = c(var_dim, lon_dim, lat_dim, "samples"),
                  fun = .multi.eofs, lon = lon, lat = lat, dates = dates, 
                  neof_max = neof_max, neof_composed = neof_composed, 
                  minvar = minvar, xlim = lon_lim, ylim = lat_lim, 
                  lon_dim = lon_dim, lat_dim = lat_dim, ncores = ncores)
  
  # Expand back samples to compacted dims
  dim(result$coeff) <- c(dims[-ind], dim(result$coeff)[-1])
  # Recover first lon and first lat list
  dd = dim(result[[lon_dim]])[1]; m = matrix(1, nrow = dd, ncol = length(dim(result[[lon_dim]])));
  m[1:dd] = 1:dd; result[[lon_dim]] = result[[lon_dim]][m]
  dd = dim(result[[lat_dim]])[1]; m = matrix(1, nrow = dd, ncol = length(dim(result[[lat_dim]])));
  m[1:dd] = 1:dd; result[[lat_dim]] = result[[lat_dim]][m]

  return(result)
}

#'Atomic MultiEOF
#'@param field_arr_raw An array of dimension: (n_field, lon, lat, time).
#'  where n_field is the number of variables over which to calculate the 
#'  multi_eofs.
#'@param neof_composed Number of composed eofs to return in output.
#'@param minvar Minimum variance fraction to be explained in first decomposition.
#'@param neof_max Maximum number of single eofs considered in the first 
#'  decomposition.
#'@param xlim Vector with longitudinal range limits for the calculation.
#'@param ylim Vector with latitudinal range limits for the calculation.
#'@param lon_dim String with dimension name of longitudinal coordinate.
#'@param lat_dim String with dimension name of latitudinal coordinate.
#' 
#'@return A list with elements \code{$coeff} (an array of time-varying principal 
#'component coefficients), \code{$variance} (a matrix of explained variances),
#'\code{eof_pattern} (a matrix of EOF patterns obtained by regression for each 
#'variable).
#'@noRd

.multi.eofs <- function(field_arr_raw, lon, lat, dates, neof_max = 40, 
                        neof_composed = 5, minvar = 0.6, xlim = NULL, 
                        ylim = NULL, lon_dim = "lon", lat_dim = "lat") {

  if (exists(".lm.fit")) {
    lin.fit <- .lm.fit
  } else {
    lin.fit <- lm.fit
  }
  
  # Dimensions
  n_field <- dim(field_arr_raw)[1]
  n_lon <- dim(field_arr_raw)[2]
  n_lat <- dim(field_arr_raw)[3]
  nt <- dim(field_arr_raw)[4]

  etime <- .power.date(dates)

  field_arr <- array(dim = dim(field_arr_raw))
  for (k in seq(1, n_field, 1)) {
    field_arr[k, , , ] <- .daily.anom.mean(lon, lat, field_arr_raw[k, , , ], etime)
  }

  # area weighting, based on the root of cosine
  ww <- .area.weight(lon, lat, root = T)

  # create a mask
  mask_arr <- array(dim = c(n_lon, n_lat, n_field))
  
  for (k in seq(1, n_field, 1)) {
    field_orig <- field_arr[k, , , ]

    # Check if all the time steps at one grid point are NA-consistent
    # The grid point should have all NAs or no NA along time dim.
    if (anyNA(field_orig)) {
      field_latlon <- array(field_orig, dim = c(n_lon*n_lat, nt))  # [lon*lat, time]
      na_ind <- which(is.na(field_latlon), arr.ind = T)
      if (dim(na_ind)[1] != nt * length(unique(na_ind[,1]))) {
        stop("Detected certain grid points have NAs but not consistent across time ",
             "dimension. If the grid point is NA, it should have NA at all time step.")
      }
    }
    # Build the mask
    mask <- field_orig[, , 1]
    mask[!is.finite(mask)] <- NA
    mask[is.finite(mask)] <- 1
    dim(mask) <- c(n_lon, n_lat) 
    mask_arr[,,k] <- mask
    
    # Replace mask of NAs with 0s for EOF analysis.
    field_arr[k, , , ][!is.finite(field_orig)] <- 0
    field_orig[!is.finite(field_orig)] <- 0

    # calculate the area weight
    field <- sweep(field_orig, c(1, 2), ww, "*")
    idx <- .selbox(lon, lat, xlim, ylim)
    slon <- lon[idx$ilon]
    slat <- lat[idx$ilat]
    field <- field[idx$ilon, idx$ilat, ]

    # transform 3D field in a matrix
    field <- array(field, dim = c(dim(field)[1] * dim(field)[2], dim(field)[3]))

    # calling SVD
    SVD <- svd(field, nu = neof_max, nv = neof_max)

    # extracting EOFs (loading pattern), expansions coefficient
    # and variance explained
    pattern <- array(SVD$u, dim = c(dim(field)[1], dim(field)[2], neof_max))
    coefficient <- SVD$v
    variance <- (SVD$d[1:neof_max]) ^ 2 / sum((SVD$d) ^ 2)
    reqPC <- which(cumsum(variance) > minvar)[1]
    variance <- variance[1:reqPC]
    coefficient <- coefficient[, 1:reqPC]
    if (reqPC == 1) {
      coefficient <- replicate(1, coefficient)
    }
    coefficient <- apply(coefficient, c(2), .standardize)
    regression <- array(NA, dim = c(length(lon), length(lat), neof_max))
    for (i in 1:reqPC) {
      regression[, , i] <- apply(field_orig, c(1, 2),
                                 function(x) lin.fit(as.matrix(coefficient[, i],
                                                     ncol = 1), x)$coefficients)*mask
    }
    assign(paste0("pc", k), list(coeff = coefficient, variance = variance,
                                 wcoeff = sweep(coefficient, c(2), variance, "*"),
                                 regression = regression))
  }

  newpc <- NULL
  for (k in seq(1, n_field, 1)) {
    newpc <- cbind(newpc, get(paste0("pc", k))$wcoeff)
  }
  newpc <- t(newpc)

  SVD <- svd(newpc, nu = neof_composed, nv = neof_composed)
  # extracting EOFs, expansions coefficient and variance explained
  coefficient <- SVD$v
  variance <- (SVD$d[1:(neof_composed)]) ^ 2 / sum( (SVD$d) ^ 2)
  coefficient <- apply(coefficient, c(2), .standardize)

  # linear regressions on anomalies
  regression <- array(dim = c(n_field, length(lon),
                              length(lat), neof_composed))
  for (k in seq(1, n_field, 1)) {
    for (i in 1:neof_composed) {
      regression[k, , , i] <- apply(field_arr[k, , , ], c(1, 2),
                                    function(x) lin.fit(as.matrix(coefficient[, i],
                                                        ncol = 1), x)$coefficients)*mask_arr[,,k]
    }
  }

  names(dim(coefficient)) <- c("dates", "eof")
  variance <- array(variance)
  names(dim(variance)) <- "eof"
  names(dim(regression)) <- c(names(dim(field_arr_raw))[1:3], "eof")

  out <- list(coeff = coefficient, variance = variance, eof_pattern = regression, 
              mask = mask_arr)

  out[[names(n_lon)]] <- slon
  out[[names(n_lat)]] <- slat

  return(out)
}

# new function to create simple list with date values - Oct-18
# it needs a date or PCICt object, and returns also the season subdivision
.power.date <- function(datas, verbose = FALSE) {

  # create a "season" for continuous time, used by persistance tracking
  startpoints <- c(0, which(diff(datas) > 1))
  deltapoints <- diff(c(startpoints, length(datas)))
  seas <- inverse.rle(list(lengths = deltapoints,
                           values = seq(1, length(startpoints))))

  etime <- list(
    day = as.numeric(format(datas, "%d")),
    month = as.numeric(format(datas, "%m")),
    year = as.numeric(format(datas, "%Y")), data = datas, season = seas
  )

  .printv("Time Array Built", verbose)
  .printv(paste("Length:", length(seas)), verbose)
  .printv(paste("From", datas[1], "to", datas[length(seas)]), verbose)
  return(etime)
}

# function for daily anomalies, use array predeclaration and rowMeans (40 times faster!)
.daily.anom.mean <- function(ics, ipsilon, field, etime) {
  condition <- paste(etime$day, etime$month)
  daily <- array(NA, dim = c(length(ics), length(ipsilon),
                             length(unique(condition))))
  anom <- field * NA

  for (t in unique(condition)) {
    if (sum(t == condition) == 1) {
      print(paste0("Cannot compute a mean with only one value: ",
                   "using climatological mean"))
      anom[, , which(t == condition)] <- rowMeans(field, dims = 2)
    } else {
      daily[, , which(t == unique(condition))] <- rowMeans(field[, , t == condition], dims = 2)
      anom[, , which(t == condition)] <- sweep(field[, , which(t == condition)],
                                               c(1, 2),
                                               daily[, , which(t == unique(condition))], "-")
    }
  }
  return(anom)
}
