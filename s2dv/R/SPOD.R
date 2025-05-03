#'Compute the South Pacific Ocean Dipole (SPOD) index
#'
#'The South Pacific Ocean Dipole (SPOD) index is related to the El 
#'Nino-Southern Oscillation (ENSO) and the Inderdecadal Pacific Oscillation 
#'(IPO). The SPOD index is computed as the difference of weighted-averaged SST
#'anomalies over 20ºS-48ºS, 165ºE-190ºE (NW pole) and the weighted-averaged SST
#'anomalies over 44ºS-65ºS, 220ºE-260ºE (SE pole) (Saurral et al., 2020). 
#'If different members and/or datasets are provided, the climatology (used to 
#'calculate the anomalies) is computed individually for all of them.
#'
#'@param data A numerical array to be used for the index computation with, at least, the
#'  dimensions: 1) latitude, longitude, start date and forecast month 
#'  (in case of decadal predictions), 2) latitude, longitude, year and month 
#'  (in case of historical simulations or observations). This data has to be 
#'  provided, at least, over the whole region needed to compute the index.
#'@param data_lats A numeric vector indicating the latitudes of the data.
#'@param data_lons A numeric vector indicating the longitudes of the data.
#'@param type A character string indicating the type of data ('dcpp' for 
#'  decadal predictions, 'hist' for historical simulations, or 'obs' for 
#'  observations or reanalyses).
#'@param lat_dim A character string of the name of the latitude dimension. The
#' default value is 'lat'.
#'@param lon_dim A character string of the name of the longitude dimension. The
#' default value is 'lon'.
#'@param mask An array of a mask (with 0's in the grid points that have to be 
#'  masked) or NULL (i.e., no mask is used). This parameter allows to remove 
#'  the values over land in case the dataset is a combination of surface air 
#'  temperature over land and sea surface temperature over the ocean. Also, it
#'  can be used to mask those grid points that are missing in the observational
#'  dataset for a fair comparison between the forecast system and the reference
#'  dataset. The default value is NULL.
#'@param monini An integer indicating the month in which the forecast system is
#'  initialized. Only used when parameter 'type' is 'dcpp'. The default value 
#'  is 11, i.e., initialized in November.
#'@param fmonth_dim A character string indicating the name of the forecast
#'  month dimension. Only used if parameter 'type' is 'dcpp'. The default value
#'  is 'fmonth'.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. Only used if parameter 'type' is 'dcpp'. The default value is 
#'  'sdate'.
#'@param indices_for_clim A numeric vector of the indices of the years to
#'  compute the climatology for calculating the anomalies, or NULL so the 
#'  climatology is calculated over the whole period. If the data are already 
#'  anomalies, set it to FALSE. The default value is NULL.\cr
#'  In case of parameter 'type' is 'dcpp', 'indices_for_clim' must be relative 
#'  to the first forecast year, and the climatology is automatically computed 
#'  over the common calendar period for the different forecast years.
#'@param year_dim A character string indicating the name of the year dimension
#'  The default value is 'year'. Only used if parameter 'type' is 'hist' or 
#'  'obs'.
#'@param month_dim A character string indicating the name of the month
#'  dimension. The default value is 'month'. Only used if parameter 'type' is 
#'  'hist' or 'obs'.
#'@param na.rm A logical value indicanting whether to remove NA values. The default 
#'  value is TRUE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return A numerical array with the SPOD index with the same dimensions as data except 
#'  the lat_dim, lon_dim and fmonth_dim (month_dim) in case of decadal predictions 
#'  (historical simulations or observations). In case of decadal predictions, a new dimension
#'  'fyear' is added.
#'
#'@examples
#' ## Observations or reanalyses
#' obs <- array(1:100, dim = c(year = 5, lat = 19, lon = 37, month = 12))
#' lat <- seq(-90, 90, 10)
#' lon <- seq(0, 360, 10)
#' index_obs <- SPOD(data = obs, data_lats = lat, data_lons = lon, type = 'obs')
#' 
#' ## Historical simulations
#' hist <- array(1:100, dim = c(year = 5, lat = 19, lon = 37, month = 12, member = 5))
#' lat <- seq(-90, 90, 10)
#' lon <- seq(0, 360, 10)
#' index_hist <- SPOD(data = hist, data_lats = lat, data_lons = lon, type = 'hist')
#' 
#' ## Decadal predictions
#' dcpp <- array(1:100, dim = c(sdate = 5, lat = 19, lon = 37, fmonth = 24, member = 5))
#' lat <- seq(-90, 90, 10)
#' lon <- seq(0, 360, 10)
#' index_dcpp <- SPOD(data = dcpp, data_lats = lat, data_lons = lon, type = 'dcpp', monini = 1)
#'
#'@importFrom ClimProjDiags WeightedMean CombineIndices
#'@import multiApply
#'@export
SPOD <- function(data, data_lats, data_lons, type, lat_dim = 'lat', lon_dim = 'lon', 
                 mask = NULL, monini = 11, fmonth_dim = 'fmonth', sdate_dim = 'sdate', 
                 indices_for_clim = NULL, year_dim = 'year', month_dim = 'month', 
                 na.rm = TRUE, ncores = NULL) {
  
  ## Input Checks
  # data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  # data_lats and data_lons part1
  if (!(inherits(data_lats, 'numeric') | inherits(data_lats, 'integer'))) {
    stop("Parameter 'data_lats' must be a numeric vector.")
  }
  if (!(inherits(data_lons, 'numeric') | inherits(data_lons, 'integer'))) {
    stop("Parameter 'data_lons' must be a numeric vector.")
  }
  # type
  if (!type %in% c('dcpp', 'hist', 'obs')) {
    stop("Parameter 'type' must be 'dcpp', 'hist', or 'obs'.")
  }
  # lat_dim
  if (!(is.character(lat_dim) & length(lat_dim) == 1)) {
    stop("Parameter 'lat_dim' must be a character string.")
  }
  if (!lat_dim %in% names(dim(data))) {
    stop("Parameter 'lat_dim' is not found in 'data' dimension.")
  }
  # lon_dim
  if (!(is.character(lon_dim) & length(lon_dim) == 1)) {
    stop("Parameter 'lon_dim' must be a character string.")
  }
  if (!lon_dim %in% names(dim(data))) {
    stop("Parameter 'lon_dim' is not found in 'data' dimension.")
  }
  # data_lats and data_lons part2
  if (dim(data)[lat_dim] != length(data_lats)) {
    stop("The latitude dimension of parameter 'data' must be the same",
         " length of parameter 'data_lats'.")
  }
  if (dim(data)[lon_dim] != length(data_lons)) {
    stop("The longitude dimension of parameter 'data' must be the same",
         " length of parameter 'data_lons'.")
  }
  # ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  # mask 
  if (!is.null(mask)) {
    if (is.array(mask) & identical(names(dim(mask)), c(lat_dim, lon_dim)) &
        identical(as.integer(dim(mask)), c(length(data_lats), length(data_lons)))) {
      ## To mask those grid point that are missing in the observations
      mask <- s2dv::Reorder(data = mask, order = c(lat_dim, lon_dim))
      fun_mask <- function(data, mask) {
        data[mask == 0] <- NA
        return(data)
      }
      data <- multiApply::Apply(data = data, target_dims = c(lat_dim, lon_dim),
                                fun = fun_mask, mask = mask, ncores = ncores)$output1
    } else {
      stop("Parameter 'mask' must be NULL (no mask) or a numerical array ",
           "with c(lat_dim, lon_dim) dimensions and 0 in those grid ",
           "points that have to be masked.")
    }
  }
  # monini
  if (type == 'dcpp' &&
      (!is.numeric(monini) | monini %% 1 != 0 | monini < 1 | monini > 12)) {
    stop("Parameter 'monini' must be an integer from 1 to 12.")
  }
  # fmonth_dim
  if (type == 'dcpp') {
    if (!(is.character(fmonth_dim) & length(fmonth_dim) == 1)) {
      stop("Parameter 'fmonth_dim' must be a character string.")
    }
    if (!fmonth_dim %in% names(dim(data))) {
      stop("Parameter 'fmonth_dim' is not found in 'data' dimension.")
    }
  }
  # sdate_dim
  if (type == 'dcpp') {
    if (!(is.character(sdate_dim) & length(sdate_dim) == 1)) {
      stop("Parameter 'sdate_dim' must be a character string.")
    }
    if (!sdate_dim %in% names(dim(data))) {
      stop("Parameter 'sdate_dim' is not found in 'data' dimension.")
    }
  }
  # indices_for_clim
  if (!is.null(indices_for_clim)) {
    if (!(is(indices_for_clim, "numeric") || is(indices_for_clim, "integer")) &
        !(is.logical(indices_for_clim) & !any(indices_for_clim))) {
      stop("The parameter 'indices_for_clim' must be a numeric vector ",
           "or NULL to compute the anomalies based on the whole period, ",
           "or FALSE if data are already anomalies")
    }
  }
  # year_dim
  if (type == 'hist' | type == 'obs') {
    if (!(is.character(year_dim) & length(year_dim) == 1)) {
      stop("Parameter 'year_dim' must be a character string.")
    }
    if (!year_dim %in% names(dim(data))) {
      stop("Parameter 'year_dim' is not found in 'data' dimension.")
    }
  }
  # month_dim
  if (type == 'hist' | type == 'obs') {
    if (!(is.character(month_dim) & length(month_dim) == 1)) {
      stop("Parameter 'month_dim' must be a character string.")
    }
    if (!month_dim %in% names(dim(data))) {
      stop("Parameter 'month_dim' is not found in 'data' dimension.")
    }
  }
  # na.rm
  if (!na.rm %in% c(TRUE, FALSE)) {
    stop("Parameter 'na.rm' must be TRUE or FALSE")
  }

  ## Regions for IPO_SPOD (Saurral et al., 2020)
  lat_min_1 <- -48; lat_max_1 <- -20
  lon_min_1 <- 165; lon_max_1 <- 190
  lat_min_2 <- -65; lat_max_2 <- -44
  lon_min_2 <- 220; lon_max_2 <- 260
  regions <- NULL
  regions$reg1 <- c(lon_min_1, lon_max_1, lat_min_1, lat_max_1)
  regions$reg2 <- c(lon_min_2, lon_max_2, lat_min_2, lat_max_2)
  
  mean_1 <- ClimProjDiags::WeightedMean(data = data, lon = data_lons, lat = data_lats, 
                                        region = regions$reg1,
                                        londim = lon_dim, 
                                        latdim = lat_dim)
  mean_2 <- ClimProjDiags::WeightedMean(data = data, lon = data_lons, lat = data_lats, 
                                        region = regions$reg2,
                                        londim = lon_dim, 
                                        latdim = lat_dim)
  
  data <- ClimProjDiags::CombineIndices(indices = list(mean_1, mean_2), 
                                        weights = NULL, operation = 'subtract') # (mean_1 - mean_2)
  
  if (type == 'dcpp') {
    target_dims <- c(sdate_dim, fmonth_dim)
  } else if (type %in% c('hist', 'obs')) {
    target_dims <- c(year_dim, month_dim)
  }
  
  INDEX <- multiApply::Apply(data = data, target_dims = target_dims, fun = .Indices,
                             type = type, monini = monini, indices_for_clim = indices_for_clim,
                             fmonth_dim = fmonth_dim, sdate_dim = sdate_dim, 
                             year_dim = year_dim, month_dim = month_dim, 
                             na.rm = na.rm, ncores = ncores)$output1
  return(INDEX)
}
