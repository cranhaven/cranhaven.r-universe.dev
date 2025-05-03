#'Compute the Potential Evapotranspiration
#' 
#'Compute the Potential evapotranspiration (PET) that is the amount of 
#'evaporation and transpiration that would occur if a sufficient water source 
#'were available. This function calculate PET according to the Thornthwaite, 
#'Hargreaves or Hargreaves-modified equations. 
#' 
#'This function is build to be compatible with other tools in 
#'that work with 's2dv_cube' object class. The input data must be this object 
#'class. If you don't work with 's2dv_cube', see PeriodPET. For more information 
#'on the SPEI calculation, see functions CST_PeriodStandardization and 
#'CST_PeriodAccumulation.
#'
#'@param data A named list with the needed \code{s2dv_cube} objects containing 
#'  the seasonal forecast experiment in the 'data' element for each variable. 
#'  Specific variables are needed for each method used in computing the 
#'  Potential Evapotranspiration (see parameter 'pet_method'). The accepted 
#'  variable names are fixed in order to be recognized by the function. 
#'  The accepted name corresponding to the Minimum Temperature is 'tmin', 
#'  for Maximum Temperature is 'tmax', for Mean Temperature is 'tmean' and 
#'  for Precipitation is 'pr'. The accepted variable names for each method are: 
#'  For 'hargreaves': 'tmin' and 'tmax'; for 'hargreaves_modified' are 'tmin', 
#'  'tmax' and 'pr'; for method 'thornthwaite' 'tmean' is required. The units 
#'  for temperature variables ('tmin', 'tmax' and 'tmean') need to be in Celcius 
#'  degrees; the units for precipitation ('pr') need to be in mm/month. 
#'  Currently the function works only with monthly data from different years.
#'@param pet_method A character string indicating the method used to compute 
#'  the potential evapotranspiration. The accepted methods are:
#'  'hargreaves' and 'hargreaves_modified', that require the data to have 
#'  variables tmin and tmax; and 'thornthwaite', that requires variable 
#'  'tmean'. 
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'syear'. 
#'@param leadtime_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. 
#'@param lat_dim A character string indicating the name of the latitudinal 
#'  dimension. By default it is set by 'latitude'.
#'@param na.rm A logical value indicating whether NA values should be removed 
#'  from data. It is FALSE by default. 
#'@param ncores An integer value indicating the number of cores to use in 
#'  parallel computation.
#' 
#'@examples 
#'dims <- c(time = 3, syear = 3, ensemble = 1, latitude = 1)
#'exp_tasmax <- array(rnorm(360, 27.73, 5.26), dim = dims)
#'exp_tasmin <- array(rnorm(360, 14.83, 3.86), dim = dims)
#'exp_prlr <- array(rnorm(360, 21.19, 25.64), dim = dims)
#'end_year <- 2012
#'dates_exp <- as.POSIXct(c(paste0(2010:end_year, "-08-16"), 
#'                          paste0(2010:end_year, "-09-15"), 
#'                          paste0(2010:end_year, "-10-16")), "UTC")
#'dim(dates_exp) <- c(syear = 3, time = 3)
#'lat <- c(40)
#'exp1 <- list('tmax' = exp_tasmax, 'tmin' = exp_tasmin, 'pr' = exp_prlr)
#'res <- PeriodPET(data = exp1, lat = lat, dates = dates_exp)
#'
#'@importFrom CSTools s2dv_cube
#'@export 
CST_PeriodPET <- function(data, pet_method = 'hargreaves', 
                          time_dim = 'syear', leadtime_dim = 'time',
                          lat_dim = 'latitude', na.rm = FALSE, 
                          ncores = NULL) {
  # Check 's2dv_cube'
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!all(sapply(data, function(x) inherits(x, 's2dv_cube')))) {
    stop("Parameter 'data' must be a list of 's2dv_cube' class.")
  }
  # latitude
  if (!any(names(data[[1]]$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names of parameter 'data' do not match any ",
         "of the names accepted by the package.")
  }
  # Dates
  dates_exp <- data[[1]]$attrs$Dates
  if (!'Dates' %in% names(data[[1]]$attrs)) {
    stop("Element 'Dates' is not found in 'attrs' list of 'data'. ", 
         "See 's2dv_cube' object description in README file for more ", 
         "information.")
  }
  lat_dim <- names(data[[1]]$coords)[[which(names(data[[1]]$coords) %in% .KnownLatNames())]]

  res <- PeriodPET(data = lapply(data, function(x) x$data), 
                   dates = data[[1]]$attrs$Dates, 
                   lat = data[[1]]$coords[[lat_dim]], 
                   pet_method = pet_method, time_dim = time_dim, 
                   leadtime_dim = leadtime_dim, lat_dim = lat_dim, 
                   na.rm = na.rm, ncores = ncores)
  # Add metadata
  source_files <- lapply(data, function(x) {x$attrs$source_files})
  coords <- data[[1]]$coords
  Dates <- data[[1]]$attrs$Dates
  
  metadata <- data[[1]]$attrs$Variable$metadata
  metadata[["PET"]]$longname <- "Potential evapotranspiration"
  metadata[["PET"]]$units <- "mm"

  metadata_names <- c(intersect(names(dim(res)), names(metadata)), "PET")
  suppressWarnings(
    res <- s2dv_cube(data = res, coords = coords, 
                     varName = paste0('PET'), 
                     metadata = metadata[metadata_names],
                     Dates = Dates, 
                     source_files = source_files, 
                     when = Sys.time())
  )
  return(res)
}

#'Compute the Potential Evapotranspiration
#' 
#'Compute the Potential Evapotranspiration (PET) that is the amount of 
#'evaporation and transpiration that would occur if a sufficient water source 
#'were available. This function calculate PET according to the Thornthwaite, 
#'Hargreaves or Hargreaves-modified equations. 
#' 
#'For more information on the SPEI calculation, see functions 
#'PeriodStandardization and PeriodAccumulation.
#'
#'@param data A named list of multidimensional arrays containing
#'  the seasonal forecast experiment data for each variable. 
#'  Specific variables are needed for each method used in computing the 
#'  Potential Evapotranspiration (see parameter 'pet_method'). The accepted 
#'  variable names are fixed in order to be recognized by the function. 
#'  The accepted name corresponding to the Minimum Temperature is 'tmin', 
#'  for Maximum Temperature is 'tmax', for Mean Temperature is 'tmean' and 
#'  for Precipitation is 'pr'. The accepted variable names for each method are: 
#'  For 'hargreaves': 'tmin' and 'tmax'; for 'hargreaves_modified' are 'tmin', 
#'  'tmax' and 'pr'; for method 'thornthwaite' 'tmean' is required. The units 
#'  for temperature variables ('tmin', 'tmax' and 'tmean') need to be in Celcius 
#'  degrees; the units for precipitation ('pr') need to be in mm/month. 
#'  Currently the function works only with monthly data from different years.
#'@param dates An array of temporal dimensions containing the Dates of 
#'  'data'. It must be of class 'Date' or 'POSIXct'.
#'@param lat A numeric vector containing the latitude values of 'data'.
#'@param pet_method A character string indicating the method used to compute 
#'  the potential evapotranspiration. The accepted methods are:
#'  'hargreaves' and 'hargreaves_modified', that require the data to have 
#'  variables tmin and tmax; and 'thornthwaite', that requires variable 
#'  'tmean'. 
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'syear'. 
#'@param leadtime_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. 
#'@param lat_dim A character string indicating the name of the latitudinal 
#'  dimension. By default it is set by 'latitude'.
#'@param na.rm A logical value indicating whether NA values should be removed 
#'  from data. It is FALSE by default. 
#'@param ncores An integer value indicating the number of cores to use in 
#'  parallel computation.
#' 
#'@examples 
#'dims <- c(time = 3, syear = 3, ensemble = 1, latitude = 1)
#'exp_tasmax <- array(rnorm(360, 27.73, 5.26), dim = dims)
#'exp_tasmin <- array(rnorm(360, 14.83, 3.86), dim = dims)
#'exp_prlr <- array(rnorm(360, 21.19, 25.64), dim = dims)
#'end_year <- 2012
#'dates_exp <- as.POSIXct(c(paste0(2010:end_year, "-08-16"), 
#'                          paste0(2010:end_year, "-09-15"), 
#'                          paste0(2010:end_year, "-10-16")), "UTC")
#'dim(dates_exp) <- c(syear = 3, time = 3)
#'lat <- c(40)
#'exp1 <- list('tmax' = exp_tasmax, 'tmin' = exp_tasmin, 'pr' = exp_prlr)
#'res <- PeriodPET(data = exp1, lat = lat, dates = dates_exp)
#'
#'@importFrom SPEI hargreaves thornthwaite
#'@import multiApply
#'@export 
PeriodPET <- function(data, dates, lat, pet_method = 'hargreaves', 
                      time_dim = 'syear', leadtime_dim = 'time',
                      lat_dim = 'latitude', na.rm = FALSE, 
                      ncores = NULL) {

  # Initial checks
  # data
  if (!inherits(data, 'list')) {
    stop("Parameter 'data' needs to be a named list with the needed variables.")
  }
  if (is.null(names(data))) {
    stop("Parameter 'data' needs to be a named list with the variable names.")
  }
  if (any(sapply(data, function(x) is.null(names(dim(x)))))) {
    stop("Parameter 'data' needs to be a list of arrays with dimension names.")
  }
  dims <- lapply(data, function(x) dim(x))
  first_dims <- dims[[1]]
  all_equal <- all(sapply(dims[-1], function(x) identical(first_dims, x)))
  if (!all_equal) {
    stop("Parameter 'data' variables need to have the same dimensions.")
  }
  # lat
  if (!is.numeric(lat)) {
    stop("Parameter 'lat' must be numeric.")
  }
  if (!lat_dim %in% names(dims[[1]])) {
    stop("Parameter 'data' must have 'lat_dim' dimension.")
  }
  if (any(sapply(dims, FUN = function(x) x[lat_dim] != length(lat)))) {
    stop("Parameter 'lat' needs to have the same length of latitudinal", 
         "dimension of all the variables arrays in 'data'.")
  }

  # data (2)
  if (all(c('tmin', 'tmax', 'pr') %in% names(data))) {
    # hargreaves modified: 'tmin', 'tmax', 'pr' and 'lat'
    if (!(pet_method %in% c('hargreaves_modified', 'hargreaves'))) {
      warning("Parameter 'pet_method' needs to be 'hargreaves' or ", 
              "'hargreaves_modified'. It is set to 'hargreaves_modified'.")
      pet_method <- 'hargreaves_modified'
    }
  } else if (all(c('tmin', 'tmax') %in% names(data))) { 
    if (!(pet_method %in% c('hargreaves'))) {
      warning("Parameter 'pet_method' will be set as 'hargreaves'.")
      pet_method <- 'hargreaves'
    }
  } else if (c('tmean') %in% names(data)) {
    # thornthwaite: 'tmean' (mean), 'lat'
    if (!(pet_method == 'thornthwaite')) {
      warning("Parameter 'pet_method' it is set to be 'thornthwaite'.")
      pet_method <- 'thornthwaite'
    }
  } else {
    stop("Parameter 'data' needs to be a named list with accepted ", 
          "variable names. See documentation.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!all(sapply(data, function(x) time_dim %in% names(dim(x))))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  ## leadtime_dim
  if (!is.character(leadtime_dim) | length(leadtime_dim) != 1) {
    stop("Parameter 'leadtime_dim' must be a character string.")
  }
  if (!all(sapply(data, function(x) leadtime_dim %in% names(dim(x))))) {
    stop("Parameter 'leadtime_dim' is not found in 'data' dimension.")
  }
  ## lat_dim
  if (!is.character(lat_dim) | length(lat_dim) != 1) {
    stop("Parameter 'lat_dim' must be a character string.")
  }
  if (!all(sapply(data, function(x) lat_dim %in% names(dim(x))))) {
    stop("Parameter 'lat_dim' is not found in 'data' dimension.")
  }
  # dates
  if (is.null(dates)) {
    stop("Parameter 'dates' is missing, dates must be provided.")
  }
  if (!any(inherits(dates, 'Date'), inherits(dates, 'POSIXct'))) {
    stop("Parameter 'dates' is not of the correct class, ", 
         "only 'Date' and 'POSIXct' classes are accepted.")
  }
  if (!time_dim %in% names(dim(dates)) | !leadtime_dim %in% names(dim(dates))) {
    stop("Parameter 'dates' must have 'time_dim' and 'leadtime_dim' ",
         "dimension.")
  }
  if (!all(dim(data[[1]])[c(time_dim, leadtime_dim)] == 
           dim(dates)[c(time_dim, leadtime_dim)])) {
    stop("Parameter 'dates' needs to have the same length as 'time_dim' ", 
         "and 'leadtime_dim' as 'data'.")
  }
  ## na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | any(ncores %% 1 != 0) | any(ncores < 0) |
        length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  # complete dates
  mask_dates <- .datesmask(dates, frequency = 'monthly')
  lat_mask <- array(lat, dim = c(1, length(lat)))
  names(dim(lat_mask)) <- c('dat', lat_dim)

  # extract mask of NA locations to return to NA the final result
  mask_na <- array(1, dim = dim(data[[1]]))
  if (pet_method == 'hargreaves') {
    varnames <- c('tmax', 'tmin')
    mask_na[which(is.na(data$tmax))] <- 0
    mask_na[which(is.na(data$tmin))] <- 0
  } else if (pet_method == 'hargreaves_modified') {
    varnames <- c('tmax', 'tmin', 'pr')
    mask_na[which(is.na(data$tmax))] <- 0
    mask_na[which(is.na(data$tmin))] <- 0
    mask_na[which(is.na(data$pr))] <- 0
  } else if (pet_method == 'thornthwaite') {
    varnames <- c('tmean')
    mask_na[which(is.na(data$tmean))] <- 0
  }

  # replace NA with 0
  for (dd in 1:length(data)) {
    data[[dd]][which(is.na(data[[dd]]))] <- 0
  }

  # prepare data
  target_dims_data <- lapply(data[varnames], function(x) rep(c(leadtime_dim, time_dim), 1))
  pet <- Apply(data = c(list(lat_mask = lat_mask), data[varnames]), 
               target_dims = c(list(lat_mask = 'dat'), target_dims_data), 
               fun = .pet, 
               mask_dates = mask_dates, pet_method = pet_method, 
               leadtime_dim = leadtime_dim, time_dim = time_dim, 
               output_dims = c(leadtime_dim, time_dim),
               ncores = ncores)$output1
  # reorder dims in pet_estimated
  pos <- match(names(dim(data[[1]])), names(dim(pet)))
  pet <- aperm(pet, pos)

  # restore original NAs from mask_na
  pet[which(mask_na == 0)] <- NA

  return(pet)
}

.pet <- function(lat_mask, data2, data3 = NULL, data4 = NULL, 
                 mask_dates, pet_method = 'hargreaves', 
                 leadtime_dim = 'time', time_dim = 'syear') {
 
  dims <- dim(data2)

  # create a vector from data but adding 0 to achive complete time series 
  # of the considered period 
  # (starting in January of the first year) so that the solar radiation 
  # estimation is computed in each case for the correct month

  if (!is.null(data2)) {
    data_tmp <- as.vector(data2)
    data2 <- array(0, dim = length(mask_dates))
    count <- 1
    for (dd in 1:length(mask_dates)) {
      if (mask_dates[dd] == 1) {
        data2[dd] <- data_tmp[count]
        count <- count + 1
      }
    }
    rm(data_tmp)
  }
  if (!is.null(data3)) {
    data_tmp <- as.vector(data3)
    data3 <- array(0, dim = length(mask_dates))
    count <- 1
    for (dd in 1:length(mask_dates)) {
      if (mask_dates[dd] == 1) {
        data3[dd] <- data_tmp[count]
        count <- count + 1
      }
    }
    rm(data_tmp)
  }
  if (!is.null(data4)) {
    data_tmp <- as.vector(data4)
    data4 <- array(0, dim = length(mask_dates))
    count <- 1
    for (dd in 1:length(mask_dates)) {
      if (mask_dates[dd] == 1) {
        data4[dd] <- data_tmp[count]
        count <- count + 1
      }
    }
    rm(data_tmp)
  }
  if (pet_method == 'hargreaves') {
    pet <- hargreaves(Tmin = as.vector(data3), Tmax = as.vector(data2), 
                      lat = lat_mask, na.rm = FALSE, verbose = FALSE)
    # line to return the vector to the size of the actual original data
    pet <- array(pet[which(mask_dates == 1)], dim = dims)
  }

  if (pet_method == 'hargreaves_modified') {
    pet <- hargreaves(Tmin = as.vector(data3), Tmax = as.vector(data2), 
                      lat = lat_mask, Pre = as.vector(data4), na.rm = FALSE,
                      verbose = FALSE)
    pet <- array(pet[which(mask_dates == 1)], dim = dims)
  }
  
  if (pet_method == 'thornthwaite') {
    pet <- thornthwaite(as.vector(data2), lat = lat_mask, na.rm = TRUE,
                        verbose = FALSE)
    # line to return the vector to the size of the actual original data
    pet <- array(pet[which(mask_dates == 1)], dim = dims)
  }
  return(pet)
}
