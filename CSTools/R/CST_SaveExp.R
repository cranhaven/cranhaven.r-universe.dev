#'Save objects of class 's2dv_cube' to data in NetCDF format
#'
#'@author Perez-Zanon Nuria, \email{nuria.perez@bsc.es}
#'
#'@description This function allows to divide and save a object of class 
#''s2dv_cube' into a NetCDF file, allowing to reload the saved data using 
#'\code{CST_Start} or \code{CST_Load} functions. It also allows to save any 
#''s2dv_cube' object that follows the NetCDF attributes conventions.
#'
#'@param data An object of class \code{s2dv_cube}.
#'@param destination A character string containing the directory name in which 
#'  to save the data. NetCDF file for each starting date are saved into the 
#'  folder tree: 'destination/Dataset/variable/'. By default the function 
#'  saves the data into the working directory.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'. It can be NULL if there is no
#'  start date dimension.
#'@param ftime_dim A character string indicating the name of the forecast time  
#'  dimension. If 'Dates' are used, it can't be NULL. If there is no forecast 
#'  time dimension, 'Dates' will be set to NULL and will not be used. By 
#'  default, it is set to 'time'. 
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  It can be NULL if there is no dataset dimension. By default, it is set to 
#'  'dataset'. 
#'@param var_dim A character string indicating the name of variable dimension. 
#'  It can be NULL if there is no variable dimension. By default, it is set to 
#'  'var'. 
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. It can be NULL if there is no member dimension. By default, it is
#'   set to 'member'. 
#'@param startdates A vector of dates that will be used for the filenames 
#'  when saving the data in multiple files (single_file = FALSE). It must be a 
#'  vector of the same length as the start date dimension of data. It must be a 
#'  vector of class \code{Dates}, \code{'POSIXct'} or character with lenghts 
#'  between 1 and 10. If it is NULL, the coordinate corresponding the the start 
#'  date dimension or the first Date of each time step will be used as the name 
#'  of the files. It is NULL by default.
#'@param single_file A logical value indicating if all object is saved in a 
#'  single file (TRUE) or in multiple files (FALSE). When it is FALSE, 
#'  the array is separated for datasets, variable and start date. When there are 
#'  no specified time dimensions, the data will be saved in a single file by 
#'  default. The output file name when 'single_file' is TRUE is a character 
#'  string containing: '<var>_<first_sdate>_<last_sdate>.nc'; when it is FALSE, 
#'  it is '<var>_<sdate>.nc'. It is FALSE by default. 
#'@param drop_dims (optional) A vector of character strings indicating the 
#'  dimension names of length 1 that need to be dropped in order that they don't 
#'  appear in the netCDF file. Only is allowed to drop dimensions that are not 
#'  used in the computation. The dimensions used in the computation are the ones 
#'  specified in: sdate_dim, ftime_dim, dat_dim, var_dim and memb_dim. It is 
#'  NULL by default.
#'@param extra_string (Optional) A character string to be included as part of 
#'  the file name, for instance, to identify member or realization. When 
#'  single_file is TRUE, the 'extra_string' will substitute all the default 
#'  file name; when single_file is FALSE, the 'extra_string' will be added 
#'  in the file name as: '<var>_<extra_string>_<sdate>.nc'. It is NULL by 
#'  default.
#'@param units_hours_since (Optional) A logical value only available for the 
#'  case: 'Dates' have forecast time and start date dimension, 'single_file' is 
#'  TRUE and 'time_bounds' are not used. When it is TRUE, it saves the forecast 
#'  time with units of 'hours since'; if it is FALSE, the time units will be a 
#'  number of time steps with its corresponding frequency (e.g. n days, n months 
#'  or n hours). It is FALSE by default. 
#'@param global_attrs (Optional) A list with elements containing the global 
#'  attributes to be saved in the NetCDF.
#'
#'@return Multiple or single NetCDF files containing the data array.\cr
#'\item{\code{single_file is TRUE}}{
#'  All data is saved in a single file located in the specified destination  
#'  path with the following name (by default): 
#'  '<variable_name>_<first_sdate>_<last_sdate>.nc'. Multiple variables
#'  are saved separately in the same file. The forecast time units 
#'  are calculated from each start date (if sdate_dim is not NULL) or from 
#'  the time step. If 'units_hours_since' is TRUE, the forecast time units 
#'  will be 'hours since <each start date>'. If 'units_hours_since' is FALSE, 
#'  the forecast time units are extracted from the frequency of the time steps 
#'  (hours, days, months); if no frequency is found, the units will be ’hours 
#'  since’. When the time units are 'hours since' the time ateps are assumed to 
#'  be equally spaced.
#'}
#'\item{\code{single_file is FALSE}}{
#'  The data array is subset and stored into multiple files. Each file 
#'  contains the data subset for each start date, variable and dataset. Files 
#'  with different variables and datasets are stored in separated directories 
#'  within the following directory tree: 'destination/Dataset/variable/'. 
#'  The name of each file will be by default: '<variable_name>_<sdate>.nc'. 
#'  The forecast time units are calculated from each start date (if sdate_dim 
#'  is not NULL) or from the time step. The forecast time units will be 'hours 
#'  since <each start date>'.
#'}
#' 
#'@seealso \code{\link[startR]{Start}}, \code{\link{as.s2dv_cube}} and 
#'\code{\link{s2dv_cube}}
#'
#'@examples
#'\dontrun{
#'data <- lonlat_temp_st$exp
#'CST_SaveExp(data = data, ftime_dim = 'ftime', var_dim = 'var', 
#'            dat_dim = 'dataset', sdate_dim = 'sdate')
#'}
#'
#'@export
CST_SaveExp <- function(data, destination = "./", startdates = NULL, 
                        sdate_dim = 'sdate', ftime_dim = 'time', 
                        memb_dim = 'member', dat_dim = 'dataset', 
                        var_dim = 'var', drop_dims = NULL, 
                        single_file = FALSE, extra_string = NULL, 
                        global_attrs = NULL, units_hours_since = FALSE) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  # Check object structure
  if (!all(c('data', 'attrs') %in% names(data))) {
    stop("Parameter 'data' must have at least 'data' and 'attrs' elements ",
         "within the 's2dv_cube' structure.")
  }
  if (!inherits(data$attrs, 'list')) {
    stop("Level 'attrs' must be a list with at least 'Dates' element.")
  }
  # metadata
  if (!is.null(data$attrs$Variable$metadata)) {
    if (!inherits(data$attrs$Variable$metadata, 'list')) {
      stop("Element metadata from Variable element in attrs must be a list.")
    }
  }
  # Dates
  if (is.null(data$attrs$Dates)) {
    stop("Element 'Dates' from 'attrs' level cannot be NULL.")
  }
  if (is.null(dim(data$attrs$Dates))) {
    stop("Element 'Dates' from 'attrs' level must have time dimensions.")
  }
  # sdate_dim
  if (!is.null(sdate_dim)) {
    if (!is.character(sdate_dim)) {
      stop("Parameter 'sdate_dim' must be a character string.")
    }
  }
  # startdates
  if (is.null(startdates)) {
    if (is.character(data$coords[[sdate_dim]])) {
      startdates <- data$coords[[sdate_dim]]
    }
  }

  SaveExp(data = data$data,
          destination = destination, 
          coords = data$coords,
          Dates = data$attrs$Dates, 
          time_bounds = data$attrs$time_bounds,
          startdates = startdates,
          varname = data$attrs$Variable$varName,
          metadata = data$attrs$Variable$metadata,
          Datasets = data$attrs$Datasets, 
          sdate_dim = sdate_dim, ftime_dim = ftime_dim, 
          memb_dim = memb_dim, 
          dat_dim = dat_dim, var_dim = var_dim, 
          drop_dims = drop_dims, 
          single_file = single_file,
          extra_string = extra_string, 
          global_attrs = global_attrs,
          units_hours_since = units_hours_since)
}
#'Save a multidimensional array with metadata to data in NetCDF format
#'@description This function allows to save a data array with metadata into a 
#'NetCDF file, allowing to reload the saved data using \code{Start} function 
#'from StartR package. If the original 's2dv_cube' object has been created from 
#'\code{CST_Load()}, then it can be reloaded with \code{Load()}.
#'
#'@author Perez-Zanon Nuria, \email{nuria.perez@bsc.es}
#'
#'@param data A multi-dimensional array with named dimensions.
#'@param destination A character string indicating the path where to store the 
#'  NetCDF files.
#'@param coords A named list with elements of the coordinates corresponding to 
#'  the dimensions of the data parameter. The names and length of each element 
#'  must correspond to the names of the dimensions. If any coordinate is not 
#'  provided, it is set as an index vector with the values from 1 to the length 
#'  of the corresponding dimension.
#'@param Dates A named array of dates with the corresponding sdate and forecast 
#'  time dimension. If there is no sdate_dim, you can set it to NULL. 
#'  It must have ftime_dim dimension.
#'@param time_bounds (Optional) A list of two arrays of dates containing 
#'  the lower (first array) and the upper (second array) time bounds 
#'  corresponding to Dates. Each array must have the same dimensions as Dates.
#'  If 'Dates' parameter is NULL, 'time_bounds' are not used. It is NULL by 
#'  default.
#'@param startdates A vector of dates that will be used for the filenames 
#'  when saving the data in multiple files (single_file = FALSE). It must be a 
#'  vector of the same length as the start date dimension of data. It must be a 
#'  vector of class \code{Dates}, \code{'POSIXct'} or character with lenghts 
#'  between 1 and 10. If it is NULL, the coordinate corresponding the the start 
#'  date dimension or the first Date of each time step will be used as the name 
#'  of the files. It is NULL by default.
#'@param varname A character string indicating the name of the variable to be 
#'  saved.
#'@param metadata A named list where each element is a variable containing the
#'  corresponding information. The information must be contained in a list of 
#'  lists for each variable.
#'@param Datasets A vector of character string indicating the names of the 
#'  datasets.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'. It can be NULL if there is no
#'  start date dimension.
#'@param ftime_dim A character string indicating the name of the forecast time  
#'  dimension. By default, it is set to 'time'. It can be NULL if there is no 
#'  forecast time dimension.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  By default, it is set to 'dataset'. It can be NULL if there is no dataset  
#'  dimension.
#'@param var_dim A character string indicating the name of variable dimension. 
#'  By default, it is set to 'var'. It can be NULL if there is no variable  
#'  dimension.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. By default, it is set to 'member'. It can be NULL if there is no 
#'  member dimension.  
#'@param drop_dims (optional) A vector of character strings indicating the 
#'  dimension names of length 1 that need to be dropped in order that they don't 
#'  appear in the netCDF file. Only is allowed to drop dimensions that are not 
#'  used in the computation. The dimensions used in the computation are the ones 
#'  specified in: sdate_dim, ftime_dim, dat_dim, var_dim and memb_dim. It is 
#'  NULL by default.
#'@param single_file A logical value indicating if all object is saved in a 
#'  single file (TRUE) or in multiple files (FALSE). When it is FALSE, 
#'  the array is separated for datasets, variable and start date. When there are 
#'  no specified time dimensions, the data will be saved in a single file by 
#'  default. The output file name when 'single_file' is TRUE is a character 
#'  string containing: '<var>_<first_sdate>_<last_sdate>.nc'; when it is FALSE, 
#'  it is '<var>_<sdate>.nc'. It is FALSE by default. 
#'@param extra_string (Optional) A character string to be included as part of 
#'  the file name, for instance, to identify member or realization. When 
#'  single_file is TRUE, the 'extra_string' will substitute all the default 
#'  file name; when single_file is FALSE, the 'extra_string' will be added 
#'  in the file name as: '<var>_<extra_string>_<sdate>.nc'. It is NULL by 
#'  default.
#'@param global_attrs (Optional) A list with elements containing the global 
#'  attributes to be saved in the NetCDF.
#'@param units_hours_since (Optional) A logical value only available for the 
#'  case: Dates have forecast time and start date dimension, single_file is 
#'  TRUE and 'time_bounds' is NULL. When it is TRUE, it saves the forecast time 
#'  with units of 'hours since'; if it is FALSE, the time units will be a number 
#'  of time steps with its corresponding frequency (e.g. n days, n months or n 
#'  hours). It is FALSE by default. 
#'
#'@return Multiple or single NetCDF files containing the data array.\cr
#'\item{\code{single_file is TRUE}}{
#'  All data is saved in a single file located in the specified destination  
#'  path with the following name (by default): 
#'  '<variable_name>_<first_sdate>_<last_sdate>.nc'. Multiple variables
#'  are saved separately in the same file. The forecast time units 
#'  are calculated from each start date (if sdate_dim is not NULL) or from 
#'  the time step. If 'units_hours_since' is TRUE, the forecast time units 
#'  will be 'hours since <each start date>'. If 'units_hours_since' is FALSE, 
#'  the forecast time units are extracted from the frequency of the time steps 
#'  (hours, days, months); if no frequency is found, the units will be ’hours 
#'  since’. When the time units are 'hours since' the time ateps are assumed to 
#'  be equally spaced.
#'}
#'\item{\code{single_file is FALSE}}{
#'  The data array is subset and stored into multiple files. Each file 
#'  contains the data subset for each start date, variable and dataset. Files 
#'  with different variables and datasets are stored in separated directories 
#'  within the following directory tree: 'destination/Dataset/variable/'. 
#'  The name of each file will be by default: '<variable_name>_<sdate>.nc'. 
#'  The forecast time units are calculated from each start date (if sdate_dim 
#'  is not NULL) or from the time step. The forecast time units will be 'hours 
#'  since <each start date>'.
#'}
#' 
#'@examples
#'\dontrun{
#'data <- lonlat_temp_st$exp$data
#'lon <- lonlat_temp_st$exp$coords$lon
#'lat <- lonlat_temp_st$exp$coords$lat
#'coords <- list(lon = lon, lat = lat)
#'Datasets <- lonlat_temp_st$exp$attrs$Datasets
#'varname <- 'tas'
#'Dates <- lonlat_temp_st$exp$attrs$Dates
#'metadata <- lonlat_temp_st$exp$attrs$Variable$metadata
#'SaveExp(data = data, coords = coords, Datasets = Datasets, varname = varname, 
#'        Dates = Dates, metadata = metadata, single_file = TRUE, 
#'        ftime_dim = 'ftime', var_dim = 'var', dat_dim = 'dataset')
#'}
#' 
#'@import easyNCDF
#'@importFrom s2dv Reorder
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
SaveExp <- function(data, destination = "./", coords = NULL, 
                    Dates = NULL, time_bounds = NULL, startdates = NULL, 
                    varname = NULL, metadata = NULL, Datasets = NULL, 
                    sdate_dim = 'sdate', ftime_dim = 'time', 
                    memb_dim = 'member', dat_dim = 'dataset', var_dim = 'var', 
                    drop_dims = NULL, single_file = FALSE, extra_string = NULL,
                    global_attrs = NULL, units_hours_since = FALSE) {
  ## Initial checks
  # data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  dimnames <- names(dim(data))
  if (is.null(dimnames)) {
    stop("Parameter 'data' must be an array with named dimensions.")
  }
  if (!is.null(attributes(data)$dimensions)) {
    attributes(data)$dimensions <- NULL
  }
  # destination
  if (!is.character(destination) | length(destination) > 1) {
    stop("Parameter 'destination' must be a character string of one element ",
         "indicating the name of the file (including the folder if needed) ",
         "where the data will be saved.")
  }
  # drop_dims
  if (!is.null(drop_dims)) {
    if (!is.character(drop_dims) | any(!drop_dims %in% names(dim(data)))) {
      warning("Parameter 'drop_dims' must be character string containing ", 
              "the data dimension names to be dropped. It will not be used.")
    } else if (!all(dim(data)[drop_dims] %in% 1)) {
      warning("Parameter 'drop_dims' can only contain dimension names ", 
              "that are of length 1. It will not be used.")
    } else if (any(drop_dims %in% c(ftime_dim, sdate_dim, dat_dim, memb_dim, var_dim))) {
      warning("Parameter 'drop_dims' contains dimensions used in the computation. ",
              "It will not be used.")
      drop_dims <- NULL
    } else {
      data <- Subset(x = data, along = drop_dims, 
                     indices = lapply(1:length(drop_dims), function(x) 1), 
                     drop = 'selected')
      dimnames <- names(dim(data))
    }
  }
  # coords
  if (!is.null(coords)) {
    if (!inherits(coords, 'list')) {
      stop("Parameter 'coords' must be a named list of coordinates.")
    }
    if (is.null(names(coords))) {
      stop("Parameter 'coords' must have names corresponding to coordinates.")
    }
  } else {
    coords <- sapply(dimnames, function(x) 1:dim(data)[x])
  }
  # varname
  if (is.null(varname)) {
    varname <- 'X'
  } else if (length(varname) > 1) {
    multiple_vars <- TRUE
  } else {
    multiple_vars <- FALSE
  }
  if (!all(sapply(varname, is.character))) {
    stop("Parameter 'varname' must be a character string with the ",
         "variable names.")
  }
  # single_file
  if (!inherits(single_file, 'logical')) {
    warning("Parameter 'single_file' must be a logical value. It will be ", 
            "set as FALSE.")
    single_file <- FALSE
  }
  # extra_string
  if (!is.null(extra_string)) {
    if (!is.character(extra_string)) {
      stop("Parameter 'extra_string' must be a character string.")
    }
  }
  # global_attrs
  if (!is.null(global_attrs)) {
    if (!inherits(global_attrs, 'list')) {
      stop("Parameter 'global_attrs' must be a list.")
    }
  }

  ## Dimensions checks
  # Spatial coordinates
  if (!any(dimnames %in% .KnownLonNames()) | 
      !any(dimnames %in% .KnownLatNames())) {
    lon_dim <- NULL
    lat_dim <- NULL
  } else {
    lon_dim <- dimnames[which(dimnames %in% .KnownLonNames())]
    lat_dim <- dimnames[which(dimnames %in% .KnownLatNames())]
  }
  # ftime_dim
  if (!is.null(ftime_dim)) {
    if (!is.character(ftime_dim)) {
      stop("Parameter 'ftime_dim' must be a character string.")
    }
    if (!all(ftime_dim %in% dimnames)) {
      stop("Parameter 'ftime_dim' is not found in 'data' dimension. Set it ", 
           "as NULL if there is no forecast time dimension.")
    }
  }
  # sdate_dim
  if (!is.null(sdate_dim)) {
    if (!is.character(sdate_dim)) {
      stop("Parameter 'sdate_dim' must be a character string.")
    }
    if (!all(sdate_dim %in% dimnames)) {
      stop("Parameter 'sdate_dim' is not found in 'data' dimension.")
    }
  }
  # memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim)) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!all(memb_dim %in% dimnames)) {
      stop("Parameter 'memb_dim' is not found in 'data' dimension. Set it ", 
           "as NULL if there is no member dimension.")
    }
  }
  # dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim)) {
      stop("Parameter 'dat_dim' must be a character string.")
    }
    if (!all(dat_dim %in% dimnames)) {
      stop("Parameter 'dat_dim' is not found in 'data' dimension. Set it ", 
           "as NULL if there is no Datasets dimension.")
    }
    n_datasets <- dim(data)[dat_dim]
  } else {
    n_datasets <- 1
  }
  # var_dim
  if (!is.null(var_dim)) {
    if (!is.character(var_dim)) {
      stop("Parameter 'var_dim' must be a character string.")
    }
    if (!all(var_dim %in% dimnames)) {
      stop("Parameter 'var_dim' is not found in 'data' dimension. Set it ", 
           "as NULL if there is no variable dimension.")
    }
    n_vars <- dim(data)[var_dim]
  } else {
    n_vars <- 1
  }
  # minimum dimensions
  if (all(dimnames %in% c(var_dim, dat_dim))) {
    if (!single_file) {
      warning("Parameter data has only ", 
              paste(c(var_dim, dat_dim), collapse = ' and '), " dimensions ", 
              "and it cannot be splitted in multiple files. All data will ", 
              "be saved in a single file.")
      single_file <- TRUE
    }
  }
  # Dates (1): initial checks
  if (!is.null(Dates)) {
    if (!any(inherits(Dates, "POSIXct"), inherits(Dates, "Date"))) {
      stop("Parameter 'Dates' must be of 'POSIXct' or 'Dates' class.")
    }
    if (is.null(dim(Dates))) {
      stop("Parameter 'Dates' must have dimension names.")
    }
    if (all(is.null(ftime_dim), is.null(sdate_dim))) {
      warning("Parameters 'ftime_dim' and 'sdate_dim' can't both be NULL ",
              "if 'Dates' are used. 'Dates' will not be used.")
      Dates <- NULL
    }
    # sdate_dim in Dates
    if (!is.null(sdate_dim)) {
      if (!sdate_dim %in% names(dim(Dates))) {
        warning("Parameter 'sdate_dim' is not found in 'Dates' dimension. ",
                "Dates will not be used.")
        Dates <- NULL
      }
    }
    # ftime_dim in Dates
    if (!is.null(ftime_dim)) {
      if (!ftime_dim %in% names(dim(Dates))) {
        warning("Parameter 'ftime_dim' is not found in 'Dates' dimension. ",
                "Dates will not be used.")
        Dates <- NULL
      }
    }
  }
  # time_bounds 
  if (!is.null(time_bounds)) {
    if (!inherits(time_bounds, 'list')) {
      stop("Parameter 'time_bounds' must be a list with two dates arrays.")
    }
    time_bounds_dims <- lapply(time_bounds, function(x) dim(x))
    if (!identical(time_bounds_dims[[1]], time_bounds_dims[[2]])) {
      stop("Parameter 'time_bounds' must have 2 arrays with same dimensions.")
    }
    if (is.null(Dates)) {
      time_bounds <- NULL
    } else {
      name_tb <- sort(names(time_bounds_dims[[1]]))
      name_dt <- sort(names(dim(Dates)))
      if (!identical(dim(Dates)[name_dt], time_bounds_dims[[1]][name_tb])) {
        stop(paste0("Parameter 'Dates' and 'time_bounds' must have same length ",
                    "of all dimensions."))
      }
    }
  }
  # Dates (2): Check dimensions
  if (!is.null(Dates)) {
    if (any(dim(Dates)[!names(dim(Dates)) %in% c(ftime_dim, sdate_dim)] != 1)) {
      stop("Parameter 'Dates' can have only 'sdate_dim' and 'ftime_dim' ",
           "dimensions of length greater than 1.")
    }
    # drop dimensions of length 1 different from sdate_dim and ftime_dim
    dim(Dates) <- dim(Dates)[names(dim(Dates)) %in% c(ftime_dim, sdate_dim)]

    # add ftime if needed
    if (is.null(ftime_dim)) {
      warning("A 'time' dimension of length 1 will be added to 'Dates'.") 
      dim(Dates) <- c(time = 1, dim(Dates))
      dim(data) <- c(time = 1, dim(data))
      dimnames <- names(dim(data))
      ftime_dim <- 'time'
      if (!is.null(time_bounds)) {
        time_bounds <- lapply(time_bounds, function(x) {
          dim(x) <- c(time = 1, dim(x))
          return(x)
        })
      }
      units_hours_since <- TRUE
    }
    # add sdate if needed
    if (is.null(sdate_dim)) {
      if (!single_file) {
        dim(Dates) <- c(dim(Dates), sdate = 1)
        dim(data) <- c(dim(data), sdate = 1)
        dimnames <- names(dim(data))
        sdate_dim <- 'sdate'
        if (!is.null(time_bounds)) {
          time_bounds <- lapply(time_bounds, function(x) {
            dim(x) <- c(dim(x), sdate = 1)
            return(x)
          })
        }
        if (!is.null(startdates)) {
          if (length(startdates) != 1) {
            warning("Parameter 'startdates' must be of length 1 if 'sdate_dim' is NULL.",
                    "They won't be used.")
            startdates <- NULL
          }
        }
      }
      units_hours_since <- TRUE
    }
  }
  # startdates
  if (!is.null(Dates)) {
    # check startdates
    if (is.null(startdates)) {
      startdates <- Subset(Dates, along = ftime_dim, 1, drop = 'selected')
    } else if (any(nchar(startdates) > 10, nchar(startdates) < 1)) {
      warning("Parameter 'startdates' should be a character string containing ", 
              "the start dates in the format 'yyyy-mm-dd', 'yyyymmdd', 'yyyymm', ", 
              "'POSIXct' or 'Dates' class. Files will be named with Dates instead.")
      startdates <- Subset(Dates, along = ftime_dim, 1, drop = 'selected')
    }
  } else if (!single_file) {
    warning("Dates must be provided if 'data' must be saved in separated files. ",
            "All data will be saved in a single file.")
    single_file <- TRUE
  }
  # startdates
  if (is.null(startdates)) {
    if (is.null(sdate_dim)) {
      startdates <- 'XXX'
    } else {
      startdates <- rep('XXX', dim(data)[sdate_dim])
    }
  } else {
    if (any(inherits(startdates, "POSIXct"), inherits(startdates, "Date"))) {
      startdates <- format(startdates, "%Y%m%d")
    }
    if (!is.null(sdate_dim)) {
      if (dim(data)[sdate_dim] != length(startdates)) {
        warning(paste0("Parameter 'startdates' doesn't have the same length ",
                       "as dimension '", sdate_dim,"', it will not be used."))
        startdates <- Subset(Dates, along = ftime_dim, 1, drop = 'selected')
        startdates <- format(startdates, "%Y%m%d")
      }
    }
  }

  # Datasets
  if (is.null(Datasets)) {
    Datasets <- rep('XXX', n_datasets )
  }
  if (inherits(Datasets, 'list')) {
    Datasets <- names(Datasets)
  }
  if (n_datasets > length(Datasets)) {
    warning("Dimension 'Datasets' in 'data' is greater than those listed in ",
            "element 'Datasets' and the first element will be reused.")
    Datasets <- c(Datasets, rep(Datasets[1], n_datasets - length(Datasets)))
  } else if (n_datasets < length(Datasets)) {
    warning("Dimension 'Datasets' in 'data' is smaller than those listed in ",
            "element 'Datasets' and only the firsts elements will be used.")
    Datasets <- Datasets[1:n_datasets]
  }

  ## NetCDF dimensions definition
  excluded_dims <- var_dim
  if (!is.null(Dates)) {
    excluded_dims <- c(excluded_dims, sdate_dim, ftime_dim)
  }
  if (!single_file) {
    excluded_dims <- c(excluded_dims, dat_dim)
  }

  ## Unknown dimensions check
  alldims <- c(dat_dim, var_dim, sdate_dim, lon_dim, lat_dim, ftime_dim, memb_dim)
  if (!all(dimnames %in% alldims)) {
    unknown_dims <- dimnames[which(!dimnames %in% alldims)]
    memb_dim <- c(memb_dim, unknown_dims)
  }

  filedims <- c(dat_dim, var_dim, sdate_dim, lon_dim, lat_dim, ftime_dim, memb_dim)
  filedims <- filedims[which(!filedims %in% excluded_dims)]

  # Delete unneded coords
  coords[c(names(coords)[!names(coords) %in% filedims])] <- NULL
  out_coords <- NULL
  for (i_coord in filedims) {
    # vals
    if (i_coord %in% names(coords)) {
      if (length(coords[[i_coord]]) != dim(data)[i_coord]) {
        warning(paste0("Coordinate '", i_coord, "' has different lenght as ",
                       "its dimension and it will not be used."))
        out_coords[[i_coord]] <- 1:dim(data)[i_coord]
      } else if (is.numeric(coords[[i_coord]])) {
        out_coords[[i_coord]] <- as.vector(coords[[i_coord]])
      } else {
        out_coords[[i_coord]] <- 1:dim(data)[i_coord]
      }
    } else {
      out_coords[[i_coord]] <- 1:dim(data)[i_coord]
    }
    dim(out_coords[[i_coord]]) <- dim(data)[i_coord]
    
    ## metadata
    if (i_coord %in% names(metadata)) {
      if ('variables' %in% names(attributes(metadata[[i_coord]]))) {
        # from Start: 'lon' or 'lat'
        attrs <- attributes(metadata[[i_coord]])[['variables']]
        attrs[[i_coord]]$dim <- NULL
        attr(out_coords[[i_coord]], 'variables') <- attrs
      } else if (inherits(metadata[[i_coord]], 'list')) {
        # from Start and Load: main var
        attr(out_coords[[i_coord]], 'variables') <- list(metadata[[i_coord]])
        names(attributes(out_coords[[i_coord]])$variables) <- i_coord
      } else if (!is.null(attributes(metadata[[i_coord]]))) {
        # from Load
        attrs <- attributes(metadata[[i_coord]])
        # We remove because some attributes can't be saved
        attrs <- NULL 
        attr(out_coords[[i_coord]], 'variables') <- list(attrs)
        names(attributes(out_coords[[i_coord]])$variables) <- i_coord
      }
    }
  }

  if (!single_file) {
    for (i in 1:n_datasets) {
      path <- file.path(destination, Datasets[i], varname)
      for (j in 1:n_vars) {
        if (!dir.exists(path[j])) {
          dir.create(path[j], recursive = TRUE)
        }
        startdates <- gsub("-", "", startdates)
        dim(startdates) <- c(length(startdates))
        names(dim(startdates)) <- sdate_dim
        if (is.null(dat_dim) & is.null(var_dim)) {
          data_subset <- data
        } else if (is.null(dat_dim)) {
          data_subset <- Subset(data, c(var_dim), list(j), drop = 'selected')
        } else if (is.null(var_dim)) {
          data_subset <- Subset(data, along = c(dat_dim), list(i), drop = 'selected')
        } else {
          data_subset <- Subset(data, c(dat_dim, var_dim), list(i, j), drop = 'selected')
        }
        target <- names(dim(data_subset))[which(!names(dim(data_subset)) %in% c(sdate_dim, ftime_dim))]
        target_dims_data <- c(target, ftime_dim)
        if (is.null(Dates)) {
          input_data <- list(data_subset, startdates)
          target_dims <- list(target_dims_data, NULL)
        } else if (!is.null(time_bounds)) {
          input_data <- list(data_subset, startdates, Dates, 
                             time_bounds[[1]], time_bounds[[2]])
          target_dims = list(target_dims_data, NULL, 
                             ftime_dim, ftime_dim, ftime_dim)
        } else {
          input_data <- list(data_subset, startdates, Dates)
          target_dims = list(target_dims_data, NULL, ftime_dim)
        }
        Apply(data = input_data,
              target_dims = target_dims,
              fun = .saveexp, 
              destination = path[j],
              coords = out_coords, 
              ftime_dim = ftime_dim, 
              varname = varname[j], 
              metadata_var = metadata[[varname[j]]], 
              extra_string = extra_string,
              global_attrs = global_attrs)
      }
    }
  } else {
    # time_bnds
    if (!is.null(time_bounds)) {
      time_bnds <- c(time_bounds[[1]], time_bounds[[2]])
    }
    # Dates
    remove_metadata_dim <- TRUE
    if (!is.null(Dates)) {
      if (is.null(sdate_dim)) {
        sdates <- Dates[1]
        # ftime definition
        leadtimes <- as.numeric(difftime(Dates, sdates, units = "hours"))
      } else {
        # sdate definition
        sdates <- Subset(Dates, along = ftime_dim, 1, drop = 'selected')
        differ <- as.numeric(difftime(sdates, sdates[1], units = "hours"))
        dim(differ) <- dim(data)[sdate_dim]
        differ <- list(differ)
        names(differ) <- sdate_dim
        out_coords <- c(differ, out_coords)
        attrs <- list(units = paste('hours since', sdates[1]),
                      calendar = 'proleptic_gregorian', longname = sdate_dim)
        attr(out_coords[[sdate_dim]], 'variables')[[sdate_dim]] <- attrs
        # ftime definition
        Dates <- Reorder(Dates, c(ftime_dim, sdate_dim)) 
        differ_ftime <- array(dim = dim(Dates))
        for (i in 1:length(sdates)) {
          differ_ftime[, i] <- as.numeric(difftime(Dates[, i], Dates[1, i], 
                                          units = "hours"))
        }
        dim(differ_ftime) <- dim(Dates)
        leadtimes <- Subset(differ_ftime, along = sdate_dim, 1, drop = 'selected')
        if (!all(apply(differ_ftime, 1, function(x){length(unique(x)) == 1}))) {
          warning("Time steps are not equal for all start dates. Only ", 
                  "forecast time values for the first start date will be saved ", 
                  "correctly.")
        }
      }
      if (all(!units_hours_since, is.null(time_bounds))) {
        if (all(diff(leadtimes/24) == 1)) {
          # daily values
          units <- 'days'
          leadtimes_vals <- round(leadtimes/24) + 1
        } else if (all(diff(leadtimes/24) %in% c(28, 29, 30, 31))) {
          # monthly values
          units <- 'months'
          leadtimes_vals <- round(leadtimes/(30.437*24)) + 1   
        } else {
          # other frequency
          units <- 'hours'
          leadtimes_vals <- leadtimes + 1
        }
      } else {
        units <- paste('hours since', paste(sdates, collapse = ', '))
        leadtimes_vals <- leadtimes
      }

      # Add time_bnds
      if (!is.null(time_bounds)) {
        if (is.null(sdate_dim)) {
          sdates <- Dates[1]
          time_bnds <- c(time_bounds[[1]], time_bounds[[2]])
          leadtimes_bnds <- as.numeric(difftime(time_bnds, sdates, units = "hours"))
          dim(leadtimes_bnds) <- c(dim(Dates), bnds = 2)
        } else {
          # assuming they have sdate and ftime
          time_bnds <- lapply(time_bounds, function(x) {
            x <- Reorder(x, c(ftime_dim, sdate_dim))
            return(x)
          })
          time_bnds <- c(time_bounds[[1]], time_bounds[[2]])
          dim(time_bnds) <- c(dim(Dates), bnds = 2)
          differ_bnds <- array(dim = c(dim(time_bnds)))
          for (i in 1:length(sdates)) {
            differ_bnds[, i, ] <- as.numeric(difftime(time_bnds[, i, ], Dates[1, i], 
                                            units = "hours"))
          }
          # NOTE (TODO): Add a warning when they are not equally spaced?
          leadtimes_bnds <- Subset(differ_bnds, along = sdate_dim, 1, drop = 'selected')
        }
        # Add time_bnds
        leadtimes_bnds <- Reorder(leadtimes_bnds, c('bnds', ftime_dim))
        leadtimes_bnds <- list(leadtimes_bnds)
        names(leadtimes_bnds) <- 'time_bnds'
        out_coords <- c(leadtimes_bnds, out_coords)
        attrs <- list(units = paste('hours since', paste(sdates, collapse = ', ')),
                      calendar = 'proleptic_gregorian',
                      long_name = 'time bounds', unlim = FALSE)
        attr(out_coords[['time_bnds']], 'variables')$time_bnds <- attrs
      }
      # Add ftime var
      dim(leadtimes_vals) <- dim(data)[ftime_dim]
      leadtimes_vals <- list(leadtimes_vals)
      names(leadtimes_vals) <- ftime_dim
      out_coords <- c(leadtimes_vals, out_coords)
      attrs <- list(units = units, calendar = 'proleptic_gregorian',
                    longname = ftime_dim, 
                    dim = list(list(name = ftime_dim, unlim = TRUE)))
      if (!is.null(time_bounds)) {
        attrs$bounds = 'time_bnds'
      }
      attr(out_coords[[ftime_dim]], 'variables')[[ftime_dim]] <- attrs
      for (j in 1:n_vars) {
        remove_metadata_dim <- FALSE
        metadata[[varname[j]]]$dim <- list(list(name = ftime_dim, unlim = TRUE))
      }
      # Reorder ftime_dim to last
      if (length(dim(data)) != which(names(dim(data)) == ftime_dim)) {
        order <- c(names(dim(data))[which(!names(dim(data)) %in% c(ftime_dim))], ftime_dim)
        data <- Reorder(data, order)
      }
    }
    # var definition
    extra_info_var <- NULL
    for (j in 1:n_vars) {
      varname_j <- varname[j]
      metadata_j <- metadata[[varname_j]]
      if (is.null(var_dim)) {
        out_coords[[varname_j]] <- data
      } else {
        out_coords[[varname_j]] <- Subset(data, var_dim, j, drop = 'selected')
      }
      if (!is.null(metadata_j)) {
        if (remove_metadata_dim) metadata_j$dim <- NULL
        attr(out_coords[[varname_j]], 'variables') <- list(metadata_j)
        names(attributes(out_coords[[varname_j]])$variables) <- varname_j
      }
      # Add global attributes
      if (!is.null(global_attrs)) {
        attributes(out_coords[[varname_j]])$global_attrs <- global_attrs
      }
    }
    if (is.null(extra_string)) {
      first_sdate <- startdates[1]
      last_sdate <- startdates[length(startdates)]
      gsub("-", "", first_sdate)
      file_name <- paste0(paste(c(varname, 
                                  gsub("-", "", first_sdate), 
                                  gsub("-", "", last_sdate)), 
                                  collapse = '_'), ".nc")
    } else {
      nc <- substr(extra_string, nchar(extra_string)-2, nchar(extra_string))
      if (nc == ".nc") {
        file_name <- extra_string
      } else {
        file_name <- paste0(extra_string, ".nc")
      }
    }
    full_filename <- file.path(destination, file_name)
    ArrayToNc(out_coords, full_filename)
  }
}

.saveexp <- function(data, coords, destination = "./",
                     startdates = NULL, dates = NULL, 
                     time_bnds1 = NULL, time_bnds2 = NULL, 
                     ftime_dim = 'time', varname = 'var', 
                     metadata_var = NULL, extra_string = NULL,
                     global_attrs = NULL) {
  remove_metadata_dim <- TRUE
  if (!is.null(dates)) {
    if (!any(is.null(time_bnds1), is.null(time_bnds2))) {
      time_bnds <- c(time_bnds1, time_bnds2)
      time_bnds <- as.numeric(difftime(time_bnds, dates[1], units = "hours"))
      dim(time_bnds) <- c(dim(data)[ftime_dim], bnds = 2)
      time_bnds <- Reorder(time_bnds, c('bnds', ftime_dim))
      time_bnds <- list(time_bnds)
      names(time_bnds) <- 'time_bnds'
      coords <- c(time_bnds, coords)
      attrs <- list(units = paste('hours since', dates[1]),
                    calendar = 'proleptic_gregorian',
                    longname = 'time bounds')
      attr(coords[['time_bnds']], 'variables')$time_bnds <- attrs
    }
    # Add ftime_dim
    differ <- as.numeric(difftime(dates, dates[1], units = "hours"))
    dim(differ) <- dim(data)[ftime_dim]
    differ <- list(differ)
    names(differ) <- ftime_dim
    coords <- c(differ, coords)
    attrs <- list(units = paste('hours since', dates[1]),
                  calendar = 'proleptic_gregorian',
                  longname = ftime_dim,
                  dim = list(list(name = ftime_dim, unlim = TRUE)))
    if (!is.null(time_bnds1)) {
      attrs$bounds = 'time_bnds'
    }
    attr(coords[[ftime_dim]], 'variables')[[ftime_dim]] <- attrs
    metadata_var$dim <- list(list(name = ftime_dim, unlim = TRUE))
    remove_metadata_dim <- FALSE
  }
  # Add data
  coords[[varname]] <- data
  if (!is.null(metadata_var)) {
    if (remove_metadata_dim) metadata_var$dim <- NULL
    attr(coords[[varname]], 'variables') <- list(metadata_var)
    names(attributes(coords[[varname]])$variables) <- varname
  }
  # Add global attributes
  if (!is.null(global_attrs)) {
    attributes(coords[[varname]])$global_attrs <- global_attrs
  }

  if (is.null(extra_string)) {
    file_name <- paste0(varname, "_", startdates, ".nc")
  } else {
    file_name <- paste0(varname, "_", extra_string, "_", startdates, ".nc")
  }
  full_filename <- file.path(destination, file_name)
  ArrayToNc(coords, full_filename)
}