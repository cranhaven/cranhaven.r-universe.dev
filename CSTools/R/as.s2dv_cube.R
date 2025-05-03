#'Conversion of 'startR_array' or 'list' objects to 's2dv_cube'
#'
#'This function converts data loaded using Start function from startR package or 
#'Load from s2dv into an 's2dv_cube' object.
#'
#'@author Perez-Zanon Nuria, \email{nuria.perez@bsc.es}
#'@author Nicolau Manubens, \email{nicolau.manubens@bsc.es}
#'
#'@param object An object of class 'startR_array' generated from function 
#'  \code{Start} from startR package or a list output from function \code{Load} 
#'  from s2dv package. Any other object class will not be accepted.
#'@param remove_attrs_coords A logical value indicating whether to remove the 
#'  attributes of the coordinates (TRUE) or not (FALSE). The default value is 
#'  FALSE.
#'@param remove_null Optional. A logical value indicating whether to remove the  
#'  elements that are NULL (TRUE) or not (FALSE) of the output object. It is 
#'  only used when the object is an output from function \code{Load}. The 
#'  default value is FALSE.
#' 
#'@return The function returns an 's2dv_cube' object to be easily used with 
#'functions with the prefix \code{CST} from CSTools and CSIndicators packages. 
#'The object is mainly a list with the following elements:\cr
#'\itemize{
#'  \item{'data', array with named dimensions;}
#'  \item{'dims', named vector of the data dimensions;}
#'  \item{'coords', list of named vectors with the coordinates corresponding to 
#'        the dimensions of the data parameter;}
#'  \item{'attrs', named list with elements:
#'    \itemize{
#'      \item{'Dates', array with named temporal dimensions of class 'POSIXct' 
#'            from time values in the data;}
#'      \item{'Variable', has the following components:
#'        \itemize{
#'          \item{'varName', character vector of the short variable name. It is  
#'                usually specified in the parameter 'var' from the functions 
#'                Start and Load;}
#'          \item{'metadata', named list of elements with variable metadata. 
#'                They can be from coordinates variables (e.g. longitude) or 
#'                main variables (e.g. 'var');}
#'        }
#'      }
#'      \item{'Datasets', character strings indicating the names of the 
#'            datasets;}
#'      \item{'source_files', a vector of character strings with complete paths 
#'            to all the found files involved in loading the data;}
#'      \item{'when', a time stamp of the date issued by the Start() or Load() 
#'            call to obtain the data;}
#'      \item{'load_parameters', it contains the components used in the 
#'            arguments to load the data from Start() or Load() functions.}
#'    }
#'  }
#'}
#'
#'@seealso \code{\link{s2dv_cube}}, \code{\link{CST_Start}}, 
#'\code{\link[startR]{Start}} and \code{\link{CST_Load}}
#'@examples
#'\dontrun{
#'# Example 1: convert an object from startR::Start function to 's2dv_cube'
#'library(startR)
#'repos <- '/esarchive/exp/ecmwf/system5_m1/monthly_mean/$var$_f6h/$var$_$sdate$.nc'
#'data <- Start(dat = repos,
#'              var = 'tas',
#'              sdate = c('20170101', '20180101'),
#'              ensemble = indices(1:5),
#'              time = 'all',
#'              latitude = indices(1:5),
#'              longitude = indices(1:5),
#'              return_vars = list(latitude = 'dat', longitude = 'dat', time = 'sdate'),
#'              retrieve = TRUE)
#'data <- as.s2dv_cube(data)
#'# Example 2: convert an object from s2dv::Load function to 's2dv_cube'
#'startDates <- c('20001101', '20011101', '20021101',
#'                '20031101', '20041101', '20051101')
#'data <- Load(var = 'tas', exp = 'system5c3s', 
#'             nmember = 2, sdates = startDates,
#'             leadtimemax = 3, latmin = 10, latmax = 30,
#'             lonmin = -10, lonmax = 10, output = 'lonlat')
#'data <- as.s2dv_cube(data)
#'}
#'@export
as.s2dv_cube <- function(object, remove_attrs_coords = FALSE, 
                         remove_null = FALSE) {
  
  if (is.list(object) & length(object) == 11) {
    if (is.null(object) || (is.null(object$mod) && is.null(object$obs))) {
      stop("The s2dv::Load call did not return any data.")
    }
    obs <- object
    obs$mod <- NULL
    object$obs <- NULL
    names(object)[[1]] <- 'data' # exp
    names(obs)[[1]] <- 'data' # obs
    # obs
    if (!is.null(obs$data)) {
      obs_exist <- TRUE
      obs$Datasets$exp <- NULL
      obs$Datasets <- obs$Datasets$obs
    } else {
      obs_exist <- FALSE
    }
    # object
    if (!is.null(object$data)) {
      exp_exist <- TRUE
      object$Datasets$obs <- NULL
      object$Datasets <- object$Datasets$exp
    } else {
      exp_exist <- FALSE
    }
    result <- list()
    # obs and exp
    if (obs_exist & exp_exist) {
      obs_exp = list(exp = object, obs = obs)
    } else if (obs_exist & !exp_exist) {
      obs_exp = list(obs = obs)
    } else {
      obs_exp = list(exp = object)
    }
    i <- 0
    for (obj_i in obs_exp) {
      i <- i + 1
      # attrs
      obj_i$attrs <- within(obj_i, rm(list = c('data')))
      obj_i <- within(obj_i, rm(list = names(obj_i$attrs)))
      dates <- obj_i$attrs$Dates$start
      attr(dates, 'end') <- obj_i$attrs$Dates$end
      if (!is.null(dates)) {
        dim(dates) <- dim(obj_i$data)[c('ftime', 'sdate')]
        obj_i$attrs$Dates <- dates
      }
      # Variable
      varname <- obj_i$attrs$Variable$varName
      varmetadata <- NULL
      varmetadata[[varname]] <- attributes(obj_i$attrs$Variable)[-1]
      obj_i$attrs$Variable <- list(varName = varname, metadata = varmetadata)
      # dims
      obj_i$dims <- dim(obj_i$data)
      # coords
      obj_i$coords <- sapply(names(dim(obj_i$data)), function(x) NULL)
      # sdate
      obj_i$coords$sdate <- obj_i$attrs$load_parameters$sdates
      if (!remove_attrs_coords) attr(obj_i$coords$sdate, 'indices') <- FALSE
      # lon
      if (!is.null(obj_i$attrs$lon)) {
        if (remove_attrs_coords) {
          obj_i$coords$lon <- as.vector(obj_i$attrs$lon)
        } else {
          obj_i$coords$lon <- obj_i$attrs$lon
          dim(obj_i$coords$lon) <- NULL
          attr(obj_i$coords$lon, 'indices') <- FALSE
        }
        obj_i$attrs$Variable$metadata$lon <- obj_i$attrs$lon
        obj_i$attrs <- within(obj_i$attrs, rm(list = 'lon'))
      }
      # lat
      if (!is.null(obj_i$attrs$lat)) {
        if (remove_attrs_coords) {
          obj_i$coords$lat <- as.vector(obj_i$attrs$lat)
        } else {
          obj_i$coords$lat <- obj_i$attrs$lat
          dim(obj_i$coords$lat) <- NULL
          attr(obj_i$coords$lat, 'indices') <- FALSE
        }
        obj_i$attrs$Variable$metadata$lat <- obj_i$attrs$lat
        obj_i$attrs <- within(obj_i$attrs, rm(list = 'lat'))
      }
      # member
      obj_i$coords$member <- 1:obj_i$dims['member']
      if (!remove_attrs_coords) attr(obj_i$coords$member, 'indices') <- TRUE
      # dataset
      if (!is.null(names(obj_i$attrs$Datasets))) {
        obj_i$coords$dataset <- names(obj_i$attrs$Datasets)
        if (!remove_attrs_coords) attr(obj_i$coords$dataset, 'indices') <- FALSE
        obj_i$attrs$Datasets <- names(obj_i$attrs$Datasets)
      } else {
        obj_i$coords$dataset <- 1:obj_i$dims['dataset']
        if (!remove_attrs_coords) attr(obj_i$coords$dataset, 'indices') <- TRUE
      }
      # ftime
      obj_i$coords$ftime <- 1:obj_i$dims['ftime']
      if (!remove_attrs_coords) attr(obj_i$coords$ftime, 'indices') <- TRUE
      # remove NULL values
      if (isTRUE(remove_null)) {
        obj_i$attrs$load_parameters <- .rmNullObs(obj_i$attrs$load_parameters)
      }
      obj_i <- obj_i[c('data', 'dims', 'coords', 'attrs')]
      class(obj_i) <- 's2dv_cube'
      if (names(obs_exp)[[i]] == 'exp') {
        result$exp <- obj_i
      } else {
        result$obs <- obj_i
      }
    }
    if (is.list(result)) {
      if (is.null(result$exp)) {
          result <- result$obs
      } else if (is.null(result$obs)) {
          result <- result$exp
      } else {
          warning("The output is a list of two 's2dv_cube' objects",
                  " corresponding to 'exp' and 'obs'.")
      }
    }

  } else if (inherits(object, 'startR_array')) {
    # From Start:
    result <- list()
    result$data <- as.vector(object)
    ## dims
    dims <- dim(object)
    dim(result$data) <- dims
    result$dims <- dims
    ## coords
    result$coords <- sapply(names(dims), function(x) NULL)
    # Find coordinates
    FileSelector <- attributes(object)$FileSelectors
    VariablesCommon <- names(attributes(object)$Variables$common)
    dat <- names(FileSelector)[1]
    VariablesDat <- names(attributes(object)$Variables[[dat]])
    varName <- NULL
    for (i_coord in names(dims)) {
      if (i_coord %in% names(FileSelector[[dat]])) { # coords in FileSelector
        coord_in_fileselector <- FileSelector[[dat]][which(i_coord == names(FileSelector[[dat]]))]
        if (length(coord_in_fileselector) == 1) {
          if (length(coord_in_fileselector[[i_coord]][[1]]) == dims[i_coord]) {
            # TO DO: add var_dim parameter
            if (i_coord %in% c('var', 'vars')) {
              varName <- as.vector(coord_in_fileselector[[i_coord]][[1]])
            }
            if (remove_attrs_coords) {
              result$coords[[i_coord]] <- as.vector(coord_in_fileselector[[i_coord]][[1]])
            } else {
              result$coords[[i_coord]] <- coord_in_fileselector[[i_coord]][[1]]
              attr(result$coords[[i_coord]], 'indices') <- FALSE
            }
          } else {
            result$coords[[i_coord]] <- 1:dims[i_coord]
            if (!remove_attrs_coords) attr(result$coords[[i_coord]], 'indices') <- TRUE
          }
        }
      } else if (i_coord %in% VariablesCommon) { # coords in common
        coord_in_common <- attributes(object)$Variables$common[[which(i_coord == VariablesCommon)]]
        if (inherits(coord_in_common, "POSIXct")) {
          result$attrs$Dates <- coord_in_common
        }
        if (length(coord_in_common) == dims[i_coord]) {
          if (remove_attrs_coords) {
            if (inherits(coord_in_common, "POSIXct")) {
              result$coords[[i_coord]] <- 1:dims[i_coord]
              attr(result$coords[[i_coord]], 'indices') <- TRUE
            } else {
              result$coords[[i_coord]] <- as.vector(coord_in_common)
            }
          } else {
            result$coords[[i_coord]] <- coord_in_common
            attr(result$coords[[i_coord]], 'indices') <- FALSE
          }
        } else {
          result$coords[[i_coord]] <- 1:dims[i_coord]
          if (!remove_attrs_coords) attr(result$coords[[i_coord]], 'indices') <- TRUE
        }
      } else if (!is.null(VariablesDat)) { # coords in dat
        if (i_coord %in% VariablesDat) {
          coord_in_dat <- attributes(object)$Variables[[dat]][[which(i_coord == VariablesDat)]]
          if (inherits(coord_in_dat, "POSIXct")) {
            result$attrs$Dates <- coord_in_dat
          }
          if (length(coord_in_dat) == dims[i_coord]) {
            if (remove_attrs_coords) {
              if (inherits(coord_in_dat, "POSIXct")) {
                result$coords[[i_coord]] <- coord_in_dat
              } else {
                result$coords[[i_coord]] <- as.vector(coord_in_dat)
              }
            } else {
              result$coords[[i_coord]] <- coord_in_dat
              attr(result$coords[[i_coord]], 'indices') <- FALSE
            }
          } else {
            result$coords[[i_coord]] <- 1:dims[i_coord]
            if (!remove_attrs_coords) attr(result$coords[[i_coord]], 'indices') <- TRUE
          }
        } else {
          result$coords[[i_coord]] <- 1:dims[i_coord]
          if (!remove_attrs_coords) attr(result$coords[[i_coord]], 'indices') <- TRUE
        }
      } else { # missing other dims
        result$coords[[i_coord]] <- 1:dims[i_coord]
        if (!remove_attrs_coords) attr(result$coords[[i_coord]], 'indices') <- TRUE
      }
      dim(result$coords[[i_coord]]) <- NULL
    }

    # attrs
    ## varName
    if (!is.null(varName)) {
      result$attrs$Variable$varName <- varName
    }
    ## Variables
    for (var_type in names(attributes(object)$Variables)) {
      if (!is.null(attributes(object)$Variables[[var_type]])) {
        for (var in names(attributes(object)$Variables[[var_type]])) {
          attr_variable <- attributes(object)$Variables[[var_type]][[var]]
          if (is.null(result$attrs$Dates)) {
            if (inherits(attr_variable, "POSIXct")) {
              result$attrs$Dates <- attr_variable
            }
          }
          if (is.null(result$attrs$Variable$metadata[[var]])) {
            result$attrs$Variable$metadata[[var]] <- attr_variable
          }
        }
      }
    }
    ## Datasets
    if (length(names(FileSelector)) > 1) {
      # lon name
      known_lon_names <- .KnownLonNames()
      lon_name_dat <- names(dims)[which(names(dims) %in% known_lon_names)]
      # lat name
      known_lat_names <- .KnownLatNames()
      lat_name_dat <- names(dims)[which(names(dims) %in% known_lat_names)]
      result$attrs$Datasets <- names(FileSelector)
      # TO DO: add dat_dim parameter
      if (any(names(dims) %in% c('dat', 'dataset'))) {
        dat_dim <- names(dims)[which(names(dims) %in% c('dat', 'dataset'))]
        result$coords[[dat_dim]] <- names(FileSelector)
        if (!remove_attrs_coords) attr(result$coords[[dat_dim]], 'indices') <- FALSE
      }
      for (i in 2:length(names(FileSelector))) {
        if (!is.null(lon_name_dat)) {
          if (any(result$coords[[lon_name_dat]] != as.vector(attributes(object)$Variables[[names(FileSelector)[i]]][[lon_name_dat]]))) {
            warning("'lon' values are different for different datasets. ", 
                    "Only values from the first will be used.")
          }
        }
        if (!is.null(lat_name_dat)) {
          if (any(result$coords[[lat_name_dat]] != as.vector(attributes(object)$Variables[[names(FileSelector)[i]]][[lat_name_dat]]))) {
            warning("'lat' values are different for different datasets. ", 
                    "Only values from the first will be used.")
          }
        }
      }
    } else {
      result$attrs$Datasets <- names(FileSelector)
    }
    ## when
    result$attrs$when <- Sys.time()
    ## source_files
    result$attrs$source_files <- attributes(object)$Files
    ## load_parameters
    result$attrs$load_parameters <- attributes(object)$FileSelectors
    class(result) <- 's2dv_cube'
  } else {
    stop("The class of parameter 'object' is not implemented",
         " to be converted into 's2dv_cube' class yet.")
  }
  return(result)
}