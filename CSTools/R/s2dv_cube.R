#'Creation of a 's2dv_cube' object
#'
#'@description This function allows to create an 's2dv_cube' object by passing 
#'information through its parameters. This function will be needed if the data 
#'hasn't been loaded using CST_Start or has been transformed with other methods. 
#'An 's2dv_cube' object has many different components including metadata. This 
#'function will allow to create 's2dv_cube' objects even if not all elements 
#'are defined and for each expected missed parameter a warning message will be 
#'returned.
#'
#'@author Perez-Zanon Nuria, \email{nuria.perez@bsc.es}
#'
#'@param data A multidimensional array with named dimensions, typically with 
#'  dimensions: dataset, member, sdate, time, lat and lon.
#'@param coords A list of named vectors with the coordinates corresponding to 
#'  the dimensions of the data parameter. If any coordinate has dimensions, they 
#'  will be set as NULL. If any coordinate is not provided, it is set as an 
#'  index vector with the values from 1 to the length of the corresponding 
#'  dimension. The attribute 'indices' indicates wether the coordinate is an 
#'  index vector (TRUE) or not (FALSE).
#'@param varName A character string indicating the abbreviation of the variable 
#'  name.
#'@param metadata A named list where each element is a variable containing the
#'  corresponding information. The information can be contained in a list of 
#'  lists for each variable.
#'@param Datasets Character strings indicating the names of the dataset. It 
#'  there are multiple datasets it can be a vector of its names or a list of
#'  lists with additional information.
#'@param Dates A POSIXct array of time dimensions containing the Dates.
#'@param when A time stamp of the date when the data has been loaded. This 
#'  parameter is also found in Load() and Start() functions output.
#'@param source_files A vector of character strings with complete paths to all 
#'  the found files involved in loading the data.
#'@param \dots Additional elements to be added in the object. They will be 
#'  stored in the end of 'attrs' element. Multiple elements are accepted.
#'
#'@return The function returns an object of class 's2dv_cube' with the following 
#' elements in the structure:\cr
#'\itemize{
#'  \item{'data', array with named dimensions;}
#'  \item{'dims', named vector of the data dimensions;}
#'  \item{'coords', list of named vectors with the coordinates corresponding to 
#'  the dimensions of the data parameter;}
#'  \item{'attrs', named list with elements:
#'    \itemize{
#'      \item{'Dates', array with named temporal dimensions of class 'POSIXct' from 
#'      time values in the data;}
#'      \item{'Variable', has the following components:
#'        \itemize{
#'          \item{'varName', with the short name of the loaded variable as specified 
#'          in the parameter 'var';}
#'          \item{'metadata', named list of elements with variable metadata. 
#'                They can be from coordinates variables (e.g. longitude) or 
#'                main variables (e.g. 'var');}
#'        }
#'      }
#'      \item{'Datasets', character strings indicating the names of the dataset;}
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
#'@seealso \code{\link[s2dv]{Load}} and \code{\link{CST_Start}}
#'@examples
#'exp_original <- 1:100
#'dim(exp_original) <- c(lat = 2, time = 10, lon = 5)
#'exp1 <- s2dv_cube(data = exp_original)
#'class(exp1)
#'coords <- list(lon = seq(-10, 10, 5), lat = c(45, 50))
#'exp2 <- s2dv_cube(data = exp_original, coords = coords) 
#'class(exp2)
#'metadata <- list(tas = list(level = '2m'))
#'exp3 <- s2dv_cube(data = exp_original, coords = coords,
#'                  varName = 'tas', metadata = metadata)
#'class(exp3)
#'Dates = as.POSIXct(paste0(rep("01", 10), rep("01", 10), 1990:1999), format = "%d%m%Y")
#'dim(Dates) <- c(time = 10)
#'exp4 <- s2dv_cube(data = exp_original, coords = coords,
#'                  varName = 'tas', metadata = metadata,
#'                  Dates = Dates)  
#'class(exp4)
#'exp5 <- s2dv_cube(data = exp_original, coords = coords,
#'                  varName = 'tas', metadata = metadata,
#'                  Dates = Dates, when = "2019-10-23 19:15:29 CET")  
#'class(exp5)
#'exp6 <- s2dv_cube(data = exp_original, coords = coords,
#'                  varName = 'tas', metadata = metadata,
#'                  Dates = Dates,
#'                  when = "2019-10-23 19:15:29 CET", 
#'                  source_files = c("/path/to/file1.nc", "/path/to/file2.nc"))
#'class(exp6)
#'exp7 <- s2dv_cube(data = exp_original, coords = coords,
#'                  varName = 'tas', metadata = metadata,
#'                  Dates = Dates,
#'                  when = "2019-10-23 19:15:29 CET", 
#'                  source_files = c("/path/to/file1.nc", "/path/to/file2.nc"),
#'                  Datasets = list(
#'                    exp1 = list(InitializationsDates = list(Member_1 = "01011990", 
#'                                                            Members = "Member_1"))))  
#'class(exp7)
#'dim(exp_original) <- c(dataset = 1, member = 1, time = 10, lat = 2, lon = 5)
#'exp8 <- s2dv_cube(data = exp_original, coords = coords,
#'                  varName = 'tas', metadata = metadata,
#'                  Dates = Dates, original_dates = Dates)  
#'class(exp8)
#'@export
s2dv_cube <- function(data, coords = NULL, varName = NULL, metadata = NULL, 
                      Datasets = NULL, Dates = NULL, when = NULL, 
                      source_files = NULL, ...) {

  # data
  if (is.null(data) | !is.array(data) | is.null(names(dim(data)))) {
    stop("Parameter 'data' must be an array with named dimensions.")
  }
  # dims
  dims <- dim(data)

  ## coords
  if (!is.null(coords)) {
    if (!all(names(coords) %in% names(dims))) {
      coords <- coords[-which(!names(coords) %in% names(dims))]
    }
    for (i_coord in names(dims)) {
      if (i_coord %in% names(coords)) {
        if (length(coords[[i_coord]]) != dims[i_coord]) {
          warning(paste0("Coordinate '", i_coord, "' has different lenght as ",
                         "its dimension and it will not be used."))
          coords[[i_coord]] <- 1:dims[i_coord]
          attr(coords[[i_coord]], 'indices') <- TRUE
        } else {
          attr(coords[[i_coord]], 'indices') <- FALSE
        }
      } else {
        warning(paste0("Coordinate '", i_coord, "' is not provided ",
                       "and it will be set as index in element coords."))
        coords[[i_coord]] <- 1:dims[i_coord]
        attr(coords[[i_coord]], 'indices') <- TRUE
      }
    }
    dim(coords[[i_coord]]) <- NULL
  } else {
    coords <- sapply(names(dims), function(x) 1:dims[x])
    for (i in 1:length(coords)) {
      attr(coords[[i]], "indices") <- TRUE
    }
  }

  ## attrs
  attrs <- list()
  # Dates
  if (is.null(Dates)) {
    warning("Parameter 'Dates' is not provided so the metadata ",
            "of 's2dv_cube' object will be incomplete.")
    attrs$Dates <- NULL
  } else if (length(Dates) == 1 & inherits(Dates[1], "POSIXct")) {
    attrs$Dates <- Dates
  } else {
    if (!is.array(Dates)) {
      warning("Parameter 'Dates' must be an array with named time dimensions.")
    } else {
      if (is.null(names(dim(Dates)))) {
        warning("Parameter 'Dates' must have dimension names.")
      } else if (!all(names(dim(Dates)) %in% names(dims))) {
        warning("Parameter 'Dates' must have the corresponding time dimension names in 'data'.")
      } else {
        if (inherits(Dates[1], "POSIXct")) {
          attrs$Dates <- Dates
        } else {
          warning("Parameter 'Dates' must be of class 'POSIXct'.")
        }
      }
    }
  }
  # Variable
  if (is.null(varName)) {
    warning("Parameter 'varName' is not provided so the metadata ",
            "of 's2dv_cube' object will be incomplete.")
    attrs$Variable$varName <- NULL
  } else {
    if (!is.character(varName)) {
      warning("Parameter 'varName' must be a character.")
    } else {
      attrs$Variable$varName <- varName
    }
  }
  if (is.null(metadata)) {
    warning("Parameter 'metadata' is not provided so the metadata ",
            "of 's2dv_cube' object will be incomplete.")
    attrs$Variable$metadata <- NULL
  } else {
    if (!is.list(metadata)) {
       metadata <- list(metadata)
    }
    attrs$Variable$metadata <- metadata
  }
  # Datasets
  if (!is.null(Datasets)) {
    attrs$Datasets <- Datasets
  }
  # when
  if (!is.null(when)) {
    attrs$when <- when
  }
  # source_files
  if (!is.null(source_files)) {
    attrs$source_files <- source_files
  }
  # dots
  dots <- list(...)
  if (length(dots) != 0) {
    for (i_arg in 1:length(dots)) {
      attrs[[names(dots)[[i_arg]]]] <- dots[[i_arg]]
    }
  }

  ## object
  object <- list(data = data, dims = dims, coords = coords, attrs = attrs)
  class(object) <- 's2dv_cube'
  return(object)
}
