#'NetCDF dimension reader for 'startR'
#'
#'A dimension reader function for NetCDF files, intended for use as parameter 
#''file_dim_reader' in a Start() call. It complies with the input/output 
#'interface required by Start() defined in the documentation for the parameter 
#''file_dim_reader' of that function.\cr\cr
#'This function uses the function NcReadDims() in the package 'easyNCDF'.
#'
#'@param file_path A character string indicating the path to the data file to 
#'  read. See details in the documentation of the parameter 'file_dim_reader' 
#'  of the function Start(). The default value is NULL.
#'@param file_object An open connection to a NetCDF file, optionally with 
#'  additional header information. See details in the documentation of the 
#'  parameter 'file_dim_reader' of the function Start(). The default value is 
#'  NULL.
#'@param file_selectors  A named list containing the information of the path of 
#'  the file to read data from. It is automatically provided by Start(). See 
#'  details in the documentation of the parameter 'file_dim_reader' of the 
#'  function Start(). The default value is NULL.
#'@param inner_indices A named list of numeric vectors indicating the indices 
#'  to take from each of the inner dimensions in the requested file. It is 
#'  automatically provided by Start(). See details in the documentation of the 
#'  parameter 'file_dim_reader' of the function Start(). The default value is 
#'  NULL.
#'@param synonims A named list indicating the synonims for the dimension names 
#'  to look for in the requested file, exactly as provided in the parameter 
#'  'synonims' in a Start() call. See details in the documentation of the 
#'  parameter 'file_dim_reader' of the function Start().
#'
#'@return A named numeric vector with the names and sizes of the dimensions of 
#'  the requested file.
#'@examples
#'  data_path <- system.file('extdata', package = 'startR')
#'  file_to_open <- file.path(data_path, 'obs/monthly_mean/tos/tos_200011.nc')
#'  file_selectors <- c(dat = 'dat1', var = 'tos', sdate = '200011')
#'  first_round_indices <- list(time = 1, latitude = 1:8, longitude = 1:16)
#'  synonims <- list(dat = 'dat', var = 'var', sdate = 'sdate', time = 'time',
#'                   latitude = 'latitude', longitude = 'longitude')
#'  dim_of_file <- NcDimReader(file_to_open, NULL, file_selectors,
#'                             first_round_indices, synonims)
#'@seealso \code{\link{NcOpener}} \code{\link{NcDataReader}} 
#'  \code{\link{NcCloser}} \code{\link{NcVarReader}}
#'@import easyNCDF
#'@importFrom stats setNames 
#'@export
NcDimReader <- function(file_path = NULL, file_object = NULL, 
                        file_selectors = NULL, inner_indices = NULL,
                        synonims) {
  close <- FALSE
  if (!is.null(file_object)) {
    file_to_read <- file_object
    file_path <- file_object$filename
  } else if (!is.null(file_path)) {
    file_to_read <- NcOpener(file_path)
    close <- TRUE
  } else {
    stop("Either 'file_path' or 'file_object' must be provided.")
  }

  vars_in_file <- easyNCDF::NcReadVarNames(file_to_read)
  if (any(c('var', 'variable') %in% names(inner_indices))) {
    vars_to_read <- inner_indices[[which(names(inner_indices) %in% c('var', 'variable'))[1]]]
    var_tag <- names(inner_indices)[[which(names(inner_indices) %in% c('var', 'variable'))[1]]]
  } else if (any(c('var', 'variable') %in% names(file_selectors))) {
    vars_to_read <- file_selectors[[which(names(file_selectors) %in% c('var', 'variable'))[1]]]
    var_tag <- names(file_selectors)[[which(names(file_selectors) %in% c('var', 'variable'))[1]]]
  } else if (length(vars_in_file) == 1) {
    vars_to_read <- vars_in_file
    file_selectors <- c(file_selectors, list(var = vars_in_file))
    var_tag <- 'var'
  } else {
    stop("NcDimReader expected to find a requested 'var' or 'variable' in 'file_selectors'.")
  }

  if ((length(vars_to_read) == 1) && (vars_to_read[1] == 'var_names')) {
    result <- setNames(length(vars_in_file), var_tag)
  } else {
    vars_to_read <- sapply(vars_to_read, 
      function(x) {
        if (x %in% names(synonims)) {
          x_in_file <- which(synonims[[x]] %in% vars_in_file)
          if (length(x_in_file) < 1) {
            stop("Could not find variable '", x, "' (or its synonims if ",
                 "specified) in the file ", file_path)
          }
          if (length(x_in_file) > 1) {
            stop("Found more than one matches for the synonims of the ",
                 "variable '", x, "' in the file ", file_path)
          }
          synonims[[x]][x_in_file]
        } else {
          if (is.character(x) && !(x %in% c('all', 'last', 'first'))) {
            if (!(x %in% vars_in_file)) {
              stop("Could not find variable '", x, "' (or its synonims if ",
                   "specified) in the file ", file_path)
            }
          }
          x
        }
      })
    vars_to_read <- SelectorChecker(vars_to_read, vars_in_file, 
                                    return_indices = FALSE)
    read_dims <- easyNCDF::NcReadDims(file_to_read, vars_to_read)
    if (any(c('var', 'variable') %in% names(inner_indices))) {
      names(read_dims)[which(names(read_dims) == 'var')] <- var_tag
      read_dims[var_tag] <- length(vars_in_file)
    } else {
      read_dims <- read_dims[-which(names(read_dims) == 'var')]
    }
    result <- read_dims
  }

  if (close) {
    NcCloser(file_to_read)
  }

  result
}
