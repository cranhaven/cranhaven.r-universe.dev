#'NetCDF variable reader for 'startR'
#'
#'This is an auxiliary variable reader function for NetCDF files, intended for 
#'use as parameter 'file_var_reader' in a Start() call. It complies with the 
#'input/output interface required by Start() defined in the documentation for 
#'the parameter 'file_var_reader' of that function.\cr\cr
#'This function uses the function NcDataReader() in the package 'startR', 
#'which in turn uses NcToArray() in the package 'easyNCDF', which in turn uses 
#'nc_var_get() in the package 'ncdf4'.
#'
#'@param file_path A character string indicating the path to the data file to 
#'  read the variable from. See details in the documentation of the parameter 
#'  'file_var_reader' of the function Start(). The default value is NULL.
#'@param file_object An open connection to a NetCDF file, optionally with 
#'  additional header information. See details in the documentation of the 
#'  parameter 'file_var_reader' of the function Start(). The default value is 
#'  NULL.
#'@param file_selectors A named list containing the information of the path of 
#'  the file to read data from. It is automatically provided by Start(). See 
#'  details in the documentation of the parameter 'file_var_reader' of the 
#'  function Start(). The default value is NULL.
#'@param var_name A character string with the name of the variable to be read.
#'  The default value is NULL.
#'@param synonims A named list indicating the synonims for the dimension names 
#'  to look for in the requested file, exactly as provided in the parameter 
#'  'synonims' in a Start() call. See details in the documentation of the 
#'  parameter 'file_var_reader' of the function Start().
#'
#'@return A multidimensional data array with the named dimensions, potentially 
#'  with the attribute 'variables' with additional auxiliary data. See details 
#'  in the documentation of the parameter 'file_var_reader' of the function 
#'  Start().
#'@examples
#'  data_path <- system.file('extdata', package = 'startR')
#'  file_to_open <- file.path(data_path, 'obs/monthly_mean/tos/tos_200011.nc')
#'  file_selectors <- c(dat = 'dat1', var = 'tos', sdate = '200011')
#'  synonims <- list(dat = 'dat', var = 'var', sdate = 'sdate', time = 'time',
#'                   latitude = 'latitude', longitude = 'longitude')
#'  var <- NcVarReader(file_to_open, NULL, file_selectors,
#'                      'tos', synonims)
#'@seealso \code{\link{NcOpener}} \code{\link{NcDataReader}} 
#'  \code{\link{NcCloser}} \code{\link{NcDimReader}}
#'@export
NcVarReader <- function(file_path = NULL, file_object = NULL, 
                        file_selectors = NULL, var_name = NULL,
                        synonims) {
  if (!is.null(file_object)) {
    file_to_read <- file_object
  } else if (!is.null(file_path)) {
    file_to_read <- file_path
  } else {
    stop("Either 'file_path' or 'file_object' must be provided.")
  }
  if (var_name %in% c('var_names')) {
    vars_in_file <- easyNCDF::NcReadVarNames(file_to_read)
    vars_in_file <- sapply(vars_in_file,
      function(x) {
        which_entry <- which(sapply(synonims, function(y) x %in% y))
        if (length(which_entry) > 0) {
          names(synonims)[which_entry]
        } else {
          x
        }
      })
    dim(vars_in_file) <- c(var_names = length(vars_in_file))
    vars_in_file
  } else {
    NcDataReader(file_path, file_object, list(var = var_name), NULL, synonims)
  }
}
