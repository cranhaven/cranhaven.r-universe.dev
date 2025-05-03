#'NetCDF file opener for 'startR'
#'
#'This is a file opener function for NetCDF files, intended for use as parameter 
#''file_opener' in a Start() call. This function complies with the input/output 
#'interface required by Start() defined in the documentation for the parameter 
#''file_opener'.\cr\cr
#'This function uses the function NcOpen() in the package 'easyNCDF', which in 
#'turn uses nc_open() in the package 'ncdf4'.
#'
#'@param file_path A character string indicating the path to the data file to 
#'  read. See details in the documentation of the parameter 'file_opener' of the 
#'  function Start().
#'@return An open connection to a NetCDF file with additional header 
#'  information as returned by nc_open() in the package 'ncdf4'. See details in 
#'  the documentation of the parameter 'file_opener' of the function Start().
#'@examples
#'data_path <- system.file('extdata', package = 'startR')
#'path_obs <- file.path(data_path, 'obs/monthly_mean/tos/tos_200011.nc')
#'connection <- NcOpener(path_obs)
#'NcCloser(connection)
#'@seealso \code{\link{NcDimReader}} \code{\link{NcDataReader}} 
#'  \code{\link{NcCloser}} \code{\link{NcVarReader}}
#'@import easyNCDF
#'@export
NcOpener <- function(file_path) {
  easyNCDF::NcOpen(file_path)
}
