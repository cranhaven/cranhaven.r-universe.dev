#'NetCDF file closer for 'startR'
#'
#'This is a file closer function for NetCDF files, intended for use as 
#'parameter 'file_closer' in a Start() call. This function complies with the 
#'input/output interface required by Start() defined in the documentation for 
#'the parameter 'file_closer'.\cr\cr
#'This function uses the function NcClose() in the package 'easyNCDF',
#'which in turn uses nc_close() in the package 'ncdf4'.
#'
#'@param file_object An open connection to a NetCDF file, optionally with 
#'  additional header information. See details in the documentation of the 
#'  parameter 'file_closer' of the function Start().
#'@return This function returns NULL.
#'@examples
#'data_path <- system.file('extdata', package = 'startR')
#'path_obs <- file.path(data_path, 'obs/monthly_mean/tos/tos_200011.nc') 
#'connection <- NcOpener(path_obs)
#'NcCloser(connection)
#'@seealso \code{\link{NcOpener}} \code{\link{NcDataReader}} 
#'  \code{\link{NcDimReader}} \code{\link{NcVarReader}}
#'@import easyNCDF
#'@export
NcCloser <- function(file_object) {
  easyNCDF::NcClose(file_object)
}
