#' Get variable names from netcdf file
#'
#' Wrapper around \code{\link[ncdf4.helpers:nc.get.variable.list]{ncdf4.helpers::nc.get.variable.list}}.
#'
#' @param filename .nc file
#'
#' @return vector of variable names
#'
#' @export
#'
#' @examples
#' # example data from EURO-CORDEX (cropped for size)
#' fn1 <- system.file("extdata", "test1.nc", package = "eurocordexr")
#' get_varnames(fn1)
#'
get_varnames <- function(filename){

  ncobj <- ncdf4::nc_open(filename)
  vars <- ncdf4.helpers::nc.get.variable.list(ncobj)
  ncdf4::nc_close(ncobj)
  vars

}
