#' Convert a netcdf array to long format as data.table (raw version)
#'
#' Extracts a variable from netcdf, similar to \code{\link{nc_grid_to_dt}}, but
#' in a raw version. Requires more manual info, does not convert time units
#' (like 'days since x'), and requires xy variables not dimensions (like a
#' curvilinear grid).
#'
#' Names of netcdf variables can be inquired in a terminal with "ncdump -h", or
#' in R for example using \code{ncdf4::nc_open(filename)}.
#'
#' Example use cases: netcdf file has issues with dimensions (no dimvar), time
#' variable is in non-standard format.
#'
#'
#'
#' @param filename Complete path to .nc file.
#' @param variable Character, name of the variable to extract (required).
#' @param var_t Character, name of the time variable. (recommended if
#'   has_time=TRUE)
#' @param var_x (optional) Character, name of the x coordinate variable (not
#'   dimension!). If both var_x and var_y are supplied, results will have xy
#'   columns, too.
#' @param var_y (optional) Character, name of the y coordinate variable (not
#'   dimension!). If both var_x and var_y are supplied, results will have xy
#'   columns, too.
#' @param has_time Boolean, if \code{TRUE} (default), will read time information
#'   from file. Use \code{FALSE}, if file has no time dimension/variable.
#' @param icell_raster_pkg Boolean, if \code{TRUE}, cell indices will be ordered
#'   as if you were extracting the data with the raster package. See also
#'   \code{\link{nc_grid_to_dt}}
#'
#' @return A \code{\link[data.table]{data.table}} with columns icell (cell
#'   index), time (if has_time=T), value. Only if var_x and var_y supplied also
#'   x and y. Column names except icell are variable names as in the netcdf
#'   file.
#'
#' @seealso \code{\link{nc_grid_to_dt}}
#'
#' @export
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' # example data
#' fn3 <- system.file("extdata", "test3.nc", package = "eurocordexr")
#' dat <- nc_grid_to_dt_raw(fn3, "tasmax", "Times")
#' str(dat)
nc_grid_to_dt_raw <- function(filename,
                              variable,
                              var_t,
                              var_x,
                              var_y,
                              has_time = TRUE,
                              icell_raster_pkg = TRUE){

  # NSE in R CMD check
  icell <- x <- y <- NULL

  ncobj <- RNetCDF::open.nc(filename)

  if(missing(variable)){
    stop("variable must be supplied")
  }

  arr_var <- RNetCDF::var.get.nc(ncobj, variable)
  nx <- dim(arr_var)[1]
  ny <- dim(arr_var)[2]

  if(has_time){

    if(missing(var_t)){
      dimnames <- ncdf4.helpers::nc.get.dim.names(ncdf4::nc_open(filename), variable)
      var_t <- dimnames[3]
      message("var_t not supplied (not recommended), guessing time variable name: ", var_t)
    }

    dim_t <- RNetCDF::var.get.nc(ncobj, var_t)
    ndates <- length(dim_t)

    dat <- data.table(icell =  rep(1:(nx*ny), ndates),
                      date = rep(dim_t, each = nx * ny),
                      value = as.vector(arr_var))
    setnames(dat, "date", var_t)

  } else {
    dat <- data.table(icell =  1:(nx*ny),
                      value = as.vector(arr_var))
    ndates <- 1
  }

  if(icell_raster_pkg){
    # raster package orders cells differently (y inverse)
    l_icell_split_y <- split(1:(nx*ny), rep(1:ny, each = nx))
    dat[, icell := rep(unlist(rev(l_icell_split_y)), ndates)]
  }


  if(!missing(var_x) & !missing(var_y)){
    dim_x <- RNetCDF::var.get.nc(ncobj, var_x)
    dim_y <- RNetCDF::var.get.nc(ncobj, var_y)

    dat[, x := rep(as.vector(dim_x), ndates)]
    dat[, y := rep(as.vector(dim_y), ndates)]
    setnames(dat, c("x", "y"), c(var_x, var_y))
  }

  setnames(dat, "value", variable)

  RNetCDF::close.nc(ncobj)
  return(dat)
}



