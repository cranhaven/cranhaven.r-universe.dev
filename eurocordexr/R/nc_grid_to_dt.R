#' Convert a netcdf array to long format as data.table
#'
#' Extracts a variable from netcdf, and returns a
#' \code{\link[data.table]{data.table}} with cell index, date, values, and
#' optionally: coordinates.
#'
#' Coordinates are usually not put in the result, because it saves space. It is
#' recommended to merge them after the final operations. The unique cell index
#' is more efficient. However, if you plan to merge to data extracted with the
#' raster package (assuming the same grid), then cell indices might differ. Set
#' \code{icell_raster_pkg} to \code{TRUE}, to have the same cell indices. Note
#' that raster and ncdf4 have different concepts of coordinates (cell corner vs.
#' cell center), so merging based on coordinates can produce arbitrary results
#' (besides rounding issues).
#'
#'
#' @param filename Complete path to .nc file.
#' @param variable Name of the variable to extract from \code{filename}
#'   (character).
#' @param icell_raster_pkg Boolean, if \code{TRUE}, cell indices will be ordered
#'   as if you were extracting the data with the raster package.
#' @param add_xy Boolean, if \code{TRUE}, adds columns with x and y coordinates.
#' @param interpolate_to_standard_calendar Boolean, if \code{TRUE} will use
#'   \code{\link{map_non_standard_calendar}} to interpolate values to a standard
#'   calendar.
#' @param date_range (optional) two-element vector of class Date (min, max),
#'   which will be used to extract only parts of the netcdf file
#' @param verbose Boolean, if \code{TRUE}, prints more information.
#'
#' @return A \code{\link[data.table]{data.table}} with columns: \itemize{\item
#'   icell: Cell index \item date: Date of class \code{\link{Date}}, if file has
#'   a standard calendar. Date as character, if it has a non-standard calendar
#'   (360, noleap) and if \code{interpolate_to_standard_calendar} is set to
#'   \code{FALSE}. If \code{interpolate_to_standard_calendar} is \code{TRUE},
#'   it's always of class \code{\link{Date}}. \item variable: Values, column is
#'   renamed to input \code{variable} \item (optional) x,y: Coordinates of
#'   netcdf dimensions, will be renamed to dimension names found in array named
#'   after input \code{variable}}
#'
#'
#' @section Warning: Netcdf files can be huge, so loading everything in memory
#'   can rapidly crash your R session. Think first about subsetting or
#'   aggregating (e.g. using CDO:
#'   \url{https://code.mpimet.mpg.de/projects/cdo/}).
#'
#' @seealso The raster and terra packages can also open netcdf files and create
#'   data.frames with \code{raster::as.data.frame}
#'   or \code{terra::as.data.frame}. But, it does
#'   not handle non-standard calendars, and returns a data.frame, which is
#'   slower than data.table.
#'
#' @export
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' # example data from EURO-CORDEX (cropped for size)
#' fn1 <- system.file("extdata", "test1.nc", package = "eurocordexr")
#' dat <- nc_grid_to_dt(fn1)
#' str(dat)
nc_grid_to_dt <- function(filename,
                          variable,
                          icell_raster_pkg = TRUE,
                          add_xy = FALSE,
                          interpolate_to_standard_calendar = FALSE,
                          date_range,
                          verbose = FALSE){

  # NSE in R CMD check
  icell <- x <- y <- value <- NULL

  ncobj <- ncdf4::nc_open(filename,
                          readunlim = FALSE)

  if(verbose) cat("Succesfully opened file:", filename, "\n")

  if(missing(variable)){
    variable <- get_varnames(filename)[1]
    if(verbose) cat("No variable supplied. Took first one:", variable, "\n")
  }

  dimnames <- ncdf4.helpers::nc.get.dim.names(ncobj, variable)

  dim_x <- ncdf4::ncvar_get(ncobj, dimnames[1])
  dim_y <- ncdf4::ncvar_get(ncobj, dimnames[2])


  times <- ncdf4.helpers::nc.get.time.series(ncobj, variable)

  if(all(is.na(times))){

    if(verbose) cat("No time information found in nc file.\n")
    dates <- NA

  } else if(! attr(times, "cal") %in% c("gregorian", "proleptic_gregorian")){

    if(verbose) cat("Non-standard calendar found:", attr(times, "cal"), "\n")

    dates <- as.character(trunc(times, "day"))

  } else if(startsWith(ncobj$dim$time$units, "months since")){
    # ncdf4.helpers workaround for "months since" time information
    origin <- lubridate::as_date(sub("months since ", "", ncobj$dim$time$units))
    dates <- origin + months(floor(ncdf4::ncvar_get(ncobj, "time")))
    times <- dates

  } else {

    times %>%
      PCICt::as.POSIXct.PCICt() %>%
      as.Date -> dates
  }





  nx <- length(dim_x)
  ny <- length(dim_y)

  if(missing(date_range)){
    ndates <- length(dates)
    arr_var <- ncdf4::ncvar_get(ncobj, variable)

  } else {
    stopifnot(date_range[1] <= date_range[2])
    # workaround for 360 calendar
    if(inherits(dates, "character")) date_range <- as.character(date_range)

    i_date_start <- min(which(dates >= date_range[1]))
    i_date_end <- max(which(dates <= date_range[2]))
    ndates <- i_date_end - i_date_start + 1
    arr_var <- ncdf4::ncvar_get(ncobj, variable, start = c(1,1,i_date_start), count = c(-1,-1, ndates))

    dates <- dates[i_date_start : i_date_end]
    times <- times[i_date_start : i_date_end]
  }

  dat <- data.table(icell =  rep(1:(nx*ny), ndates),
                    date = rep(dates, each = nx * ny),
                    value = as.vector(arr_var))


  if(icell_raster_pkg){
    # raster package orders cells differently (y inverse)
    l_icell_split_y <- split(1:(nx*ny), rep(1:ny, each = nx))
    dat[, icell := rep(unlist(rev(l_icell_split_y)), ndates)]
  }


  if(add_xy){
    dat[, x := rep(dim_x, ny * ndates)]
    dat[, y :=  rep(dim_y, each = nx) %>% rep(ndates)]
    setnames(dat, c("x", "y"), dimnames[1:2])
  }

  if(interpolate_to_standard_calendar &&
     !all(is.na(times)) &&
     ! attr(times, "cal") %in% c("gregorian", "proleptic_gregorian")){

    if(verbose) cat("Interpolating to standard calendar.\n")

    bycols <- "icell"
    if(add_xy) bycols <- c(bycols, dimnames[1:2])

    dtx <- map_non_standard_calendar(times)

    dat <- dat[,
                list(date = dtx$dates_full, value = value[dtx$idx_pcict]),
                by = bycols]

  }


  setnames(dat, "value", variable)

  ncdf4::nc_close(ncobj)
  return(dat)
}



