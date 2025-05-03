#' Extract time series of a single grid cell of a rot-pole daily netcdf to
#' data.table
#'
#' Creates a \code{\link[data.table]{data.table}} from a rotated pole netcdf (as
#' usually found in RCMs), which includes values and date. Useful for extracting
#' e.g. the series for a station. Requires that dimension variables in netcdf
#' file contain rlon and rlat, and that it contains daily data.
#'
#' Calculates the euclidean distance, and takes the grid cell with minimal
#' distance to \code{point_lon} and \code{point_lat}. Requires that the .nc file
#' contains variables lon[rlon, rlat] and lat[rlon, rlat].
#'
#' @param filename Complete path to .nc file.
#' @param variable Name of the variable to extract from \code{filename}
#'   (character).
#' @param point_lon Numeric longitude of the point to extract (decimal degrees).
#' @param point_lat Numeric latitude of the point to extract (decimal degrees).
#' @param interpolate_to_standard_calendar Boolean, if \code{TRUE} will use
#'   \code{\link{map_non_standard_calendar}} to interpolate values to a standard
#'   calendar.
#' @param verbose Boolean, if \code{TRUE}, will print more information.
#' @param add_grid_coord Boolean, if \code{TRUE}, will add columns to the result
#'   which give the longitude and latitude of the underlying grid.
#'
#' @return A \code{\link[data.table]{data.table}} with two columns: the dates in
#'   date, and the values in a variable named after input \code{variable}. The
#'   date column is of class \code{\link{Date}}, unless the .nc
#'   file has a non-standard calendar (360, noleap) and
#'   \code{interpolate_to_standard_calendar} is set to \code{FALSE}, in which it
#'   will be character. If \code{add_grid_coord} is set to \code{TRUE}, then
#'   two more columns named grid_lon and grid_lat.
#'
#' @export
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' # example data from EURO-CORDEX (cropped for size)
#'
#' # standard calendar
#' fn1 <- system.file("extdata", "test1.nc", package = "eurocordexr")
#' dt1 <- rotpole_nc_point_to_dt(
#'   filename = fn1,
#'   variable = "tasmin",
#'   point_lon = 11.31,
#'   point_lat = 46.5,
#'   verbose = TRUE
#' )
#'
#' # non-standard calendar (360)
#' fn2 <- system.file("extdata", "test2.nc", package = "eurocordexr")
#'
#' # read as is
#' dt2 <- rotpole_nc_point_to_dt(fn2, "tasmin", 11.31, 46.5)
#' str(dt2) # chr date
#' dt2[86:94, ] # e.g. 30th of February in 360 calendar
#'
#' # interpolate to standard
#' dt3 <- rotpole_nc_point_to_dt(fn2, "tasmin", 11.31, 46.5,
#'                               interpolate_to_standard_calendar = TRUE)
#' str(dt3) # class Date
#' dt3[86:94, ] # standard calender
#'
rotpole_nc_point_to_dt <- function(filename,
                                   variable,
                                   point_lon,
                                   point_lat,
                                   interpolate_to_standard_calendar = FALSE,
                                   verbose = FALSE,
                                   add_grid_coord = FALSE){

  ncobj <- ncdf4::nc_open(filename,
                          readunlim = FALSE)

  if(verbose) cat("Succesfully opened file:", filename, "\n")

  grid_lon <- ncdf4::ncvar_get(ncobj, "lon")
  grid_lat <- ncdf4::ncvar_get(ncobj, "lat")

  grid_squared_dist <- (grid_lat - point_lat)^2 + (grid_lon - point_lon)^2

  # indices of [lon, lat]
  cell_xy <- arrayInd(which.min(grid_squared_dist),
                      dim(grid_squared_dist))

  if(verbose){
    # actual cell coordinates
    cat("Point longitude = ", point_lon, " ## Closest grid cell = ", grid_lon[cell_xy], "\n")
    cat("Point latitude = ", point_lat, " ## Closest grid cell = ", grid_lat[cell_xy], "\n")

    # euclidean distance of stn to grid cell midpoint [degrees]
    cat("Euclidean distance in degrees = ", sqrt(grid_squared_dist[cell_xy]), "\n")
  }

  if(missing(variable)){
    variable <- get_varnames(filename)[1]
    if(verbose) cat("No variable supplied. Took first one:", variable, "\n")
  }

  values <- as.vector(
    ncdf4.helpers::nc.get.var.subset.by.axes(
      ncobj,
      variable,
      list(X = cell_xy[1], Y = cell_xy[2])
    )
  )



  times <- ncdf4.helpers::nc.get.time.series(ncobj, variable)

  if(all(is.na(times))){

    if(verbose) cat("No time information found in nc file.\n")
    dates <- NA

  } else if(! attr(times, "cal") %in% c("gregorian", "proleptic_gregorian")){

    # modification for non-standard calendars
    if(verbose) cat("Non-standard calendar found:", attr(times, "cal"), "\n")

    if(interpolate_to_standard_calendar){

      if(verbose) cat("Interpolating to standard calendar.\n")

      dtx <- map_non_standard_calendar(times)

      dates <- dtx$dates_full
      values <- values[dtx$idx_pcict]


    } else {

      dates <- as.character(trunc(times, "day"))

    }

  }  else if(startsWith(ncobj$dim$time$units, "months since")){
    # ncdf4.helpers workaround for "months since" time information
    origin <- lubridate::as_date(sub("months since ", "", ncobj$dim$time$units))
    dates <- origin + months(floor(ncdf4::ncvar_get(ncobj, "time")))
    times <- dates

  } else {
    # standard calendar: extract time dimension in IDate format (assuming daily data)
    times %>%
      PCICt::as.POSIXct.PCICt() %>%
      as.Date -> dates
  }







  ncdf4::nc_close(ncobj)

  dat <- data.table(date = dates,
                    value = values)
  setnames(dat, "value", variable)

  if(add_grid_coord){
    dat[, ":="(grid_lon = grid_lon[cell_xy],
               grid_lat = grid_lat[cell_xy])]
  }


  return(dat)
}



