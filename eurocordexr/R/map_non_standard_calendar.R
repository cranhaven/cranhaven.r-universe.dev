#' Create map indices from non-standard calendars
#'
#' Interpolates non-standard calendars (360 and noleap) to the standard
#' Gregorian. Assumes daily data as input.
#'
#'
#' @param times Vector of class PCICt (will be truncated to days).
#'
#'
#' @return A \code{\link[data.table]{data.table}} with columns:
#'
#'   \itemize{ \item dates_full: sequence of standard dates from min to max date
#'   in input times as data.table::IDate \item dates_pcict_inter: which dates in
#'   PCICt from times correspond to the standard dates \item idx_pcict: the
#'   index associated to the input times to be used for mapping e.g. values }
#'
#' @seealso Can be used internally in \code{\link{rotpole_nc_point_to_dt}} and
#'   \code{\link{nc_grid_to_dt}} by setting the respective arguments.
#'
#' @export
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' # example data from EURO-CORDEX (cropped for size)
#' # non-standard calendar (360)
#' fn2 <- system.file("extdata", "test2.nc", package = "eurocordexr")
#' ncobj <- ncdf4::nc_open(fn2)
#'
#' # read as PCICt-class
#' times <- ncdf4.helpers::nc.get.time.series(ncobj, "tasmin")
#' str(times)
#'
#' dtx <- map_non_standard_calendar(times)
#' dtx[58:64]
#'
map_non_standard_calendar <- function(times){


  dates_pcict <- trunc(times, "day")

  dates_pcict %>%
    min %>%
    as.character %>%
    lubridate::ymd() -> date_min

  dates_pcict %>%
    max %>%
    as.character %>%
    lubridate::ymd() -> date_max

  # if last day of year is Dec 30, make it Dec 31
  if(lubridate::month(date_max) == 12 & lubridate::day(date_max) == 30){
    lubridate::`day<-`(date_max, 31)
  }

  dates_full <- seq(date_min, date_max, by = "day")

  seq_pcict <- seq(1, length(dates_pcict), length.out = length(dates_full))
  idx_pcict <- round(seq_pcict)

  dtx <- data.table(dates_full = dates_full,
                    dates_pcict_inter = as.character(dates_pcict[idx_pcict]),
                    idx_pcict = idx_pcict)

  return(dtx)

}

