

#' @title Concatenate Date and Time
#' 
#' @description 
#' Concatenate date and time information from two objects.
#' 
#' @param date_ an R object containing \link[base]{Date} information
#' 
#' @param time_ an R object containing time (\link[base]{POSIXt}) information 
#' 
#' @details 
#' Function [date_time_] is useful as clinicians may put date and time in different columns.
#' 
#' @returns 
#' Function [date_time_] returns a \link[base]{POSIXct} object.
#' 
#' @examples
#' (today = Sys.Date())
#' (y = ISOdatetime(year = c(1899, 2010), month = c(12, 3), day = c(31, 22), 
#'   hour = c(15, 3), min = 2, sec = 1, tz = 'UTC'))
#' date_time_(today, y)
#' @export
date_time_ <- function(date_, time_) {
  
  # grab date information from `date_`
  if (inherits(date_, what = 'POSIXct')) date_ <- as.POSIXlt.POSIXct(date_)
  if (inherits(date_, what = 'Date')) date_ <- as.POSIXlt.Date(date_)
  if (!inherits(date_, what = 'POSIXlt')) stop('`date_` must be convertible to `POSIXlt`')
  
  # grab time information from `time_`
  if (inherits(time_, what = 'Date')) stop('`time_` cannot be `Date` object')
  if (inherits(time_, what = 'POSIXct')) time_ <- as.POSIXlt.POSIXct(time_)
  if (!inherits(time_, what = 'POSIXlt')) stop('`time_` must be convertible to `POSIXlt`')
  tz <- unique.default(unclass(time_)$zone)
  # attr(time_, which = 'tzone', exact = TRUE) # wrong.  May have length > 1
  
  date. <- unclass(date_) # # `date.$*` is much faster than `date_$*`
  time. <- unclass(time_)
  
  ISOdatetime(
    year = date.$year + 1900, month = date.$mon + 1, day = date.$mday,
    hour = time.$hour, min = time.$min, sec = time.$sec, tz = tz)
}




#' @title Concatenate a \link[base]{Date} and a \link[base]{difftime} Object
#' 
#' @description ..
#' 
#' @param date_ an R object containing \link[base]{Date} information
#' 
#' @param difftime_ a \link[base]{difftime} object
#' 
#' @param tz \link[base]{character} scalar, time zone, 
#' see \link[base]{as.POSIXlt.Date} and \link[base]{ISOdatetime}
#' 
#' @param tol \link[base]{numeric} scalar, tolerance in finding second.  
#' Default `sqrt(.Machine$double.eps)` as in \link[base]{all.equal.numeric}
#' 
#' @returns 
#' Function [date_difftime_] returns a \link[base]{POSIXct} object.
#' 
#' @note
#' For now, I do not know how to force function `readxl::read_excel` to read a column 
#' as \link[base]{POSIXt}.  
#' By default, such column will be read as \link[base]{difftime}.
#' 
#' See `lubridate:::date.default` for the handling of year and month!
#' 
#' @examples
#' (x = as.Date(c('2022-09-10', '2023-01-01', NA, '2022-12-31')))
#' y = as.difftime(c(47580.3, NA, 48060, 30660), units = 'secs')
#' units(y) = 'hours'
#' y
#' date_difftime_(x, y)
#' @export
date_difftime_ <- function(date_, difftime_, tz = 'UTC', tol = sqrt(.Machine$double.eps)) {
  
  # grab date information from `date_`
  if (inherits(date_, what = 'POSIXct')) date_ <- as.POSIXlt.POSIXct(date_)
  if (inherits(date_, what = 'Date')) date_ <- as.POSIXlt.Date(date_, tz = tz)
  if (!inherits(date_, what = 'POSIXlt')) stop('`date_` must be convertible to `POSIXlt`')
  
  if (!inherits(difftime_, what = 'difftime')) stop('`difftime_` needs to be `difftime`')
  # ?base::`units<-.difftime` may cause floating error. Avoid.
  units(difftime_) <- 'secs' # only use this once
  
  difftime. <- unclass(difftime_)
  if (any(difftime. %/% (60*60*24) > 1, na.rm = TRUE)) stop('`difftime_` cannot be greater than 1 day')
  hr <- difftime. %/% (60*60)
  min <- (difftime. %% (60*60)) %/% 60
  sec <- (difftime. %% 60)
  sec_int <- round(sec)
  id <- which(abs(sec - sec_int) < tol)
  sec[id] <- sec_int[id]
  
  date. <- unclass(date_)
  ISOdatetime(year = date.$year + 1900, month = date.$mon + 1, day = date.$mday, 
              hour = hr, min = min, sec = sec, tz = tz)
}



#as.difftime.






