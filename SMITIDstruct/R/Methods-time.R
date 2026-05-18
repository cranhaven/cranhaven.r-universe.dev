# Part of the SMITIDstruct R package.
# Copyright (C) 2018 Jean-François Rey <jean-francois.rey@inra.fr>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,i
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#


#' is.timestamp
#' @description Check if a numeric represent a timestamp
#' @param time a numeric
#' @return TRUE if time >= 1971
#' @export
is.timestamp <- function(time) {
    if(is.numeric(time) && as.numeric(format(as.Date(as.POSIXct(as.numeric(time), origin="1970-01-01", tz = "GMT")), format="%Y")) >= 1971 ) return(TRUE)
    else return(FALSE)
    
    return(FALSE)
}

#' is.juliendate
#' @description Chekc if a numeric is not a timestamp
#' @param time a numeric
#' @return TRUE if time is a julien day, otherwise FALSE
#' @export
is.juliendate <- function(time) {
    return(!is.timestamp(time))
}

#' is.StringDate
#' @description Check if a string represent a date
#' @param date a string or a vector of string (without NA)
#' @return TRUE if date contains date format
#' @export
is.StringDate <- function(date) {
    #if( any(is.na(as.numeric(date))) ) { return(TRUE) }
    if( all(grepl("^[0-9]+$",as.character(date))) ) return(FALSE)
    else return(TRUE)
}

#' getDate
#' @description Converte timestamp to Date (string)
#' @param time a timestamp or vector of
#' @param format Date format output (default \%Y-\%m-\%dT\%H:\%M:\%S)
#' @return time as string date 
#' @export
getDate <- function(time, format="%Y-%m-%dT%H:%M:%S") {
    
    if(is.numeric(time)) {
            return(format(as.Date(as.POSIXct(as.numeric(time), origin="1970-01-01T00:00:00", tz = "GMT")), format=format))
    }
    return(FALSE)
}

#' getTimestamp
#' @description Get the timestamp of Date
#' @param date a date (as string) or vector of
#' @param format the date format (default \%Y-\%m-\%dT\%H:\%M:\%S)
#' @return timestamp of the date(s)
#' @export
getTimestamp <- function(date, format="%Y-%m-%dT%H:%M:%S") {
        as.numeric(as.POSIXct(strptime(date, format=format, tz = "GMT")))
}
