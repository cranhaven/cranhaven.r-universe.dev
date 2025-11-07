#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################################################## #
# working with other representations of time indices #
# ################################################## #


#' Conversion between \code{tind} and Other Time Index Representations
#'
#' @description
#' Besides \code{Date}, \code{POSIXct}, and \code{POSIXlt} classes from
#' package \pkg{base}, \pkg{tind} currently supports conversion between
#' \code{tind} and the following classes: \code{yearmon}, \code{yearqtr}
#' (both from package \pkg{zoo}), \code{timeDate} (from package \pkg{timeDate}),
#' \code{chron}, \code{dates}, \code{times} (from package \pkg{chron}),
#' and \code{IDate}, \code{ITime} (from package \pkg{data.table}).
#'
#' @details
#' Date-time indices resulting from conversion of \code{chron} objects
#' always have time zone set to UTC. Use \code{\link{tzone<-}}
#' or \code{\link{as.tzone}} methods when necessary.
#'
#' @param x an R object to be converted.
#' @param ... (ignored) further arguments passed to or from other methods.
#' @param digits an integer value (0--6) determining the number of decimal places
#'               for seconds to be preserved during conversion (0 by default).
#'
#' @return \code{as.xxx} returns an object of \code{xxx} class of the same
#' length as the argument.
#'
#' @name tind-other
#' @aliases as.yearmon as.yearqtr as.timeDate as.chron as.dates as.times
#' as.IDate as.ITime
#'
#' @seealso \code{\link{as.tind}} and \link{tind-coercion} for conversions
#' to and from \code{tind}, \code{\link{date2num}} and \code{\link{num2date}}
#' for conversion between \code{tind} and integer representations of dates
#' (days since ...) found in different software packages.
#'
NULL


# zoo::yearmon, zoo::yearqtr
# ###################################################################

.tind_coercible.yearmon <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.yearmon <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("m", long = long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.yearmon <- function(x, ...)
{
    res <- yf2tind(.unclass(x), "m")
    return (res)
}


#' @rdname tind-other
#' @usage as.yearmon(x, ...)
#' @exportS3Method zoo::as.yearmon
as.yearmon.tind <- function(x, ...)
{
    .expect_type(.get.type(x), "m", 1L)
    return (structure(year_frac(x), class = "yearmon"))
}


.tind_coercible.yearqtr <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.yearqtr <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("q", long = long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.yearqtr <- function(x, ...)
{
    res <- yf2tind(.unclass(x), "q")
    return (res)
}


#' @rdname tind-other
#' @usage as.yearqtr(x, ...)
#' @exportS3Method zoo::as.yearqtr
as.yearqtr.tind <- function(x, ...)
{
    .expect_type(.get.type(x), "q", 1L)
    return (structure(year_frac(x), class = "yearqtr"))
}



# timeDate::timeDate
# ###################################################################

.tind_coercible.timeDate <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.timeDate <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("t", long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.timeDate <- function(x, digits = 0L, ...)
{
    tz <- suppressWarnings(tryCatch(.check_tz(x@FinCenter),
                                    error = function(e) NULL))
    if (is.null(tz)) {
        mes <- gettextf("failed to map %s %s to a time zone",
                        sQuote("FinCenter"), dQuote(x@FinCenter))
        warning(mes, call. = FALSE, domain = NA)
    }
    res <- as.tind.POSIXct(x@Data, tz = tz, digits = digits)
    return (res)
}


#' @rdname tind-other
#' @usage as.timeDate(x, ...)
#' @exportS3Method timeDate::as.timeDate
as.timeDate.tind <- function(x, ...)
{
    .expect_type(.get.type(x), "t", 1L)
    tz <- .get.tz(x)
    x <- as.POSIXct(.unclass(x), tz = "GMT", origin = "1970-01-01")
    res <- methods::new("timeDate", Data = x, FinCenter = tz)
    return (res)
}


#' @keywords internal
#' @export
tzone.timeDate <- function(x) x@FinCenter


#' @keywords internal
#' @export
`tzone<-.timeDate` <- function(x, value)
{
    x@FinCenter <- value
    return (x)
}


#' @keywords internal
#' @export
as.tzone.timeDate <- function(x, tz)
    as.timeDate.tind(as.tzone(as.tind.timeDate(x), tz))



# chron::chron, chron::dates, chron::times
# ###################################################################

.tind_coercible.chron <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.chron <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("t", long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.chron <- function(x, digits = 0L, ...)
{
    digits <- .check_digits(digits)
    res <- as.tind(round(.unclass(x) * 86400, digits = digits), tz = "UTC")
    return (res)
}


#' @rdname tind-other
#' @usage as.chron(x, ...)
#' @exportS3Method chron::as.chron
as.chron.tind <- function(x, ...)
{
    .expect_type(.get.type(x), c("d", "h", "t"), 1L)
    if (.get.type(x) == "h") return (as.times.tind(x))
    if (.get.type(x) == "d") return (as.dates.tind(x))
    x <- as.tzone(x, "UTC")
    res <- structure(.unclass(x) / 86400,
                     format = c("m/d/y", "h:m:s"),
                     origin = c(month = 1, day = 1, year = 1970),
                     class = c("chron", "dates", "times"))
    return (res)
}


#' @keywords internal
#' @export
tzone.chron <- function(x) "UTC"


.tind_coercible.dates <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.dates <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("d", long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.dates <- function(x, ...)
{
    ind <- .validate_d(.unclass(x))
    res <- .tind(ind, "d")
    return (res)
}


#' @rdname tind-other
#' @usage as.dates(x, ...)
#' @exportS3Method chron::as.dates
as.dates.tind <- function(x, ...)
{
    .expect_type(.get.type(x), "d", 1L)
    res <- .unclass(x)
    return (structure(.require_mode(res, "double"),
                      format = "m/d/y",
                      origin = c(month = 1, day = 1, year = 1970),
                      class = c("dates", "times")))
}


.tind_coercible.times <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.times <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("h", long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.times <- function(x, digits = 0L, ...)
{
    digits <- .check_digits(digits)
    res <- as.tind(round(unclass(x) * 86400, digits = digits), "h")
    return (res)
}


#' @rdname tind-other
#' @usage as.times(x)
#' @exportS3Method chron::as.times
as.times.tind <- function(x, ...)
{
    .expect_type(.get.type(x), "h", 1L)
    res <- structure(unclass(x) / 86400, format = "h:m:s", class = "times")
    return (res)
}



# data.table::IDate, data.table::ITime
# ###################################################################

.tind_coercible.IDate <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.IDate <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("d", long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.IDate <- function(x, ...)
{
    ind <- .validate_d(.jdn2d(2440588L + unclass(x)))
    res <- .tind(ind, "d")
    return (res)
}


#' @rdname tind-other
#' @usage as.IDate(x, ...)
#' @exportS3Method data.table::as.IDate
as.IDate.tind <- function(x, ...)
{
    .expect_type(.get.type(x), "d", 1L)
    res <- structure(.unclass(x), class = c("IDate", "Date"))
    return (res)
}


.tind_coercible.ITime <- function(x) TRUE


#' @keywords internal
#' @export
ti_type.ITime <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("h", long, valid = valid, rm.names = TRUE))
}


#' @rdname tind-other
#' @export
as.tind.ITime <- function(x, ...)
{
    res <- as.tind(unclass(x), "h")
    return (res)
}


#' @rdname tind-other
#' @usage as.ITime(x)
#' @exportS3Method data.table::as.ITime
as.ITime.tind <- function(x, ...)
{
    .expect_type(.get.type(x), "h", 1L)
    res <- structure(.unclass(x), class = "ITime")
    return (res)
}


## NOTE: a trick to turn off R CMD check warning
as.yearmon  <- NULL
as.yearqtr  <- NULL
as.timeDate <- NULL
as.chron    <- NULL
as.dates    <- NULL
as.times    <- NULL
as.IDate    <- NULL
as.ITime    <- NULL



# date2num, num2date
# ###################################################################

#' Conversion between Dates and Their Integer Representations
#'
#' @description
#' \code{date2num} and \code{num2date} support conversion between
#' \code{tind} dates and integer representations of dates (days since ...)
#' found in different software packages.
#'
#' @param x a \code{tind} with dates or an integer vector.
#' @param format a character value determing numeric representation of date;
#'               currently, the following are supproted: \code{"R"},
#'               \code{"MATLAB"}, \code{"Excel"}, \code{"SAS"},
#'               \code{"JDN"} (Julian Day Number).
#'
#' @return \code{date2num} returns an integer vector
#' and \code{num2date} returns \code{tind} representing dates.
#'
#' @seealso \code{\link{jdn}} for description of Julian Day Numbers.
#'
#' @examples
#' (td <- today())
#' fmts <- c("R", "MATLAB", "Excel", "SAS", "JDN")
#' (n <- sapply(fmts, function(fmt) date2num(td, fmt)))
#' lapply(fmts, function(fmt) num2date(n[fmt], fmt))
#'
#' @name date2num
#'
NULL


.origins <- c(`R`      = 2440588L, # jdn("1970-01-01")
              `MATLAB` = 1721059L, # jdn("0000-01-01") - 1L
              `Excel`  = 2415019L, # jdn("1900-01-01") - 2L
              `SAS`    = 2436935L, # jdn("1960-01-01")
              `JDN`    = 0L)


#' @rdname date2num
#' @export
date2num <- function(x, format)
{
    x <- as.tind(x)
    .expect_type(.get.type(x), "d")
    if (missing(format)) format <- NULL
    orig <- .match.arg(format, .origins)
    return (jdn(x) - orig)
}


#' @rdname date2num
#' @export
num2date <- function(x, format)
{
    if (!is.numeric(x)) {
        mes1 <- gettextf("invalid %s argument", sQuote("x"))
        mes2 <- gettextf("expected a numeric vector")
        stop(paste0(mes1, "; ", mes2), domain = NA)
    }
    if (missing(format)) format <- NULL
    orig <- .match.arg(format, .origins)
    return (jdn2tind(.require_mode(x, "integer") + orig))
}

