#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ###################################################### #
# working with different time index types and time units #
# ###################################################### #


# ti_type
# ###################################################################

#' Get Time Index Type
#'
#' @description
#' \code{ti_type} method returns time index type as a character value, either
#' in short form (single letter, code used internally) or long form (name).
#'
#' \code{is.instant} returns \code{TRUE} for continuous time indices representing
#' points in time (date-time, time of day, and numeric indices) and \code{FALSE}
#' for time discrete indices that represent periods of time, for example, days representing
#' (usually) 24 hours, weeks, months, quarter, and years (as well as integer indices).
#'
#' @note Behaviour of \code{is.instant} differs from that of identically named
#' function in \pkg{lubridate} package, which returns \code{TRUE} for all time
#' classes including dates.
#'
#' @param x an object of \code{tind} class or an object coercible to it.
#' @param long a logical value, if \code{FALSE}, internal single-character code
#'             of index type is returned; if \code{TRUE}, long (human-readable)
#'             name is returned (\code{TRUE} by default).
#' @param valid a logical value, if \code{TRUE}, syntactically valid names
#'              will be returned (\code{FALSE} by default).
#'
#' @return A character value for \code{ti_type}, a logical value
#' for \code{is.instant}.
#'
#' @seealso \code{\link{t_unit}}.
#'
#' @examples
#' ti_type(as.tind(1999))
#' ti_type(as.tind(1999), FALSE)
#' ti_type(as.tind("2001q3"))
#' ti_type(as.tind("2001q3"), FALSE)
#' ti_type(as.tind("2003-11"))
#' ti_type(as.tind("2003-11"), FALSE)
#' ti_type(as.tind("2004-W53"))
#' ti_type(as.tind("2004-W53"), FALSE)
#' ti_type(as.tind("2020-02-29"))
#' ti_type(as.tind("2020-02-29"), FALSE)
#' ti_type(today())
#' ti_type(today(), FALSE)
#' is.instant(today())
#' ti_type(now())
#' ti_type(now(), FALSE)
#' is.instant(now())
#' ti_type(Sys.Date())
#' ti_type(Sys.Date(), FALSE)
#' is.instant(Sys.Date())
#' ti_type(Sys.time())
#' ti_type(Sys.time(), FALSE)
#' is.instant(Sys.time())
#'
#' @export
#'
ti_type <- function(x, long = TRUE, valid = FALSE) UseMethod("ti_type")


#' @keywords internal
#' @export
ti_type.default <- function(x, long = TRUE, valid = FALSE)
{
    mes <- gettextf("%s is not recognised as a class representing time indices",
                    .class2str(x))
    stop(mes, call. = FALSE, domain = NA)
}


#' @rdname ti_type
#' @export
ti_type.tind <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type(.get.type(x), long = long, valid = valid, rm.names = TRUE))
}


#' @rdname ti_type
#' @export
ti_type.Date <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("d", long = long, valid = valid, rm.names = TRUE))
}


#' @rdname ti_type
#' @export
ti_type.POSIXt <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.ti_type("t", long = long, valid = valid, rm.names = TRUE))
}


#' @rdname ti_type
#' @export
is.instant <- function(x) .is.instant(ti_type(x, FALSE))



# t_unit
# ###################################################################

#' Get Time Unit
#'
#' @description
#' \code{t_unit} method returns time unit of time difference object
#' as a character value, either in short form (most often single letter,
#' code used internally) or long form (name).
#'
#' \code{\link[base]{units}} method for \code{tdiff} objects is equivalent
#' to \code{t_unit} with \code{x} argument only.
#'
#' @param x an object of \code{tdiff} class or an object coercible to it.
#' @param long a logical value, if \code{FALSE}, internal code
#'             of time unit is returned; if \code{TRUE}, long (human-readable)
#'             name is returned (\code{TRUE} by default).
#' @param valid a logical value, if \code{TRUE}, syntactically valid names
#'              will be returned (\code{FALSE} by default). Currently,
#'              has no impact as all unit names are syntactically valid.
#'
#' @details
#' Time differences (for example differences between date-time indices) are internally
#' represented as number of seconds. However, the returned time unit is
#' automatically determined based on the resolution of the argument. If all time
#' differences are full hours or full minutes, appropriate unit is returned,
#' see Examples. For time differences that contain zeros and missing values only,
#' returned unit is a second.
#'
#' @return A character value.
#'
#' @seealso \link{tdiff}, \code{\link{ti_type}}.
#'
#' @examples
#' (x <- as.tdiff(1, "y"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(1, "q"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(1, "m"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(1, "w"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(1, "d"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(1, "h"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(1, "min"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(1, "s"))
#' t_unit(x)
#' t_unit(x, FALSE)
#' # automatic unit determination
#' (x <- as.tdiff(600, "s")) # ten minutes
#' t_unit(x)
#' t_unit(x, FALSE)
#' (x <- as.tdiff(7200, "s")) # two hours
#' t_unit(x)
#' t_unit(x, FALSE)
#'
#' @export
t_unit <- function(x, long = TRUE, valid = FALSE) UseMethod("t_unit")


#' @keywords internal
#' @export
t_unit.default <- function(x, long = TRUE, valid = FALSE)
{
    mes <- gettextf("%s is not recognised as a class representing time differences",
                    .class2str(x))
    stop(mes, call. = FALSE, domain = NA)
}


#' @rdname t_unit
#' @export
t_unit.tdiff <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    return (.t_unit(.get_t_unit(x), long = long, valid = valid, rm.names = TRUE))
}


#' @rdname t_unit
#' @export
t_unit.difftime <- function(x, long = TRUE, valid = FALSE)
{
    .checkTRUEFALSE(long)
    .checkTRUEFALSE(valid)
    u <- attr(x, "units")
    u <- if (grepl("^min", u)) "min" else substr(u, 1L, 1L)
    return (.t_unit(u, long = long, valid = valid, rm.names = TRUE))
}


#' @rdname t_unit
#' @export
units.tdiff <- function(x)
    .t_unit(.get_t_unit(x), long = TRUE, valid = FALSE, rm.names = TRUE)


#' @keywords internal
#' @export
`units<-.tdiff` <- function(x, value)
{
    mes <- gettextf("%s method not defined for class %s", sQuote("units<-"),
                    .class2str(x))
    stop(mes, call. = FALSE, domain = NA)
}


