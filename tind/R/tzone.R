#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############ #
# tzone method #
# ############ #


# tzone, tzone<-
# ###################################################################

#' Read or Set Time Zone for Date-Time Indices
#'
#' @description
#' Date-time indices (objects of \code{tind} class of type \code{"t"})
#' always have the time zone attribute set. The time zone setting affects
#' how time (measured relative to the Unix epoch in UTC) is translated to local
#' time. Objects of base \code{POSIXct} and \code{POSIXlt} classes also have
#' an optional time zone attribute. \code{tzone} method is also implemented for some
#' other classes supported by the \pkg{tind} package.
#'
#' List of time zones supported by a particular R installation can be obtained
#' via a call to \code{\link[base]{OlsonNames}} function, see Examples.
#'
#' @details
#' If the provided name is not in the list of supported time zones,
#' an attempt is made to identify it via approximate match. If the result is
#' a single time zone, it is accepted with a warning.
#'
#' Unambiguous city names are automatically recognised, see Examples.
#'
#' An attempt to set time zone attribute of a time index
#' of different type than date-time (\code{"t"}) will lead to an error.
#' In case of \code{POSIXct}/\code{POSIXlt} objects with no time zone attribute,
#' the extractor returns the system time zone.
#'
#' @param x an object of \code{tind} class or of \code{POSIXct}/\code{POSIXlt}
#'          classes (or of other class for which the method was implemented).
#' @param value a character value, the new time zone attribute.
#'
#' @return For the extractor, the time zone as a character value (or \code{NULL}
#' for classes without time zone attribute). For the replacement, the argument
#' with the modified time zone.
#'
#' @seealso \code{\link{as.tzone}}.
#'
#' @examples
#' # check time in the system time zone
#' (nw <- now())
#' # get time zone
#' tzone(nw)
#' # set time zone to UTC
#' tzone(nw) <- "UTC"
#' nw
#' tzone(nw)
#' # check time in different time zones
#' if ("Asia/Tokyo" %in% OlsonNames()) {
#' tzone(nw) <- "Asia/Tokyo"
#' nw
#' }
#' if ("Europe/Warsaw" %in% OlsonNames()) {
#' tzone(nw) <- "Europe/Warsaw"
#' nw
#' }
#' if ("America/New_York" %in% OlsonNames()) {
#' tzone(nw) <- "America/New_York"
#' nw
#' }
#' # try invalid time zone => error
#' try(
#' tzone(nw) <- "Hasdfg/Qwerty"
#' )
#' # unambiguous city names are automatically recognised
#' tzone(nw) <- "Tokyo"
#' nw
#' tzone(nw) <- "Warsaw"
#' nw
#' tzone(nw) <- "New York"
#' nw
#' # incomplete names and approximate matches are also recognised with a warning
#' if ("Europe/Warsaw" %in% OlsonNames()) try({
#' tzone(nw) <- "Warsa"
#' nw
#' })
#' if ("America/New_York" %in% OlsonNames()) try({
#' tzone(nw) <- "NewYork"
#' nw
#' })
#'
#' # list first 6 supported time zones using base::OlsonNames
#' head(OlsonNames())
#' # list first 6 supported time zones with string "Europe"
#' head(grep("Europe", OlsonNames(), value = TRUE))
#' # list first 6 supported time zones with string "Asia"
#' head(grep("Asia", OlsonNames(), value = TRUE))
#' # list first 6 supported time zones with string "Africa"
#' head(grep("Africa", OlsonNames(), value = TRUE))
#' # list first 6 supported time zones with string "America"
#' head(grep("America", OlsonNames(), value = TRUE))
#'
#' @name tzone
#'
NULL


#' @rdname tzone
#' @export
tzone <- function(x) UseMethod("tzone")


#' @rdname tzone
#' @export
`tzone<-` <- function(x, value) UseMethod("tzone<-")


#' @keywords internal
#' @export
tzone.default <- function(x)
{
    .check_tind_coercible(x)
    return (NULL)
}


#' @keywords internal
#' @export
`tzone<-.default` <- function(x, value)
{
    if (!.tind_coercible(x)) {
        mes <- gettextf("%s is not recognised as a class representing time indices",
                        .class2str(x))
    } else {
        mes <- gettextf("%s method not defined for class %s", sQuote("tzone<-"),
                        .class2str(x))
    }
    stop(mes, call. = FALSE, domain = NA)
}


#' @rdname tzone
#' @export
tzone.tind <- function(x) .get.tz(x)


#' @rdname tzone
#' @export
`tzone<-.tind` <- function(x, value)
{
    .expect_type(.get.type(x), "t", 1L)
    return (.set.tz(x, .check_tz(value)))
}


#' @rdname tzone
#' @export
tzone.tinterval <- function(x) tzone(x$start)


#' @rdname tzone
#' @export
`tzone<-.tinterval` <- function(x, value)
{
    st <- x$start
    en <- x$end
    tzone(st) <- tzone(en) <- value
    return (.tinterval(st, en))
}


#' @keywords internal
#' @export
tzone.Date <- function(x) NULL


#' @rdname tzone
#' @export
tzone.POSIXct <- function(x)
{
    tz <- attr(x, "tzone")
    return (if (is.null(tz) || tz == "") Sys.timezone() else tz)
}


#' @rdname tzone
#' @export
`tzone<-.POSIXct` <- function(x, value)
{
    attr(x, "tzone") <- .check_tz(value)
    return (x)
}


#' @rdname tzone
#' @export
tzone.POSIXlt <- function(x)
{
    tz <- attr(x, "tzone")[[1L]]
    return (if (is.null(tz) || tz == "") Sys.timezone() else tz)
}


#' @rdname tzone
#' @export
`tzone<-.POSIXlt` <- function(x, value)
{
    x <- as.POSIXct(x)
    attr(x, "tzone") <- .check_tz(value)
    x <- as.POSIXlt(x)
    return (x)
}


# as.tzone
# ###################################################################

#' Get the Same Date and Time in a Different Time Zone
#'
#' @description
#' This method allows to determine time index representing the same date and
#' time in a different time zone.
#'
#' @details
#' The underlying time (as measured by number of seconds since the Unix epoch in UTC)
#' will change so that the date-time components in the new and old time zones
#' are the same. For \code{tind} arguments, if (due to DST time changes or UTC
#' offset changes) date-time indices do not occur in the new time zone,
#' \code{NA}s are introduced with a warning. For \code{tinterval} arguments, the result
#' is adjusted with a warning, in order not to create open-ended time intervals.
#'
#' The method is implemented for objects of \code{tind} class of type \code{"t"}
#' (date-time), objects of \code{tinterval} class of type \code{"t"} (time intervals),
#' as well as base \code{POSIXct} and \code{POSIXlt} classes.
#'
#' List of time zones supported by the particular R installation can be obtained
#' via a call to \code{\link[base]{OlsonNames}} function.
#'
#' @param x an object of \code{tind} class or of \code{POSIXct}/\code{POSIXlt}
#'          classes (or of other class for which the method was implemented).
#' @param tz a character value determining the new time zone.
#'           See \code{\link{tzone}} documentation for information on time zones.
#'
#' @return An object of the same class and length as \code{x} with adjusted
#' underlying date-time representation and time zone set to \code{tz}.
#'
#' @name as.tzone
#'
#' @seealso \code{\link{tzone}} method and \code{\link{date_time}} for construction
#' od date-time indices from its components.
#'
#' @examples
#' if (all(c("Europe/Warsaw", "America/New_York") %in% OlsonNames())) {
#' # check time in one time zone
#' print(nw <- now(tz = "Europe/Warsaw"))
#' # the same date-time in a new time zone
#' print(nw2 <- as.tzone(nw, "America/New_York"))
#' # note the time difference (equal to the difference of UTC offsets)
#' # warning on different time zones will be issued
#' print(suppressWarnings(nw2 - nw))
#' try(nw2 - nw)
#' }
#'
NULL


#' @rdname as.tzone
#' @export
as.tzone <- function(x, tz) UseMethod("as.tzone")


#' @keywords internal
#' @export
as.tzone.default <- function(x, tz)
{
    if (!.tind_coercible(x)) {
        mes <- gettextf("%s is not recognised as a class representing time indices",
                        .class2str(x))
    } else {
        mes <- gettextf("%s method not defined for class %s", sQuote("tzone<-"),
                        .class2str(x))
    }
    stop(mes, call. = FALSE, domain = NA)
}


#' @rdname as.tzone
#' @export
as.tzone.tind <- function(x, tz)
{
    .expect_type(.get.type(x), "t", 1L)
    tz0 <- .get.tz(x)
    tz <- .check_tz(tz)
    res <- .tind(.astz(.unclass(x), tz0, tz), "t", tz)
    nok <- !is.na(x) & is.na(res)
    if (any(nok)) {
        nok <- which.max(nok)
        mes1 <- gettextf("NAs introduced")
        mes2 <- gettextf("first position %s: %s", format(nok, scientific = FALSE),
                         .t2char(x[nok], tz = tz0, FALSE, FALSE))
        mes3 <- gettextf("time zone: %s", .check_tz(tz))
        warning(paste0(mes1, "; ", mes2, "; ", mes3), call. = FALSE, domain = NA)
    }

    return (res)
}


#' @rdname as.tzone
#' @export
as.tzone.POSIXct <- function(x, tz)
{
    tz <- .check_tz(tz)
    return (as.POSIXct(as.tzone(as.tind(x, digits = 6L), tz)))
}


#' @rdname as.tzone
#' @export
as.tzone.POSIXlt <- function(x, tz)
{
    tz <- .check_tz(tz)
    y <- date_time(.tind(.Call(C_plt_ymd2d, x$year, x$mon, x$mday, names(x)), "d"),
                   .tind(.Call(C_plt_hms2h, x$hour, x$min, x$sec, NULL), "h"),
                   tz = tz)
    return (as.POSIXlt(y))
}


#' @rdname as.tzone
#' @export
as.tzone.tinterval <- function(x, tz)
{
    s0 <- x$start
    e0 <- x$end
    tz0 <- .get.tz(s0)
    tz <- .check_tz(tz)
    s1 <- .tind(.astz(.unclass(s0), tz0, tz), "t", tz)
    si <- is.na(s1) & !is.na(s0)
    if (any(si)) {
        s1[si] <- as.date_time(as.date(s0[si]), tz = tz) +
                               (s0[si] - floor_t(s0[si], "1d"))
        ii <- which.max(si)
        mes0 <- gettextf("starting times adjusted")
        mes1 <- gettextf("first position %s: %s", format(ii, scientific = FALSE),
                         .t2char(s0[ii], tz = tz0, FALSE, FALSE))
        mes2 <- gettextf("time zone: %s", tz)
        warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
    }
    e1 <- .tind(.astz(.unclass(e0), tz0, tz), "t", tz)
    ei <- is.na(e1) & !is.na(e0)
    if (any(ei)) {
        e1[ei] <- as.date_time(as.date(e0[ei]), tz = tz) +
                               (e0[ei] - floor_t(e0[ei], "1d"))
        ii <- which.max(ei)
        mes0 <- gettextf("end times adjusted")
        mes1 <- gettextf("first position %s: %s", format(ii, scientific = FALSE),
                         .t2char(e0[ii], tz = tz0, FALSE, FALSE))
        mes2 <- gettextf("time zone: %s", tz)
        warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
    }

    .tinterval(s1, e1)
}

