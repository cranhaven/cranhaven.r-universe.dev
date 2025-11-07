#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ##################### #
# rounding time indices #
# ##################### #


#' Rounding Time Indices
#'
#' @description
#' Time indices can be rounded to different time units (depending on the type
#' of time index at hand, see Details).
#'
#' \code{trunc_t} rounds the indices down to a given \code{unit}
#' with change of index type where applicable.
#'
#' \code{floor_t} rounds the indices down to a (multiple of a) unit.
#'
#' \code{ceiling_t} rounds the indices up to a (multiple of a) unit.
#'
#' \code{round_t} rounds the indices to the closest multiple of a unit, i.e.
#' the result of \code{floor_t} or \code{ceiling_t}, whichever is closer.
#'
#' @details
#' \strong{Units and Unit Multiples}
#'
#' For \code{trunc_t}, \code{unit} has to be a character string determining
#' resolution / type to which indices should be truncated. For the remaining
#' functions, \code{unit} argument can be a number (the default unit for index
#' type will be used), a character string with unit name, an object
#' of \code{tdiff} class, or a character string with a number and unit name.
#'
#' The following unit names are accepted:
#' \describe{
#'   \item{\code{"y"}, \code{"year"}, \code{"years"}}{years,}
#'   \item{\code{"q"}, \code{"quarter"}, \code{"quarters"}}{quarters,}
#'   \item{\code{"m"}, \code{"mon"}, \code{"month"}, \code{"months"}}{months,}
#'   \item{\code{"w"}, \code{"week"}, \code{"weeks"}}{weeks,}
#'   \item{\code{"d"}, \code{"day"}, \code{"days"}}{days,}
#'   \item{\code{"h"}, \code{"hour"}, \code{"hours"}}{hours,}
#'   \item{\code{"min"}, \code{"minute"}, \code{"minutes"}}{minutes,}
#'   \item{\code{"s"}, \code{"sec"}, \code{"second"}, \code{"seconds"}}{seconds.}
#' }
#' The default unit for date-time and time of day indices is a second.
#'
#' The list of admissible multiples of units can be found in the documentation
#' of \code{\link{resolution_t}} method.
#'
#' For time indices of types \code{"i"} and \code{"n"} (integer and numeric indices)
#' unit can be any finite, positive integer / numeric value.
#'
#' In case of a tie (\code{x - floor_t(x, *)} equal to \code{ceiling_t(x, *) - x}),
#' \code{round_t} returns the result of \code{ceiling_t}.
#'
#' \strong{Controlling behaviour of \code{ceiling_t} (and \code{round_t})}
#'
#' For non-instant time indices, i.e indices that actually represent periods of time
#' (days weeks, months, etc.) \code{ceiling_t} rounds to the first index in a period
#' by default. For instance, when rounding dates to months, the first day of a month
#' will be unchanged and other days will be rounded to the first day in the
#' \emph{following} month. This behaviour can be altered by setting \code{ceiling}
#' argument. If set to \code{"following"}, the first index (day in our example)
#' in the following period will be returned. If set to \code{"last"}, the last
#' index (day in our example) in the period will be returned. See Examples.
#'
#' @note
#' Methods \code{floor}, \code{ceiling}, \code{round}, and \code{trunc} are
#' not implemented for \code{tind} class due to generics' argument list
#' limitations.
#'
#' @param x an object of \code{tind} class (or an R object coercible to it).
#' @param unit a character string determining unit (expected by \code{trunc_t}),
#'             a number, an object of \code{tdiff} class, or a character string
#'             with a number and unit name.
#' @param ceiling (optional) a character string determining the behaviour
#'                of \code{ceiling_t} (and \code{round_t}), see Details.
#'
#' @return An object of \code{tind} class of the same type and length as \code{x}
#' except for \code{trunc_t}, for which the type of the result is determined
#' based on \code{unit} argument.
#'
#' @seealso \code{\link{resolution_t}} method.
#'
#' @examples
#' (d <- as.tind("2024-08-27"))
#' floor_t(d, "w")
#' trunc_t(d, "w")
#' ceiling_t(d, "w")
#' round_t(d, "w")
#' floor_t(d, "m")
#' trunc_t(d, "m")
#' ceiling_t(d, "m")
#' round_t(d, "m")
#' floor_t(d, "3m")
#' ceiling_t(d, "3m")
#' round_t(d, "3m")
#'
#' (dt <- as.tind("2024-08-27 13:51:52"))
#' floor_t(dt, 10)
#' floor_t(dt, "10s")
#' ceiling_t(dt, "10s")
#' round_t(dt, "10s")
#' floor_t(dt, "min")
#' trunc_t(dt, "min")
#' ceiling_t(dt, "min")
#' round_t(dt, "min")
#' floor_t(dt, "h")
#' trunc_t(dt, "h")
#' ceiling_t(dt, "h")
#' round_t(dt, "h")
#' floor_t(dt, "2h")
#' ceiling_t(dt, "2h")
#' round_t(dt, "2h")
#' floor_t(dt, "d")
#' trunc_t(dt, "d")
#' ceiling_t(dt, "d")
#' round_t(dt, "d")
#'
#' # corner cases - DST change (02:00 missing)
#' (dt <- date_time("2025-03-30", H = c(0:1, 3:6)))
#' floor_t(dt, "2h")
#' ceiling_t(dt, "2h")
#'
#' # adjusting behaviour of ceiling_t for non-instant time indices
#' # a short sequence of dates covering two months
#' (ds <- as.tind("2023-01-01") + -2:2)
#' # default behaviour
#' ceiling_t(ds, "2m")
#' ceiling_t(ds, "2m", ceiling = "default")
#' # round up to the first day in the following 2-month period
#' ceiling_t(ds, "2m", ceiling = "following")
#' # round up to the last day in the current 2-month period
#' ceiling_t(ds, "2m", ceiling = "last")
#' # a corner case, note that we will get the next day as a result
#' ceiling_t(today(), "1d", ceiling = "following")
#'
#' @name rounding
#'
NULL


#' @rdname rounding
#' @export
trunc_t <- function(x, unit)
{
    x <- as.tind(x)
    type <- .get.type(x)
    tz <- .get.tz(x)

    if (type %in% c("i", "n")) {
        mes1 <- gettextf("%s function not defined for type %s",
                        sQuote("trunc_t"), .ti_type2char(type))
        mes2 <- gettextf("use %s function instead", sQuote("floor_t"))
        stop(paste0(mes1, "; ", mes2), domain = NA)
    }

    unit <- list(n = 1, unit = .check_unit(unit))
    unit <- .check_fl_ceil_type_unit(type, unit)
    utype <- unit$type

    if (type %in% utype) utype <- type
    if (type == utype) {
        if (!(type %in% c("t", "h"))) return (x)
        if (!length(x)) return (x)
        fname <- paste0(".floor_", utype, "_", unit$unit)
        attributes(x) <- NULL
        return (.tind(do.call(fname, c(list(x, unit$n), tz)), type, tz))
    }

    return (.cast(x, utype))
}


#' @rdname rounding
#' @export
floor_t <- function(x, unit)
{
    x <- as.tind(x)
    type <- .get.type(x)
    tz <- .get.tz(x)

    if (!is.numeric(unit)) unit <- as.tdiff(unit)
    nunit <- .tdiff2nunit(unit)
    nunit <- .check_fl_ceil_type_unit(type, nunit)
    utype <- nunit$type

    if (!length(x)) return (x)

    x <- .unclass(x)
    fname <- if (utype %in% c("t", "h")) sprintf(".floor_%s_", utype) else ".floor_"
    if (type == utype) {
        x <- do.call(paste0(fname, nunit$unit), c(list(x, nunit$n), tz))
    } else {
        x <- do.call(paste0(".", type, "2", utype), c(list(x), tz))
        x <- do.call(paste0(fname, utype), list(x, nunit$n))
        x <- do.call(paste0(".", utype, "2", type), c(list(x), tz))
    }

    return (.tind(x, type, tz))
}


#' @rdname rounding
#' @export
ceiling_t <- function(x, unit, ceiling = c("default", "following", "last"))
{
    ceiling <- .match.arg(ceiling)
    x <- as.tind(x)
    type <- .get.type(x)
    tz <- .get.tz(x)

    if (!is.numeric(unit)) unit <- as.tdiff(unit)
    nunit <- .tdiff2nunit(unit)
    nunit <- .check_fl_ceil_type_unit(type, nunit)
    utype <- nunit$type

    if (!length(x)) return (x)

    if (!.is.instant(type) && ceiling != "default") x <- x + 1L

    x <- .unclass(x)
    cname <- if (utype %in% c("t", "h")) sprintf(".ceiling_%s_", utype) else ".ceiling_"
    if (type == utype) {
        x <- do.call(paste0(cname, nunit$unit), c(list(x, nunit$n), tz))
    } else {
        x0 <- do.call(paste0(".", type, "2", utype), c(list(x), tz))
        x1 <- x0 + 1L
        x0 <- do.call(paste0(cname, utype), list(x0, nunit$n))
        x1 <- do.call(paste0(cname, utype), list(x1, nunit$n))
        x0 <- do.call(paste0(".", utype, "2", type), c(list(x0), tz))
        x1 <- do.call(paste0(".", utype, "2", type), c(list(x1), tz))
        ic <- x0 != x
        ic <- ic & !is.na(ic)
        x[ic] <- x1[ic]
    }
    x <- .tind(x, type, tz)

    if (!.is.instant(type) && ceiling == "last") x <- x - 1L

    return (x)
}


#' @rdname rounding
#' @export
round_t <- function(x, unit, ceiling = c("default", "following", "last"))
{
    x <- as.tind(x)
    xf <- floor_t(x, unit)
    xc <- ceiling_t(x, unit, ceiling = ceiling)
    ic <- (xc - x) <= (x - xf)
    ic <- ic & !is.na(ic)
    xf[ic] <- xc[ic]
    return (xf)
}



# disable floor, ceiling, round, and trunc methods
# ###################################################################

#' @keywords internal
#' @export
floor.tind <- function(x)
{
    mes1 <- gettextf("%s method not defined for class %s", sQuote("floor"),
                     dQuote("tind"))
    mes2 <- gettextf("use %s function instead", sQuote("floor_t"))
    stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
}

#' @keywords internal
#' @export
ceiling.tind <- function(x)
{
    mes1 <- gettextf("%s method not defined for class %s", sQuote("ceiling"),
                     dQuote("tind"))
    mes2 <- gettextf("use %s function instead", sQuote("ceiling_t"))
    stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
}

#' @keywords internal
#' @export
round.tind <- function(x, digits)
{
    mes1 <- gettextf("%s method not defined for class %s", sQuote("round"),
                     dQuote("tind"))
    mes2 <- gettextf("use %s function instead", sQuote("round_t"))
    stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
}

#' @keywords internal
#' @export
trunc.tind <- function(x, ...)
{
    mes1 <- gettextf("%s method not defined for class %s", sQuote("trunc"),
                     dQuote("tind"))
    mes2 <- gettextf("use %s function instead", sQuote("trunc_t"))
    stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
}

