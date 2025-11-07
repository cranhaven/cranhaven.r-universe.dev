#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################## #
# Ops Group Generics #
# ################## #


#' Operations Involving Time Indices, Time Differences, and Time Intervals
#'
#' @description
#' Basic arithmetic and comparison operators are implemented
#' for time indices, time differences, and time intervals where applicable.
#'
#' Operators \code{+} and \code{-} allow for shifting
#' time indices and computing differences between two indices. Time intervals
#' can be shifted using these, too. When the second operand in \code{+} and \code{-}
#' is numeric the underlying time unit is used. For time of day and date-time indices
#' this is always a second.
#'
#' Convenience operators
#' \code{\%+y\%}, \code{\%-y\%},  \code{\%+q\%}, \code{\%-q\%},
#' \code{\%+m\%}, \code{\%-m\%}, \code{\%+w\%}, \code{\%-w\%},
#' \code{\%+d\%}, \code{\%-d\%}, \code{\%+h\%}, \code{\%-h\%},
#' \code{\%+min\%}, \code{\%-min\%}, \code{\%+s\%}, and \code{\%-s\%}
#' can used to shift time indices (and intervals) by years, quarters, months,
#' weeks, days, hours minutes, and seconds.
#' See Details for their behaviour in corner cases.
#'
#' For all index types except for integer and numeric indices differences
#' between time indices are retuned as objects of \code{tdiff} class.
#'
#' Comparison operators are available for time indices (\code{tind})
#' and time differences (\code{tdiff}).
#'
#' @details
#' One can only subtract from time indices and divide time differences.
#'
#' Unary \code{+} and \code{-} operators are supported for \code{tdiff} only.
#'
#' Results of arithmetic operations are always validated and can become \code{NA}s
#' (when out of range) or be rounded, for example, when dividing
#' time differences in days by numbers that are not divisors.
#'
#' Shifting time intervals beyond valid index ranges can lead to spurious results
#' as beginnings or ends of time intervals become \code{NA}s and intervals can become
#' entire line.
#'
#' \code{==} and \code{!=} operators for time indices only accept same types
#' of indices. The remaining comparison operators accept different types
#' provided that conversion can be performed.
#'
#' \strong{Corner Cases}
#'
#' When shifting dates by months one is faced with a dilemma:
#' should month after March 31st be April 30th or May 1st?
#' The convention in \pkg{tind} is that the result of \code{\%+m\% n}
#' always falls in the \code{n}th month after the month in which
#' a given date falls and in corner cases the last day in the resulting
#' month is returned. Similar logic is applied to shifts by years
#' and quarters as well as shifts of weeks by years (some years have 53 weeks).
#' See Examples.
#'
#' Shifting date-time indices by days can also be problematic in front of DST changes,
#' when the resulting date has 23 hours (one hour missing) or 25 hours
#' (one hour repeated). When hour is missing, the next hour is selected (no \code{NA}
#' is returned). When hour is doubled, the second occurrence
#' is selected. See Examples.
#'
#' @param e1,e2 a \code{tind}, \code{tdiff}, \code{tinterval}, or a numeric vector.
#'
#' @return Comparison operators and \code{!} return logical vectors.
#'
#' Differences of time indices are returned as objects of \code{tdiff} class,
#' except for arguments of type \code{"i"} or \code{"n"} (integer/numeric
#' indices), in which case an integer or numeric vector is returned.
#'
#' Shifting time indices and time intervals produces time indices and time
#' intervals, respectively.
#'
#' Operations involving time differences return time differences.
#'
#' @seealso \code{\link{tind}} class and its constructor,
#' \code{\link{calendrical-computations}} for calendrical computations.
#'
#' @examples
#' # list the last 10 days including today
#' today() + (-9:0)
#'
#' # how many days have passed since the beginning of the year?
#' today() - floor_t(today(), "y")
#' # same but the result is not tdiff
#' day_of_year(today()) - 1
#'
#' # single time interval
#' x <- "2024-06-01 08:00" %--% "2024-06-01 16:00"
#' # shift by 0, 1, ..., 5 days
#' x %+d% 0:5
#'
#' # are we in or after 2026?
#' today() >= 2026
#' # are we after 2025?
#' today() > 2025
#'
#' # corner cases - ends of months and shifts by months
#' as.date("2024-01-31") %+m% 0:5
#' # same
#' as.date("2024-01-31") + mnths(0:5)
#'
#' # corner cases - 53rd week of year
#' as.week(202053) %-y% 0:5
#'
#' # corner cases - DST changes and shifts by days
#' if ("Europe/Warsaw" %in% OlsonNames()) {
#'     # 2020-10-25 had 25h with 02:00 repeated
#'     print(as.date_time("2020-10-24 02:00", tz = "Europe/Warsaw") %+h% 23:26)
#'     print(as.date_time("2020-10-24 02:00", tz = "Europe/Warsaw") %+d% 1)
#'     print(as.date_time("2020-10-26 02:00", tz = "Europe/Warsaw") %-h% 26:23)
#'     print(as.date_time("2020-10-26 02:00", tz = "Europe/Warsaw") %-d% 1)
#'     # 2021-03-28 had 23h with 02:00 missing
#'     print(as.date_time("2021-03-27 02:00", tz = "Europe/Warsaw") %+h% 22:25)
#'     print(as.date_time("2021-03-27 02:00", tz = "Europe/Warsaw") %+d% 1)
#'     print(as.date_time("2021-03-29 02:00", tz = "Europe/Warsaw") %-h% 25:22)
#'     print(as.date_time("2021-03-29 02:00", tz = "Europe/Warsaw") %-d% 1)
#' }
#'
#' @name Ops
#' @aliases + - * / %% %/% == != < <= > >= !
#'
NULL



## NOTE: Below, there is a workaround to address limitations of S3 dispatch
## mechanism, esp. prior to the introduction of 'chooseOpsMethod' in 4.3.0.
## Here, we first forward everything to `Ops.tind` and dispatch from there.
## This might be adjusted to use 'chooseOpsMethod' (or maybe a newer mechanism)
## at some point.


#' @name Ops
#' @export
`Ops.tind` <- function(e1, e2)
{
## NOTE: this is for debugging only
# if (nargs() == 1L) cat(paste("Ops.tind:", .Generic, class(e1)[1], "\n"))
# else cat(paste("Ops.tind:", class(e1)[1], .Generic, class(e2)[1], "\n"))

    # unary ops
    if (nargs() == 1L) return (.Ops.unary(.Generic, e1))
    # handle logical
    if (is.logical(e1)) e1 <- as.integer(e1)
    if (is.logical(e2)) e2 <- as.integer(e2)
    # at least one operand is tinterval
    if (is.tinterval(e1) || is.tinterval(e2)) {
        return (.Ops.tinterval(.Generic, e1, e2))
    }
    # at least one operand is tind
    if (is.tind(e1) || is.tind(e2)) return (.Ops.tind(.Generic, e1, e2))
    # we only have tdiff left
    return (.Ops.tdiff(.Generic, e1, e2))
}


#' @rawNamespace S3method(Ops,tdiff,Ops.tind)
NULL

#' @rawNamespace S3method(Ops,tinterval,Ops.tind)
NULL


# error messages
.err.ops1 <- function(.Generic, e1)
{
    mes <- gettextf("unary operator %s not defined for %s objects",
                    sQuote(.Generic), .class2str(e1))
    stop(mes, call. = FALSE, domain = NA)
}

.err.ops2 <- function(.Generic, e1, e2)
{
    mes <- gettextf("operator %s not defined for %s and %s objects",
                    sQuote(.Generic), .class2str(e1), .class2str(e2))
    stop(mes, call. = FALSE, domain = NA)
}



# unary ops
.Ops.unary <- function(.Generic, e1)
{
    if (is.tind(e1) || is.tinterval(e1) || !(.Generic %in% c("!", "+", "-")))
        .err.ops1(.Generic, e1)
    # +,-,! for tdiff
    if (.Generic == "+") return (e1)
    if (.Generic == "!") return (!as.logical(e1))
    return (.cp_attr(-.unclass(e1), e1))
}


# ops involving tind
.Ops.tind <- function(.Generic, e1, e2)
{
    if (.Generic == "-") {
        if (!is.tind(e1)) .err.ops2(.Generic, e1, e2)
        if (is.tind(e2)) return (.t_diff(e1, e2))
        if (is.tdiff(e2)) return (.t_inc_by_unit(e1, .unclass(e2), .get_tdiff_type(e2), TRUE))
        if (is.vector(e2, "numeric")) return (.t_inc(e1, -e2))
        .err.ops2(.Generic, e1, e2)
    } else if (.Generic == "+") {
        if (is.tind(e1) && is.tind(e2)) .err.ops2(.Generic, e1, e2)
        if (is.tdiff(e2)) return (.t_inc_by_unit(e1, .unclass(e2), .get_tdiff_type(e2)))
        if (is.tdiff(e1)) return (.t_inc_by_unit(e2, .unclass(e1), .get_tdiff_type(e1)))
        if (is.tind(e1)) { ti <- e1; by <- e2; } else { ti <- e2; by <- e1 }
        if (is.vector(by, "numeric")) return (.t_inc(ti, by))
        .err.ops2(.Generic, e1, e2)
    } else if (.Generic %in% c("==", "!=", "<=", "<", ">=", ">")) {
        return (.Compare.tind(.Generic, e1, e2))
    }
    .err.ops2(.Generic, e1, e2)
}


.Compare.tind <- function(.Generic, e1, e2)
{
    e1 <- as.tind(e1)
    e2 <- as.tind(e2)
    tp1 <- .get.type(e1)
    tp2 <- .get.type(e2)
    tz1 <- .get.tz(e1)
    tz2 <- .get.tz(e2)
    attributes(e1) <- attributes(e2) <- NULL
    if (.Generic %in% c("==", "!=")) {
        if (tp1 == tp2) {
            if ((tp1 == "t") && (tz1 != tz2)) .warn_diff_tz(tz1, tz2)
        } else {
            mes <- gettextf("time index type mismatch in %s", sQuote(.Generic))
            stop(paste0(mes, ": ", .ti_type2char(c(tp1, tp2))),
                 call. = FALSE, domain = NA)
        }
    } else {
        if (tp1 == tp2) {
            if ((tp1 == "t") && (tz1 != tz2)) .warn_diff_tz(tz1, tz2)
        } else if (tp2 %in% .lo_res_cast(tp1)) {
            arglist <- if (tp1 == "t") list(e1, tz1) else list(e1)
            e1 <- do.call(paste0(".", tp1, "2", tp2), arglist)
        } else if (tp1 %in% .lo_res_cast(tp2)) {
            arglist <- if (tp2 == "t") list(e2, tz2) else list(e2)
            e2 <- do.call(paste0(".", tp2, "2", tp1), arglist)
        } else {
            # this will throw except for i/n, n/i cases
            .max_res(c(tp1, tp2), .Generic, weak = TRUE)
        }
    }
    .check_lengths(length(e1), length(e2))
    return (suppressWarnings(do.call(.Generic, list(e1, e2))))
}


# tdiff operators
.Ops.tdiff <- function(.Generic, e1, e2)
{
    td1 <- is.tdiff(e1)
    td2 <- is.tdiff(e2)
    if (.Generic %in% c("+", "-", "*", "==", "!=", "<=", "<", ">=", ">")) {
        if (td1 && td2) {
            if (.Generic == "*") .err.ops2(.Generic, e1, e2)
            tp1 <- .get_tdiff_type(e1)
            tp2 <- .get_tdiff_type(e2)
            if (tp1 != tp2) {
                mes <- gettextf("time unit mismatch in %s", sQuote(.Generic))
                stop(paste0(mes, ": ", .t_unit2char(.get_t_unit(e1)), ", ",
                                       .t_unit2char(.get_t_unit(e2))),
                     call. = FALSE, domain = NA)
            }
            type <- tp1
            e1 <- .unclass(e1)
            e2 <- .unclass(e2)
        } else if (!is.vector(e1, "numeric") && !is.vector(e2, "numeric")) {
            .err.ops2(.Generic, e1, e2)
        } else if (td1) {
            type <- .get_tdiff_type(e1)
            if ((type == "t") && (.Generic != "*")) {
                un <- .get_t_unit(e1)
                if (un == "h") e2 <- e2 * 3600 else if (un == "min") e2 <- e2 * 60
            }
            e1 <- .unclass(e1)
        } else {
            type <- .get_tdiff_type(e2)
            if ((type == "t") && (.Generic != "*")) {
                un <- .get_t_unit(e2)
                if (un == "h") e1 <- e1 * 3600 else if (un == "min") e1 <- e1 * 60
            }
            e2 <- .unclass(e2)
        }
    } else if (.Generic %in% c("/", "%%", "%/%")) {
        if (td2 || !is.vector(e2, "numeric")) .err.ops2(.Generic, e1, e2)
        type <- .get_tdiff_type(e1)
        e1 <- .unclass(e1)
    } else .err.ops2(.Generic, e1, e2)
    # actual computation
    .check_lengths(length(e1), length(e2))
    res <- suppressWarnings(do.call(.Generic, list(e1, e2)))
    if (.Generic %in% c("+", "-", "*", "/", "%%", "%/%"))
        return (.tdiff(.validate_tdiff(res, type), type))
    return (res)
}


# ops involving tinterval
.Ops.tinterval <- function(.Generic, e1, e2)
{
    if (.Generic == "-") {
        if (is.tdiff(e2) || is.vector(e2, "numeric")) return (.tint_inc(e1, -e2))
        .err.ops2(.Generic, e1, e2)
    }
    if (.Generic == "+") {
        if (is.tdiff(e2) || is.vector(e2, "numeric")) return (.tint_inc(e1, e2))
        if (is.tdiff(e1) || is.vector(e1, "numeric")) return (.tint_inc(e2, e1))
        .err.ops2(.Generic, e1, e2)
    }
    .err.ops2(.Generic, e1, e2)
}



# internal routines - time index differences and increments
# ###################################################################

.t_diff <- function(x, y)
{
    tp1 <- .get.type(x)
    tp2 <- .get.type(y)
    if (tp1 != tp2) {
        mes <- gettextf("time index type mismatch in %s", sQuote("-"))
        stop(paste0(mes, ": ", .ti_type2char(c(tp1, tp2))),
             call. = FALSE, domain = NA)
    }
    if (tp1 == "t" && (.get.tz(x) != .get.tz(y)))
        .warn_diff_tz(.get.tz(x), .get.tz(y))

    .check_lengths(length(x), length(y))
    diff <- suppressWarnings(.unclass(x) - .unclass(y))
    if (tp1 %in% c("i", "n")) return (diff)
    if (tp1 == "h") tp1 <- "t"
    if (tp1 == "t") diff <- round(diff, digits = 6L)
    return (.tdiff(diff, tp1))
}


.t_inc <- function(x, by)
{
    type <- .get.type(x)
    if (!.is.instant(type)) by <- .require_mode(by, "integer")
    .check_lengths(length(x), length(by))
    y <- suppressWarnings(.unclass(x) + by)
    y <- do.call(paste0(".validate_", type), list(y))
    return (.cp_attr(y, x))
}


.t_inc_by_unit <- function(x, n, unit, neg = FALSE)
{
    # there'll be a recursive call
    if (is.tinterval(x)) return (.tint_inc_by_unit(x, n, unit, neg))

    ok <- TRUE
    if (!is.null(unit) && unit == "t") unit <- "s"
    opname <- paste0("%", if (neg) "-" else "+", unit, "%")
    x <- as.tind(x)
    if (!is.numeric(n)) ok <- FALSE else if (neg) n <- -n
    if (!ok) {
        mes <- gettextf("invalid arguments of %s operator", sQuote(opname))
        stop(mes, call. = FALSE, domain = NA)
    }

    type <- .get.type(x)
    by <- .check_inc_x_by_y(type, n, unit)
    tz <- .get.tz(x)

    if (is.null(by$unit)) {
        if (!.is.instant(type)) by$n <- .require_mode(by$n, "integer")
        .check_lengths(length(x), length(by$n))
        y <- suppressWarnings(.unclass(x) + by$n)
    } else {
        by$n <- .require_mode(by$n, "integer")
        y <- do.call(paste0(".inc_", type, "_by_", by$unit),
                     c(list(.unclass(x), by$n), tz))
    }

    y <- do.call(paste0(".validate_", type), list(y))
    return (.tind(y, type, tz))
}


.tint_inc <- function(x, by)
{
    ## FIXME: in corner cases this can lead to incorrect results
    ## e.g.: (as.year(9978) %--% as.year(9987)) + 10 # OK
    ##       (as.year(9978) %--% as.year(9987)) + 20 # OK
    ##       (as.year(9978) %--% as.year(9987)) + 30 # NOT OK
    .check_lengths(length(x$start), length(by))
    s1 <- suppressWarnings(x$start + by)
    e1 <- suppressWarnings(x$end + by)
    return (.tinterval(s1, e1))
}


.tint_inc_by_unit <- function(x, n, unit, neg = FALSE)
{
    ## FIXME: in corner cases this can lead to incorrect results, see .tint_inc
    .check_lengths(length(x$start), length(n))
    s1 <- suppressWarnings(.t_inc_by_unit(x$start, n, unit, neg))
    e1 <- suppressWarnings(.t_inc_by_unit(x$end, n, unit, neg))
    return (.tinterval(s1, e1))
}



# operators `%+-x%`
# ###################################################################

#' @rdname Ops
#' @usage e1 \%+y\% e2
#' @export
`%+y%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "y")

#' @rdname Ops
#' @usage e1 \%-y\% e2
#' @export
`%-y%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "y", neg = TRUE)

#' @rdname Ops
#' @usage e1 \%+q\% e2
#' @export
`%+q%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "q")

#' @rdname Ops
#' @usage e1 \%-q\% e2
#' @export
`%-q%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "q", neg = TRUE)

#' @rdname Ops
#' @usage e1 \%+m\% e2
#' @export
`%+m%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "m")

#' @rdname Ops
#' @usage e1 \%-m\% e2
#' @export
`%-m%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "m", neg = TRUE)

#' @rdname Ops
#' @usage e1 \%+w\% e2
#' @export
`%+w%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "w")

#' @rdname Ops
#' @usage e1 \%-w\% e2
#' @export
`%-w%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "w", neg = TRUE)

#' @rdname Ops
#' @usage e1 \%+d\% e2
#' @export
`%+d%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "d")

#' @rdname Ops
#' @usage e1 \%-d\% e2
#' @export
`%-d%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "d", neg = TRUE)

#' @rdname Ops
#' @usage e1 \%+h\% e2
#' @export
`%+h%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "h")

#' @rdname Ops
#' @usage e1 \%-h\% e2
#' @export
`%-h%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "h", neg = TRUE)

#' @rdname Ops
#' @usage e1 \%+min\% e2
#' @export
`%+min%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "min")

#' @rdname Ops
#' @usage e1 \%-min\% e2
#' @export
`%-min%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "min", neg = TRUE)

#' @rdname Ops
#' @usage e1 \%+s\% e2
#' @export
`%+s%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "s")

#' @rdname Ops
#' @usage e1 \%-s\% e2
#' @export
`%-s%` <- function(e1, e2) .t_inc_by_unit(e1, e2, "s", neg = TRUE)



# diff
# ###################################################################

#' Lagged Differences for \code{tind} and \code{tdiff} Objects
#'
#' \code{diff} method for \code{tind} and \code{tdiff} works in a standard way.
#' For all index types except for integer and numeric indices, differences
#' of time indices are retuned as objects of \code{tdiff} class.
#'
#' @param x an object of \code{tind} class or \code{tdiff} class.
#' @param lag an integer value.
#' @param differences an integer value.
#' @param ... (ignored) further arguments passed to or from other methods.
#'
#' @return An object of \code{tdiff} class, except for \code{x} argument
#' of \code{tind} class of type \code{"i"} or \code{"n"} (integer/numeric
#' indices), in which case an integer or numeric vector is returned.
#'
#' @seealso \link{Ops}.
#'
#' @examples
#' (nn <- sample(1:10))
#' (x <- today() + nn)
#' # all 3 should be the same
#' diff(x, 2, 2)
#' as.tdiff(diff(nn, 2, 2), "d")
#' diff(as.tdiff(nn, "d"), 2, 2)
#'
#' @name diff
#'
NULL


#' @rdname diff
#' @export
diff.tind <- function(x, lag = 1L, differences = 1L, ...)
{
    lag <- as.integer(lag)
    differences <- as.integer(differences)
    if (!is.numeric(lag) || (length(lag) != 1L) || is.na(lag) || (lag < 1L)) {
        mes0 <- gettextf("invalid %s argument", sQuote("lag"))
        mes1 <- gettextf("positive integer expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    if (!is.numeric(differences) || (length(differences) != 1L) ||
        is.na(differences) || (differences < 1L)) {
        mes0 <- gettextf("invalid %s argument", sQuote("differences"))
        mes1 <- gettextf("positive integer expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    n <- length(x)
    if (lag >= n) res <- x[0L] - x[0L]
    else res <- x[(lag + 1):n] - x[1L:(n - lag)]

    if (differences > 1L)
        return (diff(res, lag = lag, differences = differences - 1L))

    return (res)
}


#' @rdname diff
#' @export
diff.tdiff <- function(x, lag = 1L, differences = 1L, ...)
    .tdiff(NextMethod("diff"), .get_tdiff_type(x))



# mean, this also makes median work
# ###################################################################

#' @keywords internal
#' @export
mean.tind <- function(x, na.rm = FALSE, ...)
{
    chkDots(...)
    mn <- min(x, na.rm = na.rm)
    if (is.na(mn)) return (mn)
    return (mn + mean(x - mn, na.rm = na.rm))
}

