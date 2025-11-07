#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ######################### #
# sequences of time indices #
# ######################### #


## NOTE: both seq method and seq.tind are exported allowing for implicit
## conversion to tind
#' @rawNamespace S3method(seq,tind,seq.tind)
#' @rawNamespace export(seq.tind)
NULL


#' Create a Sequence of Time Indices
#'
#' @description
#' \code{seq} method for objects of \code{tind} class allows to easily construct
#' sequences of time indices of all supported types.
#'
#' @details
#' \code{seq} method requires that exactly two of the three arguments \code{from},
#' \code{to}, and \code{length.out} are provided. If \code{along.with} is not
#' \code{NULL}, its length is used as value of \code{length.out}.
#'
#' \code{by} can be a number, an object of \code{tdiff} class (of length 1), or an object
#' coercible to \code{tdiff} like \code{"3w"} denoting step by three weeks.
#' \code{by} cannot be \code{NA} and cannot be 0 when both \code{from} and \code{to} are
#' provided. Given both \code{from} and \code{to}, sign of \code{by}
#' has to agree with the order of \code{from} and \code{to}.
#' When \code{by} is a number, the underlying unit of time is assumed. For time of day
#' and date-time indices this is always a second.
#'
#' \code{from} and \code{to} can be of different types provided that conversion is possible.
#' The result is of higher resolution. This allows to easily construct series like
#' from begging of the month to today, from today till the end of next year, etc.
#' See Examples.
#'
#' \code{seq.tind} slightly differs from \code{\link[base]{seq.Date}}
#' in terms of interface and requirements with respect to arguments.
#' Firstly, \code{from} argument can be missing (provided that \code{to} and
#' \code{length.out} are given). Secondly, \code{by} has the default value of 1.
#'
#' Both \code{seq} method for \code{tind} and \code{seq.tind} function are
#' exported allowing for conversion to \code{tind} as in
#' \code{seq.tind("2025-01", "2025-12")}.
#'
#' @param from an object of \code{tind} class or an R object coercible to it.
#' @param to an object of \code{tind} class or an R object coercible to it.
#' @param by a numeric value, a \code{tdiff}, or a character string determining increment.
#' @param length.out an integer value, the desired length.
#' @param along.with any R object, length of this argument will be taken as the desired length.
#' @param ... (ignored) further arguments passed to or from other methods.
#'
#' @return An object of \code{tind} class.
#'
#' @name seq
#'
#' @examples
#' # sequences of dates by 1 and 2 months from now
#' (td <- today())
#' seq(td, by = "1m", length.out = 12)
#' seq(td, by = "2m", length.out = 6)
#' # sequences of dates by 1 and 2 months to now
#' seq(to = td, by = "1m", length.out = 12)
#' seq(to = td, by = "2m", length.out = 6)
#' # sequence of dates from the beginning of the month till today
#' seq(floor_t(td, "m"), td)
#' # same
#' seq(as.month(td), td)
#' # sequence of dates from today till the end of the next month
#' seq(td, as.month(td) + 1)
#' # sequence of date-time from now to midnight by 1 hour
#' (nw <- now())
#' seq(nw, ceiling_t(nw, "1d"), by = "1h")
#' # same
#' seq(nw, as.date(nw), by = "1h")
#' # sequence (date-time) from full hour to now by 2 minutes
#' seq(floor_t(nw, "1h"), nw, by = "2min")
#' # sequence (time of day) from full hour to now by 2 minutes
#' seq(floor_t(as.time(nw), "1h"), as.time(nw), by = "2min")
#' # sequence (date-time) from now down to full hour by 2 minutes
#' seq(nw, floor_t(nw, "1h"), by = "-2min")
#' # sequence (time of day) from now down to full hour by 2 minutes
#' seq(as.time(nw), floor_t(as.time(nw), "1h"), by = "-2min")
#' # sequence (date-time) of length 10 from now down by 10 seconds
#' seq(nw, by = -10, length.out = 10)
#' # sequence (time of day) of length 10 from now down by 10 seconds
#' seq(as.time(nw), by = -10, length.out = 10)
#' # explicit call to seq.tind with conversion
#' seq.tind("2025-01", "2025-12")
#' ## corner cases
#' # from 2025-12-30 23:00 till end of 2025, note that 2025-12-31 24:00
#' # (that is 2025-01-01 00:00) is excluded from the result as it is in the next year
#' seq(as.tind("2025-12-30 23:00", tz = "UTC"), "2025", by = "5h")
#' # from end of 2025 down to 2025-12-30 23:00, note that 2025-12-31 24:00
#' # (that is 2025-01-01 00:00) is excluded from the result as it is in the next year
#' seq(as.tind("2025"), as.tind("2025-12-30 23:00", tz = "UTC"), by = "-5h")
#'
seq.tind <- function(from, to, by = 1, length.out = NULL, along.with = NULL, ...)
{
    missfrom <- missing(from)
    missto <- missing(to)
    # check args
    if (missfrom && missto) {
        mes <- gettextf("%s and %s arguments missing", sQuote("from"), sQuote("to"))
        stop(mes, call. = FALSE, domain = NA)
    }
    if (!missfrom) {
        from <- as.tind(from)
        if ((length(from) != 1L) || is.na(from)) {
            mes1 <- gettextf("invalid %s argument", sQuote("from"))
            mes2 <- gettextf("single non-NA index expected")
            stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
        }
    }
    if (!missto) {
        to <- as.tind(to)
        if ((length(to) != 1L) || is.na(to)) {
            mes1 <- gettextf("invalid %s argument", sQuote("to"))
            mes2 <- gettextf("single non-NA index expected")
            stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
        }
    }
    if (!missing(along.with)) length.out <- length(along.with)
    if (is.null(length.out)) {
        if (missto) {
            mes <- gettextf("%s and %s arguments missing", sQuote("to"),
                            sQuote("length.out"))
            stop(mes, call. = FALSE, domain = NA)
        }
        if (missfrom) {
            mes <- gettextf("%s and %s arguments missing", sQuote("from"),
                            sQuote("length.out"))
            stop(mes, call. = FALSE, domain = NA)
        }
    } else {
        if (!missfrom && !missto) {
            mes <- gettextf("%s, %s, and %s arguments provided",
                            sQuote("from"), sQuote("to"), sQuote("length.out"))
            stop(mes, call. = FALSE, domain = NA)
        }
        if (length(length.out) != 1 || !is.numeric(length.out) ||
            !is.finite(length.out) || length.out < 0) {
            mes1 <- gettextf("invalid %s argument", sQuote("length.out"))
            mes2 <- gettextf("non-negative integer expected")
            stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
        }
    }

    # check by arg
    if (!is.numeric(by)) by <- as.tdiff(by)
    by <- .tdiff2nunit(by)

    # missing to
    if (missto) {
        by <- .check_inc_x_by_y(.get.type(from), by$n, by$unit)
        incs <- seq(from = 0, by = by$n, length.out = length.out)
        return (.t_inc_by_unit(from, incs, by$unit))
    }

    # missing from
    if (missfrom) {
        by <- .check_inc_x_by_y(.get.type(to), by$n, by$unit)
        incs <- seq(to = 0, by = by$n, length.out = length.out)
        return (.t_inc_by_unit(to, incs, by$unit))
    }

    # from and to provided
    ftp <- .get.type(from)
    ttp <- .get.type(to)
    tp <- .max_res(c(ftp, ttp))
    by <- .check_inc_x_by_y(tp, by$n, by$unit)
    if (by$n == 0) {
        mes <- gettextf("%s argument equals 0", sQuote("by"))
        stop(mes, call. = FALSE, domain = NA)
    }

    # adjust from and to
    to0 <- to
    from0 <- from
    if (ftp == ttp) {
        if (tp == "t" && .get.tz(from) != .get.tz(to)) {
            .warn_diff_tz(.get.tz(from), .get.tz(to), TRUE)
            to0 <- to <- .cast(to, "t", tz = .get.tz(from))
        }
    } else if (ftp == tp) {
        tz <- .get.tz(from)
        to <- if (by$n > 0) .cast(to + 1, tp, tz) - (tp != "t")
              else .cast(to, tp, tz)
    } else { # ttp == tp
        tz <- .get.tz(to)
        from <- if (by$n > 0) .cast(from, tp, tz)
                else .cast(from + 1, tp, tz) - (tp != "t")
    }
    # check by
    if ((from > to) && (by$n > 0) || (from < to) && (by$n < 0)) {
        mes <- gettextf("wrong sign in %s argument", sQuote("by"))
        stop(mes, call. = FALSE, domain = NA)
    }

    # result
    if (is.null(by$unit)) {
        res <- seq(from = .unclass(from), to = .unclass(to), by = by$n)
        if (tp %in% c("t", "h")) res <- round(res, 6L)
        res <- .require_mode(res, .mode(tp))
        res <- .cp_attr(res, from)
    } else {
        df <- .unclass(diff(.cast(c(from, to), by$unit)))
        incs <- seq(from = 0, to = df, by = by$n)
        res <- .t_inc_by_unit(from, incs, by$unit)
    }

    # trim result in corner cases
    if (tp == "t") {
        n <- length(res)
        if ((by$n < 0) && (res[1L] > from0)) res <- res[-1L]
        if ((by$n > 0) && (res[n] > to0)) res <- res[-n]
    }

    return (res)
}

