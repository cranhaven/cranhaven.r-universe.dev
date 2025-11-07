#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ###################################################### #
# working with ordered and regularly spaced time indices #
# ###################################################### #


#' Working with Ordered and Regularly Spaced Time Indices
#'
#' @description
#' \code{is.ordered_t} method checks if time indices form a strictly
#' increasing sequence without \code{NA} values.
#'
#' \code{is.regular} method checks if time indices form a strictly
#' increasing, regularly spaced sequence without \code{NA} values.
#'
#' \code{as.regular} returns regularly spaced sequence of time
#' indices based on strictly increasing time indices provided.
#'
#' \code{extend_regular} extends strictly increasing sequence of time indices
#' by \code{n} points after the last taking into account the resolution
#' of the sequence provided.
#'
#' @details
#' \code{n} argument of \code{extend_regular} can be negative. In that case
#' \code{-n} points are added before the first index. The function may
#' fail in corner cases (if the result would be out of range).
#'
#' Creating regular date-time sequences in front of DST/UTC offset changes
#' can be impossible. If the algorithm fails, an error is issued. In general,
#' this should not be a problem when DST change is by 1 hour and the resolution
#' of the indices is 1 hour or higher.
#'
#' @param x an object of \code{tind} class or of other time index class
#'          supported by \pkg{tind} package.
#' @param n an integer value, number of time stamps to be added, see Details.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A logical value for \code{is.ordered_t} and \code{is.regular}.
#' An object of \code{tind} class for \code{as.regular} and \code{extend_regular}.
#'
#' @seealso
#' \code{\link{resolution_t}} method.
#'
#' @examples
#' # months, resulution 2m
#' (ms <- tind(y = 2023, m = 1 + 2 * (0:5)))
#' is.regular(ms)
#' extend_regular(ms, 3)
#' (ms <- tind(y = 2023, m = c(1, 3, 5, 9)))
#' is.regular(ms)
#' as.regular(ms)
#' # date, resulution 15d
#' (ds <- tind(y = 2024, m = rep(1:3, each = 2), d = c(1, 16)))
#' is.regular(ds)
#' extend_regular(ds, -4)
#' (ds <- ds[-2L])
#' is.regular(ds)
#' as.regular(ds)
#'
#' # corner cases
#' tz <- "Europe/Warsaw"
#' if (tz %in% OlsonNames()) {
#'     # switch to DST
#'     print(hours_in_day("2025-03-30", tz = tz))
#'     # this will work with step from 1am to 3am
#'     tt <- date_time("2025-03-30", H = c(0, 4:8), tz = tz)
#'     print(resolution_t(tt))
#'     as.regular(tt)
#' }
#' if (tz %in% OlsonNames()) {
#'     # this will fail due to missing 2am
#'     tt <- date_time("2025-03-30", H = c(0, 4, 6, 8), tz = tz)
#'     print(resolution_t(tt))
#'     try(as.regular(tt))
#' }
#' if (tz %in% OlsonNames()) {
#'     # this will work again (step by 4h)
#'     tt <- date_time("2025-03-30", H = c(0, 4, 12), tz = tz)
#'     print(resolution_t(tt))
#'     as.regular(tt)
#' }
#'
#' @name ordered-regular
#'
NULL


#' @rdname ordered-regular
#' @export
is.ordered_t <- function(x) UseMethod("is.ordered_t")


#' @keywords ordered-regular
#' @export
is.ordered_t.default <- function(x)
{
    .check_tind_coercible(x)
    return (is.ordered_t.tind(as.tind(x)))
}


#' @rdname ordered-regular
#' @export
is.ordered_t.tind <- function(x) .Call(C_is_ordered, x, TRUE)


#' @rdname ordered-regular
#' @export
is.regular <- function(x) UseMethod("is.regular")


#' @keywords ordered-regular
#' @export
is.regular.default <- function(x)
{
    .check_tind_coercible(x)
    return (is.regular.tind(as.tind(x)))
}


#' @rdname ordered-regular
#' @export
is.regular.tind <- function(x)
{
    if (!.Call(C_is_ordered, x, TRUE)) return (FALSE)
    if (length(x) <= 1L) return (TRUE)

    rx <- resolution_t(x)
    type <- .get.type(x)

    # n
    if (type == "n") {
        if (!is.finite(rx)) return (FALSE)
        x <- as.numeric(x)
        return (max(abs(diff(x) / rx - 1)) < .Machine$double.eps^.5)
    }
    # i
    if (type == "i") return (all(diff(x) == rx))
    # h
    if (type == "h") return (.Call("is_regular_t", x, rx))
    # t, d, w, m, q, y
    type <- .get_tdiff_type(rx)
    x <- .cast(x, type)
    if (type == "t") {
        ## FIXME: The code below might not work correctly for time zones in which
        ## some days do not start at full hours or do not last multiples of hours.
        if (rx <= hours(1)) return (.Call("is_regular_t", x, rx))
        return (.Call("is_regular_th", as.time(x), rx))
    }
    if (type %in% c("d", "w")) {
        dx <- diff(x)
        if (rx == 1) return (all(dx == 1))
        # rx = 15d, 2w, 4w, 13w, 26w
        return (all(dx <= rx + 1))
    }
    return (all(diff(x) == rx))
}


#' @rdname ordered-regular
#' @export
as.regular <- function(x, ...) UseMethod("as.regular")


#' @keywords ordered-regular
#' @export
as.regular.default <- function(x, ...)
{
    .check_tind_coercible(x)
    return (as.regular.tind(as.tind(x)))
}


#' @rdname ordered-regular
#' @export
as.regular.tind <- function(x, ...)
{
    x <- as.tind(x)
    if (!is.ordered_t(x)) {
        mes <- gettextf("ordered time indices expected")
        stop(mes, call. = FALSE, domain = NA)
    }

    if (is.regular.tind(x)) return (x)

    type <- .get.type(x)
    tz <- .get.tz(x)
    rng <- range(x)
    rx <- resolution_t(x)

    if (type %in% c("i", "n", "h")) {
        if (is.na(rx)) {
            mes <- gettextf("resolution cannot be determined")
            stop(mes, call. = FALSE, domain = NA)
        }
        typer <- type
    } else {
        typer <- .get_tdiff_type(rx)
        x <- .cast(x, typer)
        rng <- .cast(rng, typer)
    }

    if ((type == "t") && (typer == "t")) {
        ## FIXME: The code below might not work correctly for time zones in which
        ## some days do not start at full hours or do not last multiples of hours.
        if (rx <= hours(1)) {
            return (seq.tind(rng[1L], rng[2L], rx))
        } else {
            res <- seq.tind(rng[1L], rng[2L], "1h")
            res <- unique(floor_t(res, rx))
            if (!is.regular(res)) {
                mes <- gettextf("failed to construct regularly spaced time indices")
                stop(mes, call. = FALSE, domain = NA)
            }
            return (.tind(res, type, tz))
        }
    } else if (typer %in% c("w", "d")) {
        res <- seq.tind(rng[1L], rng[2L])
        if (rx != 1) res <- unique(floor_t(res, rx))
    } else { # regular cases
        res <- seq.tind(rng[1L], rng[2L], rx)
    }

    return (if (type == typer) res else .cast(res, type, tz))
}


#' @rdname ordered-regular
#' @export
extend_regular <- function(x, n)
{
    x <- as.tind(x)
    if (!is.ordered_t(x)) {
        mes <- gettextf("ordered time indices expected")
        stop(mes, call. = FALSE, domain = NA)
    }
    if (!length(x)) {
        mes <- gettextf("argument of length 0")
        stop(mes, call. = FALSE, domain = NA)
    }
    if ((length(n) != 1L) || !is.numeric(n) ||
        is.na(suppressWarnings(as.integer(n)))) {
        mes0 <- gettextf("invalid %s argument", sQuote("n"))
        mes1 <- gettextf("single non-NA integer expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    n <- as.integer(n)
    if (!n) return (x[0L])

    type <- .get.type(x)
    tz <- .get.tz(x)
    rng <- range(x)
    rx <- resolution_t(x)

    if (type %in% c("i", "n", "h")) {
        if (is.na(rx)) {
            mes <- gettextf("resolution cannot be determined")
            stop(mes, call. = FALSE, domain = NA)
        }
        typer <- type
    } else {
        typer <- .get_tdiff_type(rx)
        x <- .cast(x, typer)
        rng <- .cast(rng, typer)
    }
    if (!is.regular(x)) {
        mes <- gettextf("original time indices are not regular")
        warning(mes, call. = FALSE, domain = NA)
    }

    if ((type == "t") && (typer == "t") && (rx <= hours(1))) {
        ## FIXME: The code below might not work correctly for time zones in which
        ## some days do not start at full hours or do not last multiples of hours.
        res <- if (n > 0L) rng[2L] + (1L:n) * rx else rng[1L] + (n:-1L) * rx
    } else if ((type == "t") && (typer == "t") || (typer %in% c("w", "d")) && (rx > 1)) {
        nun <- .tdiff2nunit(rx)
        res <- if (n > 0L) rng[2L] + as.tdiff(1L:(nun$n * n + n), nun$unit)
               else rng[1L] + as.tdiff((nun$n * n + n):-1L, nun$unit)
        res <- unique(floor_t(res, rx))
        ires <- if (n > 0L) seq.int(from = 2L, length.out = n)
                else seq.int(to = length(res), length.out = -n)
        res <- res[ires]
        if (type == "t") {
            check <- if (n > 0) c(x[length(x)], res) else c(res, x[1L])
            if (!is.regular(check)) res[c(1L, abs(n))] <- NA
        }
    } else { # regular cases
        res <- if (n > 0L) rng[2L] + (1L:n) * rx else rng[1L] + (n:-1L) * rx
    }

    if (type != typer) res <- .cast(res, type, tz)
    if (anyNA(res[c(1L, abs(n))])) {
        mes <- gettextf("failed to regularly extend time indices")
        stop(mes, call. = FALSE, domain = NA)
    }
    return (res)
}

