#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ####################### #
# pretty time axis labels #
# ####################### #


#' Pretty Breakpoints for Time Indices
#'
#' @description
#' Determine locations of pretty breakpoints for time indices.
#' \code{pretty} method for objects of \code{tind} class employs separate
#' algorithms for each type / resolution, see Details.
#'
#' @details
#' Resolution of ticks (see \code{\link{resolution_t}} method) is always the same or
#' lower than the resolution of the argument and lower resolutions have to be
#' multiples of the resolution of the argument. This way, the ticks are never
#' placed, for example, every 5 years for indices with a 2-year resolution.
#'
#' For years, the tics are placed at powers of 10 times 1, 2, or 5.
#'
#' For quarters, ticks are placed every quarter, every second quarter (1st and 3rd),
#' or on first the quarters of years selected by separate procedure for years.
#'
#' For months, ticks are placed every 1, 2, 3, 4, 6 months or on January
#' of years selected by the separate procedure for years.
#'
#' For weeks, ticks are placed every 1, 2, 4, 13, 26 weeks or on the first weeks
#' of years selected by the separate procedure for years.
#'
#' For dates, depending on the number of observations, ticks can be placed:
#' \itemize{
#'     \item every day,
#'     \item every Monday, Wednesday, and Friday,
#'     \item every Monday and Thursday,
#'     \item every Monday,
#'     \item every 1st and 16th day of a month,
#'     \item on 1st days of months selected by the separate procedure for months.
#' }
#'
#' For date-time and time of day, the placement of ticks depends on the resolution
#' of indices. When all indices are at full hours, ticks are placed
#' at full hours only. Similar approach is taken for minutes and seconds. Divisors
#' of 24 are used for hours and divisors of 60 for minutes and seconds.
#' For date-time indices spanning more than a couple of days, ticks are placed
#' on midnights of days selected by the separate procedure for dates.
#'
#' Due to the design of the algorithm, in some corner cases (esp. for time of
#' day and weeks) the number of intervals might differ significantly from
#' the expected number \code{n}.
#'
#' @param x an object of \code{tind} class.
#' @param n an integer value giving the expected number of intervals.
#' @param min.n an integer value giving the minimal number of intervals.
#' @param ... (ignored) further arguments passed to or from other methods
#'
#' @return An object of \code{tind} class.
#'
#' @seealso
#' \code{\link{resolution_t}} method, \code{\link{axis_t}} for computing
#' time axis parameters for plotting, \code{\link{axis.tind}} for creating axes
#' with \pkg{graphics} package, \code{\link{scale_tind}} for creating axes
#' with \pkg{ggplot2}.
#'
#' @name pretty
#'
#' @examples
#' (td <- tind(y = sample(2010:2018, 4, replace = TRUE),
#'             m = sample(1:12, 4, replace = TRUE),
#'             d = sample(1:2, 4, replace = TRUE)))
#' pretty(td)
#' pretty(td, 3)
#' pretty(td, 10)
#' (th <- tind(H = sample(0:23, 4, replace = TRUE),
#'             M = sample(0:3 * 15, 4, replace = TRUE)))
#' pretty(th)
#' pretty(th, 3)
#' pretty(th, 10)
#' (tdt <- date_time(td[1], th))
#' pretty(tdt)
#' pretty(tdt, 3)
#' pretty(tdt, 10)
#'
#' @export
pretty.tind <- function(x, n = 5L, min.n = n %/% 2L, ...)
{
    n <- as.integer(n)
    min.n <- as.integer(min.n)
    if ((length(n) != 1) || is.na(n) || (n < 0)) {
        mes0 <- gettextf("invalid %s argument", sQuote("n"))
        mes1 <- gettextf("nonnegative integer expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    if ((length(min.n) != 1) || is.na(min.n) || (min.n < 0)) {
        mes0 <- gettextf("invalid %s argument", sQuote("min.n"))
        mes1 <- gettextf("nonnegative integer expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    if (min.n > n) {
        mes <- gettextf("%s greater than %s", sQuote("min.n"), sQuote("n"))
        stop(mes, call. = FALSE, domain = NA)
    }

    if (anyNA(x)) x <- x[!is.na(x)]
    if (!length(x)) return (x)

    type <- .get.type(x)
    tz <- .get.tz(x)
    res <- do.call(paste0(".pretty_", type),
                   c(list(as.vector(x)), tz, as.integer(n), as.integer(min.n)))
    # handle corner cases
    res <- do.call(paste0(".validate_", type), list(res))
    res <- res[!is.na(res)]
    return (.tind(res, type, tz))
}

