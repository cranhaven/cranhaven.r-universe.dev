#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ########## #
# time spans #
# ########## #


#' Determine Time Span of Time Indices and Time Intervals
#'
#' @description
#' \code{tspan} method determines the time span of time indices and time intervals.
#'
#' @details
#' Given \code{tind} argument, \code{tspan} returns a single \code{tdiff}
#' (or a single number for types \code{"i"} and \code{"n"}) giving time span
#' of indices.
#'
#' \code{tspan} for \code{tinterval} argument returns a \code{tdiff} of the same
#' length as the argument giving spans of time intervals.
#'
#' Time spans are determined differently for time indices representing
#' periods of time (for example days) and for time indices that represent points
#' in time as determined by \code{\link{is.instant}} function. A sequence
#' of 3 consecutive dates has span of 3 days and a sequence of 5 consecutive
#' integers has span 5. On the other hand, a sequence of 3 consecutive hours
#' will have span of 2 hours, the difference between the last and the first hour,
#' see Examples.
#'
#' @param x an object of \code{tind} class (or of other time index class
#'          supported by \pkg{tind} package) or an object of \code{tinterval}
#'          class.
#' @param na.rm a logical value indicating whether missing values should be removed.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A \code{tdiff} or a numeric vector (for integer and numeric indices).
#'
#' @seealso \code{\link{tinterval}} class, \code{\link{is.instant}} function,
#' \code{\link{resolution_t}} method for determining the resolution of time indices,
#' \code{\link{is.regular}} method for checking if time indices form a regular
#' sequence.
#'
#' @examples
#' # 4 consecutive days
#' (x <- today() + 0:3)
#' tspan(x)
#' # 10 consecutive integers
#' (x <- as.tind(1:10, "i"))
#' tspan(x)
#' # 4 consecutive hours
#' (x <- tind(H = 12:15))
#' # span is 3 hours (not 4)
#' tspan(x)
#' # the same
#' tind(H = 15) - tind(H = 12)
#'
#' @name tspan
#'
NULL


#' @rdname tspan
#' @export
tspan <- function(x, ...) UseMethod("tspan")


#' @rdname tspan
#' @export
tspan.default <- function(x, ...)
{
    .check_tind_coercible(x)
    return (tspan.tind(as.tind(x), ...))
}


#' @rdname tspan
#' @export
tspan.tind <- function(x, na.rm = FALSE, ...)
{
    .checkTRUEFALSE(na.rm)
    chkDots(...)
    res <- diff(range(x, na.rm = na.rm)) + !.is.instant(.get.type(x))
    if (!length(x)) res[] <- 0L
    return (res)
}


#' @rdname tspan
#' @export
tspan.tinterval <- function(x, ...)
{
    chkDots(...)
    dx <- x$end - x$start + !.is.instant(.get.type(x$start))
    dx[dx < 0] <- 0L
    return (dx)
}

