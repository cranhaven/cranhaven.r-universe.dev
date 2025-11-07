#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ########################## #
# resolution of time indices #
# ########################## #


#' Determine the Resolution of Time Indices
#'
#' @description
#' \code{resolution_t} method determines resolution of time indices.
#'
#' For time index types other than integer index (\code{"i"}) and numeric index
#' (\code{"n"}), \code{resolution_t} returns an object of \code{tdiff} class.
#' The following multiples of units can be returned by \code{resolution_t} method:
#' \describe{
#'   \item{\code{"y"} (years):}{1, 2, 5, 10, 20, 50, 100, 200, 500, 1000.}
#'   \item{\code{"q"} (quarters):}{1, 2.}
#'   \item{\code{"m"} (months):}{1, 2, 3, 4, 6.}
#'   \item{\code{"w"} (weeks):}{1, 2, 4, 13, 26.}
#'   \item{\code{"d"} (days):}{1, 15 (1st and 16th day of a month).}
#'   \item{\code{"h"} (hours):}{1, 2, 3, 4, 6, 8, 12.}
#'   \item{\code{"min"} (minutes):}{1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30.}
#'   \item{\code{"s"} (seconds):}{1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30 and
#'                                1, 2, or 5 times negative powers of 10.}
#' }
#'
#' Basic resolution (1) for given type is always returned for vectors with 1 or no
#' non-\code{NA} value.
#'
#' An integer value is returned for type \code{"i"} (integer index) and
#' a numeric value (possibly \code{NA_real_}) for type \code{"n"} (numeric index).
#'
#' @param x an object of \code{tind} class or of other time index class
#'          supported by \pkg{tind} package.
#'
#' @return For all types except for integer index (\code{"i"}) and numeric index
#' (\code{"n"}) \code{resolution_t} returns an object of \code{tdiff} class.
#' An integer value for type \code{"i"} (integer index) and a numeric value
#' (possibly \code{NA_real_}) for type \code{"n"} (numeric index) are returned.
#'
#' @seealso \link{rounding} for rounding time indices to specified resolution,
#' \code{\link{is.regular}} method for checking if time indices form a regular
#' sequence, \code{\link{tspan}} method for determining time span of indices.
#'
#' @examples
#' (ds <- tind(y = 2024, m = rep(1:3, each = 2), d = c(1, 16)))
#' resolution_t(ds)
#' (ms <- tind(y = 2023, m = 1 + 2 * (0:5)))
#' resolution_t(ms)
#' (th <- tind(H = 13, M = (0:3) * 15))
#' resolution_t(th)
#' (dt <- tind(y = 2025, m = 2, d = 1, H = 13, M = 27, S = (0:5) * 10))
#' resolution_t(dt)
#'
#' @name resolution_t
#'
NULL


#' @rdname resolution_t
#' @export
resolution_t <- function(x) UseMethod("resolution_t")


#' @keywords internal
#' @export
resolution_t.default <- function(x)
{
    .check_tind_coercible(x)
    return (resolution_t.tind(as.tind(x)))
}


#' @rdname resolution_t
#' @export
resolution_t.tind <- function(x)
{
    res <- do.call(paste0(".res_", .get.type(x)), c(list(x), .get.tz(x)))
    if (.get.type(x) %in% c("i", "n")) return (res)
    return (as.tdiff(res[[1L]], res[[2L]]))
}

