#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################### #
# cut method for tind #
# ################### #


#' Group Time Indices into Periods / Convert to a Factor
#'
#' @description
#' \code{cut} method for objects of \code{tind} class allows to map / group
#' time indices into periods. The periods can be determined based on indices
#' provided by the user or by (multiples of) units of time.
#'
#' @details
#' \code{breaks} argument controls how indices are grouped. It can be a number
#' or a character string determining resolution (or an object of \code{tdiff} class).
#' Alternatively, \code{breaks} can be an object of \code{tind} class with cut points.
#'
#' When \code{breaks} determines resolution, only selected multiples of units
#' are allowed, similarly to \code{floor_t} function. Documentation of admissible
#' units and multiples can be found in Details section of \code{\link{resolution_t}}
#' method documentation. If selected resolution corresponds to an index of different
#' type (for example grouping dates to 2-month periods), conversion takes place.
#'
#' This method differs from \code{\link[base]{cut.POSIXt}} and \code{\link[base]{cut.Date}}
#' in two aspects. Firstly, the periods are selected differently, they are always
#' aligned to resolution, see Examples. Secondly, as it does not rely
#' on \code{seq} but rounding of indices, the levels may be discontinuous. If users want
#' to replicate behaviour of \code{cut} from \pkg{base}, they should provide \code{tind}
#' constructed via \code{\link{seq.tind}} method as \code{breaks} argument.
#'
#' When \code{breaks} is a \code{tind} object, it is expected to be sorted
#' without \code{NA}s. By default, indices in \code{x}
#' are matched to the closest index to the left (largest index that is not greater
#' than the argument). If \code{right} is set to \code{TRUE}, indices are matched
#' to the closest index to the right (smallest index that is not smaller than
#' the argument). \code{right} cannot be set to \code{TRUE} if \code{breaks}
#' is not a \code{tind}. It is acceptable that \code{breaks} is of lower resolution
#' than \code{x} provided that \code{x} is convertible to it. In such situations,
#' \code{right} cannot be set to \code{TRUE}.
#'
#' By default, \code{cut.tind} returns a factor with levels created using
#' \code{as.character} method. If \code{labels} argument is set
#' to \code{FALSE}, only the integer vector (of the same length as argument)
#' of mappings to intervals is returned (as in \pkg{base} method).
#' If set to \code{NA}, a 2-element list is returned, with integer vector
#' of mappings as the first element and time indices determining intervals
#' (grouping, levels) as the second.
#' \code{labels} can only take \code{TRUE/FALSE/NA} values.
#'
#' @param x an object of \code{tind} class.
#' @param breaks a numeric value or a character string determining intervals,
#'               or an object of \code{tind} class with cut points, see Details.
#' @param labels a logical value controlling the return type, which
#'               can be a factor (if \code{TRUE}, the default), integer vector,
#'               or a 2-element list.
#' @param right a logical value determing whether indices should be matched
#'              to the closest left cut point or to the closest right cut point,
#'              see Details.
#' @param ... (ignored) further arguments passed to or from other methods.
#'
#' @return A factor if \code{labels} is \code{TRUE}, an integer vector if \code{FALSE},
#' and a 2-element list if \code{NA}, see Details.
#'
#' @seealso \link{rounding} and \code{\link{resolution_t}} for description
#' of admissible units and multiples that can be used for \code{breaks} argument.
#' \code{\link{match_t}} for matching time indices to other indices and time
#' intervals.
#'
#' @name cut
#'
#' @examples
#' # basic use
#' (d <- seq.tind("2023-09-14", "2023-12-16"))
#' cut(d, "15d")
#' cut(d, "m")
#' cut(d, "2m")
#' # tind given as breaks
#' cut(d, as.date(c("2023-09-01", "2023-11-16", "2023-12-16")))
#' cut(d, seq.tind("2023-01", "2023-12"))
#' # random order with NAs
#' (d <- sample(c(d, NA)))
#' cut(d, "15d")
#' cut(d, "m")
#' cut(d, "2m")
#' # different behaviour of cut for tind and Date (alignment to 2 month resolution,
#' # which means Jan, Mar, May, Jul, Sep, Nov)
#' (d <- seq.tind("2023-12-16", "2024-03-01"))
#' cut(d, "2 months")
#' cut(as.Date(d), "2 months")
#' # replicate behviour of cut.Date by providing sequence of months
#' cut(d, seq.tind("2023-12", "2024-03", by = "2m"))
#' # same
#' cut(d, seq.tind(as.month(min(d)), as.month(max(d)), by = "2m"))
#' # check
#' all.equal(cut(as.Date(d), "2 months", labels = FALSE),
#'           cut(d, seq.tind("2023-12", "2024-03", by = "2m"), labels = FALSE))
#'
#' @export
#'
cut.tind <- function(x, breaks, labels = TRUE, right = FALSE, ...)
{
    if (!is.logical(labels) || (length(labels) != 1L)) {
        mes0 <- gettextf("invalid %s argument", sQuote("labels"))
        mes1 <- gettextf("TRUE/FALSE/NA expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    .checkTRUEFALSE(right)

    x <- as.tind(x)
    xtype <- .get.type(x)
    xtz <- .get.tz(x)

    if (is.tind(breaks)) {
        brtype <- .get.type(breaks)
        brtz <- .get.tz(breaks)
        if (xtype == brtype) {
            if ((xtype == "t") && (xtz != brtz)) .warn_diff_tz(xtz, brtz)
        } else if (right) {
            mes <- gettextf("time index type mismatch in %s with %s set to %s",
                            sQuote("cut.tind"), sQuote("right"), "TRUE")
            stop(mes, call. = FALSE, domain = NA)
        } else if (!(brtype %in% .lo_res_cast(xtype))) {
            .stop_cast(xtype, brtype)
        } else x <- as.tind(x, type = brtype)
        if (!.Call(C_is_ordered, breaks, TRUE)) {
            mes0 <- gettextf("invalid %s argument", sQuote("breaks"))
            mes1 <- gettextf("ordered, non-duplicated indices without NAs expected")
            stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
        }
        map <- if (right) .match_right(x, breaks) else .match_left(x, breaks)
    } else {
        if (right) {
            mes <- gettextf("%s cannot be set to %s if %s is not of %s class",
                            sQuote("right"), "TRUE", sQuote("breaks"),
                            dQuote("tind"))
            stop(mes, call. = FALSE, domain = NA)
        }

        if (!is.numeric(breaks)) breaks <- as.tdiff(breaks)
        nunit <- .tdiff2nunit(breaks)
        nunit <- .check_fl_ceil_type_unit(xtype, nunit)

        xb <- if (xtype != nunit$type) .cast(x, nunit$type) else x
        fname <- if (nunit$type %in% c("t", "h")) sprintf(".floor_%s_", nunit$type)
                 else ".floor_"
        fname <- paste0(fname, nunit$unit)
        xb <- .cp_attr(do.call(fname, c(list(as.vector(xb), nunit$n),
                                        .get.tz(xb))), xb)
        if (.Call(C_is_ordered, xb, FALSE)) {
            breaks <- .Call(C_unique_ord, xb)
            map <- .Call(C_match_ord, xb, breaks, NA_integer_)
        } else {
            breaks <- if (anyNA(xb)) xb[!is.na(xb)] else xb
            breaks <- sort(unique(breaks))
            map <- match(xb, breaks)
        }
    }

    if (isTRUE(labels)) {
        attributes(map) <- list(class = c("ordered", "factor"), levels = format(breaks))
        return (map)
    }
    if (isTRUE(!labels)) return (map)
    return (list(map, breaks))
}

