#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############################# #
# merge method for time indices #
# ############################# #


#' Merging Time-indexed Data
#'
#' @description
#' \code{merge} method for \code{tind} allows to join two (or more) time-indexed
#' datasets also in cases when the indices are of different types. The method
#' is intended for advanced users.
#'
#' The method takes two \code{tind} vectors (\code{x} and \code{y}) and returns
#' a three-element list containing resulting indices and mappings (integer
#' indices) from the original indices to the final ones allowing to select
#' appropriate rows from dataset indexed by \code{x} and \code{y}, see Examples.
#'
#' @details
#' By default (\code{all = FALSE}), inner join is performed. \code{x} and \code{y}
#' can be indices of different types but conversion of the higher resolution
#' to the lower should be possible.
#'
#' If \code{all.x = TRUE}, left join is performed. All indices from \code{x}
#' are preserved. \code{y} can then be of the same or lower resolution than \code{x}.
#'
#' If \code{all.y = TRUE}, right join is performed. All indices from \code{y}
#' are preserved. \code{x} can then be of the same or lower resolution than \code{y}.
#'
#' If \code{all = TRUE}, outer join is performed. All indices from \code{x}
#' and \code{y} are preserved. Indices in \code{x} and \code{y} have to be
#' of the same type in this case.
#'
#' Setting \code{all} argument silently overrides both \code{all.x} and \code{all.y}.
#'
#' \code{NA}s are \emph{never} matched.
#'
#' The method is optimized in case both indices are strictly increasing without
#' \code{NA}s (time series applications). In other cases, it employs
#' \code{\link[base]{merge}} method for specially constructed data frames.
#'
#' The method also accepts more than two arguments (time indices). In this case,
#' it is expected that all are strictly increasing without \code{NA}s (time
#' series applications only). \code{all.x} and \code{all.y} cannot be used
#' with more than two arguments.
#'
#' \code{all} can be a vector of logical values indicating which indices have
#' to always be included in the result (\code{TRUE}) and which have to be
#' matched (\code{FALSE}). In 2-argument case, for example, \code{all = c(TRUE, FALSE)}
#' is equivalent to \code{all.x = TRUE} and \code{all = c(FALSE, TRUE)}
#' to \code{all.y = TRUE}.
#'
#' @param x,y an object of \code{tind} class.
#' @param ... (optional) further time indices.
#' @param all a logical value, equivalent to setting both \code{all.x}
#'            and \code{all.y} to the same value. Alternatively, a logical
#'            vector in case of more than 2 arguments. See Details.
#' @param all.x a logical value, if \code{TRUE}, all \code{x} observations
#'              are included in the result even if there are no corresponding
#'              time indices in \code{y}.
#' @param all.y a logical value, analogous to \code{all.x}.
#'
#' @return A three-element list with the first element (\code{index}) containing
#' the final time indices, and the remaining two (\code{xi} and \code{yi})
#' mappings from \code{x} and \code{y} to these indices. If additional time
#' indices are provided, the length of the returned list equals the number of
#' all arguments (including \code{x} and \code{y}) plus one (for the final index
#' at the beginning of the list).
#'
#' @seealso \code{\link{match_t}} for matching time indices.
#'
#' @name merge
#'
#' @examples
#' # construct sample data frames
#' (dates1 <- tind(y = 2023, m = rep(1:4, each = 2), d = c(1, 16)))
#' (dates2 <- dates1 %+m% 1)
#' (mnths <- as.month("2022-12") + 0:3)
#' (df1 <- data.frame(dates1, nd1 = as.numeric(dates1),
#'                    downame = day_of_week(dates1, labels = TRUE, abbreviate = FALSE)))
#' (df2 <- data.frame(dates2, nd2 = as.numeric(dates2), dow = day_of_week(dates2)))
#' (df3 <- data.frame(mnths, nm = as.numeric(mnths),
#'                    mname = month(mnths, labels = TRUE, abbreviate = FALSE)))
#' # inner join - dates
#' (mti <- merge(df1[[1L]], df2[[1L]]))
#' data.frame(index = mti[[1L]],
#'            df1[mti[[2L]], -1L, drop = FALSE],
#'            df2[mti[[3L]], -1L, drop = FALSE])
#' # inner join - dates and months
#' (mti <- merge(df1[[1L]], df3[[1L]]))
#' data.frame(index = mti[[1L]],
#'            df1[mti[[2L]], -1L, drop = FALSE],
#'            df3[mti[[3L]], -1L, drop = FALSE])
#' # left join - dates
#' (mti <- merge(df1[[1L]], df2[[1L]], all.x = TRUE))
#' data.frame(index = mti[[1L]],
#'            df1[mti[[2L]], -1L, drop = FALSE],
#'            df2[mti[[3L]], -1L, drop = FALSE])
#' # left join - dates and months
#' (mti <- merge(df1[[1L]], df3[[1L]], all.x = TRUE))
#' data.frame(index = mti[[1L]],
#'            df1[mti[[2L]], -1L, drop = FALSE],
#'            df3[mti[[3L]], -1L, drop = FALSE])
#' # right join - dates
#' (mti <- merge(df1[[1L]], df2[[1L]], all.y = TRUE))
#' data.frame(index = mti[[1L]],
#'            df1[mti[[2L]], -1L, drop = FALSE],
#'            df2[mti[[3L]], -1L, drop = FALSE])
#' # right join - months and dates
#' (mti <- merge(df3[[1L]], df2[[1L]], all.y = TRUE))
#' data.frame(index = mti[[1L]],
#'            df3[mti[[2L]], -1L, drop = FALSE],
#'            df2[mti[[3L]], -1L, drop = FALSE])
#' # outer join - dates
#' (mti <- merge(df1[[1L]], df2[[1L]], all = TRUE))
#' data.frame(index = mti[[1L]],
#'            df1[mti[[2L]], -1L, drop = FALSE],
#'            df2[mti[[3L]], -1L, drop = FALSE])
#'
#' @export
merge.tind <- function(x, y, ..., all = FALSE, all.x = all, all.y = all)
{
    x <- as.tind(x)
    y <- as.tind(y)
    nz <- ...length()

    ## NOTE: merging date-time and time of day is a very special case as
    ## the result of conversion of ordered date-time to time of day is,
    ## in general, not ordered. The wrappers / workarounds below handle
    ## this case without defaulting to the data.frame method, which is
    ## significantly slower. The time of day cases rely on y and table
    ## being ordered (not x).
    .intersect_ord <- function(x, y)
    {
        if ((.get.type(y) != "h") || .Call(C_is_ordered, x, FALSE))
            .Call(C_intersect_ord, x, y)
        i <- .match_exct(x, y, 0L)
        return (y[sort(unique(i))])
    }
    .match_ord <- function(x, table, nomatch = NA_integer_)
    {
        if ((.get.type(table) != "h") || .Call(C_is_ordered, x, FALSE))
            return (.Call(C_match_ord, x, table, nomatch))
        return (.match_exct(x, table, nomatch))
    }

    ## 2-argument case
    if (!nz) {
        if (!missing(all)) {
            lapply(all, function(all) .checkTRUEFALSE(all))
            all <- rep_len(unlist(all), 2L)
            all.x <- all[1L]
            all.y <- all[2L]
        } else {
            .checkTRUEFALSE(all.x)
            .checkTRUEFALSE(all.y)
        }
        all <- all.x && all.y

        xtp <- .get.type(x)
        ytp <- .get.type(y)
        xtz <- .get.tz(x)
        ytz <- .get.tz(y)

        ordered <- .Call(C_is_ordered, x, TRUE) && .Call(C_is_ordered, y, TRUE)

        if (all) { # full join
            if (xtp != ytp) {
                mes <- gettextf("time index type mismatch in %s",
                                sQuote("merge(x, y, all = TRUE)"))
                mes <- paste0(mes, ": ", .ti_type2char(c(xtp, ytp)))
                stop(mes, call. = FALSE, domain = NA)
            }
            if ((xtp == "t") && (xtz != ytz)) {
                .warn_diff_tz(xtz, ytz, first = TRUE)
                if (!ordered) y <- .cast(y, "t", xtz)
            }
            if (ordered) return (.Call(C_merge_out_ord, x, y))
            dx <- data.frame(xi = seq_along(x), index = x)
            dy <- data.frame(yi = seq_along(y), index = y)
            dxy <- merge(dx, dy, all = TRUE, incomparables = NA)
            return (list(index = dxy$index, xi = dxy$xi, yi = dxy$yi))
        } else if (all.x) { # left join
            if (xtp == ytp) {
                if ((xtp == "t") && (xtz != ytz)) .warn_diff_tz(xtz, ytz, first = TRUE)
            } else if (!(ytp %in% .lo_res_cast(xtp)))
                .stop_cast(xtp, ytp, "merge(x, y, all.x = TRUE)")
            if (ordered) {
                yi <- .match_ord(.cast(x, ytp, ytz), y)
                return (list(index = x, xi = seq_along(x), yi = yi))
            }
            dx <- data.frame(xi = seq_along(x), index = x, index0 = .cast(x, ytp, ytz))
            dy <- data.frame(yi = seq_along(y), index0 = y)
            dxy <- merge(dx, dy, all.x = TRUE, incomparables = NA)
            return (list(index = dxy$index, xi = dxy$xi, yi = dxy$yi))
        } else if (all.y) { # right join
            if (xtp == ytp) {
                if ((xtp == "t") && (xtz != ytz)) .warn_diff_tz(ytz, xtz, first = TRUE)
            } else if (!(xtp %in% .lo_res_cast(ytp)))
                .stop_cast(ytp, xtp, "merge(x, y, all.y = TRUE)")
            if (ordered) {
                xi <- .match_ord(.cast(y, xtp, xtz), x)
                return (list(index = y, xi = xi, yi = seq_along(y)))
            }
            dx <- data.frame(xi = seq_along(x), index0 = x)
            dy <- data.frame(yi = seq_along(y), index = y, index0 = .cast(y, xtp, xtz))
            dxy <- merge(dx, dy, all.y = TRUE, incomparables = NA)
            return (list(index = dxy$index, xi = dxy$xi, yi = dxy$yi))
        } else { # inner join
            if (xtp == ytp) {
                if ((xtp == "t") && (xtz != ytz)) {
                    .warn_diff_tz(xtz, ytz, first = TRUE)
                    if (!ordered) y <- .cast(y, "t", xtz)
                }
                if (ordered) return (.Call(C_merge_in_ord, x, y))
                dx <- data.frame(xi = seq_along(x), index = x)
                dy <- data.frame(yi = seq_along(y), index = y)
                dxy <- merge(dx, dy, all = FALSE, incomparables = NA)
                return (list(index = dxy$index, xi = dxy$xi, yi = dxy$yi))
            } else {
                tp <- .max_res(c(xtp, ytp), "merge(x, y, all = FALSE)", weak = TRUE)
                if (xtp != tp) {
                    y2 <- .cast(y, xtp)
                    if (ordered) {
                        z <- .intersect_ord(y2, x)
                        yi <- which(as.logical(.match_ord(y2, z, 0L)))
                        xi <- .match_ord(y2[yi], x)
                        return (list(index = y[yi], xi = xi, yi = yi))
                    }
                    dx <- data.frame(xi = seq_along(x), index0 = x)
                    dy <- data.frame(yi = seq_along(y), index = y, index0 = y2)
                    dxy <- merge(dx, dy, all = FALSE, incomparables = NA)
                    return (list(index = dxy$index, xi = dxy$xi, yi = dxy$yi))
                } else {
                    x2 <- .cast(x, ytp)
                    if (ordered) {
                        z <- .intersect_ord(x2, y)
                        xi <- which(as.logical(.match_ord(x2, z, 0L)))
                        yi <- .match_ord(x2[xi], y)
                        return (list(index = x[xi], xi = xi, yi = yi))
                    }
                    dx <- data.frame(xi = seq_along(x), index = x, index0 = x2)
                    dy <- data.frame(yi = seq_along(y), index0 = y)
                    dxy <- merge(dx, dy, all = FALSE, incomparables = NA)
                    return (list(index = dxy$index, xi = dxy$xi, yi = dxy$yi))
                }
            }
        }
    }

    ## generic case
    # all
    if (!missing(all.x) || !missing(all.y)) {
        mes <- gettextf("%s or %s provided with more than two vectors of time indices",
                        sQuote("all.x"), sQuote("all.y"))
        stop(mes, call. = FALSE, domain = NA)
    }
    lapply(all, function(all) .checkTRUEFALSE(all))
    all <- rep_len(unlist(all), 2L + nz)

    # args
    zs <- lapply(list(...), as.tind)
    args <- c(list(x, y), zs)
    if (!all(sapply(args, function(arg) .Call(C_is_ordered, arg, TRUE)))) {
        mes <- gettextf("ordered time indices expected with more than two vectors of time indices")
        stop(mes, call. = FALSE, domain = NA)
    }
    # do naming of maps here so that we don't have to do it later
    names(args) <- c("xi", "yi", paste0("z", seq_len(nz), "i"))

    # types and time zones
    tps <- sapply(args, .get.type)
    if (any(tps == "t")) {
        tzs <- unique(unlist(lapply(args, .get.tz)))
        tz <- tzs[1L]
        if (length(tzs) > 1L) .warn_diff_tz(tz, first = TRUE)
    } else tz <- NULL

    if (any(all)) {
        utps <- unique(tps[all])
        if (length(utps) > 1L) {
            mes <- gettextf("time index type mismatch in %s",
                            sQuote("merge(x, y, ..., all = TRUE)"))
            mes <- paste0(mes, ": ", .ti_type2char(utps))
            stop(mes, call. = FALSE, domain = NA)
        }
    }

    # index
    if (all(all)) { # full outer join
        ind <- args[[1L]]
        for (i in 2L:length(args)) {
            ind <- .Call(C_union_ord, ind, args[[i]])
        }
        ind <- .tind(ind, tps[1L], tz)
    } else if (!any(all)) { # full inner join
        mr <- .max_res(tps, "merge(x, y, ..., all = FALSE)", weak = TRUE)
        imr <- which.max(tps == mr)
        ind <- args[[imr]]
        for (i in seq_along(args)) {
            if (i == imr) next
            indi <- args[[i]]
            ind0 <- .cast(ind, tps[i])
            intsct <- .intersect_ord(ind0, indi)
            ind <- ind[as.logical(.match_ord(ind0, intsct, 0L))]
        }
    } else { # mixed case
        mr <- .max_res(tps, "merge(x, y, ...)", weak = TRUE)
        iall <- which(all)
        if (tps[iall[1L]] != mr) {
            .stop_cast(mr, tps[iall[1L]], "merge(x, y, ...)")
        }
        ind <- args[[iall[1L]]]
        if (length(iall) >= 2L) for (i in 2L:length(iall)) {
            ind <- .Call(C_union_ord, ind, args[[iall[i]]])
        }
    }

    # maps
    maps <- lapply(args, function(x) .match_ord(.cast(ind, .get.type(x)), x))
    return (c(list(index = ind), maps))
}


