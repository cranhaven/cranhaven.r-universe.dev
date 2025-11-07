#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############################## #
# tind class and its constructor #
# ############################## #


# tind constructor
# ###################################################################

#' A Common Representation of Time Indices of Different Types
#'
#' @description
#' \code{tind} is an S3 class representing time indices of different
#' types (years, quarters, months, ISO 8601 weeks, dates, date-time,
#' and arbitrary integer/numeric indices). Time indices are represented
#' by vectors of integers or doubles with type attribute and
#' time zone attribute (date-time only). Objects of \code{tind} behave like plain
#' vectors and can be easily used in data frames.
#'
#' A \code{tind} object would usually be created using \code{\link{as.tind}}
#' method or using \code{\link{parse_t}} and \code{\link{strptind}} functions.
#' \code{tind} constructor allows to create time indices from components
#' (like year, month, day) and to create vectors of a given length
#' filled with \code{NA} values.
#'
#' \code{is.tind} function checks whether an object is of \code{tind} class.
#'
#' @details
#' \code{tind} class supports the following types of time indices:
#' \describe{
#'     \item{years}{internal code \code{"y"}.}
#'     \item{quarters}{internal code \code{"q"}.}
#'     \item{months}{internal code \code{"m"}.}
#'     \item{weeks}{internal code \code{"w"}.}
#'     \item{dates}{internal code \code{"d"}.}
#'     \item{date-time}{internal code \code{"t"}.}
#'     \item{time of day}{internal code \code{"h"}.}
#'     \item{arbitrary integer index}{\code{"i"}.}
#'     \item{arbitrary numeric index}{\code{"n"}.}
#' }
#'
#' Valid ranges for time indices are:
#' \describe{
#'      \item{years (\code{"y"})}{\code{0000}--\code{9999}.}
#'      \item{quarters (\code{"q"})}{\code{0000q1}--\code{9999q4}.}
#'      \item{months (\code{"m"})}{\code{0000-01}--\code{9999-12}.}
#'      \item{weeks (\code{"w"})}{\code{0000-W01}--\code{9999-W52}.}
#'      \item{dates (\code{"d"})}{\code{0000-01-01}--\code{9999-12-31}.}
#'      \item{date-time (\code{"t"})}{from \code{0000-01-01 15:00:00Z} to \code{9999-12-31 09:00:00Z}
#'                  (between \code{-62167165200} and \code{253402246800}
#'                  seconds since the Unix epoch).}
#'      \item{time of day (\code{"h"})}{from \code{00:00} to \code{24:00}
#'                  (between \code{0} and \code{86400} seconds since midnight).}
#' }
#'
#' @param ... components of time index to be constructed (in arbitrary order),
#' the following are accepted:
#' \describe{
#'      \item{y}{year.}
#'      \item{q}{quarter.}
#'      \item{m}{month.}
#'      \item{w}{week (ISO 8601).}
#'      \item{d}{day.}
#'      \item{j}{day of year.}
#'      \item{u}{day of week (ISO 8601).}
#'      \item{H}{hour.}
#'      \item{M}{minute.}
#'      \item{S}{second.}
#' }
#' @param length an integer value specifying the desired length.
#' @param type a character value determining time index type
#'             (\code{y} - years, \code{q} - quarters, \code{m} - months,
#'             \code{w} - weeks, \code{d} - dates, \code{t} - date-time,
#'             \code{h} - time of day,
#'             \code{n} - numeric value, \code{i} - integer value).
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#' @param x any R object.
#'
#' @return An object of \code{tind} class for \code{tind} or a logical
#' value for \code{is.tind}.
#'
#' @seealso \code{\link{as.tind}} for conversion to \code{tind},
#' \code{\link{parse_t}} and \code{\link{strptind}} functions for
#' parsing character strings, \code{\link{date_time}} for construction
#' of date-time indices from date and time components, \link{tind-methods}
#' for basic methods.
#'
#' @examples
#' # years
#' tind(y = 2010:2020)
#' tind(type = "y")
#' tind(length = 11, type = "y")
#'
#' # quarters
#' tind(y = rep(2020:2023, each = 4), q = 1:4)
#' tind(q = 1:4, y = rep(2020:2023, each = 4))
#' tind(type = "q")
#' tind(length = 4, type = "q")
#'
#' # months
#' tind(y = 2023, m = 1:12)
#' tind(m = 1:12, y = 2023)
#' tind(type = "m")
#' tind(length = 12, type = "m")
#'
#' # weeks
#' tind(y = 2024, w = 1 + 2 * (0:25))
#' tind(type = "w")
#' tind(length = 13, type = "w")
#'
#' # dates
#' tind(m = 3, d = 15, y = 2024)
#' tind(y = 2024, m = rep(1:3, each = 2), d = c(1, 15))
#' tind(type = "d")
#' tind(length = 6, type = "d")
#'
#' # time of day
#' tind(H = 16, M = (0:3) * 15)
#' tind(type = "h")
#' tind(length = 4, type = "h")
#'
#' # date-time
#' # system time zone
#' tind(y = 2024, m = 8, d = 2, H = 16, M = (0:3) * 15)
#' tind(y = 2024, m = 8, d = 2, H = 16, M = (0:3) * 15)
#' tind(y = 2024, m = 8, d = 2, H = 16, M = 0, S = 10 * (0:5))
#' # time zone explicitly provided
#' tind(y = 2024, m = 8, d = 2, H = 16, M = (0:3) * 15, tz = "UTC")
#' tind(type = "t")
#' tind(type = "t", tz = "UTC")
#' tind(length = 4, type = "t")
#' tind(length = 4, type = "t", tz = "UTC")
#'
#' # integer and numeric indices
#' # (cannot be constructed from components like above)
#' tind(length = 10, type = "i")
#' tind(length = 10, type = "n")
#'
#' @name tind
#'
#' @export
#'
tind <- function(..., length = 0L, type = NULL, tz = NULL)
{
    typetz <- .check_type_tz(type, tz)
    type <- typetz$type
    tz <- typetz$tz
    # 2nd way of invoking the constructor
    if (!...length()) {
        if (is.null(type)) stop("type not provided")
        nas <- rep(if (.is.instant(type)) NA_real_ else NA_integer_, length)
        return (.tind(nas, type, tz))
    }
    # 1st way of invoking the constructor
    if (!missing(length)) {
        mes <- gettextf("%s provided with time index components (%s) in %s constructor",
                        sQuote("length"), sQuote("..."), dQuote("tind"))
        stop(mes, call. = FALSE, domain = NA)
    }
    comp <- list(...)
    nms <- names(comp)
    if (is.null(nms) || any(nms == "")) {
        mes <- gettextf("unnamed arguments in %s constructor", dQuote("tind"))
        stop(mes, call. = FALSE, domain = NA)
    }
    tpign <- .infer_type(nms, numonly = TRUE)
    dplctd <- duplicated(nms)
    if (any(dplctd)) {
        dplctd <- unique(nms[dplctd])
        mes <- gettextf("duplicated time index components: %s",
                        toString(dQuote(dplctd)))
        stop(mes, call. = FALSE, domain = NA)
    }
    if (!is.null(type) && type != tpign[[1L]]) {
        mes <- gettextf("type inferred (%s) is different from type provided as argument (%s)",
                        .ti_type2char(tpign[[1L]], dash = TRUE), .ti_type2char(type, dash = TRUE))
        stop(mes, call. = FALSE, domain = NA)
    }
    type <- tpign[[1L]]
    ign <- tpign[[2L]]
    if (length(ign)) {
        mes0 <- gettextf("the following components will be ignored: %s",
                        .ti_comp2char(ign))
        mes1 <- gettextf("type inferred: %s", .ti_type2char(type))
        warning(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
        comp <- comp[!(nms %in% ign)]
        nms <- names(comp)
    }
    # fast exit and checks
    len <- sapply(comp, length)
    n <- .check_lengths(len)
    if (!n) return (.tind(if (.is.instant(type)) double() else integer(), type, tz))
    ## NOTE: special case - date-time indices are constructed in 2 steps,
    ## so we need to ensure correct recycling.
    if ((type == "t") && any(n %% len)) for (ci in seq_along(len)) {
        if (n %% len[ci]) comp[[ci]] <- rep_len(comp[[ci]], n)
    }

    # construct the result (for date-time only date part)
    if (type == "y") {
        res <- .validate_y(comp[["y"]])
    } else if (type == "q") {
        res <- suppressWarnings(.validate_yq(comp[["y"]], comp[["q"]]))
    } else if (type == "m") {
        res <- suppressWarnings(.validate_ym(comp[["y"]], comp[["m"]]))
    } else if (type == "w") {
        res <- suppressWarnings(.validate_yw(comp[["y"]], comp[["w"]]))
    } else if ((type == "d") || (type == "t")) {
        if ("m" %in% nms) {
            res <- suppressWarnings(.validate_ymd(comp[["y"]], comp[["m"]],
                                                  comp[["d"]]))
        } else if ("w" %in% nms) {
            res <- suppressWarnings(.validate_ywu(comp[["y"]], comp[["w"]],
                                                  comp[["u"]]))
        } else if ("j" %in% nms) {
            res <- suppressWarnings(.validate_yj(comp[["y"]], comp[["j"]]))
        }
    }
    # time, date-time
    if ((type == "h") || (type == "t")) {
        resh <- suppressWarnings(.validate_hms(comp[["H"]],
                                    if ("M" %in% nms) comp[["M"]] else 0L,
                                    if ("S" %in% nms) comp[["S"]] else 0))
    }

    # finalise construction of date-time
    if (type == "t") {
        tz <- .check_tz(tz)
        res <- .dhz2t(res, resh, integer(), tz, 1L)
    } else {
        if (type == "h") res <- resh
        tz <- NULL
    }
    # check NAs
    nna0 <- rep(TRUE, n)
    lapply(comp, function(x) nna0 <<- suppressWarnings(nna0 & !is.na(x)))
    nas <- is.na(res)
    if (any(nna0 & nas)) {
        mes0 <- gettextf("NAs introduced")
        ii <- which.max(nna0 & nas)
        first <- character(length(comp))
        for (j in seq_along(comp)) {
            iij <- (ii - 1L) %% len[j] + 1L
            first[j] <- sprintf("%s[%s] = %s", names(comp)[j],
                                format(iij, scientific = FALSE), comp[[j]][iij])
        }
        mes1 <- gettextf("first occurrence: %s", toString(first))
        if (type == "t") mes1 <- paste0(mes1, "; ", gettextf("time zone: %s", tz))
        warning(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    return (.tind(res, type, tz))
}


# internal tind constructor, attributes, copying and removing attributes
# ###################################################################

## NOTE: The initial idea was to store index type and time zone as separate
## attributes but these got lost in some operations, e.g., involving data frames
## (POSIXct has similar issues). For this reason, they are stored in class attribute.
## The order of class attributes is `c("tind", "tind:type:tz")`.
## tdiff implementation takes the same approach.
##
## Almost all other functions rely on these 6 except for C implementations
## of some operations on ordered indices.

.tind <- function(x, type, tz = NULL)
{
## NOTE: this is for debugging only
# if (storage.mode(x) != .mode(type)) stop("invalid tind's storage mode")
    structure(x, dim = NULL, class = c("tind", paste(c("tind", type, tz),
                                                     collapse = ":")))
}

.get.type <- function(x)
{
    cl <- class(x)
    clt <- which(grepl("^tind:", cl))
    return (strsplit(cl[clt], ":", fixed = TRUE)[[1L]][2L])
}

.get.tz <- function(x)
{
    cl <- class(x)
    clt <- which(grepl("^tind:", cl))
    splt <- strsplit(cl[clt], ":", fixed = TRUE)[[1L]]
    return (if (splt[2L] == "t") splt[3L] else NULL)
}

.set.tz <- function(x, tz)
{
    cl <- class(x)
    clt <- which(grepl("^tind:", cl))
    cl[clt] <- paste0("tind:t:", tz)
    class(x) <- cl
    return (x)
}

.cp_attr <- function(x, y)
{
    attr(x, "class") <- attr(y, "class")
    return (x)
}

# this not only removes class attribute but all other except for names
.unclass <- function(x)
{
    atx <- attributes(x)
    if (is.null(atx) || (length(atx) == 1) && (names(atx) == "names")) return (x)
    attributes(x) <- atx["names"]
    return (x)
}


# is.tind
# ###################################################################

#' @rdname tind
#' @export
is.tind <- function(x) inherits(x, "tind")


# is.numeric
# ###################################################################

#' @keywords internal
#' @export
is.numeric.tind <- function(x) FALSE

