#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################## #
# basic tind methods #
# ################## #


#' Basic Methods for \code{tind} Class
#'
#' @description
#' \code{tind} class supports all standard methods for vectors and
#' and vector-based classes like \code{Date} or \code{POSIXct}.
#'
#' @details
#' \code{tind} class supports standard indexing via \code{[]} and \code{[[]]}
#' operators, as well as replacement, In replacement, it is expected that the
#' right hand side is of the same type as the indexed object.
#'
#' \code{length}, \code{length<-}, and \code{rep} methods work in a standard way.
#'
#' \code{rev}, \code{head}, \code{tail}, as they are implemented using \code{[]}
#' operator, are also available for objects of \code{tind} class.
#'
#' Concatenation method (\code{c}) works in a standard way. It is expected that
#' all arguments are of the same type. Arguments that are not of \code{tind} class
#' are converted.
#'
#' \code{min}, \code{max}, and \code{range} work in a standard way. If the results
#' are not proper time indices (for example maximum over a vector of length 0),
#' \code{NA}s are returned.
#'
#' \code{unique}, \code{duplicated}, \code{order}, \code{sort}, etc.
#' work in a standard way.
#'
#' \code{print} prints time indices on the console and invisibly returns its
#' argument.
#'
#' \code{summary} method returns summary information about time indices.
#'
#' @param x an object of \code{tind} class.
#' @param i an integer vector of indices or a logical vector indicating selection.
#' @param value replacement value.
#' @param ... objects of \code{tind} class (for \code{c}, \code{min}, \code{max},
#'            and \code{range}) or additional arguments passed to or from methods.
#' @param object an object of \code{tind} class.
#'
#' @return In general, methods return objects of \code{tind} class.
#'
#' \code{print} returns its argument invisibly and is used for its side effect.
#'
#' \code{summary} returns an object of class \code{c("summaryDefault", "table")}.
#'
#' @seealso \code{\link{format}} for formatting time indices,
#' \link{Ops} for operations on time indices.
#'
#' @name tind-methods
#'
#' @examples
#' # test sample
#' (dd <- as.tind(20210131) + sample((0:9), 15, replace = TRUE))
#' # indexing
#' dd[1]
#' dd[[1]]
#' dd[[1]] <- dd[[1]] + 1
#' dd
#' dd[2:3] <- dd[2:3] + 1
#' dd
#' # this will generate an error
#' try(
#' dd[10] <- now()
#' )
#' # length, length<-
#' length(dd)
#' length(dd) <- 7
#' dd
#' # rep, head, tail, rev
#' rep(dd, 2)
#' head(dd, 3)
#' tail(dd, -5)
#' rev(dd)
#' # min, max, range
#' min(dd)
#' max(dd)
#' range(dd)
#' # unique, duplicated
#' unique(dd)
#' duplicated(dd)
#' # order, sort
#' order(dd)
#' sort(dd)
#' # concatenation
#' c(dd, rev(dd))
#' # attempt at concatenating different types will result in an error
#' try(
#' c(today(), now())
#' )
#'
#'
NULL


# indexing
# ###################################################################

#' @rdname tind-methods
#' @export
`[.tind` <- function(x, i) .cp_attr(NextMethod("["), x)


#' @rdname tind-methods
#' @export
`[<-.tind` <- function(x, i, value)
{
    xtype <- .get.type(x)
    xtz <- .get.tz(x)
    if (.tind_base_coercible(value))
        value <- as.tind(value, type = xtype, tz = xtz)
    else value <- as.tind(value)

    vtype <- .get.type(value)
    vtz <- .get.tz(value)
    if (xtype != vtype) {
        mes <- gettextf("time index type mismatch in %s", sQuote("[<-.tind"))
        mes <- paste0(mes, ": ", .ti_type2char(xtype), ", ", .ti_type2char(vtype))
        stop(mes, call. = FALSE, domain = NA)
    }
    if ((xtype == "t") && (xtz != vtz)) .warn_diff_tz(xtz, vtz)

    return (NextMethod("[<-"))
}


#' @rdname tind-methods
#' @export
`[[.tind` <- function(x, i) .cp_attr(NextMethod("[["), x)


#' @rdname tind-methods
#' @export
`[[<-.tind` <- function(x, i, value)
{
    xtype <- .get.type(x)
    xtz <- .get.tz(x)
    if (.tind_base_coercible(value)) value <- as.tind(value, type = xtype, tz = xtz)
    else value <- as.tind(value)

    vtype <- .get.type(value)
    vtz <- .get.tz(value)
    if (xtype != vtype) {
        mes <- gettextf("time index type mismatch in %s", sQuote("[[<-.tind"))
        mes <- paste0(mes, ": ", .ti_type2char(xtype), ", ", .ti_type2char(vtype))
        stop(mes, call. = FALSE, domain = NA)
    }
    if ((xtype == "t") && (xtz != vtz)) .warn_diff_tz(xtz, vtz)

    return (NextMethod("[[<-"))
}


# length
# ###################################################################

#' @rdname tind-methods
#' @export
`length<-.tind` <- function(x, value) .cp_attr(NextMethod("length<-"), x)


# rep
# ###################################################################

#' @rdname tind-methods
#' @export
rep.tind <- function(x, ...) .cp_attr(NextMethod("rep"), x)


# concatenation
# ###################################################################

#' @rawNamespace S3method(c,tind,c.tind)
#' @rawNamespace export(c.tind)
NULL

#' @rdname tind-methods
c.tind <- function(...)
{
    argl <- list(...)
    argl <- lapply(argl, function(x) if (.tind_coercible(x)) as.tind(x) else x)
    if (!any(sapply(argl, is.tind))) argl <- lapply(argl, as.tind)
    type <- sapply(argl, function(x) if (is.tind(x)) .get.type(x) else NA_character_)
    type <- unique(type[!is.na(type)])
    if (length(type) > 1L) {
        mes <- gettextf("time index type mismatch in %s", sQuote("c.tind"))
        stop(mes, call. = FALSE, domain = NA)
    }
    if (type == "t") {
        tz <- sapply(argl, function(x) if (is.tind(x)) .get.tz(x) else NA_character_)
        tz <- unique(tz[!is.na(tz)])
        if (length(tz) > 1L) { tz <- tz[1]; .warn_diff_tz(tz, first = TRUE) }
    } else tz <- NULL
    argl <- lapply(argl, function(x) as.tind(x, type, tz))

    return (.tind(unlist(lapply(argl, .unclass)), type, tz))
}


# Math, Summary, Complex groups
# ###################################################################

#' @keywords internal
#' @export
Math.tind <- function(x, ...)
{
    if (!(.Generic %in% c("cummin", "cummax"))) {
        mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                        dQuote("tind"))
        stop(mes, call. = FALSE, domain = NA)
    }
    chkDots(...)
    return (.cp_attr(NextMethod(.Generic), x))
}


#' @keywords internal
#' @export
Summary.tind <- function(..., na.rm = FALSE)
{
    if (!(.Generic %in% c("min", "max", "range"))) {
        mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                        dQuote("tind"))
        stop(mes, call. = FALSE, domain = NA)
    }
    .checkTRUEFALSE(na.rm)
    x <- if (...length() == 1L) ...elt(1L) else c(...)
    type <- .get.type(x)
    # range
    if (.Generic == "range")
        return (.cp_attr(.Call(C_range, x, na.rm), x))
    # generic code
    type <- .get.type(x)
    sx <- suppressWarnings(NextMethod(.Generic, x, na.rm = na.rm))
    sx <- do.call(paste0(".validate_", type), list(sx))
    return (.cp_attr(sx, x))
}


#' @keywords internal
#' @export
Complex.tind <- function(z)
{
    mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                    dQuote("tind"))
    stop(mes, call. = FALSE, domain = NA)
}


# xtfrm
# ###################################################################

#' @keywords internal
#' @export
xtfrm.tind <- function(x) .unclass(x)


# unique
# ###################################################################

#' @rdname tind-methods
#' @export
unique.tind <- function(x, ...)
{
    if (.Call(C_is_ordered, x, FALSE)) .Call(C_unique_ord, x)
    else .cp_attr(NextMethod("unique"), x)
}

