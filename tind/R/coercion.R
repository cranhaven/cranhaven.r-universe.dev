#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############################################################## #
# coercion of time indices - base classes, different index types #
# ############################################################## #


# internal - basic types coercible to tind
# ###################################################################

.tind_base_coercible <- function(x)
    (is.numeric(x) || is.logical(x) || is.character(x) || is.factor(x))


# internal - other time index classes recognised by tind
# ###################################################################

.tind_coercible <- function(x) UseMethod(".tind_coercible")

.tind_coercible.default <- function(x) FALSE

.tind_coercible.Date <- function(x) TRUE

.tind_coercible.POSIXt <- function(x) TRUE

.check_tind_coercible <- function(x)
{
    if (!.tind_coercible(x)) {
        mes <- gettextf("%s is not recognised as a class representing time indices",
                        .class2str(x))
        stop(mes, call. = FALSE, domain = NA)
    }
}


# internal routines - type conversion
# ###################################################################

.cast <- function(x, to, tz = NULL)
{
    xtype <- .get.type(x)

    if (xtype == to) {
        if ((xtype == "t") && (!is.null(tz))) return(.set.tz(x, tz))
        return (x)
    }
    xtz <- .get.tz(x)
    x <- .unclass(x)
    if (xtype == "t") {
        arglist <- list(x, xtz)
        tz <- NULL
    } else if (to == "t") {
        arglist <- list(x, tz)
    } else {
        tz <- NULL
        arglist <- list(x)
    }
    ind <- do.call(paste0(".", xtype, "2", to), arglist)
    res <- .tind(ind, to, tz)
    return (res)
}


.require_type <- function(x, type, tz = NULL, lower = FALSE)
{
    xtp <- .get.type(x)
    ok <- if (lower) ((type == xtp) || (type %in% .lo_res_cast(xtp)))
          else (type %in% c(.lo_res_cast(xtp), xtp, .hi_res_cast(xtp)))
    if (!ok) .stop_cast(xtp, type, 1L)
    return (.cast(x, type, tz))
}



# as.tind
# ###################################################################

#' Conversion to \code{tind} Class
#'
#' @description
#' \code{as.tind} method allows for conversion of numeric and character vectors
#' as well as objects of \code{Date}, \code{POSIXct}, and \code{POSIXlt}
#' classes to \code{tind} objects to \code{tind}.
#'
#' \code{as.tind} method for \code{tind} class allows to change the type
#' of index of an object of \code{tind} class.
#' Convenience functions \code{as.year}, \code{as.quarter}, \code{as.month},
#' \code{as.week}, \code{as.date}, \code{as.date_time}, \code{as.time} allow
#' to quickly convert argument to the indicated type.
#'
#' @details
#' \strong{Numeric vectors}
#'
#' The following numeric representations are automatically recognised
#' (between year 1800 and 2199): \code{YYYY} (years), \code{YYYYQ} (quarters),
#' \code{YYYYMM} (months), and \code{YYYYMMDD} (dates). Conversion from
#' numeric vectors to other index types requires type specification via \code{type}
#' argument.
#'
#' Date-time indices are represented as number of seconds since the Epoch
#' (1970-01-01 00:00 UTC). Time of day is represented as the number of seconds
#' since midnight.
#'
#' \strong{Character vectors}
#'
#' \code{as.tind} automatically recognises many different formats. If automatic
#' recognition fails, \code{type} argument could help identify the format.
#' For less standard formats / representations, one can use either \code{format}
#' argument (which is forwarded to \code{\link{strptind}}) or \code{order}
#' argument (which is forwarded to \code{\link{parse_t}}).
#'
#' \strong{Data frames}
#'
#' If a data frame has one column, it is converted using appropriate method
#' depending on the column type. In case there are two or more columns, they are
#' pasted and the resulting character vector is parsed. As this may not be
#' computationally efficient, other ways of constructing \code{tind} should be
#' considered, for example, use of \code{\link{tind}} constructor.
#'
#' \strong{\code{Date} and \code{POSIXt} classes}
#'
#' Conversion of \code{Date} objects returns time index of type \code{"d"} (date).
#' \code{POSIXct} and \code{POSIXlt} classes are converted to index of type \code{"t"}
#' (date-time). If time zone attribute is not set for the argument, system time zone
#' is assumed.
#'
#' \strong{Other classes representing time indices}
#'
#' For conversions between \code{tind} class and other classes (from packages
#' other than \pkg{base}), see \code{\link{tind-other}}.
#'
#' @param x an R object (e.g., a numeric vector, a character vector, a \code{Date}
#'                  or \code{POSIXct} object).
#' @param type a character determining time index type or \code{NULL}.
#' @param format a character determining input format(s) as in \code{\link{strptind}}.
#'               (or \code{NULL}).
#' @param order a character determining order(s) of time index components
#'              in the input as in \code{\link{parse_t}} (or \code{NULL}).
#' @param locale (optional) a character value determining locale or \code{NULL}
#'               (the default, interpreted as the current system locale),
#'               see \link{calendar-names} for information on locale settings.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#' @param digits an integer value (0--6) determining the number of decimal places
#'               for seconds to be preserved during conversion (0 by default).
#' @param ... further arguments passed to or from other methods.
#'
#' @name as.tind
#'
#' @return An object of \code{tind} class of length equal to the length of the argument.
#' For data frame version the length of the result is equal to the number of rows
#' in the data frame.
#'
#' @seealso \code{\link{tind}} constructor,
#' \code{\link{strptind}} function for \code{format} specifications,
#' \code{\link{parse_t}} function for \code{order} specifications,
#' \link{tind-coercion} for conversions from \code{tind},
#' and \code{\link{tind-other}} for conversions between \code{tind} class and
#' other classes (from packages other than \pkg{base}).
#'
#' @examples
#' ## numeric and character arguments
#' # years
#' as.tind(1999)
#' as.tind("1999")
#' # quarters
#' as.tind(20043)
#' as.tind("20043")
#' # months
#' as.tind(200109)
#' as.tind("2001-09")
#' as.tind("200109")
#' # need proper locale to recognise English month names
#' as.tind("Sep 2001", locale = "C")
#' # weeks (ISO 8601)
#' # numeric YYYYWW representation is not automatically recognised, need type
#' as.tind(200936, "w")
#' as.tind("2009-W36")
#' # dates
#' as.tind(20200726)
#' as.tind("2020-07-26")
#' # need proper locale to recognise English month names
#' as.tind("Jul 26, 2020", locale = "C")
#' as.tind("07/26/20")
#' # date-time
#' as.tind("2000-08-16 08:17:38")
#' # time
#' as.tind("08:17:38")
#' as.tind(08 * 3600 + 17 * 60 + 38, type = "h")
#'
#' ## conversion from Date and POSIXct
#' as.tind(Sys.Date())
#' as.tind(Sys.time())
#'
#' ## as.year, ..., as.time
#' # today
#' (x <- today())
#' as.year(x)
#' as.quarter(x)
#' as.month(x)
#' as.week(x)
#' # midnight
#' as.date_time(x)
#' # current time
#' (x <- now())
#' as.year(x)
#' as.quarter(x)
#' as.month(x)
#' as.week(x)
#' as.date(x)
#' as.time(x)
#'
NULL


#' @rdname as.tind
#' @export
as.tind <- function(x, ...) UseMethod("as.tind")


#' @keywords internal
#' @export
as.tind.default <- function(x, ...)
{
    if (is.null(x)) return (as.tind.numeric(numeric(), ...))
    mes <- gettextf("%s method not defined for class %s", sQuote("as.tind"),
                    .class2str(x))
    stop(mes, call. = FALSE, domain = NA)
}


#' @rdname as.tind
#' @export
as.tind.numeric <- function(x, type = NULL, tz = NULL, ...)
{
    typetz <- .check_type_tz(type, tz)
    type <- typetz$type
    tz <- typetz$tz
    # type provided
    if (!is.null(type)) {
        ind <- .parse_num(x, type)
        names(ind) <- names(x)
        return (.tind(ind, type, tz))
    }
    # automatic parsing - try to guess type
    ind <- .parse_num(x, NULL)
    if (is.null(ind)) {
        mes0 <- gettextf("time index type could not be automatically inferred")
        mes1 <- gettextf("provide %s argument", sQuote("type"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    type <- ind[[2L]]
    ind <- ind[[1L]]
    return (.tind(ind, type, tz))
}


#' @keywords internal
#' @export
as.tind.logical <- function(x, type = NULL, tz = NULL, ...)
{
    if (any(!is.na(x))) warning("NAs introduced by coercion", domain = "tind")
    return (tind(length = length(x), type = type, tz = tz))
}


#' @rdname as.tind
#' @export
as.tind.character <- function(x, type = NULL, format = NULL, order = NULL,
                              locale = NULL, tz = NULL, ...)
{
    if (!is.null(format) && !is.null(order)) {
        mes <- gettextf("%s provided together with %s",
                        sQuote("format"), sQuote("order"))
        stop(mes, call. = FALSE, domain = NA)
    }
    typetz <- .check_type_tz(type, tz)
    type <- typetz$type
    tz <- typetz$tz

    if (!is.null(type) && (type %in% c("i", "n")) &&
        (!is.null(format) || !is.null(order))){
        mes <- gettextf("%s or %s provided for type %s",
                        sQuote("format"), sQuote("order"), .ti_type2char(type))
        stop(mes, call. = FALSE, domain = NA)
    }

    # forward to strptind
    if (!is.null(format))
        return (do.call(strptind, c(list(x = x, format = format),
                                    locale = locale, tz = tz, type = type)))
    # forward to parse_t
    if (!is.null(order))
        return (do.call(parse_t, c(list(x = x, order = order),
                                   locale = locale, tz = tz, type = type)))
    # automatic parsing
    ind <- .parse(x, type, locale, tz)
    if (is.null(ind)) {
        mes0 <-  gettextf("time index type could not be automatically inferred")
        mes1 <-  gettextf("provide %s, %s, or %s argument", sQuote("type"),
                          sQuote("format"), sQuote("order"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    return (do.call(.tind, ind))
}


#' @keywords internal
#' @export
as.tind.factor <- function(x, ...) as.tind(levels(x), ...)[as.integer(x)]


#' @rdname as.tind
#' @export
as.tind.Date <- function(x, ...)
{
    ind <- .validate_d(.unclass(x))
    return (.tind(ind, "d"))
}


#' @rdname as.tind
#' @export
as.tind.POSIXct <- function(x, tz = NULL, digits = 0L, ...)
{
    tz <- .check_tz(if (!is.null(tz)) tz else attr(x, "tzone"))
    ind <- .validate_t(round(.unclass(x), .check_digits(digits)))
    return (.tind(ind, "t", tz))
}


#' @rdname as.tind
#' @export
as.tind.POSIXlt <- function(x, tz = NULL, digits = 0L, ...)
    as.tind.POSIXct(as.POSIXct(x), tz = tz, digits = digits)


#' @rdname as.tind
#' @export
as.tind.data.frame <- function(x, ...)
{
    nc <- ncol(x)
    if (!nc) {
        mes <- gettextf("trying to convert a data frame with no columns to %s",
                        sQuote("tind"))
        stop(mes, call. = FALSE, domain = NA)
    }
    if (nc == 1L) return (as.tind(x[[1L]], ...))
    return (as.tind(do.call(paste, as.list(x)), ...))
}


#' @rdname as.tind
#' @export
as.tind.tind <- function(x, type = NULL, tz = NULL, ...)
{
    if (is.null(type) && is.null(tz)) return (x)
    typetz <- .check_type_tz(type, tz)
    if ((typetz$type == .get.type(x)) && is.null(tz)) return (x)
    return (.require_type(x, typetz$type, typetz$tz))
}


#' @rdname as.tind
#' @export
as.year <- function(x, ...)
    if (is.null(x) || .tind_base_coercible(x)) as.tind(x, type = "y", ...) else
        as.tind(as.tind(x, ...), type = "y")

#' @rdname as.tind
#' @export
as.quarter <- function(x, ...)
    if (is.null(x) || .tind_base_coercible(x)) as.tind(x, type = "q", ...) else
        as.tind(as.tind(x, ...), type = "q")

#' @rdname as.tind
#' @export
as.month <- function(x, ...)
    if (is.null(x) || .tind_base_coercible(x)) as.tind(x, type = "m", ...) else
        as.tind(as.tind(x, ...), type = "m")

#' @rdname as.tind
#' @export
as.week <- function(x, ...)
    if (is.null(x) || .tind_base_coercible(x)) as.tind(x, type = "w", ...) else
        as.tind(as.tind(x, ...), type = "w")

#' @rdname as.tind
#' @export
as.date <- function(x, ...)
    if (is.null(x) || .tind_base_coercible(x)) as.tind(x, type = "d", ...) else
        as.tind(as.tind(x, ...), type = "d")

#' @rdname as.tind
#' @export
as.date_time <- function(x, tz = NULL, ...)
    if (is.null(x) || .tind_base_coercible(x)) as.tind(x, type = "t", tz = tz, ...) else
        as.tind(as.tind(x, ...), type = "t", tz = tz)

#' @rdname as.tind
#' @export
as.time <- function(x, ...)
    if (is.null(x) || .tind_base_coercible(x)) as.tind(x, type = "h", ...) else
        as.tind(as.tind(x, ...), type = "h")



# coercion to other types
# ###################################################################

#' Conversion of Objects of \code{tind} Class
#'
#' @description
#' Objects of \code{tind} class can be easily converted to built-in R classes
#' inluding \code{numeric}, \code{integer}, \code{character}, \code{Date},
#' \code{POSIXct}, \code{POSIXlt}, and \code{data.frame}.
#'
#' @details
#' \code{as.double} and \code{as.numeric} return internal representation
#' for particular time index type (seconds, days, weeks etc. since ...).
#'
#' For years, quarters, months, weeks, and dates, \code{as.integer} returns
#' representation in the form \code{YYYY}, \code{YYYYQ}, \code{YYYYMM}, \code{YYYYWW},
#' and \code{YYYYMMDD}, respectively. For other index types, \code{as.integer}
#' returns internal representation of time indices converted to integer.
#'
#' \code{as.character} returns character vector with standard (ISO 8601)
#' representation of time indices. For customisable output formats,
#' see \code{\link{format}}.
#'
#' \code{as.Date}, \code{as.POSIXct}, and \code{as.POSIXlt} return objects
#' of classes \code{Date}, \code{POSIXct}, and \code{POSIXlt}, respectively.
#'
#' \code{as.data.frame} returns a 1-column data frame with time indices and
#' allows to work with time indices in data frames.
#'
#' @param x an object of \code{tind} class.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#' @param ... further arguments passed to or from other methods.
#'
#' @return \code{as.xxx} returns an object of \code{xxx} class of the same
#' length as the argument. \code{as.data.frame} returns a data frame with a single
#' column and the number of rows equal to the length of the argument.
#'
#' @name tind-coercion
#'
#' @seealso \code{\link{format}} for customisable character output formats,
#' \code{\link{as.tind}} for conversion to \code{tind}.
#' For conversions between \code{tind} class and other classes (from packages
#' other than \pkg{base}), see \link{tind-other}.
#'
NULL



#' @rdname tind-coercion
#' @export
as.integer.tind <- function(x, ...)
{
    type <- .get.type(x)
    if (type == "q") return (.Call(C_q2num, x))
    if (type == "m") return (.Call(C_m2num, x))
    if (type == "w") return (.Call(C_w2num, x))
    if (type == "d") return (.Call(C_d2num, x))
    return (as.vector(x, "integer"))
}


#' @rdname tind-coercion
#' @export
as.double.tind <- function(x, ...) as.vector(x, "double")



#' @rdname tind-coercion
#' @export
as.character.tind <- function(x, ...)
{
    if (...length()) return (format.tind(x, ...))
    if (!length(x)) return (character())
    type <- .get.type(x)
    tz <- .get.tz(x)
    x <- .unclass(x)
    arglist <- if (type == "t") list(x, tz, TRUE, FALSE) else list(x)
    res <- do.call(paste0(".", type, "2char"), arglist)
    return (res)
}


#' @rdname tind-coercion
#' @export
as.Date.tind <- function(x, ...)
{
    x <- .require_type(x, "d")
    return (as.Date(.unclass(x), origin = "1970-01-01"))
}


#' @rdname tind-coercion
#' @export
as.POSIXct.tind <- function(x, tz = NULL, ...)
{
    tz <- if (.get.type(x) == "t" && is.null(tz)) .get.tz(x)
          else .check_tz(tz)
    x <- .require_type(x, "t", tz)
    return (as.POSIXct(.unclass(x), tz = tz, origin = "1970-01-01"))
}


#' @rdname tind-coercion
#' @export
as.POSIXlt.tind <- function(x, tz = NULL, ...)
{
    tz <- if (.get.type(x) == "t" && is.null(tz)) .get.tz(x)
          else .check_tz(tz)
    x <- .require_type(x, "t", tz)
    res <- as.POSIXlt(.unclass(x), tz = tz, origin = "1970-01-01")
    res$sec <- round(res$sec, digits = 6)
    return (res)
}


#' @rdname tind-coercion
#' @export
as.data.frame.tind <- function(x, ...)
    as.data.frame.vector(x, nm = paste(deparse(substitute(x), width.cutoff = 50L),
                                       collapse = " "), ...)


#' @keywords internal
#' @export
as.logical.tind <- function(x, ...) (!is.na(x) | NA)


#' @keywords internal
#' @export
as.list.tind <- function(x, ...)
{
    tp <- .get.type(x)
    tz <-.get.tz(x)
    res <- NextMethod("as.list")
    return (lapply(res, function(x) .tind(x, tp, tz)))
}


