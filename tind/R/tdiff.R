#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################ #
# time differences #
# ################ #


#' Time Differences
#'
#' @description
#' Objects of \code{tdiff} class represent time differences and are similar
#' to \code{\link[base]{difftime}} objects. \code{tdiff} objects are created
#' by subtracting two time indices (of types other than \code{"i"} and \code{"n"})
#' or via calls to \code{as.tdiff} method. An alternative way of constructing
#' \code{tdiff} objects is to call \code{years}, \code{qrtrs}, \code{mnths},
#' \code{weeks}, \code{days}, \code{hours}, \code{mins}, and \code{secs}
#' convenience functions.
#'
#' The following units (argument \code{unit}) are supported:
#' \describe{
#'     \item{\code{"y"} (\code{"year"}, \code{"years"})}{differences in years,}
#'     \item{\code{"q"} (\code{"quarter"}, \code{"quarters"})}{differences in quarters,}
#'     \item{\code{"m"} (\code{"month"}, \code{"months"})}{differences in months,}
#'     \item{\code{"w"} (\code{"week"}, \code{"weeks"})}{differences in weeks,}
#'     \item{\code{"d"} (\code{"day"}, \code{"days"})}{differences in days,}
#'     \item{\code{"h"} (\code{"hour"}, \code{"hours"})}{differences in hours,}
#'     \item{\code{"min"} (\code{"mins"}, \code{"minute"}, \code{"minutes"})}{differences in minutes,}
#'     \item{\code{"s"} (\code{"secs"}, \code{"second"}, \code{"seconds"})}{differences in seconds.}
#' }
#'
#' Standard methods for vectors and conversion from / to numeric and character
#' vectors are implemented for this class.
#'
#' @details
#' \code{tdiff} objects are implemented as vectors of integers (for differences
#' in years, quarters, months, weeks, and days) or vectors of doubles (for time
#' differences). Time differences are internally represented in seconds
#' but when printing the actual time unit (hour, minute, second) is automatically
#' inferred and used.
#'
#' Valid ranges for \code{tdiff} values depend on unit. These are defined
#' by differences of the maximal and minimal valid time indices of the type
#' corresponding to the time unit.
#'
#' @note
#' Since \code{as.difftime} is not implemented in \pkg{base} as an S3 generic,
#' conversion from \code{tdiff} to \code{difftime} is not provided.
#'
#' @param x a numeric vector or any R object coercible to \code{tdiff},
#'          for \code{as.tdiff} and \code{years}, \code{qrtrs}, etc.;
#'          an object of \code{tdiff} class for methods.
#' @param unit a character string, name of the time unit, see Details.
#' @param i an integer vector of indices or a logical vector indicating selection.
#' @param value replacement value.
#' @param na.rm a logical value indicating whether missing values should be removed.
#' @param object an object of \code{tdiff} class.
#' @param ... further arguments passed to or from other methods.
#'
#' @return \code{as.tdiff} as well as convenience functions \code{years},
#' \code{qrtrs}, etc., return objects of \code{tdiff} class.
#'
#' \code{is.tdiff} returns a logical value.
#'
#' In general, methods for \code{tdiff} return objects of \code{tdiff} class.
#'
#' \code{as.character} and \code{format} return character vectors.
#'
#' \code{as.data.frame} returns a data frame with a single
#' column and the number of rows equal to the length of the argument.
#'
#' \code{print} returns its argument invisibly and is used for its side effect.
#'
#' \code{summary} returns an object of class \code{c("summaryDefault", "table")}.
#'
#' @seealso \link{Ops} for operations on time indices and
#' time differences.
#'
#' @name tdiff
#'
#' @examples
#' # how many days have passed since Jan 1, 2000?
#' today() - as.date("2000-01-01")
#' # how many months have passed since Sep 2008?
#' as.month(today()) - as.month("2008-09")
#' # create time differences in quarters
#' as.tdiff(-2:2, "q")
#' # same
#' (x <- qrtrs(-2:2))
#' # add to today
#' today() + x
#'
NULL


# internal tdiff constructor
# ###################################################################

.tdiff <- function(x, type)
{
## NOTE: this is for debugging only
# if (storage.mode(x) != .mode(type)) stop("invalid tdiff's storage mode")
    structure(x, dim = NULL, class = c("tdiff", paste0("tdiff:", type)))
}


# tdiff type, unit
# ###################################################################

.get_tdiff_type <- function(x)
{
    cl <- class(x)
    clt <- which(grepl("^tdiff:", cl))
    return (strsplit(cl[clt], ":", fixed = TRUE)[[1L]][[2L]])
}


.get_t_unit <- function(x)
{
    cl <- class(x)
    clt <- which(grepl("^tdiff:", cl))
    type <- strsplit(cl[clt], ":", fixed = TRUE)[[1L]][[2L]]
    return (if (type != "t") type else .Call(C_tunit, x))
}


# convert single tdiff to a number-unit pair
# used by rounding funcs, cut, seq, scales / axes
# ###################################################################

.tdiff2nunit <- function(x)
{
    if ((length(x) != 1L) || !is.finite(x)) {
        mes1 <- gettextf("invalid %s argument", sQuote(deparse(substitute(x))))
        mes2 <- gettextf("single non-NA value expected")
        stop(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
    }
    n <- as.vector(x)
    if (is.numeric(x)) return (list(n = n, unit = NULL))
    unit <- .get_t_unit(x)
    if (unit == "h") n <- n / 3600 else if (unit == "min") n <- n / 60
    return (list(n = n, unit = unit))
}


# tdiff validation
# ###################################################################

.validate_tdiff <- function(x, type)
{
    if (type == "t") x <- round(x, digits = 6L)
    else {
        if (!is.integer(x)) x <- round(x)
        x <- .require_mode(x, "integer")
    }
    return (switch (type,
                    y = .Call(C_validate_tdiff_y, x),
                    q = .Call(C_validate_tdiff_q, x),
                    m = .Call(C_validate_tdiff_m, x),
                    w = .Call(C_validate_tdiff_w, x),
                    d = .Call(C_validate_tdiff_d, x),
                    t = .Call(C_validate_tdiff_t, x)))
}


# is.tdiff
# ###################################################################

#' @rdname tdiff
#' @export
is.tdiff <- function(x) inherits(x, "tdiff")


# is.numeric
# ###################################################################

#' @keywords internal
#' @export
is.numeric.tdiff <- function(x) FALSE


# as.tdiff
# ###################################################################

#' @rdname tdiff
#' @export
as.tdiff <- function(x, ...) UseMethod("as.tdiff")


#' @keywords internal
#' @export
as.tdiff.default <- function(x, ...)
{
    if (is.null(x)) return (as.tdiff.numeric(numeric(), ...))
    mes <- gettextf("%s method not defined for class %s", sQuote("as.tdiff"),
                    .class2str(x))
    stop(mes, call. = FALSE, domain = NA)
}


#' @keywords internal
#' @export
as.tdiff.tdiff <- function(x, ...) x


#' @rdname tdiff
#' @export
as.tdiff.numeric <- function(x, unit, ...)
{
    unit <- .check_unit(unit)
    td <- .unclass(x)
    smh <- c("s", "min", "h")
    if (unit %in% smh) {
        if (unit == "h") td <- td * 3600 else if (unit == "min") td <- td * 60
        tp <- "t"
    } else tp <- unit
    td <- .validate_tdiff(td, tp)
    if (any(!is.na(x) & is.na(td))) warning("NAs introduced", call. = FALSE)
    return (.tdiff(td, tp))
}


#' @keywords internal
#' @export
as.tdiff.logical <- function(x, ...) as.tdiff.numeric(as.numeric(x), ...)


#' @rdname tdiff
#' @export
as.tdiff.character <- function(x, ...)
{
    ptd <- .parse_tdiff(x)
    if (is.null(ptd)) {
        mes <-  gettextf("parse error / could not recognise format in %s",
                         sQuote("as.tdiff"))
        stop(mes, domain = NA, call. = FALSE)
    }
    return (.tdiff(ptd[[1L]], ptd[[2L]]))
}


#' @keywords internal
#' @export
as.tdiff.factor <- function(x, unit, ...)
    as.tdiff(levels(x), unit = unit, ...)[as.integer(x)]


#' @rdname tdiff
#' @export
as.tdiff.difftime <- function(x, ...)
{
    unit <- attr(x, "units")
    unit <- if (grepl("^min", unit)) "min" else substr(unit, 1L, 1L)
    td <- .unclass(x)
    smh <- c("s", "min", "h")
    if (unit %in% smh) {
        if (unit == "h") td <- td * 3600 else if (unit == "min") td <- td * 60
        tp <- "t"
    } else tp <- unit
    td <- .validate_tdiff(td, tp)
    if (any(!is.na(x) & is.na(td))) warning("NAs introduced", call. = FALSE)
    return (.tdiff(td, tp))
}



# years, quarters, months, weeks, days, hours, minutes, seconds
# ###################################################################

#' @rdname tdiff
#' @export
years <- function(x) as.tdiff.numeric(as.numeric(x), "y")

#' @rdname tdiff
#' @export
qrtrs <- function(x) as.tdiff.numeric(as.numeric(x), "q")

#' @rdname tdiff
#' @export
mnths <- function(x) as.tdiff.numeric(as.numeric(x), "m")

#' @rdname tdiff
#' @export
weeks <- function(x) as.tdiff.numeric(as.numeric(x), "w")

#' @rdname tdiff
#' @export
days <- function(x) as.tdiff.numeric(as.numeric(x), "d")

#' @rdname tdiff
#' @export
hours <- function(x) as.tdiff.numeric(as.numeric(x), "h")

#' @rdname tdiff
#' @export
mins <- function(x) as.tdiff.numeric(as.numeric(x), "min")

#' @rdname tdiff
#' @export
secs <- function(x) as.tdiff.numeric(as.numeric(x), "s")



# coercion
# ###################################################################

#' @rdname tdiff
#' @export
as.character.tdiff <- function(x, ...)
{
    if (...length()) return (format.tdiff(x, ...))
    if (!length(x)) return (character())
    type <- .get_tdiff_type(x)
    xc <- if (type == "t") .Call(C_tdiff_t2char, x)
          else .Call(C_tdiff_yqmwd2char, x, type)
    return (xc)
}


#' @rdname tdiff
#' @export
format.tdiff <- function(x, ...)
{
    n <- length(x)
    if (!n) return (character())
    type <- .get_tdiff_type(x)
    fx <- as.character(x)
    aux <- if (type != "t") .Call(C_aux_tdiff, as.vector(x), type)
           else .Call(C_aux_tdiff, as.integer(round(as.vector(x) / 86400)), "d")
    fx <- .Call(C_format_tdiff, fx, aux)
    nms <- names(x)
    names(fx) <- nms
    return (fx)
}


#' @keywords internal
#' @export
as.double.tdiff <- function(x, ...) as.vector(x, "double")


#' @keywords internal
#' @export
as.integer.tdiff <- function(x, ...)
{
    un <- .get_t_unit(x)
    if (un == "h") return (as.integer(as.vector(x) / 3600))
    if (un == "min") return (as.integer(as.vector(x) / 60))
    if (un == "s") return (as.vector(x, "integer"))
    return (as.vector(x))
}


#' @keywords internal
#' @export
as.list.tdiff <- function(x, ...)
{
    tp <- .get_tdiff_type(x)
    res <- NextMethod("as.list")
    return (lapply(res, function(x) .tdiff(x, tp)))
}


#' @rdname tdiff
#' @export
as.data.frame.tdiff <- function(x, ...)
    as.data.frame.vector(x, nm = paste(deparse(substitute(x), width.cutoff = 50L),
                                       collapse = " "), ...)



# indexing
# ###################################################################

#' @rdname tdiff
#' @export
`[.tdiff` <- function(x, i) .cp_attr(NextMethod("["), x)


#' @rdname tdiff
#' @export
`[<-.tdiff` <- function(x, i, value)
{
    xunit <- .get_t_unit(x)
    if (is.numeric(value) || is.logical(value)) {
        value <- .unclass(as.tdiff(value, xunit))
        return (NextMethod("[<-"))
    } else value <- as.tdiff(value)
    if (.get_tdiff_type(x) != .get_tdiff_type(value)) {
        mes <- gettextf("time unit mismatch in %s", sQuote("[<-.tdiff"))
        mes <- paste0(mes, ": ", .t_unit2char(xunit), ", ",
                      .t_unit2char(.get_t_unit(value)))
        stop(mes, call. = FALSE, domain = NA)
    }
    return (NextMethod("[<-"))
}


#' @rdname tdiff
#' @export
`[[.tdiff` <- function(x, i) .cp_attr(NextMethod("[["), x)


#' @rdname tdiff
#' @export
`[[<-.tdiff` <- function(x, i, value)
{
    xunit <- .get_t_unit(x)
    if (is.numeric(value) || is.logical(value)) {
        value <- .unclass(as.tdiff(value, xunit))
        return (NextMethod("[[<-"))
    } else value <- as.tdiff(value)
    if (.get_tdiff_type(x) != .get_tdiff_type(value)) {
        mes <- gettextf("time unit mismatch in %s", sQuote("[[<-.tdiff"))
        mes <- paste0(mes, ": ", .t_unit2char(xunit), ", ",
                      .t_unit2char(.get_t_unit(value)))
        stop(mes, call. = FALSE, domain = NA)
    }
    return (NextMethod("[[<-"))
}



# `length<-.tdiff`
# ###################################################################

#' @keywords internal
#' @export
`length<-.tdiff` <- function(x, value) .cp_attr(NextMethod("length<-"), x)


# rep
# ###################################################################

#' @rdname tdiff
#' @export
rep.tdiff <- function(x, ...) .cp_attr(NextMethod("rep"), x)


# concatenation
# ###################################################################

#' @rdname tdiff
#' @export
c.tdiff <- function(...)
{
    argl <- list(...)
    argl <- lapply(argl, function(x) if (is.numeric(x) || is.logical(x)) x
                                     else as.tdiff(x))
    istdiff <- sapply(argl, is.tdiff)
    if (!any(istdiff)) {
        mes <- gettextf("cannot determine result type in %s", sQuote("c.tdiff"))
        stop(mes, call. = FALSE, domain = NA)
    }
    type <- sapply(argl[istdiff], function(x) .get_tdiff_type(x))
    type <- unique(type)
    if (length(type) > 1L) {
        mes <- gettextf("time unit mismatch in %s", sQuote("c.tdiff"))
        stop(mes, call. = FALSE, domain = NA)
    }

    if (!all(istdiff)) {
        argl[!istdiff] <- lapply(argl[!istdiff], function(x) .validate_tdiff(x, type))
    }

    return (.tdiff(unlist(lapply(argl, .unclass)), type))
}


# Math, Summary, Complex groups, mean
# ###################################################################

#' @rdname tdiff
#' @export
Math.tdiff <- function(x, ...)
{
    if (!(.Generic %in% c("abs", "sign", "round", "signif", "cummin", "cummax"))) {
        mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                        dQuote("tdiff"))
        stop(mes, call. = FALSE, domain = NA)
    }

    if (.Generic == "sign") return (sign(.unclass(x)))

    if (.Generic %in% c("round", "signif")) {
        res <- do.call(.Generic, list(as.numeric(x), ...))
        res <- .cp_attr(res, x)
        names(res) <- names(x)
        return (res)
    }
    res <- do.call(.Generic, list(.unclass(x), ...))
    return (.cp_attr(res, x))
}


#' @rdname tdiff
#' @export
Summary.tdiff <- function(..., na.rm = FALSE)
{
    if (!(.Generic %in% c("all", "any", "min", "max", "range", "sum"))) {
        mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                        dQuote("tdiff"))
        stop(mes, call. = FALSE, domain = NA)
    }

    .checkTRUEFALSE(na.rm)
    x <- if (...length() == 1L) ..1 else c(...)
    # all, any
    if (.Generic %in% c("all", "any"))
        return (suppressWarnings(NextMethod(x, na.rm = na.rm)))
    # range
    if (.Generic == "range")
        return (.cp_attr(.Call(C_range, x, na.rm), x))
    # generic code
    res <- NextMethod(.Generic, x, na.rm = na.rm)
    res <- .validate_tdiff(res, .get_tdiff_type(x))
    return (.cp_attr(res, x))
}


#' @keywords internal
#' @export
Complex.tdiff <- function(z)
{
    mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                    dQuote("tdiff"))
    stop(mes, call. = FALSE, domain = NA)
}


#' @rdname tdiff
#' @export
mean.tdiff <- function(x, na.rm = FALSE, ...)
{
    type <- .get_tdiff_type(x)
    mn <- mean(as.vector(x), na.rm = na.rm, ...)
    if (type != "t") mn <- as.integer(round(mn))
    return (.tdiff(mn, type))
}


# xtfrm
# ###################################################################

#' @keywords internal
#' @export
xtfrm.tdiff <- function(x) as.vector(x)


# unique
# ###################################################################

#' @rdname tdiff
#' @export
unique.tdiff <- function(x, ...)
    .cp_attr(NextMethod("unique", x, incomparables = FALSE), x)

