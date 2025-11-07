#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############## #
# time intervals #
# ############## #


# tinterval constructor
# ###################################################################

#' Time Intervals
#'
#' @description
#' Objects of auxiliary \code{tinterval} class represent time intervals as pairs
#' of time indices (start and end). Time intervals can be constructed via a call
#' to \code{tinterval} function or using convenience \code{\%--\%} operator.
#' Open-ended intervals are supported.
#' The main applications of this class are set operations (see \link{set-ops})
#' and checking if a particular time index belongs to (one of) given interval(s)
#' (see \code{\link{match_t}}).
#'
#' @details
#' \code{tinterval} constructor takes two arguments: beginnings and ends of intervals.
#' Additional arguments (passed via \code{...}) are forwarded to \code{as.tind}
#' method. \code{x \%--\% y} is equivalent to \code{tinterval(x, y)}.
#'
#' \code{as.tinterval} can be used to construct time intervals from character
#' strings, two-element lists, or two-column data frames. Additionally,
#' \code{as.tinterval} allows to convert time intervals represented using
#' one type of time indices to time intervals represented by time indices
#' of higher resolution (for example months to dates).
#'
#' Internally, time intervals are represented by lists of two vectors. However,
#' in operations they behave like vectors with standard indexing and replacement
#' operators implemented.
#'
#' Interval limits can be accessed via \code{$} operator: \code{x$start}
#' returns vector of beginnings of intervals in \code{x} and \code{x$end}
#' vector of ends.
#'
#' For discrete time indices (represented as integers, i.e. years, quarters,
#' months, weeks, dates, arbitrary integer indices) time interval \code{a \%--\% b}
#' represents all indices falling in \code{a} or after and in \code{b} or before, i.e.
#' the set: \eqn{\{x: a \le x \wedge x \le b\} = \{a, a + 1, \dots, b - 1, b\}}.
#' For continuous time indices (representing point in time, i.e. date-time, time of day,
#' arbitrary numeric indices) time interval \code{a \%--\% b} represents
#' all indices starting with \code{a} and \emph{before} \code{b}, i.e. the set:
#' \eqn{[a, b)}.
#' The difference in interpretations between discrete and continuous time indices
#' assures consistency during conversions.
#' Consider time interval \code{"2025-08-02" \%--\% "2025-08-03"}. This represents
#' all date-time indices falling on one of those two days, so exactly \code{2025-08-02 00:00}
#' or after but before \code{2025-08-04 00:00}.
#'
#' @param start an object of \code{tind} class or an R object coercible to it,
#'              beginning of the interval(s).
#' @param end an object of \code{tind} class or an R object coercible to it,
#'              end of the interval(s).
#' @param x an object of \code{tinterval} class or an R object passed
#'          to \code{as.tinterval}.
#' @param type a character determining time index type or \code{NULL}.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#' @param ... objects of \code{tinterval} class to be concatenated by \code{c}
#'            or additional arguments passed to or from methods.
#' @param sep a character string used as separator between start and end
#'            of an interval (\code{" -- "} by default).
#' @param open a character string used to print open interval ends
#'             (\code{"..."} by default).
#' @param aux a logical value, if \code{TRUE} (the default), auxiliary
#'            information (time spans of intervals) is added to the output.
#' @param i an integer vector of indices or a logical vector indicating selection.
#' @param value replacement value, should be coercible to \code{tinterval}.
#' @param empty a character string used to mark empty intervals (\code{"-"} by default).
#' @param object an object of \code{tinterval} class.
#'
#' @return \code{tinterval}, \code{\%--\%}, and \code{as.tinterval} return
#' objects of \code{tinterval} class.
#'
#' \code{is.tinterval} returns a logical value.
#'
#' In general, methods for \code{tinterval} return objects of \code{tinterval} class.
#'
#' \code{as.character} and \code{format} return character vectors.
#'
#' \code{as.list} and \code{as.data.frame} return a two-element list and
#' a two-column data frame, respectively. Names are set to \code{c("start", "end")}.
#'
#' \code{print} returns its argument invisibly and is used for its side effect.
#'
#' \code{summary} returns an object of class \code{c("summaryDefault", "table")}.
#'
#' @seealso \link{set-ops} for the description of set operations on time intervals,
#' \code{\link{match_t}} for matching time indices to time intervals.
#'
#' @name tinterval
#'
#' @examples
#' td <- today()
#' # from today till the day after tomorrow
#' td %--% (td + 2)
#' # from today till the end of next year
#' td %--% (as.year(td) + 1)
#' # from the beginning of the year till today
#' as.year(td) %--% td
#' # 9 to 5
#' as.time("9am") %--% as.time("5pm")
#' # 7 to 9 and 4 to 6 via constructor...
#' tinterval(as.time(c("7am", "4pm")), as.time(c("9am", "6pm")))
#' # ... or more naturally via concatenation
#' c(as.time("7am") %--% as.time("9am"), as.time("4pm") %--% as.time("6pm"))
#' # automatic parsing
#' as.tinterval(c("2023-01 -- 2024-06", "2024-12 -- 2025-03"))
#' # empty time interval
#' as.tinterval(c("2024-01 -- 2023-06"))
#' # open time interval
#' "2024-01" %--% NULL
#' "2024-01" %--% as.month(NA)
#' as.tinterval(c("2024-01 -- ..."))
#' # +/- operators
#' (x <- tinterval(td, td + 2))
#' x + c(0, 7, 14)
#' x %+w% 0:2
#' # indexing
#' (x <- "2023-01" %--% "2024-06")
#' (x <- x %+y% c(0, 2, 4))
#' x[2:3]
#' x[-1]
#' # beginnings and ends of intervals
#' x$start
#' x$end
#' # conversion from interval represented by months to dates
#' (x <- "2025-07" %--% "2025-08")
#' as.tinterval(x, "d")
#' # conversion from interval represented by dates to date-time (see Details)
#' (x <- "2025-08-02" %--% "2025-08-03")
#' as.tinterval(x, "t")
#'
NULL


# internal tinterval constructor
# ###################################################################

.tinterval <- function(start, end)
{
    if (.get.type(start) == "h") {
        if (anyNA(start)) start[is.na(start)] <- 0
        if (anyNA(end)) end[is.na(end)] <- 86400
    }
    structure(list(start = start, end = end), class = "tinterval")
}



# tinterval constructor
# ###################################################################

#' @rdname tinterval
#' @export
tinterval <- function(start = NULL, end = NULL, ...)
{
    if (is.null(start) && is.null(end)) {
        mes <- gettextf("%s and %s arguments missing or NULL", sQuote("start"),
                        sQuote("end"))
        stop(mes, call. = FALSE, domain = NA)
    }
    if (!is.null(start)) start <- as.tind(start, ...)
    if (!is.null(end)) end <- as.tind(end, ...)
    if (is.null(start)) start <- as.tind(rep(NA, length(end)), .get.type(end),
                                         .get.tz(end))
    if (is.null(end)) end <- as.tind(rep(NA, length(start)), .get.type(start),
                                     .get.tz(start))
    # lenghts
    ns <- length(start)
    ne <- length(end)
    if (ns != ne) {
        mes <- gettextf("different lengths of arguments: %d, %d", ns, ne)
        stop(mes, call. = FALSE, domain = NA)
    }
    # handle types
    ts <- .get.type(start)
    te <- .get.type(end)
    if (ts == te) {
        if (ts == "t" && (.get.tz(start) != .get.tz(end))) {
            .warn_diff_tz(.get.tz(start), .get.tz(end), first = TRUE)
            end <- .set.tz(end, .get.tz(start))
        }
    } else {
        tt <- .max_res(c(ts, te))
        if (ts == tt) end <- .cast(end + 1, ts, .get.tz(start)) - !.is.instant(ts)
        else start <- .cast(start, te, .get.tz(end))
    }
    # names
    snms <- names(start)
    enms <- names(end)
    if (!is.null(snms) && !is.null(enms)) {
        nms <- snms
        id <- snms != enms
        if (anyNA(id)) id[is.na(id)] <- TRUE
        nms[id] <- paste0(snms[id], ".", enms[id])
        names(end) <- names(start) <- nms
    } else if (!is.null(snms)) {
        names(end) <- names(start)
    } else if (!is.null(enms)) {
        names(start) <- names(end)
    }

    return (.tinterval(start, end))
}


#' @rdname tinterval
#' @export
`%--%` <- function(start, end) tinterval(start, end)



# is.tinterval
# ###################################################################

#' @rdname tinterval
#' @export
is.tinterval <- function(x) inherits(x, "tinterval")



# as.tinterval
# ###################################################################

#' @rdname tinterval
#' @export
as.tinterval <- function(x, ...) UseMethod("as.tinterval")


#' @keywords internal
#' @export
as.tinterval.default <- function(x, ...)
{
    mes <- gettextf("%s method not defined for class %s", sQuote("as.tinterval"),
                    .class2str(x))
    stop(mes, call. = FALSE, domain = NA)
}


#' @rdname tinterval
#' @export
as.tinterval.character <- function(x, sep, ...)
{
    if (missing(sep)) {
        seps <- c(" -- ", "--", " / ", "/")
        for (sep in seps) {
            res <- .parse_tinterval(x, sep)
            if (!is.null(res)) break
        }
    } else {
        if (!is.character(sep) || length(sep) != 1L || !nchar(sep)) {
            mes0 <- gettextf("invalid %s argument", sQuote("sep"))
            mes1 <- gettextf("nonempty character string expected")
            stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
        }
        res <- .parse_tinterval(x, sep)
    }
    if (is.null(res)) {
        mes <-  gettextf("parse error / could not recognise format in %s",
                         sQuote("as.tinterval"))
        stop(mes, domain = NA, call. = FALSE)
    }
    res <- as.tind(c(res$start, res$end), ...)
    n <- length(res) %/% 2L
    return (.tinterval(res[1L:n], res[n + 1L:n]))
}


#' @rdname tinterval
#' @export
as.tinterval.tinterval <- function(x, type = NULL, tz = NULL, ...)
{
    if (missing(type) && (missing(tz) || is.null(tz))) return (x)
    typetz <- .check_type_tz(type, tz)

    st <- x$start
    en <- x$end
    xtype <- .get.type(st)

    if (typetz$type == xtype) {
        if (is.null(tz)) return (x)
        return (.tinterval(.set.tz(st, typetz$tz), .set.tz(en, typetz$tz)))
    }

    type <- typetz$type
    tz <- typetz$tz

    if (!(type %in% .hi_res_cast(xtype)))
        .stop_cast(xtype, type)

    return (.tinterval(.cast(st, type, tz),
                       .cast(en + 1, type, tz) - !.is.instant(type)))
}


#' @rdname tinterval
#' @export
as.tinterval.list <- function(x, ...)
{
    if (length(x) != 2L) stop("expected a 2-element list")
    return (tinterval(x[[1L]], x[[2L]], ...))
}


#' @rdname tinterval
#' @export
as.tinterval.data.frame <- function(x, ...)
{
    if (ncol(x) != 2L) stop("expected a 2-column data frame")
    res <- as.tinterval.list(as.list(x, ...))
    if (nrow(x) && any(rownames(x) != as.character(1L:nrow(x))))
        names(res) <- rownames(x)
    return (res)
}



# coercion
# ###################################################################

#' @rdname tinterval
#' @export
as.character.tinterval <- function(x, ...)
{
    if (...length()) return (format.tinterval(x, ...))
    if (!length(x)) return (character())
    nms <- names(x$start)
    res <- .Call(C_tinterval2char, as.character(c(x$start, x$end)))
    names(res) <- nms
    return (res)
}


#' @rdname tinterval
#' @export
format.tinterval <- function(x, sep = " -- ", open = "...",
                             aux = TRUE, empty = "-", ...)
{
    if (!(n <- length(x$start))) return (character())
    if (!is.character(sep) || length(sep) != 1L || !nchar(sep)) {
        mes0 <- gettextf("invalid %s argument", sQuote("sep"))
        mes1 <- gettextf("nonempty character string expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    if (!is.character(open) || length(open) != 1L) {
        mes0 <- gettextf("invalid %s argument", sQuote("open"))
        mes1 <- gettextf("character string expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    .checkTRUEFALSE(aux)
    if (!is.character(empty) || length(empty) != 1L) {
        mes0 <- gettextf("invalid %s argument", sQuote("empty"))
        mes1 <- gettextf("character string expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    nms <- names(x$start)
    type <- .get.type(x$start)
    # format indices
    fx <- format(c(x$start, x$end), ...)
    # aux info
    if (aux) {
        dx <- x$end - x$start + !.is.instant(.get.type(x$start))
        iiempty <- as.vector(dx) <= 0
        iiempty <- !is.na(iiempty) & iiempty
        iiopen <- is.na(x$start) | is.na(x$end)
        iifinite <- !iiempty & !iiopen
        fdx <- character(n)
        fdx[iiopen] <- open
        fdx[iiempty] <- empty
        fdx[iifinite] <- as.character(dx[iifinite])
        if (type %in% c("q", "m", "w", "d", "t")) {
            if (type == "t") {
                faux <- as.integer(round(as.vector(dx[iifinite]) / 86400))
                type <- "d"
            } else faux <- as.vector(dx[iifinite])
            faux <- .Call(C_aux_tdiff, faux, type)
            iiaux <- nzchar(faux)
            fdx[which(iifinite)[iiaux]] <- faux[iiaux]
        }
    } else fdx <- character(n)
    # put together
    fx <- .Call(C_format_tinterval, fx, fdx, sep, open)
    names(fx) <- nms
    return (fx)
}


#' @rdname tinterval
#' @export
as.list.tinterval <- function(x, ...) unclass(x)


#' @rdname tinterval
#' @export
as.data.frame.tinterval <- function(x, ...) data.frame(start = x$start, end = x$end)



# ti_type
# ###################################################################

#' @keywords internal
#' @export
ti_type.tinterval <- function(x, long = TRUE, valid = FALSE)
    .ti_type(.get.type(x$start), long, valid, rm.names = TRUE)



# names
# ###################################################################

#' @keywords internal
#' @export
names.tinterval <- function(x) names(x$start)


#' @keywords internal
#' @export
`names<-.tinterval` <- function(x, value)
{
    st <- x$start
    en <- x$end
    names(st) <- names(en) <- value
    return (.tinterval(st, en))
}



# length, `length<-.tinterval` (disabled)
# ###################################################################

#' @keywords internal
#' @export
length.tinterval <- function(x) length(x$start)


#' @keywords internal
#' @export
`length<-.tinterval` <- function(x, value)
{
    mes <- gettextf("%s method not defined for class %s", sQuote("length<-"),
                    .class2str(x))
    stop(mes, call. = FALSE, domain = NA)
}



# indexing
# ###################################################################

#' @rdname tinterval
#' @export
`[.tinterval` <- function(x, i)
{
    if (missing(i)) return (x)
    return (.tinterval(x$start[i], x$end[i]))
}


#' @rdname tinterval
#' @export
`[[.tinterval` <- function(x, i) .tinterval(x$start[[i]], x$end[[i]])


#' @rdname tinterval
#' @export
`[<-.tinterval` <- function(x, i, value)
{
    if (missing(i)) i <- seq_along(x$start)
    value <- as.tinterval(value)
    xtp <- .get.type(x$start)
    if (xtp != .get.type(value$start)) {
        value <- as.tinterval(value, xtp, .get.tz(x$start))
    } else if (xtp == "t") {
        xtz <- .get.tz(x$start)
        vtz <- .get.tz(value$start)
        if (xtz != vtz) {
            .warn_diff_tz(xtz, vtz)
            value <- as.tinterval(value, tz = xtz)
        }
    }
    x$start[i] <- value$start
    x$end[i] <- value$end
    return (x)
}


#' @rdname tinterval
#' @export
`[[<-.tinterval` <- function(x, i, value)
{
    value <- as.tinterval(value)
    xtp <- .get.type(x$start)
    if (xtp != .get.type(value$start)) {
        value <- as.tinterval(value, xtp, .get.tz(x$start))
    } else if (xtp == "t") {
        xtz <- .get.tz(x$start)
        vtz <- .get.tz(value$start)
        if (xtz != vtz) {
            .warn_diff_tz(xtz, vtz)
            value <- as.tinterval(value, tz = xtz)
        }
    }
    x$start[[i]] <- value$start
    x$end[[i]] <- value$end
    return (x)
}



# $
# ###################################################################

#' @keywords internal
#' @exportS3Method utils::.DollarNames
.DollarNames.tinterval <- function(x, pattern = "")
    grep(pattern, c("start", "end"), value = TRUE)


#' @keywords internal
#' @export
`$<-.tinterval` <- function(x, name, value)
{
    cn <- c("start", "end")
    jj <- match(name, cn)
    if (is.na(jj)) {
        jj <- pmatch(name, c("start", "end"))
        if (!is.na(jj) && getOption("warnPartialMatchDollar", default = FALSE)) {
            mes <- gettextf("partial match of %s to %s", sQuote(name), sQuote(cn[jj]))
            warning(mes, call. = FALSE, domain = NA)
        }
    }

    old <- if (jj == 1L) unclass(x)$end else unclass(x)$start
    otype <- .get.type(old)
    otz <- .get.tz(old)
    if (is.null(value)) {
        new <- as.tind(rep(NA, length(old)), otype, otz)
    } else {
        new <- as.tind(value)
        if (length(new) != length(old)) {
            mes <- gettextf("replacement is of length %d, number of intervals is %d",
                            length(new), length(old))
            stop(mes, call. = FALSE, domain = NA)
        }
        ntype <- .get.type(new)
        ntz <- .get.tz(new)
        if (ntype != otype) {
            if (!(otype %in% .hi_res_cast(ntype)))
                .stop_cast(otype, ntype)
            new <- if (jj == 1L) .cast(new, otype, otz)
                   else .cast(new + 1, otype, otz) - !.is.instant(otype)
        } else if ((otype == "t") && (ntz != otz)) {
            .warn_diff_tz(otz, ntz, TRUE)
            new <- .set.tz(new, otz)
        }
    }

    names(new) <- names(old)

    return (if (jj == 1L) .tinterval(new, old) else .tinterval(old, new))
}



# concatenation
# ###################################################################

#' @rdname tinterval
#' @export
c.tinterval <- function(...)
{
    argl <-  list(...)
    if (!length(argl) || !all(sapply(argl, is.tinterval))) {
        mes <- gettextf("expected all arguments to be of %s class in %s",
                        dQuote("tinterval"), sQuote("c.tinterval"))
        stop(mes, call. = FALSE, domain = NA)
    }
    types <- unique(sapply(argl, function(x) .get.type(x$start)))
    type <- .max_res(types)
    if (type == "t") {
        tz <- unique(unlist(lapply(argl, function(x) .get.tz(x$start))))
        if (length(tz) > 1L) {
            tz <- tz[1]
            .warn_diff_tz(tz, first = TRUE)
        }
    } else tz <- NULL

    sl <- lapply(argl, function(x) x$start)
    el <- lapply(argl, function(x) x$end)
    sl <- lapply(sl, function(x) if (.get.type(x) == type) x else
                     as.tind(x, type = type, tz = tz))
    el <- lapply(el, function(x) if (.get.type(x) == type) x else
                     as.tind(x + 1L, type = type, tz = tz) - !.is.instant(type))
    if (type == "t") { # turn off multiple warnings on different time zones
        sl <- lapply(sl, function(x) .set.tz(x, tz))
        el <- lapply(el, function(x) .set.tz(x, tz))
    }

    return (.tinterval(do.call("c.tind", sl), do.call("c.tind", el)))
}



# disable methods
# ###################################################################

#' @keywords internal
#' @export
rep.tinterval <- function(x, ...)
{
    mes <- gettextf("%s method not defined for class %s", sQuote("rep"),
                    dQuote("tinterval"))
    stop(mes, call. = FALSE, domain = NA)
}


#' @keywords internal
#' @export
Math.tinterval <- function(x, ...)
{
    mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                    dQuote("tinterval"))
    stop(mes, call. = FALSE, domain = NA)
}


#' @keywords internal
#' @export
Summary.tinterval <- function(..., na.rm = FALSE)
{
    mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                    dQuote("tinterval"))
    stop(mes, call. = FALSE, domain = NA)
}


#' @keywords internal
#' @export
Complex.tinterval <- function(z)
{
    mes <- gettextf("%s method not defined for class %s", sQuote(.Generic),
                    dQuote("tinterval"))
    stop(mes, call. = FALSE, domain = NA)
}


#' @keywords internal
#' @export
mean.tinterval <- function(x, ...)
{
    mes <- gettextf("%s method not defined for class %s", sQuote("mean"),
                    dQuote("tinterval"))
    stop(mes, call. = FALSE, domain = NA)
}

