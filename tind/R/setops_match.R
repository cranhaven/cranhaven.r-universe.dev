#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################################################# #
# set operations on time intervals and time indices #
# ################################################# #


#' Set Operations on Time Intervals and Time Indices
#'
#' @description
#' Time intervals can be thought of as subsets of time line (set of integers or
#' real line, depending on index type). The following functions perform
#' operations on these sets.
#'
#' \code{unique} method for objects of \code{tinterval} class returns unique
#' representation as ordered sum of disjoint, non-adjacent intervals.
#'
#' \code{!} (negation) operator for objects of \code{tinterval} class returns
#' set-theoretical complement of the argument.
#'
#' \code{intersect_t}, \code{union_t}, \code{setdiff_t} return intersection,
#' union and (asymmetric) set difference. These three functions accept
#' both time intervals and time indices.
#'
#' Behaviour of \code{!}, \code{intersect_t}, \code{union_t}, and \code{setdiff_t}
#' is consistent with behaviour of \code{\link{\%in_t\%}} operator.
#' Consistency is also assured under type conversions.
#'
#' For time indices, \code{intersect_t}, \code{union_t}, \code{setdiff_t}
#' behave just like \code{intersect}, \code{union}, \code{setdiff} from \pkg{base}
#' (see \link[base]{sets}) but preserve class attribute.
#'
#' @details
#' For discrete time indices (represented as integers, i.e. years, quarters,
#' months, weeks, dates, arbitrary integer indices) time intervals represent
#' the following sets (ignoring empty, i.e. with \eqn{a_i > b_i}):
#' \deqn{\bigcup_{i = 1}^n A_i = \bigcup_{i = 1}^n \{x: a_i \le x \le b_i\} = \bigcup_{i = 1}^n \{a_i, a_i + 1, \dots, b_i - 1, b_i\}.}
#' \code{unique} returns the unique (canonical) representation of the set above:
#' \deqn{\bigcup_{i = 1}^{n'} A'_i = \bigcup_{i = 1}^{n'} \{a'_i, a'_i + 1, \dots, b'_i - 1, b'_i\}}
#' with \eqn{a'_i \le b'_i < a'_{i + 1} - 1}, i.e. as a sum of ordered, non-empty,
#' non-adjacent intervals.
#'
#' For continuous time indices (representing point in time, i.e. date-time, time of day,
#' arbitrary numeric indices) time intervals represent the following sets
#' (ignoring empty, i.e. with \eqn{a_i \ge b_i}):
#' \deqn{\bigcup_{i = 1}^n A_i = \bigcup_{i = 1}^n [a_i, b_i).}
#' \code{unique} returns unique representation of the set above:
#' \deqn{\bigcup_{i = 1}^{n'} A'_i = \bigcup_{i = 1}^{n'} [a'_i, b'_i)}
#' with \eqn{a'_i < b'_i < a'_{i + 1}}, i.e. as a sum of ordered, non-empty,
#' non-adjacent intervals.
#'
#' Complement of a single interval for integer indices
#' \deqn{\{x: a \le x \le b\} = \{a, a + 1, \dots, b - 1, b\}}
#' is:
#' \deqn{\{x: x < a\} \cup \{x: x > b\} = \{x: x \le a - 1\} \cup \{x: x \ge b + 1\} = \{\dots, a - 2, a - 1\} \cup \{b + 1, b + 2, \dots\}.}
#' Complement of a single interval for continuous indices
#' \deqn{[a, b)}
#' is:
#' \deqn{(-\infty, a) \cup [b, \infty).}
#' Complement of a sum of intervals is the intersection of complements.
#'
#' Set operations always return results in the canonical representation.
#'
#' @param x an object of \code{tinterval} class for \code{unique} method
#'          and \code{!} operator, \code{tinterval} or \code{tind}
#'          for \code{intersect_t}, \code{union_t}, or \code{setdiff_t}.
#' @param y an object of \code{tinterval} or \code{tind} class.
#' @param ... (ignored) further arguments passed to or from other methods.
#'
#' @return An object of \code{tinterval} or \code{tind} class representing result of the set
#' operation.
#'
#' @seealso \code{\link{tinterval}} for an overview of time interval class,
#' \code{\link{match_t}} for matching time indices
#'
#' @examples
#' (x <- tinterval("2025-03-15", "2025-03-20") + c(0, 4, 14))
#' # unique representation (non-overlapping intervals)
#' unique(x)
#' # complement
#' !x
#' # binary set operations
#' (y <- tinterval("2025-03-01", "2025-03-31"))
#' intersect_t(x, y)
#' union_t(x, y)
#' setdiff_t(x, y)
#' setdiff_t(y, x)
#' # different types of indices
#' (y <- tinterval("2025-W10", "2025-W11"))
#' intersect_t(x, y)
#' union_t(x, y)
#' setdiff_t(x, y)
#' setdiff_t(y, x)
#' # check
#' (y <- as.tinterval(y, type = "d"))
#' intersect_t(x, y)
#' union_t(x, y)
#' setdiff_t(x, y)
#' setdiff_t(y, x)
#'
#' @name set-ops
#'
NULL


#' @rdname set-ops
#' @export
unique.tinterval <- function(x, ...)
{
    if (!length(x)) return (x)
    start <- x$start
    end <- x$end
    names(start) <- names(end) <- NULL
    tp <- .get.type(start)
    # rm empty
    nempty <- is.na(start) | is.na(end) | end > start - !.is.instant(tp)
    start <- start[nempty]
    end <- end[nempty]
    n <- length(start)
    if (!n) return (.tinterval(start, end))
    # sort
    os <- order(start, na.last = FALSE)
    start <- start[os]
    end <- end[os]
    # merge adjacent/overlapping intervals
    if (n > 1L) {
        cmmend <- cummax(end)
        df <- cmmend[1L:(n - 1L)] - start[2L:n] + !.is.instant(tp)
        rm <- c(FALSE, df >= 0L)
        rm <- rm | is.na(rm)
        is <- which(diff(c(0L, rm)) > 0L) - 1L
        ie <- which(diff(c(rm, 0L)) < 0L)
        end[is] <- cmmend[ie]
        end <- end[!rm]
        start <- start[!rm]
    }
    # handle corner cases - convert endpoints at range limits to NAs
    if (tp %in% c("y", "q", "m", "w", "d", "t")) {
        if (!is.na(start[1L]) && (as.numeric(start[1L]) == .limits(tp)[1L]))
            start[1L] <- NA
        n <- length(start)
        if (!is.na(end[n]) && (as.numeric(end[n]) == .limits(tp)[2L]))
            end[n] <- NA
    }

    return (.tinterval(start, end))
}


#' @rdname set-ops
#' @export
`!.tinterval` <- function(x)
{
    x <- unique(x)
    n <- length(x)
    start <- x$start
    end <- x$end
    # special adjustment for "h"
    if (n && (.get.type(start) == "h")) {
        if (as.numeric(start[1L]) == 0) start[1L] <- NA_real_
        if (as.numeric(end[n]) == 86400) end[n] <- NA_real_
    }

    if (!n) {
        return (.tinterval(start[1L], end[1L]))
    } else if (n == 1L) {
        if (is.na(start) && is.na(end)) {
            return (.tinterval(start[0L], end[0L]))
        } else if (is.na(start) || is.na(end)) {
            nstart <- end
            nend <- start
        } else {
            nstart <- end[2L:1L]
            nend <- start[1L:2L]
        }
    } else {
        i <- 1L:(n - 1L)
        j <- 2L:n
        if (!is.na(start[1L])) { i <- c(n + 1L, i); j <- c(1L, j) }
        if (!is.na(end[n])) { i <- c(i, n); j <- c(j, n + 1L) }
        nstart <- end[i]
        nend <- start[j]
    }

    if (!.is.instant(.get.type(nstart))) {
        nstart <- nstart + 1L
        nend <- nend - 1L
    }

    return (.tinterval(nstart, nend))
}


#' @rdname set-ops
#' @export
intersect_t <- function(x, y)
{
    if ((!is.tind(x) || !is.tind(y)) && (!is.tinterval(x) || !is.tinterval(y))) {
        mes0 <- gettextf("invalid arguments of %s", sQuote("intersect_t"))
        mes1 <- gettextf("expected two objects of %s or %s class",
                         sQuote("tinterval"), sQuote("tind"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    # tind
    if (is.tind(x)) {
        xtp <- .get.type(x)
        ytp <- .get.type(y)
        if (xtp != ytp) {
            mes <- gettextf("time index type mismatch in %s", sQuote("intersect_t"))
            mes <- paste0(mes, ": ", .ti_type2char(xtp), ", ", .ti_type2char(ytp))
            stop(mes, call. = FALSE, domain = NA)
        }
        xtz <- .get.tz(x)
        if ((xtp == "t") && (xtz != .get.tz(y))) .warn_diff_tz(xtz, .get.tz(y), TRUE)
        if (.Call(C_is_ordered, x, FALSE) && .Call(C_is_ordered, y, FALSE))
            return (.Call(C_intersect_ord, x, y))
        return (.tind(intersect(as.vector(x), as.vector(y)), xtp, xtz))
    }
    # tinterval
    tx <- .get.type(x$start)
    ty <- .get.type(y$start)
    if (tx == ty) {
        tp <- tx
        tz <- .get.tz(x$start)
        if (tx == "t" && (tz != .get.tz(y$start))) {
            .warn_diff_tz(tz, .get.tz(y$start), first = TRUE)
            y <- as.tinterval(y, tz = tz)
        }
    } else {
        tp <- .max_res(c(tx, ty))
        if (tp == tx) {
            tz <- .get.tz(x$start)
            y <- as.tinterval(y, type = tp, tz = tz)
        } else {
            tz <- .get.tz(y$start)
            x <- as.tinterval(x, type = tp, tz = tz)
        }
    }

    x <- unique(x)
    y <- unique(y)
    if (!length(x)) return (x)
    if (!length(y)) return (y)

    se <- .Call(C_intersect_tint, x$start, x$end, y$start, y$end)

    return (.tinterval(.tind(se[[1L]], tp, tz), .tind(se[[2L]], tp, tz)))
}


#' @rdname set-ops
#' @export
union_t <- function(x, y)
{
    if ((!is.tind(x) || !is.tind(y)) && (!is.tinterval(x) || !is.tinterval(y))) {
        mes0 <- gettextf("invalid arguments of %s", sQuote("union_t"))
        mes1 <- gettextf("expected two objects of %s or %s class",
                         sQuote("tinterval"), sQuote("tind"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    # tind
    if (is.tind(x)) {
        xtp <- .get.type(x)
        ytp <- .get.type(y)
        if (xtp != ytp) {
            mes <- gettextf("time index type mismatch in %s", sQuote("union_t"))
            mes <- paste0(mes, ": ", .ti_type2char(xtp), ", ", .ti_type2char(ytp))
            stop(mes, call. = FALSE, domain = NA)
        }
        xtz <- .get.tz(x)
        if ((xtp == "t") && (xtz != .get.tz(y))) .warn_diff_tz(xtz, .get.tz(y), TRUE)
        if (.Call(C_is_ordered, x, FALSE) && .Call(C_is_ordered, y, FALSE))
            return (.Call(C_union_ord, x, y))
        return (.tind(union(as.vector(x), as.vector(y)), xtp, xtz))
    }
    # tinterval
    if ((.get.type(x$start) == "t") && (.get.type(y$start) == "t")) {
        if (.get.tz(x$start) != .get.tz(y$start))
            .warn_diff_tz(.get.tz(x$start), .get.tz(y$start), TRUE)
        xy <- suppressWarnings(c(unique(x), unique(y)))
    } else xy <- c(unique(x), unique(y))
    return (unique(xy))
}


#' @rdname set-ops
#' @export
setdiff_t <- function(x, y)
{
    if ((!is.tind(x) || !is.tind(y)) && (!is.tinterval(x) || !is.tinterval(y))) {
        mes0 <- gettextf("invalid arguments of %s", sQuote("setdiff_t"))
        mes1 <- gettextf("expected two objects of %s or %s class",
                         sQuote("tinterval"), sQuote("tind"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    # tind
    if (is.tind(x)) {
        xtp <- .get.type(x)
        ytp <- .get.type(y)
        if (xtp != ytp) {
            mes <- gettextf("time index type mismatch in %s", sQuote("setdiff_t"))
            mes <- paste0(mes, ": ", .ti_type2char(xtp), ", ", .ti_type2char(ytp))
            stop(mes, call. = FALSE, domain = NA)
        }
        xtz <- .get.tz(x)
        if ((xtp == "t") && (xtz != .get.tz(y))) .warn_diff_tz(xtz, .get.tz(y), TRUE)
        if (.Call(C_is_ordered, x, FALSE) && .Call(C_is_ordered, y, FALSE))
            return (.Call(C_setdiff_ord, x, y))
        return (.tind(setdiff(as.vector(x), as.vector(y)), xtp, xtz))
    }
    # tinterval
    tx <- .get.type(x$start)
    ty <- .get.type(y$start)
    if (tx == ty) {
        tp <- tx
        tz <- .get.tz(x$start)
        if (tx == "t" && (tz != .get.tz(y$start))) {
            .warn_diff_tz(tz, .get.tz(y$start), first = TRUE)
            y$start <- .set.tz(y$start, tz)
            y$end <- .set.tz(y$end, tz)
        }
    } else {
        tp <- .max_res(c(tx, ty))
        if (tp == tx) {
            tz <- .get.tz(x$start)
            y <- as.tinterval(y, type = tp, tz = tz)
        } else {
            tz <- .get.tz(y$start)
            x <- as.tinterval(x, type = tp, tz = tz)
        }
    }

    return (intersect_t(x, !y))
}



#' Matching Time Indices
#'
#' @description
#' \code{match_t} and \code{\%in_t\%} allow for matching time indices
#' to time intervals and to other sets of time indices including cases when
#' \code{table} argument is of different type than \code{x} (of lower resolution).
#'
#' @details
#' \code{\%in_t\%} always returns \code{TRUE}/\code{FALSE}. \code{NA}s
#' in \code{x} argument are \emph{never} matched (\code{FALSE} is returned).
#'
#' @return \code{match_t} and \code{\%in_t\%} return integer and logical vectors,
#' respectively. The length of the result equals length of \code{x}.
#'
#' @note
#' Since \code{match} and \code{\%in\%} are not implemented in \pkg{base} as
#' S3 generics, new functions had to be implemented.
#'
#' @param x an object of \code{tind} class.
#' @param table an object of \code{tinterval} or \code{tind} class.
#' @param nomatch an integer value to be returned when no match is found.
#'
#' @examples
#' # match dates to months
#' (x <- as.date("2025-03-02") + 15 * (0:5))
#' (table <- as.month("2025-03") + -1:1)
#' match_t(x, table)
#' # match dates to time intervals representing months
#' (table <- (as.date("2025-03-01") %--% as.date("2025-03-31")) %+m% (-1:1))
#' match_t(x, table)
#' # are dates in March 2025?
#' x %in_t% "2025-03"
#' # NAs are _never_ matched
#' (x <- as.date("2025-03-02") + c(NA, 15 * (0:5)))
#' (table <- as.month("2025-03") + c(NA, -1:1))
#' match_t(x, table)
#' x %in_t% table
#'
#' @export
match_t <- function(x, table, nomatch = NA_integer_)
{
    x <- as.tind(x)
    if (!is.tind(table) && !is.tinterval(table)) {
        table1 <- tryCatch(as.tinterval(table), error = function(e) NULL)
        if (is.null(table1)) table1 <- tryCatch(as.tind(table), error = function(e) NULL)
        if (is.null(table1)) {
            mes0 <- gettextf("invalid %s argument", sQuote("table"))
            mes1 <- gettextf("expected an object of %s or %s class",
                            sQuote("tinterval"), sQuote("tind"))
            stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
        }
        table <- table1
    }
    isttint <- is.tinterval(table)

    if (!is.numeric(nomatch) || length(nomatch) != 1L) {
            mes0 <- gettextf("invalid %s argument", sQuote("nomatch"))
            mes1 <- gettextf("integer value expected")
            stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    } else nomatch <- suppressWarnings(as.integer(nomatch))

    xtp <- .get.type(x)
    xtz <- .get.tz(x)
    ttp <- if (!isttint) .get.type(table) else .get.type(table$start)
    ttz <- if (!isttint) .get.tz(table) else .get.tz(table$start)

    if (xtp == ttp) {
        if ((xtp == "t") && (xtz != ttz)) .warn_diff_tz(xtz, ttz)
    } else {
        if (!(ttp %in% .lo_res_cast(xtp))) .stop_cast(xtp, ttp)
        x <- .cast(x, ttp)
    }

    if (!isttint) {
        if (.Call(C_is_ordered, x, FALSE) && .Call(C_is_ordered, table, FALSE))
            return (.Call(C_match_ord, x, table, nomatch))
        m <- match(x, table, nomatch)
        if (anyNA(x)) m[is.na(x)] <- nomatch
        return (m)
    }

    return (.Call(C_match_tint, x, table$start, table$end, nomatch))
}


#' @rdname match_t
#' @export
`%in_t%` <- function(x, table)
{
    x <- as.tind(x)
    if (!is.tind(table) && !is.tinterval(table)) {
        table1 <- tryCatch(as.tinterval(table), error = function(e) NULL)
        if (is.null(table1)) table1 <- tryCatch(as.tind(table), error = function(e) NULL)
        if (is.null(table1)) {
            mes0 <- gettextf("invalid %s argument", sQuote("table"))
            mes1 <- gettextf("expected an object of %s or %s class",
                             sQuote("tinterval"), sQuote("tind"))
            stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
        }
        table <- table1
    }
    isttint <- is.tinterval(table)

    xtp <- .get.type(x)
    xtz <- .get.tz(x)
    ttp <- if (!isttint) .get.type(table) else .get.type(table$start)
    ttz <- if (!isttint) .get.tz(table) else .get.tz(table$start)

    if (xtp == ttp) {
        if ((xtp == "t") && (xtz != ttz)) .warn_diff_tz(xtz, ttz)
    } else {
        if (!(ttp %in% .lo_res_cast(xtp))) .stop_cast(xtp, ttp)
        x <- .cast(x, ttp)
    }

    if (!isttint) {
        if (.Call(C_is_ordered, x, FALSE) && .Call(C_is_ordered, table, FALSE))
            return (as.logical(.Call(C_match_ord, x, table, 0L)))
        i <- `%in%`(x, table)
        if (anyNA(x)) i[is.na(x)] <- FALSE
        return (i)
    }

    table <- unique(table)
    if (.Call(C_is_ordered, x, FALSE))
        return (.Call(C_in_tint_ord, x, table$start, table$end))
    return (as.logical(.Call(C_match_tint, x, table$start, table$end, 0L)))
}

