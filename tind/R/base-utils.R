#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################# #
# utility functions #
# ################# #


# change storage mode to the required
# ###################################################################

.require_mode <- function(x, mode)
{
    if (storage.mode(x) == mode) return (x)
    suppressWarnings(storage.mode(x) <- mode)
    return (x)
}


# is TRUE or FALSE?
# ###################################################################

.isTRUEFALSE <- function(x) is.logical(x) && (length(x) == 1L) && !is.na(x)

.checkTRUEFALSE <- function(x)
{
    if (!.isTRUEFALSE(x)) {
        mes0 <- gettextf("invalid %s argument", sQuote(deparse(substitute(x))))
        mes1 <- gettextf("TRUE/FALSE expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
}

# any TRUE? (handles NAs)
# ###################################################################

.anyTRUE <- function(x) isTRUE(any(x))


# class to string
# ###################################################################

.class2str <- function(x)
    dQuote(if (is.tind(x)) "tind" else if (is.tdiff(x)) "tdiff"
           else if (is.tinterval(x)) "tinterval"
           else paste0(class(x), collapse = "/"))


# match left / exact / right with ordered y w/o NAs
# used by cut, bizday, merge
# ###################################################################

# .match_left <- function(x, table)
# {
#     m <- findInterval(x, table)
#     m[!m] <- NA_integer_
#     return (m)
# }

.match_left <- function(x, table)
    .Call(C_fi2match, findInterval(x, table), x, table, -1L, NA_integer_)


# .match_right <- function(x, table)
# {
#     m <- findInterval(x, table, left.open = TRUE) + 1L
#     m[m > length(table)] <- NA_integer_
#     return (m)
# }

.match_right <- function(x, table)
    .Call(C_fi2match, findInterval(x, table, left.open = TRUE), x, table, 1L,
          NA_integer_)


# .match_exct <- function(x, table, nomatch = NA_integer_)
# {
#     fi <- findInterval(x, table)
#     fi[!fi] <- NA_integer_
#     eq <- x == table[fi]
#     eq[is.na(eq)] <- FALSE
#     fi[!eq] <- nomatch
#     return (fi)
# }

.match_exct <- function(x, table, nomatch = NA_integer_)
    .Call(C_fi2match, findInterval(x, table), x, table, 0L, nomatch)


# check arg lengths
# ###################################################################

.check_lengths <- function(...)
{
    nn <- c(...)
    if (any(!nn)) return (0L)
    mn <- max(nn)
    if (any(mn %% nn))
        warning("longer object length is not a multiple of shorter object length",
                call. = FALSE, domain = "R")
    return (mn)
}



# significant digits
# ###################################################################

.check_digits <- function(digits)
{
    ok <- is.numeric(digits) && (length(digits) == 1L)
    if (ok) {
        digits <- suppressWarnings(as.integer(round(digits)))
        ok <- digits %in% 0L:6L
    }
    if (!ok) {
        mes0 <- gettextf("invalid %s argument", sQuote("digits"))
        mes1 <- gettextf("expected: %s", "0-6")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    return (digits)
}


# an extension of base::match.arg handling named lists of choices
# ###################################################################

.match.arg <- function(arg, choices)
{
    argname <- deparse(substitute(arg))
    if (missing(choices)) { # a simplified version of what can be found in base::match.arg
        choices <- eval(formals(sys.function(-1L))[[argname]])
        if (identical(arg, choices)) return (choices[1L])
    }
    # handles character vectors, named lists of character vectors (with
    # alernative names) and named vectors / lists
    charv <- charl <- FALSE
    if (is.list(choices) && all(sapply(choices, is.character))) {
        charl <- TRUE
        .choices <- unlist(lapply(names(choices), function(n) c(n, choices[[n]])))
        admissible <- toString(paste0(dQuote(names(choices)), " (",
                                      sapply(choices, toString), ")"))
    } else if (is.character(choices)) {
        charv <- TRUE
        .choices <- choices
        admissible <- toString(dQuote(.choices))
    } else { # named vector / list
        .choices <- names(choices)
        admissible <- toString(dQuote(.choices))
    }
    errmiss <- gettextf("%s argument missing", sQuote(argname))
    errinv <- gettextf("invalid %s argument", sQuote(argname))
    admissible <- gettextf("expected one of the following: %s", admissible)

    if (is.null(arg))
        stop(paste0(errmiss, "; ", admissible), call. = FALSE, domain = NA)
    if (!is.character(arg) || length(arg) != 1L)
        stop(paste0(errinv, "; ", admissible), call. = FALSE, domain = NA)

    i <- pmatch(arg, .choices, nomatch = 0L)
    if (sum(i != 0L) != 1L)
        stop(paste0(errinv, "; ", admissible), call. = FALSE, domain = NA)

    if (charl) {
        res <- .choices[i]
        if (!(res %in% names(choices))) {
            for (n in names(choices)) if (res %in% choices[[n]]) { res <- n; break }
        }
    } else if (charv) res <- choices[i]
    else { # named vector / list
        res <- choices[[i]]
        names(res) <- NULL
    }

    return (res)
}

