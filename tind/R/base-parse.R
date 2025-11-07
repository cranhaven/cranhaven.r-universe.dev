#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ####### #
# parsing #
# ####### #


# number parsing (used by as.tind.numeric)
# ###################################################################

.parse_num <- function(x, type)
{
    # if type is NULL, try to automatically determine type
    if (is.null(type)) {
        if (!is.integer(x)) {
            if (isTRUE(any(round(x) != x))) return (NULL)
            x <- .require_mode(x, "integer")
        }
        return (.Call(C_autoparse_num, x))
    }

    if (type %in% c("y", "t", "h", "i", "n")) {
        xx <- do.call(paste0(".validate_", type), list(x))
    } else {
        xx <- x
        xx <- .require_mode(xx, "integer")
        xx <- switch(type,
                     q = .Call(C_num2q, xx), m = .Call(C_num2m, xx),
                     w = .Call(C_num2w, xx), d = .Call(C_num2d, xx))
    }

    nok <- !is.na(x) & is.na(xx)
    if (any(nok)) {
        nok <- which.max(nok)
        mes0 <- gettextf("NAs introduced")
        mes1 <- gettextf("first position %s: %s", format(nok, scientific = FALSE),
                         as.character(x[nok]))
        if (type %in% c("i", "n")) {
            mes2 <- gettextf("type: %s", .ti_type2char(type))
            warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
        } else {
            if (type == "t")
                mes2a <- gettextf("seconds since the Epoch")
            else if (type == "h")
                mes2a <- gettextf("seconds since midnight")
            else
                mes2a <- switch(type, y = "YYYY", q = "YYYYQ", m = "YYYYMM",
                                      w = "YYYYWW", d = "YYYYMMDD")
            mes2 <- gettextf("representation: %s", mes2a)
            warning(paste0(mes0, "; ", mes1, "; ", mes2),
                    call. = FALSE, domain = NA)
        }
    }

    return (xx)
}


# parsing time indices (used by as.tind.character)
# ###################################################################

.parse <- function(x, type, locale, tz)
{
    # calendar names
    cnames <- .calendar_names(locale)

    # if type is NULL, try to automatically determine type
    # strict format specifications used here
    fmts <- list(y = c("%Y"),
                 q = c("%Yq%q", "%YQ%q", "%Y%q"),
                 m = c("%Y-%m", "%Y%m", "%B %Y", "%b %Y"),
                 w = c("%G-W%V"),
                 d = c("%Y-%m-%d", "%Y%m%d", "%m/%d/%y",
                       "%B %d, %Y", "%b %d, %Y", "%d %B %Y", "%d %b %Y",
                       "%G-W%V-%u", "%Y-%j"),
                 t = c("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M:%OS",
                       "%Y-%m-%d %H:%M%z", "%Y-%m-%d %H:%M:%S%z", "%Y-%m-%d %H:%M:%OS%z",
                       "%Y-%m-%dT%H:%M:%S%z",
                       "%Y-%m-%d %H:%M %Z", "%Y-%m-%d %H:%M:%S %Z", "%Y-%m-%d %H:%M:%OS %Z"),
                 h = c("%H:%M", "%H:%M:%S", "%H:%M:%OS"))
    if (is.null(type)) {
        types <- names(fmts)
        for (type in types) {
            res <- .parse_fmt(x, fmts[[type]], type, 0L, cnames, NULL, TRUE)
            if (!is.null(res)) return (res)
        }
        return (NULL)
    }

    if (type %in% c("i", "n")) {
        res <- do.call(paste0(".validate_", type), list(x))
        nok <- !is.na(x) & is.na(res)
        if (any(nok)) {
            pos <- which.max(nok)
            mes0 <- gettextf("NAs introduced")
            mes1 <- gettextf("first position %s: %s", format(pos, scientific = FALSE),
                             dQuote(x[pos]))
            mes2 <- gettextf("type: %s", .ti_type2char(type))
            warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
        }
        return (list(res, type, NULL))
    }

    # automatically recognised formats given type
    # order specifications used here
    fmts <- list(y = "y", q = "yq", m = "ym", w = "yw", d = c("mdy", "ymd"),
                 t = c("ymdHM", "ymdHMz", "ymdHMS", "ymdHMSz", "mdyIMp", "mdyIMSp"),
                 h = c("H", "HM", "HMS", "Ip", "IMp", "IMSp"))
    res <- .parse_fmt(x, fmts[[type]], type, 1L, cnames, tz, FALSE, TRUE)
    return (res)
}


# a wrapper for C-level parse and strptind functions used by .parse above,
# strptind and parse_t
# ###################################################################

.parse_fmt <- function(x, fmt, type, beh, cnames, tz, strptind, auto = FALSE)
{
    znames <- if (type == "t") .tzshort() else character()
    res <- if (strptind) .Call(C_strptind, x, fmt, type, beh, cnames, znames,
                               as.integer(.tind.getOption("abbr.year.start")))
           else .Call(C_parse, x, fmt, type, beh, cnames, znames,
                      as.integer(.tind.getOption("abbr.year.start")))
    if (is.null(res)) return (res)

    if (type == "t") {
        if (!is.null(tz)) tz <- .check_tz(tz)
        d <- res[[1L]]
        hms <- res[[2L]]
        z <- res[[3L]]
        if (!length(z) || anyNA(z) && all(is.na(z))) {
            res <- .dhz2t(d, hms, integer(), .check_tz(tz), 2L)
        } else {
            # UTC offset >= 100000 means index of tz abbreviation times 100000
            iz <- z >= 100000L
            iz <- !is.na(d) & !is.na(iz) & iz
            if (any(iz)) {
                ziz <- z[iz]
                uziz <- unique(ziz)
                zmap <- match(ziz, uziz)
                zoff <- .tzshort(znames[uziz %/% 100000L], tz = tz,
                                 d = range(d[iz], na.rm = TRUE))
                tz <- zoff[[2L]]
                zoff <- zoff[[1L]]
                z[iz] <- zoff[zmap]
                ina <- is.na(zoff)[zmap]
                d[iz[ina]] <- NA_integer_
            } else if (is.null(tz)) tz <- .check_tz(tz)
            res <- .dhz2t(d, hms, z, tz, 2L)
        }
        if (is.null(tz)) tz <- .check_tz(tz)
        nna <- !is.na(x) & is.na(res)
        napos <- if (any(nna)) which.max(nna) else 0L
    } else {
        napos <- if (length(res) == 4L) res[[4L]] else 0L
        res <- if (type == "h") res[[2L]] else res[[1L]]
    }

    if (napos) {
        mes0 <- gettextf("NAs introduced")
        mes1 <- gettextf("first position %s: %s", format(napos, scientific = FALSE),
                         dQuote(x[napos]))
        mes2 <- gettextf("type: %s", .ti_type2char(type))
        if (type == "t")
            mes2 <- paste0(mes2, "; ", gettextf("time zone: %s", .check_tz(tz)))
        if (auto) {
            mes3 <- gettextf("consider providing %s or %s argument",
                             sQuote("format"), sQuote("order"))
        } else {
            fmts <- toString(dQuote(fmt))
            mes3 <- if (strptind) gettextf("format: %s", fmts)
                    else gettextf("order: %s", fmts)
        }
        mes <- paste0(mes0, "; ", mes1, "; ", mes2, "; ", mes3)
        warning(mes, call. = FALSE, domain = NA)
    }

    return (list(res, type, tz))
}


# parsing time differences - this returns two-element list with values and type / unit
# ###################################################################

.parse_tdiff <- function(x)
{
    res <- .Call(C_parse_tdiff_yqmwd, x)
    if (is.null(res)) res <- .Call(C_parse_tdiff_t, x)
    if (is.null(res)) { # try H:M:S
        res <- .parse(x, NULL, NULL, NULL)
        if (!is.null(res)) {
            if (res[[2L]] == "h") {
                res <- res[1L:2L]
                res[[2L]] <- "t"
            } else res <- NULL
        }
    }
    return (res)
}


# parsing time intervals - this only returns character vector to be parsed later
# to get actual time indices (or NULL if parsing failed)
# ###################################################################

.parse_tinterval <- function(x, sep) .Call(C_parse_tinterval, x, sep)

