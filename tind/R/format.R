#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################################### #
# formatting and parsing time indices #
# ################################### #


# format
# ###################################################################

#' Conversion between Objects of \code{tind} Class and Character Vectors
#'
#' @description
#' \code{format} method converts objects of \code{tind} class to character
#' vectors given format and locale information. \code{strptind} function
#' accepts character vector with time indices and parses to create object
#' of \code{tind} class.
#'
#' @details
#' Names of accepted format specifiers except for \code{\%q} are conformant
#' with those used by \code{\link[base]{format.Date}}, \code{\link[base]{format.Date}}
#' and \code{\link[base]{strptime}}. Accepted specifiers are listed below:
#' \describe{
#'     \item{\code{\%a}}{Abbreviated weekday name.}
#'     \item{\code{\%A}}{Weekday name.}
#'     \item{\code{\%b}}{Abbreviated month name.}
#'     \item{\code{\%B}}{Month name.}
#'     \item{\code{\%d}}{Day of month (01--31).}
#'     \item{\code{\%e}}{Day of month (1--31) with a leading space for a single-digit number.}
#'     \item{\code{\%D}}{American/C99 date representation \code{\%m/\%d/\%y}.}
#'     \item{\code{\%F}}{ISO 8601 date \code{\%Y-\%m-\%d}.}
#'     \item{\code{\%g}}{The last two digits of the week-based year.}
#'     \item{\code{\%G}}{The week-based year.}
#'     \item{\code{\%H}}{Hour (00--23).}
#'     \item{\code{\%I}}{Hour, 12-hour clock (1--12).}
#'     \item{\code{\%p}}{AM/PM indicator.}
#'     \item{\code{\%j}}{Day of year (001--366).}
#'     \item{\code{\%m}}{Month (01--12).}
#'     \item{\code{\%M}}{Minute (00--59).}
#'     \item{\code{\%n}}{Newline.}
#'     \item{\code{\%OS[n]}}{Second with \code{n} (0--6) decimal places.}
#'     \item{\code{\%OS}}{Second with up to 6 decimal places (automatically detected precision during parsing).}
#'     \item{\code{\%q}}{(Not supported by base R.) Quarter (1--4).}
#'     \item{\code{\%R}}{Same as \code{\%H:\%M}.}
#'     \item{\code{\%S}}{Second (00--59), leap seconds are not accepted on input.}
#'     \item{\code{\%t}}{Tab or whitespace.}
#'     \item{\code{\%T}}{Same as \code{\%H:\%M:\%S}.}
#'     \item{\code{\%u}}{Weekday (1--7) with Monday as the first day in a week (ISO 8601).}
#'     \item{\code{\%V}}{Week (01--53) as defined in ISO 8601.}
#'     \item{\code{\%y}}{2-digit year (00--99), values 00--68 are prefixed by 20 and 69--99 by 19.}
#'     \item{\code{\%Y}}{4-digit year (0--9999), 0 is allowed by ISO 8601.}
#'     \item{\code{\%z}}{Signed offset in hours and minutes from UTC, accepted
#'                       input formats are \code{+-HHMM}, \code{+-HH},
#'                       \code{+-HH:MM}, and letter \code{Z} for UTC.}
#'     \item{\code{\%Z}}{Time zone abbreviation (also supported on input).}
#' }
#'
#' On very rare occasions (the need to use formats unsupported by \code{strptind})
#' users will have to call \code{\link[base]{strptime}} and then \code{as.tind}
#' method or perform some regex preprocessing before calling \code{strptind}.
#'
#' \code{type} argument is optional as \code{strptind} automatically determines
#' index type from components. However, it can be set as a safeguard against format
#' misspecifications.
#'
#' @note
#' The following \code{\link[base]{strptime}} specifiers (as well as some others)
#' are not supported (most often because they are locale specific or do not
#' comply with ISO 8601):
#' \describe{
#'     \item{\code{\%c}}{Locale-specific date and time.}
#'     \item{\code{\%C}}{Century (00--99).}
#'     \item{\code{\%h}}{Equivalent to \code{\%b}.}
#'     \item{\code{\%r}}{12-hour clock time using AM/PM indicator.}
#'     \item{\code{\%U}}{Week of the year (US convention).}
#'     \item{\code{\%w}}{Weekday (0--6).}
#'     \item{\code{\%W}}{Week of the year (UK convention).}
#'     \item{\code{\%x}}{Locale-specific date.}
#'     \item{\code{\%X}}{Locale-specific time.}
#' }
#'
#' @param x an object to be converted, a character vector for \code{strptind},
#'          an object of \code{tind} class for \code{format}.
#' @param format a character string or character vector determining string format(s) (see Details).
#' @param locale a character value determining locale to be used for \code{\%a},
#'               \code{\%A}, \code{\%b}, \code{\%B}, and \code{\%p} specifiers
#'               (month names, weekday names, and AM/PM indicators) or \code{NULL}
#'               (default, interpreted as the current system locale),
#'               see \link{calendar-names} for information on locale settings.
#' @param type (optional) a character value determining time index type.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#' @param ... (ignored) further arguments passed to or from other methods.
#'
#' @return \code{strptime} returns an object of \code{tind} class, \code{format}
#' returns a character vector.
#'
#' @seealso \code{\link{parse_t}} for easier to use index parsing requiring
#' order specification only, \link{calendar-names} for information on locale settings.
#'
#' @examples
#' ## years
#' # four-digit year
#' (ti <- strptind(as.character(1998:2002), "%Y"))
#' format(ti, "%Y")
#' # two-digit year
#' (ti <- strptind(c("98", "99", "00", "01", "02"), "%y"))
#' format(ti, "%y")
#' # mixture of four-digit and two-digit years
#' strptind(c("1998", "1999", "00", "01", "02"), c("%Y", "%y"))
#'
#' ## quarters
#' (ti <- strptind(c("2020Q1", "2020Q2", "2020Q3", "2020Q4"), "%YQ%q"))
#' format(ti, "%YQ%q")
#' format(ti, "%Yq%q")
#' format(ti, "%Y.%q")
#'
#' ## months
#' (ti <- strptind(c("2020-03", "2020-06", "2020-09", "2020-12"), "%Y-%m"))
#' format(ti, "%Y-%m")
#' (ti <- strptind(c("03/20", "06/20", "09/20", "12/20"), "%m/%y"))
#' format(ti, "%m/%y")
#' format(ti, "%b '%y")
#'
#' ## weeks
#' (ti <- strptind(c("2020-W01", "2020-W05", "2020-W09", "2020-W13"), "%G-W%V"))
#' format(ti, "%G-W%V")
#' format(ti, "%G, week: %V")
#' strptind(c("2020, week: 13"), "%G, week: %V")
#'
#' ## dates
#' # ISO format
#' (ti <- strptind(c("2025-03-19", "2025-06-18", "2025-09-17", "2025-12-17"), "%F"))
#' format(ti, "%F")
#' strptind(c("2025-03-19", "2025-06-18", "2025-09-17", "2025-12-17"), "%Y-%m-%d")
#' format(ti, "%Y-%m-%d")
#' # US format
#' strptind(c("03/19/25", "06/18/25", "09/17/25", "12/17/25"), "%D")
#' format(ti, "%D")
#' strptind(c("03/19/25", "06/18/25", "09/17/25", "12/17/25"), "%m/%d/%y")
#' format(ti, "%m/%d/%y")
#' # European format
#' strptind(c("19.03.2025", "18.06.2025", "17.09.2025", "17.12.2025"), "%d.%m.%Y")
#' format(ti, "%d.%m.%Y")
#' # mixed formats
#' strptind(c("03/19/25", "06/18/25", "17.09.2025", "17.12.2025"),
#'          c("%m/%d/%y", "%d.%m.%Y"))
#' strptind(c("03/19/25", "06/18/25", "2025-09-17", "2025-12-17"),
#'          c("%D", "%F"))
#'
#' ## time of day
#' (ti <- strptind("13:03:34.534", "%H:%M:%OS"))
#' format(ti, "%H:%M:%OS3")
#' format(ti, "%H:%M:%OS2")
#' format(ti, "%H:%M:%OS1")
#' strptind("13:03:34", "%H:%M:%S")
#' format(ti, "%H:%M:%S")
#' strptind("13:03", "%H:%M")
#' format(ti, "%H:%M")
#' strptind("13", "%H")
#' format(ti, "%H")
#' strptind("01:03:44 pm", "%I:%M:%S %p")
#' format(ti, "%I:%M:%S %p")
#' strptind("1:03:44 pm", "%I:%M:%S %p")
#' strptind(c("1am", "1pm"), "%I%p")
#'
#' ## date-time
#' (ti <- strptind("2025-02-01 13:03:34.534", "%F %H:%M:%OS"))
#' format(ti, "%F %H:%M:%S")
#' format(ti, "%F %H:%M:%OS2")
#' format(ti, "%F %H:%M:%S%z")
#' format(ti, "%F %H:%M:%OS2 %Z")
#' strptind("02/01/25 01:03:34pm", "%D %I:%M:%OS%p")
#'
#' @name format
#' @aliases strptind
#'
NULL


#' @rdname format
#' @export
format.tind <- function(x, format, locale = NULL, ...)
{
    # get and clear attributes
    type <- .get.type(x)
    tz <- .get.tz(x)
    x <- .unclass(x)

    # fast exit, default (ISO 8601) formatting with additional tz info for date-time
    if (missing(format) || is.null(format)) {
        if (!length(x)) return (character())
        arglist <- if (type == "t") list(x, tz, TRUE, TRUE) else list(x)
        res <- do.call(paste0(".", type, "2char"), arglist)
        return (res)
    }

    if (type %in% c("i", "n")) {
        mes0 <- gettextf("format specification for type %s is not supported",
                         .ti_type2char(type))
        mes1 <- gettextf("use %s or %s", sQuote("format(as.numeric(x), ...)"),
                         sQuote("formatC(as.numeric(x), ...)"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    # check format
    if (!is.character(format) || length(format) != 1L) {
        mes0 <- gettextf("invalid %s argument", sQuote("format"))
        mes1 <- gettextf("character string expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    if (grepl("(%[^%a-zA-Z]|%$)", gsub("%%", "", format))) {
        mes0 <- gettextf("invalid %s argument", sQuote("format"))
        mes1 <- gettextf("a letter or %s expected after %s",
                         dQuote("%"), dQuote("%"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    # substitute for D, F, R, T, OS
    format <- gsub("%D", "%m/%d/%y", format)
    format <- gsub("%F", "%Y-%m-%d", format)
    format <- gsub("%R", "%H:%M", format)
    format <- gsub("%T", "%H:%M:%S", format)

    # collect format specifiers
    nm <- regmatches(format, gregexpr("%(OS[0-6]?|[%a-zA-Z])", format))
    nm <- unique(unlist(nm))
    nm <- sub("%", "", nm)
    nm <- setdiff(nm, c("%", "t", "n"))

    # check format specifiers
    formats <- list(y = c("y", "Y"),
                    q = c("y", "Y", "q"),
                    m = c("y", "Y", "q", "m", "b", "B"),
                    w = c("g", "G", "V"),
                    d = c("y", "Y", "g", "G", "q", "m", "b", "B", "d",
                          "e", "V", "j", "u", "a", "A"),
                    t = c("y", "Y", "g", "G", "q", "m", "b", "B", "d", "e", "j",
                          "V", "u", "a", "A",
                          "H", "I", "p", "M", "S", paste0("OS", 0:6), "OS",
                          "z", "Z"),
                    h = c("H", "I", "p", "M", "S", paste0("OS", 0:6), "OS"))
    fmtall <- unique(unlist(formats))
    if (!all(nm %in% fmtall)) {
        nm <- unique(unlist(nm))
        nok <- toString(dQuote(paste0("%", setdiff(nm, fmtall))))
        mes <- gettextf("unrecognised / unsupported format specifiers: %s", nok)
        nonisow <- c("W", "w", "U")
        if (any(nonisow %in% nm)) {
            hint0 <- gettextf("Note: week and weekday specifiers not compliant with ISO 8601 (%s) are not supported",
                              toString(dQuote(paste0("%", nonisow))))
            hint1 <- gettextf("use %s and %s", dQuote("%V"), dQuote("%u"))
            hint <- paste0(hint0, "; ", hint1)
            mes <- paste0(mes, "\n", hint)
        }
        nonst <- c("c", "C", "h", "r", "x", "X")
        if (any(nonst %in% nm)) {
            hint <- gettextf("Note: nonstandard / locale dependent specifiers (%s) are not supported",
                             toString(dQuote(paste0("%", nonst))))
            mes <- paste0(mes, "\n", hint)
        }
        stop(mes, call. = FALSE, domain = NA)
    }
    if (!all(nm %in% formats[[type]])) {
        nok <- toString(dQuote(paste0("%", setdiff(nm, formats[[type]]))))
        ok <- toString(dQuote(paste0("%", formats[[type]])))
        mes0 <- gettextf("the following specifiers are not supported for type %s: %s",
                         .ti_type2char(type), nok)
        mes1 <- gettextf("admissible specifiers: %s", ok)
        mes <- paste0(mes0, "; ", mes1)
        if (any(c("y", "Y") %in% nm) && type == "w") {
            hint <- gettextf("Hint: use %s and %s (week-based year) specifiers instead of %s and %s for type %s",
                             dQuote("%g"), dQuote("%G"), dQuote("%y"), dQuote("%Y"),
                             .ti_type2char(type))
            mes <- paste0(mes, "\n", hint)
        }
        stop(mes, call. = FALSE, domain = NA)
    }

    # fast exit
    if (!length(x)) return (character())

    # calendar names
    if (any(c("b", "B", "a", "A", "p") %in% nm)) {
        cnames <- .calendar_names(locale)
        if (("p" %in% nm) && any(cnames[39L:40L] == "")) {
            mes0 <- gettextf("AM/PM indicators not available in the selected / current locale")
            mes1 <- gettextf("using default values")
            cnames[39L:40L] <- c("am", "pm")
            warning(paste0(mes0, "; ", mes1), domain = NA, call. = FALSE)
        }
    } else cnames <- character()

    # date-time
    if (type == "t") {
        dhz <- .t2dhz(x, tz = tz, "z" %in% nm, "Z" %in% nm)
        # date part
        xx <- dhz$d
        # hms part
        hms <- dhz$h
        # offset, zone
        if ("z" %in% nm) {
            off <- dhz$gmtoff
            if (is.null(off)) {
                mes <- gettextf("UTC offset not available")
                warning (mes, call. = FALSE, domain = NA)
                off <- rep(NA_integer_, length(x))
            } else off <- as.integer(off)
        } else off <- integer()
        if ("Z" %in% nm) {
            zn <- dhz$zone
            if (is.null(zn)) {
                mes0 <- gettextf("time zone abbreviation not available")
                mes1 <- paste0(gettextf("time zone: %s", tz))
                warning (paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
                zn <- rep("???", length(x))
            }
            # not NULL but 0-length means UTC
            if (!length(zn)) zn <- rep("UTC", length(x))
        } else zn <- character()
    } else if (type == "h") {
        xx <- integer(); hms <- x; off <- integer(); zn <- character()
    } else { hms <- numeric(); off <- integer(); zn <- character(); xx <- x }

    # check for %y that year is within range
    if (any(c("y", "g") %in% nm)) {
        if (type == "y") { Y <- xx }
        else if (type == "q") { Y <- .q2y(xx) }
        else if (type == "m") { Y <- .m2y(xx) }
        else if (type == "w") { Y <- .w2y(xx) }
        else { Y <- .d2y(xx) }
        abyst <- .tind.getOption("abbr.year.start")
        if (isTRUE(any(Y < 1900L + abyst | Y > 1999L + abyst))) {
            msg <- gettextf("format specifier %s (or %s) used for years outside %d-%d range",
                            dQuote("%y"), dQuote("%g"), 1900L + abyst, 1999L + abyst)
            warning(msg, call. = FALSE, domain = NA)
        }
    }

    return (.Call(C_format, xx, hms, off, zn, type, format, cnames))
}


# strptind
# ###################################################################

#' @rdname format
#' @export
strptind <- function(x, format, locale = NULL, type = NULL, tz = NULL)
{
    if (!is.character(x)) {
        nms <- names(x)
        x <- as.character(x)
        names(x) <- nms
    }

    # check format
    if (!is.character(format) || !(n <- length(format))) {
        mes0 <- gettextf("invalid %s argument", sQuote("format"))
        mes1 <- gettextf("character string or character vector expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    for (i in 1L:n) {
        if (grepl("(%[^%a-zA-Z]|%$)", gsub("%%", "", format[i]))) {
            mes0 <- gettextf("invalid %s argument", sQuote("format"))
            mes1 <- gettextf("a letter or %s expected after %s",
                            dQuote("%"), dQuote("%"))
            mes2 <- gettextf("format: %s", dQuote(format[i]))
            stop(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
        }
    }

    # substitute for %D, %F, %R, %T
    format <- gsub("%D", "%m/%d/%y", format)
    format <- gsub("%F", "%Y-%m-%d", format)
    format <- gsub("%R", "%H:%M", format)
    format <- gsub("%T", "%H:%M:%S", format)

    # collect format specifiers
    nm <- lapply(format, function(x)
                 sub("%", "", unlist(regmatches(x, gregexpr("%(OS|[a-zA-Z])", x)))))
    nm <- lapply(nm, function(x) setdiff(x, c("t", "n")))

    # construct admissible combinations of format specifiers
    fmt_y <- list("Y", "y")
    fmt_q <- lapply(fmt_y, function(y) { c(y, "q") })
    fmt_m <- c(lapply(fmt_y, function(y) { c(y, "m") }),
               lapply(fmt_y, function(y) { c(y, "b") }),
               lapply(fmt_y, function(y) { c(y, "B") }))
    fmt_w <- list(c("G", "V"), c("g", "V"))
    fmt_d <- c(lapply(fmt_y, function(y) { c(y, "j") }),
               lapply(fmt_m, function(m) { c(m, "d") }),
               lapply(fmt_m, function(m) { c(m, "e") }))
    fmt_wd <- c(lapply(fmt_w, function(w) { c(w, "u") }),
                lapply(fmt_w, function(w) { c(w, "a") }),
                lapply(fmt_w, function(w) { c(w, "A") }))
    fmt_d <- c(fmt_d, fmt_wd)
    fmt_h <- list("H", c("I", "p"))
    fmt_hm <- c(lapply(fmt_h, function(h) { c(h, "M") }))
    fmt_hms <- c(lapply(fmt_hm, function(hm) { c(hm, "S") }),
                 lapply(fmt_hm, function(hm) { c(hm, "OS") }))
    fmt_dh <- c(lapply(fmt_d, function(d) { c(d, "H") }),
                lapply(fmt_d, function(d) { c(d, "I", "p") }))
    fmt_dhm <- c(lapply(fmt_dh, function(h) { c(h, "M") }))
    fmt_dhms <- c(lapply(fmt_dhm, function(hm) { c(hm, "S") }),
                 lapply(fmt_dhm, function(hm) { c(hm, "OS") }))
    fmt_tz <- c(lapply(c(fmt_dhms, fmt_dhm, fmt_dh), function(hm) { c(hm, "z") }),
                lapply(c(fmt_dhms, fmt_dhm, fmt_dh), function(hm) { c(hm, "Z") }))
    formats <- list(t = c(fmt_tz, fmt_dhms, fmt_dhm, fmt_dh),
                    h = c(fmt_hms, fmt_hm, fmt_h), d = c(fmt_d, fmt_wd),
                    w = fmt_w, m = fmt_m, q = fmt_q, y = fmt_y)

    # check format specifiers
    fmtall <- unique(unlist(formats))
    if (!all(unique(unlist(nm)) %in% fmtall)) {
        nm <- unique(unlist(nm))
        nok <- toString(dQuote(paste0("%", setdiff(nm, fmtall))))
        mes <- gettextf("unrecognised / unsupported format specifiers: %s", nok)
        nonisow <- c("W", "w", "U")
        if (any(nonisow %in% nm)) {
            hint0 <- gettextf("Note: week and weekday specifiers not compliant with ISO 8601 (%s) are not supported",
                              toString(dQuote(paste0("%", nonisow))))
            hint1 <- gettextf("use %s and %s", dQuote("%V"), dQuote("%u"))
            hint <- paste0(hint0, "; ", hint1)
            mes <- paste0(mes, "\n", hint)
        }
        nonst <- c("c", "C", "h", "r", "x", "X")
        if (any(nonst %in% nm)) {
            hint <- gettextf("Note: nonstandard / locale dependent specifiers (%s) are not supported",
                             toString(dQuote(paste0("%", nonst))))
            mes <- paste0(mes, "\n", hint)
        }
        stop(mes, call. = FALSE, domain = NA)
    }

    # infer type
    tps <- rep(NA_character_, n)
    ign <- rep(list(character()), n)
    for (i in 1L:n) {
        tp <- NA_character_
        for (tf in names(formats)) {
            for (j in seq_along(formats[[tf]])) {
                if (all(formats[[tf]][[j]] %in% nm[[i]])) {
                    tp <- tf
                    ign[[i]] <- setdiff(nm[[i]], formats[[tf]][[j]])
                    break
                }
            }
            if (!is.na(tp)) break
        }
        tps[i] <- tp
    }
    # check inferred type(s)
    if (anyNA(tps)) {
        ok <- !is.na(tps)
        if (!sum(ok)) {
            mes <- gettextf("no valid format specification")
            stop(mes, domain = NA, call. = FALSE)
        } else {
            mes <- gettextf("ignoring invalid format specifications: %s",
                            toString(dQuote(format[!ok])))
            warning(mes, domain = NA, call. = FALSE)
        }
        format <- format[ok]
        tps <- tps[ok]
    }
    if (length(unique(tps)) > 1L) {
        mes <- gettextf("conflicting types inferred: %s",
                        .ti_type2char(unique(tps)))
        stop(mes, domain = NA, call. = FALSE)
    }
    # type and tz args
    inftype <- is.null(type)
    if (!is.null(type)) {
        typetz <- .check_type_tz(type, tz)
        if (type != tps[1L]) {
            mes <- gettextf("type inferred (%s) is different from type provided as argument (%s)",
                            .ti_type2char(tps[1L], dash = TRUE),
                            .ti_type2char(type, dash = TRUE))
            stop(mes, call. = FALSE, domain = NA)
        }
    } else {
        type <- tps[1L]
        typetz <- .check_type_tz(type, tz)
    }
    type <- typetz$type
    if (!is.null(tz)) tz <- typetz$tz

    # ignored
    if (any(sapply(ign, length))) {
        for (i in 1L:n) {
            if (length(ign[[i]])) {
                mes0 <- gettextf("the following components will be ignored: %s",
                                 toString(dQuote(paste0("%", ign[[i]]))))
                mes1 <- gettextf("format: %s", dQuote(format[i]))
                mes2 <- if (inftype) gettextf("type inferred: %s", .ti_type2char(type))
                        else gettextf("type: %s", .ti_type2char(type))
                warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
            }
        }
    }

    # calendar names
    unm <- unique(unlist(nm))
    if (any(c("b", "B", "a", "A", "p") %in% unm)) {
        cnames <- .calendar_names(locale)
        if (("p" %in% unm) && any(cnames[39L:40L] == "")) {
            mes0 <- gettextf("AM/PM indicators not available in the selected / current locale")
            mes1 <- gettextf("using default values")
            cnames[39L:40L] <- c("am", "pm")
            warning(paste0(mes0, "; ", mes1), domain = NA, call. = FALSE)
        }
    } else cnames <- character()

    # parse
    ind <- .parse_fmt(x, format, type, 1L, cnames, tz, TRUE, FALSE)
    return (do.call(.tind, ind))
}

