#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# #################### #
# parsing time indices #
# #################### #


#' Parse Character Representation of Time Indices Given the Order of Components
#'
#' @description
#' \code{parse_t} parses character vector to create an object of \code{tind}
#' class based on provided order(s) of time index components. Index type is inferred
#' from components given.
#'
#' @details
#' Accepted names of components are:
#' \describe{
#'     \item{\code{y}}{year.}
#'     \item{\code{q}}{quarter.}
#'     \item{\code{m}}{month, number (1--12) or name.}
#'     \item{\code{d}}{day.}
#'     \item{\code{j}}{day of year.}
#'     \item{\code{w}}{week (01--53) as defined in ISO 8601.}
#'     \item{\code{u}}{day of week, number (1--7 with Monday as the first day, ISO 8601) or name.}
#'     \item{\code{H}}{hour.}
#'     \item{\code{I}}{hour, 12-hour clock.}
#'     \item{\code{p}}{AM/PM indicator.}
#'     \item{\code{M}}{minute.}
#'     \item{\code{S}}{second.}
#'     \item{\code{z}}{UTC offset (\code{+-HHMM}, \code{+-HH},
#'                     \code{+-HH:MM}, or letter \code{Z} for UTC)
#'                     or time zone abbreviation (like \code{CET} or \code{CEST}).}
#' }
#'
#' The following combinations of components (in any order) are accepted for different index
#' types (whitespace between specifiers is ignored):
#' \describe{
#'     \item{year (type \code{"y"}):}{\code{y}.}
#'     \item{quarter (type \code{"q"}):}{\code{y} and \code{q}.}
#'     \item{month (type \code{"m"}):}{\code{y} and \code{m}.}
#'     \item{week (type \code{"w"}):}{\code{y} and \code{w}.}
#'     \item{date (type \code{"d"}):}{\code{y}, \code{m}, and \code{d};
#'                                    \code{y}, and \code{j};
#'                                    \code{y}, \code{w}, and \code{u}.}
#'     \item{time of day (type \code{"h"}):}{at least hour component with optional
#'                       minutes and seconds.}
#'     \item{date-time (type \code{"t"}):}{any valid combination for date and
#'                      at least hour component with optional
#'                      minutes and seconds.}
#' }
#'
#' During parsing all non-digits are skipped in front of \code{"y"}, \code{"q"},
#' \code{"w"}, \code{"d"}, \code{"j"}, \code{"H"}, \code{"I"}, \code{"M"}, \code{"S"}
#' specifiers and all non-alphanumeric characters are skipped in front of
#' \code{"m"}, \code{"u"}, \code{"p"}. Only whitespace is ignored in front of \code{"z"}
#' specifier.
#'
#' \code{parse_t} was inspired by \code{ymd}, \code{mdy}, etc. family of functions
#' from package \pkg{lubridate} but independently implemented from scratch
#' (and is a bit faster).
#'
#' @param x a character vector of time indices to be parsed.
#' @param order a character string or a character vector describing order(s) of time
#'              index components in the input (\code{x}), see Details.
#' @param locale (optional) a character value determining locale or \code{NULL}
#'               (the default, interpreted as the current system locale),
#'               see \link{calendar-names} for information on locale settings.
#' @param type (optional) a character value determining time index type.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#'
#' @return An object of \code{tind} class.
#'
#' @seealso \code{\link{strptind}} for index parsing requiring strict format
#' specification, \link{calendar-names} for information on locale settings.
#'
#' @examples
#' ## years
#' # four-digit year
#' parse_t(as.character(1998:2002), "y")
#' # two-digit year
#' parse_t(c("98", "99", "00", "01", "02"), "y")
#' # mixture of four-digit and two-digit years
#' parse_t(c("1998", "1999", "00", "01", "02"), "y")
#'
#' ## quarters
#' parse_t(c("2020Q1", "2020Q2", "2020Q3", "2020Q4"), "yq")
#'
#' ## months
#' parse_t(c("2020-03", "2020-06", "2020-09", "2020-12"), "ym")
#' parse_t(c("03/20", "06/20", "09/20", "12/20"), "my")
#' # missing leading zeros are also handled
#' parse_t(c("3/20", "6/20", "9/20", "12/20"), "my")
#'
#' ## weeks
#' # standard format
#' parse_t(c("2020-W01", "2020-W05", "2020-W09", "2020-W13"), "yw")
#' # non-standard format
#' parse_t(c("2020, week: 01", "2020, week: 05", "2020, week: 09", "2020, week: 13"), "yw")
#' # missing leading zeros are also handled
#' parse_t(c("2020, week: 1", "2020, week: 5", "2020, week: 9", "2020, week: 13"), "yw")
#'
#' ## dates
#' # ISO format
#' parse_t(c("2025-03-19", "2025-06-18", "2025-09-17", "2025-12-17"), "ymd")
#' # US format
#' parse_t(c("03/19/25", "06/18/25", "09/17/25", "12/17/25"), "mdy")
#' # missing leading zeros are handled
#' parse_t(c("3/19/25", "6/18/25", "9/17/25", "12/17/25"), "mdy")
#' # European format
#' parse_t(c("19.03.2025", "18.06.2025", "17.09.2025", "17.12.2025"), "dmy")
#' # mixed formats
#' parse_t(c("03/19/25", "06/18/25", "17.09.2025", "17.12.2025"), c("mdy", "dmy"))
#' parse_t(c("03/19/25", "06/18/25", "2025-09-17", "2025-12-17"),  c("mdy", "ymd"))
#'
#' ## time of day
#' parse_t("13:03:34.534", "HMS")
#' parse_t("13:03:34", "HMS")
#' parse_t("13:03", "HM")
#' parse_t("13", "H")
#' parse_t("1:03:44 pm", "IMSp")
#' parse_t("1pm", "Ip")
#'
#' ## date-time
#' parse_t("2025-02-01 13:03:34.534", "ymdHMS")
#' parse_t("2025-02-01 13:03:34.534", "ymdHMS", tz = "UTC")
#' parse_t("02/01/25 01:03:34pm", "mdyIMSp")
#' parse_t("02/01/25 01:03:34pm", "mdyIMSp", tz = "UTC")
#'
#' @export
#'
parse_t <- function(x, order, locale = NULL, type = NULL, tz = NULL)
{
    if (!is.character(x)) {
        nms <- names(x)
        x <- as.character(x)
        names(x) <- nms
    }

    # check order
    if (!is.character(order) || !(n <- length(order))) {
        mes0 <- gettextf("invalid %s argument", sQuote("order"))
        mes1 <- gettextf("character string or character vector expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    order <- gsub("[\\.\t\n ]", "", order)
    ord <- strsplit(order, "")
    tpsi <- lapply(ord, .infer_type)
    tps <- sapply(tpsi, function(x) x[[1L]])

    # check inferred type(s)
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

    # ignored components
    ign <- lapply(tpsi, function(x) x[[2L]])
    if (any(sapply(ign, length) > 0L)) {
        igni <- which(sapply(ign, length) > 0L)
        for (ig in igni) {
            mes0 <- gettextf("the following components will be ignored: %s",
                             .ti_comp2char(ign[[igni]]))
            mes1 <- gettextf("order: %s", dQuote(order[igni]))
            mes2 <- if (inftype) gettextf("type inferred: %s", .ti_type2char(type))
                    else gettextf("type: %s", .ti_type2char(type))
            warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
        }
    }

    # calendar names
    cnames <- .calendar_names(locale)
    if (("p" %in% unique(unlist(ord))) && any(cnames[39L:40L] == "")) {
        mes0 <- gettextf("AM/PM indicators not available in the selected / current locale")
        mes1 <- gettextf("using default values")
        cnames[39L:40L] <- c("am", "pm")
        warning(paste0(mes0, "; ", mes1), domain = NA, call. = FALSE)
    }

    # parse
    ind <- .parse_fmt(x, order, type, 1L, cnames, tz, FALSE, FALSE)
    return (do.call(.tind, ind))
}

