#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ######################## #
# calendrical computations #
# ######################## #


# current date and time
# ###################################################################

#' Current Date and Time
#'
#' \code{today} returns the current date and \code{now} returns the current
#' date and time (in the system time zone or the time zone provided by the user).
#'
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#' @param digits an integer value giving the number of decimal places for
#'               seconds (0--6, 0 by default).
#'
#' @return
#' \code{today} and \code{now} return an object of class \code{tind}
#' of length 1 and type \code{"d"} (date) and \code{"t"} (date-time),
#' respectively.
#'
#' @name current-date-time
#'
#' @examples
#' today()
#' now()
#' # millisecond accuracy
#' now(digits = 3)
#' # check current date and time in different time zones
#' if ("Asia/Tokyo" %in% OlsonNames()) {
#' now("Asia/Tokyo")
#' today("Asia/Tokyo")
#' }
#' if ("Europe/Warsaw" %in% OlsonNames()) {
#' now("Europe/Warsaw")
#' today("Europe/Warsaw")
#' }
#' if ("America/New_York" %in% OlsonNames()) {
#' now("America/New_York")
#' today("America/New_York")
#' }
#'
NULL


#' @rdname current-date-time
#' @export
today <- function(tz = NULL) as.tind(now(tz = tz), type = "d")


#' @rdname current-date-time
#' @export
now <- function(tz = NULL, digits = 0)
    as.tind(Sys.time(), tz = tz, digits = digits)



# calendar names
# ###################################################################

#' Calendar Names
#'
#' @description
#' These three functions return (abbreviated and full) names of months and days of week
#' as well as AM/PM indicators in the current or user-provided locale.
#'
#' How the month and weekday names are actually returned depends both on the
#' selected locale and character set / code page setting.
#'
#' @section Locale Settings:
#' Unfortunately, locale and character set naming were not standardised across
#' different operating systems for many years. On modern operating systems,
#' however, locale is usually of the form \code{xx_XX} (\code{xx} for language,
#' \code{XX} for country) optionally followed by a dot and a character set
#' identifier, for example, \code{UTF-8}.
#'
#' \code{"C"} is a special locale that should always be available and
#' defaults to American English.
#'
#' @param locale a character value determining locale or \code{NULL} (default,
#'               interpreted as the current system locale).
#' @param abbreviate a logical value, if \code{TRUE}, abbreviated names are returned;
#'                   if \code{FALSE}, full names are returned.
#'                   \code{TRUE} by default.
#'
#' @return A character vector of length 12, 7, or 2.
#'
#' @name calendar-names
#'
#' @seealso \code{\link{format}} for formatting objects of \code{tind} class.
#'
#' @examples
#' # current system locale
#' month_names()
#' weekday_names()
#' try(
#' ampm_indicators()
#' )
#'
#' try({
#' # English abbreviated month names
#' print(month_names("en_GB"))
#' # French month names
#' print(month_names("fr_FR.UTF-8", FALSE))
#' # German abbreviated month names
#' print(month_names("de_DE.UTF-8"))
#' # Polish abbreviated month names
#' print(month_names("pl_PL.UTF-8"))
#' # English weekday names
#' print(weekday_names("en_GB", FALSE))
#' # French abbreviated weekday names
#' print(weekday_names("fr_FR.UTF-8"))
#' # German weekday names
#' print(weekday_names("de_DE.UTF-8", FALSE))
#' # Polish abbreviated weekday names
#' print(weekday_names("pl_PL.UTF-8"))
#' # US am/pm indicators
#' print(ampm_indicators("en_US"))
#' # UK am/pm indicators
#' print(ampm_indicators("en_GB"))
#' })
#'
NULL


#' @rdname calendar-names
#' @export
month_names <- function(locale = NULL, abbreviate = TRUE)
{
    .checkTRUEFALSE(abbreviate)
    return (.calendar_names(locale)[1L:12L + 12L * !abbreviate])
}


#' @rdname calendar-names
#' @export
weekday_names <- function(locale = NULL, abbreviate = TRUE)
{
    .checkTRUEFALSE(abbreviate)
    return (.calendar_names(locale)[25L:31L + 7L * !abbreviate])
}


#' @rdname calendar-names
#' @export
ampm_indicators <- function(locale = NULL)
{
    ampm <- .calendar_names(locale)[39L:40L]
    if (any(ampm == "")) {
        ampm <- rep(NA_character_, 2L)
        warning("AM/PM indicators not available in the selected / current locale",
                call. = FALSE)
    }
    return (ampm)
}



# time index components
# ###################################################################

#' Time Index Components (Years, Months, Days, ...)
#'
#' @description
#' The following functions can be used to retrieve components of time indices.
#'
#' \code{year}, \code{quarter}, \code{month}, \code{week}, \code{day} return
#' year (0--9999), quarter (1--4), month (1--12), week (1--53, ISO 8601),
#' and day (1--31) indices as integers. When \code{month} is invoked
#' with \code{labels} argument set to \code{TRUE}, an ordered factor is returned.
#'
#' \code{day_of_week} returns index (1--7) of weekday with Monday as the first
#' day (as in ISO 8601). When invoked with \code{labels} argument
#' set to \code{TRUE}, an ordered factor is returned.
#'
#' \code{day_of_year} returns index (1--366) of the day of year as integer.
#'
#' \code{hour}, \code{minute}, \code{second} return hour (0--23),
#' minute (0--59), and second (0--59,999999) indices as integers and reals (for seconds).
#' \code{am} and \code{pm} functions determine whether time falls in the first
#' or second half of the day.
#'
#' Methods \code{weekdays}, \code{months}, and \code{quarters} from package \pkg{base}
#' are implemented but users are encouraged to use functions from \pkg{tind} package.
#'
#' @details
#' \code{year} for week arguments need not necessarily return the same value
#' as for days within the week in question when the week is the first or the last
#' in a year.
#'
#' \pkg{tind} package does not provide replacement methods for time index components.
#' In order to change, say, month one can use \code{tind} constructor or \code{\%+m\%}
#' operator (and similar operators), see Examples.
#'
#' @param x an object of \code{tind} class or an R object coercible to it.
#' @param labels a logical value, if \code{TRUE} month and weekday names are
#'               returned (as ordered factors) instead of integer indices (\code{FALSE}
#'               by default).
#' @param abbreviate a logical value, if \code{TRUE}, abbreviated names are returned;
#'                   if \code{FALSE}, full names are returned.
#'                   \code{TRUE} by default.
#' @param locale (optional) a character value determining locale or \code{NULL}
#'               (the default, interpreted as the current system locale),
#'               see \link{calendar-names} for information on locale settings.
#'
#' @return
#' All functions return integer vectors, except for \code{second}, which
#' returns numeric vectors. Additionally, \code{month} and \code{day_of_week}
#' return ordered factors if invoked with argument \code{labels} set to \code{TRUE}.
#'
#' @seealso \code{\link{tind}} class, \link{Ops}
#' for index increments / decrements and index differences, and
#' \link{calendar-names} for names of months and days of weeks in the current
#' locale. Further examples of use of these functions can be found
#' in \code{\link{calendar}} documentation.
#'
#' @name time-index-components
#'
#' @examples
#' # current date and time
#' (nw <- now())
#' # show current year, quarter, month, ...
#' year(nw)
#' quarter(nw)
#' month(nw)
#' month(nw, labels = TRUE)
#' month(nw, labels = TRUE, abbreviate = FALSE)
#' week(nw)
#' day(nw)
#' day_of_week(nw)
#' day_of_week(nw, labels = TRUE)
#' day_of_week(nw, labels = TRUE, abbreviate = FALSE)
#' day_of_year(nw)
#' hour(nw)
#' minute(nw)
#' second(nw)
#'
#' # alternatives to replacement, change month to December
#' (x <- as.date("2023-09-11"))
#' (x <- tind(y = year(x), m = 12, d = day(x)))
#' (x <- as.date("2023-09-11"))
#' (x <- x %+m% (12 - month(x)))
#'
NULL


#' @rdname time-index-components
#' @export
year <- function(x) .unclass(.require_type(as.tind(x), "y", lower = TRUE))


#' @rdname time-index-components
#' @export
quarter <- function(x)
{
    q <- .unclass(.require_type(as.tind(x), "q", lower = TRUE))
    return (.q2qrtr(q))
}


#' @rdname time-index-components
#' @export
quarters.tind <- function(x, abbreviate) paste0("Q", quarter(x))


#' @rdname time-index-components
#' @export
month <- function(x, labels = FALSE, abbreviate = TRUE, locale = NULL)
{
    .checkTRUEFALSE(labels)
    m <- .require_type(as.tind(x), "m", lower = TRUE)
    m <- .unclass(.m2mnth(m))
    if (!labels) return (m)
    attributes(m) <- list(levels = month_names(locale, abbreviate),
                          class = c("ordered", "factor"))
    return (m)
}


#' @rdname time-index-components
#' @export
months.tind <- function(x, abbreviate = FALSE)
{
    .checkTRUEFALSE(abbreviate)
    return (as.character(month(x, labels = TRUE, abbreviate = abbreviate)))
}


#' @rdname time-index-components
#' @export
week <- function(x)
{
    w <- .unclass(.require_type(as.tind(x), "w", lower = TRUE))
    return (.w2week(w))
}


#' @rdname time-index-components
#' @export
day <- function(x)
{
    d <- .unclass(.require_type(as.tind(x), "d", lower = TRUE))
    return (.d2day(d))
}


#' @rdname time-index-components
#' @export
day_of_year <- function(x)
{
    d <- .unclass(.require_type(as.tind(x), "d", lower = TRUE))
    return (.day_of_year(d))
}


#' @rdname time-index-components
#' @export
day_of_week <- function(x, labels = FALSE, abbreviate = TRUE, locale = NULL)
{
    .checkTRUEFALSE(labels)
    d <- .unclass(.require_type(as.tind(x), "d", lower = TRUE))
    d <- .day_of_week(d)
    if (!labels) return (d)
    attributes(d) <- list(levels = weekday_names(locale, abbreviate),
                          class = c("ordered", "factor"))
    return (d)
}


#' @rdname time-index-components
#' @export
weekdays.tind <- function(x, abbreviate = FALSE)
{
    .checkTRUEFALSE(abbreviate)
    return (as.character(day_of_week(x, labels = TRUE, abbreviate = abbreviate)))
}


#' @rdname time-index-components
#' @export
hour <- function(x)
{
    if (inherits(x, "POSIXlt")) return (x$hour)
    x <- as.tind(x)
    .expect_type(.get.type(x), c("t", "h"), 1L)
    type <- .get.type(x)
    tz <- .get.tz(x)
    if (type == "h") return (.h2hour(.unclass(x)))
    return (.t2hour(.unclass(x), tz = tz))
}


#' @rdname time-index-components
#' @export
am <- function(x) (hour(x) < 12)


#' @rdname time-index-components
#' @export
pm <- function(x) (hour(x) >= 12)


#' @rdname time-index-components
#' @export
minute <- function(x)
{
    if (inherits(x, "POSIXlt")) return (x$min)
    x <- as.tind(x)
    .expect_type(.get.type(x), c("t", "h"), 1L)
    type <- .get.type(x)
    tz <- .get.tz(x)
    if (type == "h") return (.h2min(x))
    return (.t2min(.unclass(x), tz = tz))
}


#' @rdname time-index-components
#' @export
second <- function(x)
{
    if (inherits(x, "POSIXlt")) return (round(x$sec, 6L))
    x <- as.tind(x)
    .expect_type(.get.type(x), c("t", "h"), 1L)
    type <- .get.type(x)
    tz <- .get.tz(x)
    if (type == "h") return (.h2sec(x))
    return (.t2sec(.unclass(x), tz = tz))
}



# time index properties
# ###################################################################

#' Time Index Properties (Leap Years, Period Lengths, DST)
#'
#' @description
#' The following functions can be used to determine whether years are leap
#' years, compute the number of subperiods within a period, and determine
#' whether Daylight Saving Time is on for particular date-time index.
#' All function are vectorised.
#'
#' \code{is.leap_year} returns \code{TRUE} for leap years and \code{FALSE} for
#' non-leap ones.
#'
#' \code{days_in_year} and \code{weeks_in_year} return the number of days
#' (365--366) and the number of weeks (52--53) in a year.
#'
#' \code{days_in_quarter} and \code{days_in_month} return the number of days
#' in a quarter (90--92) or a month (28--31), respectively.
#'
#' \code{hours_in_day} returns the number of hours in a day (24 most
#' of the time, a different number during DST/UTC offset changes).
#'
#' \code{is.dst} returns \code{TRUE} when Daylight Saving Time is on
#' and \code{FALSE} otherwise.
#'
#' @param x an object of \code{tind} class or an R object coercible to it.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#'
#' @return  \code{is.leap_year} and \code{is.dst} return logical vectors.
#' The remaining functions return integer vectors, except for \code{hours_in_day},
#' which returns numeric vectors.
#'
#' @seealso \link{time-index-components}, \link{calendrical-computations},
#' \link{Ops}, \code{\link{tzone}}, \code{\link{bizdays_in_month}}.
#'
#' @name time-index-properties
#'
NULL


#' @rdname time-index-properties
#' @export
is.leap_year <- function(x)
{
    y <- .require_type(as.tind(x), "y", lower = TRUE)
    return (.is.leap_year(y))
}


#' @rdname time-index-properties
#' @export
days_in_year <- function(x)
{
    y <- .require_type(as.tind(x), "y", lower = TRUE)
    return (365L + .is.leap_year(y))
}


#' @rdname time-index-properties
#' @export
weeks_in_year <- function(x)
{
    y <- .require_type(as.tind(x), "y", lower = TRUE)
    return (.weeks_in_year(y))
}


#' @rdname time-index-properties
#' @export
days_in_quarter  <- function(x)
{
    q <- .require_type(as.tind(x), "q", lower = TRUE)
    return (.days_in_quarter(q))
}


#' @rdname time-index-properties
#' @export
days_in_month <- function(x)
{
    m <- .require_type(as.tind(x), "m", lower = TRUE)
    return (.days_in_month(m))
}


#' @rdname time-index-properties
#' @export
hours_in_day <- function(x, tz = NULL)
{
    x <- as.tind(x)
    type <- .get.type(x)
    .expect_type(type, c("d", "t"), 1L)
    if (type == "d") {
        tz <- .check_tz(tz)
    } else { # if (type == "t")
        tz <- if (is.null(tz)) .get.tz(x) else .check_tz(tz)
        x <- .cast(x, "d")
    }
    return (.hours_in_day(.unclass(x), tz))
}


#' @rdname time-index-properties
#' @export
is.dst <- function(x)
{
    x <- as.tind(x)
    .expect_type(.get.type(x), "t", 1L)
    tz <- .get.tz(x)
    return (.isdst_t(.unclass(x), tz = tz))
}



# calendrical computations
# ###################################################################

#' Calendrical Computations in \pkg{tind} Package
#'
#' @description
#' The following functions can be used for calendrical computations, especially
#' determining dates of movable observances. All function are vectorised.
#'
#' \code{nth_day_of_year} returns the date of the nth day of a year.
#'
#' \code{last_day_in_month} and \code{last_day_in_quarter} return the date
#' of the last day in a month or a quarter.
#'
#' \code{nth_dw_in_month} returns the date of the nth day
#' of week in a month.
#'
#' \code{last_dw_in_month} returns the date of the last day
#' of week in a month.
#'
#' \code{easter} returns the date of Easter in a year.
#'
#' @param y,q,m an object of \code{tind} class or an R object coercible to it.
#' @param nth a numeric value or vector of indices (1--366 for \code{nth_day_of_year},
#'          1--5 for \code{nth_dw_in_month}).
#' @param dw a numeric value or vector of days of week (values in range 1--7
#'           with Monday as the 1st day).
#'
#' @return
#' An object of \code{tind} class with dates (type \code{"d"}).
#'
#' @seealso \link{time-index-components}, \link{time-index-properties},
#' \link{Ops}. Further examples of application of these functions
#' can be found in \code{\link{calendar}} documentation. For calendrical
#' computations involving business days see \code{\link{bizday}}.
#'
#' @name calendrical-computations
#'
#' @examples
#' # Thanksgiving in the US is observed on the fourth Thursday of November,
#' # which in 2019 was on:
#' nth_dw_in_month(4, 4, 201911)
#' # and Black Friday?
#' nth_dw_in_month(4, 4, 201911) + 1
#'
#' # Daylight Saving Time in the EU in 2019 began on the last Sunday in March,
#' # which was on:
#' last_dw_in_month(7, 201903)
#'
#' # International Monetary Market dates in 2022 - 3rd Wednesday
#' # of March, June, September, and December
#' nth_dw_in_month(3, 3, tind(y = 2022, m = 3 * 1:4))
#'
#' # determine frequencies of Easter months over the last 100 years
#' # Easter months
#' em <- month(easter(as.year(today()) + (-99:0)), labels = TRUE)
#' # table and barplot
#' table(em) / length(em) * 100
#' if (require("graphics", quietly = TRUE)) {
#'     barplot(table(em) / length(em) * 100, ylim = c(0, 100), col = "#faf06d")
#' }
#'
NULL


#' @rdname calendrical-computations
#' @export
nth_day_of_year <- function(nth, y)
{
    y <- .unclass(.require_type(as.tind(y), "y", lower = TRUE))
    d <- .validate_yj(y, nth)
    nok <- suppressWarnings(!(is.na(y) | is.na(nth)) & is.na(d))
    if (any(nok)) {
        mes1 <- gettextf("NAs introduced")
        ii <- which.max(nok)
        iin <- (ii - 1L) %% length(nth) + 1L
        iix <- (ii - 1L) %% length(y) + 1L
        first <- sprintf("nth[%s] = %s, y[%s] = %s",
                         format(iin, scientific = FALSE), as.character(nth[iin]),
                         format(iix, scientific = FALSE), .y2char(y[iix]))
        mes2 <- gettextf("first occurrence: %s", first)
        warning(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
    }
    return (.tind(d, "d"))
}


#' @rdname calendrical-computations
#' @export
last_day_in_month <- function(m)
{
    m <- .unclass(.require_type(as.tind(m), "m", lower = TRUE))
    return (.tind(.last_day_in_month(m), "d"))
}


#' @rdname calendrical-computations
#' @export
last_day_in_quarter <- function(q)
{
    q <- .unclass(.require_type(as.tind(q), "q", lower = TRUE))
    return (.tind(.last_day_in_month(.q2m(q + 1L) - 1L), "d"))
}


#' @rdname calendrical-computations
#' @export
nth_dw_in_month <- function(nth, dw, m)
{
    m <- .unclass(.require_type(as.tind(m), "m", lower = TRUE))
    d <- .nth_dw_in_month(nth, dw, m)
    nok <- suppressWarnings(!(is.na(nth) | is.na(dw) | is.na(m)) & is.na(d))
    if (any(nok)) {
        mes1 <- gettextf("NAs introduced")
        ii <- which.max(nok)
        iin <- (ii - 1L) %% length(nth) + 1L
        iid <- (ii - 1L) %% length(dw) + 1L
        iix <- (ii - 1L) %% length(m) + 1L
        first <- sprintf("nth[%s] = %s, dw[%s] = %s, m[%s] = %s",
                         format(iin, scientific = FALSE), as.character(nth[iin]),
                         format(iid, scientific = FALSE), as.character(dw[iid]),
                         format(iix, scientific = FALSE), .m2char(m[iix]))
        mes2 <- gettextf("first occurrence: %s", first)
        warning(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
    }
    return (.tind(d, "d"))
}


#' @rdname calendrical-computations
#' @export
last_dw_in_month <- function(dw, m)
{
    m <- .unclass(.require_type(as.tind(m), "m", lower = TRUE))
    d <- .last_dw_in_month(dw, m)
    nok <- suppressWarnings(!(is.na(dw) | is.na(m)) & is.na(d))
    if (any(nok)) {
        mes1 <- gettextf("NAs introduced")
        ii <- which.max(nok)
        iid <- (ii - 1L) %% length(dw) + 1L
        iix <- (ii - 1L) %% length(m) + 1L
        first <- sprintf("dw[%s] = %s, m[%s] = %s",
                         format(iid, scientific = FALSE), as.character(dw[iid]),
                         format(iix, scientific = FALSE), .m2char(m[iix]))
        mes2 <- gettextf("first occurrence: %s", first)
        warning(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
    }
    return (.tind(d, "d"))
}


#' @rdname calendrical-computations
#' @export
easter <- function(y)
{
    y <- .unclass(.require_type(as.tind(y), "y", lower = TRUE))
    return (.tind(.easter(y), "d"))
}



# construct date-time indices from date and time components
# ###################################################################

#' Construct Date-Time Indices from Date and Time Components
#'
#' @description
#' \code{date_time} can be used to create date-time indices from its components:
#' date and time of day (hour, minute, and second).
#'
#' \code{date_time_split} performs the opposite computation: given date-time
#' indices, it returns a two-element list with vectors of dates and times.
#'
#' @details
#' If arguments of \code{date_time} are of different length, they are recycled.
#'
#' When \code{grid} is set to \code{TRUE}, date-time indices are constructed
#' from all combinations of dates and times in a way similar to how
#' functions \code{expand.grid} and \code{kronecker} work, see Examples.
#'
#' If \code{H} argument is numeric, time of day is constructed from \code{H},
#' \code{M}, and \code{S} arguments. In the last step date and time are combined
#' in order to construct date-time index. If \code{H} is not numeric,
#' \code{M} and \code{S} should not be supplied and time of day is constructed
#' from \code{H} argument only.
#'
#' When provided without \code{H} argument \code{date_time} behaves just like
#' \code{as.date_time} i.e. returns the beginning of a day.
#'
#' When an hour occurs twice in a day (due to DST/UTC offset changes),
#' the second occurrence is selected with a warning. When hour is missing
#' (for the same reason), \code{NA} is returned with a warning. See Examples.
#'
#' @param d an object of \code{tind} class of type date (type \code{"d"})
#'          or an R object coercible to it.
#' @param H a numeric vector with hour values or an R object coercible to
#'          time index of time-of-day type (type \code{"h"}).
#' @param M (optional) a numeric vector with minutes.
#' @param S (optional) a numeric vector with seconds.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#' @param grid a logical value, if \code{TRUE} date-time indices are constructed
#'             from all combinations of dates and times (\code{FALSE} by default).
#' @param x an object of \code{tind} class of type date-time (type \code{"t"})
#'          or an R object coercible to it.
#'
#' @return \code{date_time} returns an object of \code{tind} class
#' with date-time indices (type \code{"t"}). \code{date_time_split}
#' returns a two-element list with vectors of dates (\code{$date}) and times (\code{$time}).
#'
#' @seealso \code{\link{tind}} constructor, \link{time-index-components},
#' \code{\link{tzone}}.
#'
#' @examples
#' date_time(today() + (0:1), "11:25:20.75")
#' date_time(today() + (0:1), as.time("11:25:20.75"))
#' date_time(today() + (0:1), 11, 25, 20.75)
#' date_time(today() + (0:1), "11:25:20")
#' date_time(today() + (0:1), 11, 25, 20)
#' date_time(today() + (0:1), "11:25")
#' date_time(today() + (0:1), 11, 25)
#' date_time(today() + (0:1), "11")
#' date_time(today() + (0:1), 11)
#' date_time(today() + (0:1))
#' # using 'grid' argument
#' date_time(today() + 0:2, c(8, 12, 16))
#' date_time(today() + 0:2, c(8, 12, 16), grid = TRUE)
#'
#' # split date-time
#' (nw <- now())
#' date_time_split(nw)
#'
#' # corner cases (with warnings)
#' if ("Europe/Warsaw" %in% OlsonNames()) try({
#'     # 2020-10-25 had 25h with 02:00 repeated
#'     date_time("2020-10-25", 0:2, tz = "Europe/Warsaw")
#' })
#' if ("Europe/Warsaw" %in% OlsonNames()) try({
#'     # 2021-03-28 had 23h with 02:00 missing
#'     date_time("2021-03-28", 0:2, tz = "Europe/Warsaw")
#' })
#'
#' @name date_time
#'
NULL


#' @name date_time
#' @export
date_time <- function(d, H, M, S, tz = NULL, grid = FALSE)
{
    d <- as.tind(d)
    .expect_type(.get.type(d), "d", 1L)
    d <- .unclass(d)
    tz <- .check_tz(tz)
    .checkTRUEFALSE(grid)

    if (missing(H)) {
        if (!missing(M) || !missing(S)) {
            mes <- gettextf("%s or %s argument provided without %s",
                            sQuote("M"), sQuote("S"), sQuote("H"))
            stop(mes, domain = NA)
        }
        return (.tind(.d2t(d, tz), "t", tz))
    }

    mmis <- missing(M)
    smis <- missing(S)
    hnum <- is.numeric(H)
    if (hnum) {
        if (mmis && !smis) {
            mes <- gettextf("%s argument provided without %s",
                            sQuote("S"), sQuote("M"))
            stop(mes, domain = NA)
        }
    } else {
        if (!mmis || !smis) {
            mes <- gettextf("%s or %s argument provided with nonnumeric %s",
                            sQuote("M"), sQuote("S"), sQuote("H"))
            stop(mes, domain = NA)
        }
    }

    # lengths
    nd <- length(d)
    nh <- length(H)
    if (mmis) M <- 0L
    if (smis) S <- 0
    nm <- length(M)
    ns <- length(S)

    # time part and length adjustment
    if (grid) {
        nhms <- .check_lengths(nh, nm, ns)
        if (!nhms || !nd) return (.tind(double(), "t", tz = tz))
        HMS <- if (hnum) suppressWarnings(.validate_hms(H, M, S))
               else .unclass(suppressWarnings(as.time(H)))
        dd <- rep(d, each = nhms)
        HMS <- rep(HMS, nd)
    } else {
        ndhms <- .check_lengths(nd, nh, nm, ns)
        if (!ndhms) return (.tind(double(), "t", tz = tz))
        if (hnum) {
            if (nd == ndhms) H <- rep_len(H, nd)
            HMS <- suppressWarnings(.validate_hms(H, M, S))
        } else HMS <- .unclass(suppressWarnings(as.time(H)))
        dd <- d
    }

    # construct final indices
    res <- .dhz2t(dd, HMS, integer(), tz, 1L)

    # check NAs
    nok <- suppressWarnings(!(is.na(d) | is.na(H) | is.na(M) | is.na(S)) & is.na(res))
    if (any(nok)) {
        ii <- which.max(nok)
        iid <- if (grid) (ii - 1L) %/% nhms + 1L else (ii - 1L) %% nd + 1L
        first <- sprintf("d[%d] = %s", iid, .d2char(d[iid]))
        iih <- (ii - 1L) %% nh + 1L
        if (hnum) {
            first <- c(first, sprintf("H[%d] = %s", iih, H[iih]))
            if (!mmis) {
                iim <- (ii - 1L) %% nm + 1L
                first <- c(first, sprintf("M[%d] = %s", iim, M[iim]))
            }
            if (!smis) {
                iis <- (ii - 1L) %% ns + 1L
                first <- c(first, sprintf("S[%d] = %s", iis, S[iis]))
            }
        } else {
            first <- c(first, sprintf("H[%d] = %s", iih, .h2char(HMS[iih])))
        }
        mes0 <- gettextf("NAs introduced")
        mes1 <- gettextf("first occurrence: %s", toString(first))
        mes2 <- gettextf("time zone: %s", tz)
        warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
    }

    return (.tind(res, "t", tz))
}


#' @name date_time
#' @export
date_time_split <- function(x)
{
    x <- as.tind(x)
    .expect_type(.get.type(x), "t", 1L)
    tz <- .get.tz(x)
    res <- .t2dhz(.unclass(x), tz, gmtoff = FALSE, zone = FALSE)
    return (list(date = .tind(res[[1L]], "d"), time = .tind(res[[2L]], "h")))
}



# year fractions
# ###################################################################

#' Converting Time Indices to Year Fractions and Back
#'
#' @description
#' \code{year_frac} computes year fraction corresponding to a time index.
#' \code{yf2tind} performs the reverse computation.
#'
#' @details
#' \code{year_fraction} returns numeric vector representing time indices
#' in the form year + year fraction. For years this is equivalent to
#' \code{as.numeric}. Year fraction is determined based on the index
#' within particular year (minus 1 for all indices except for date-time)
#' and the number of periods within the year.
#' E.g., \code{2001Q1} gives \code{2001} (\code{2001 + (1 - 1) / 4}),
#' \code{2001Q3} --- \code{2001.5} (\code{2001 + (3 - 1) / 4}),
#' \code{2010-04} (April 2010) --- \code{2010.25} (\code{2010 + (4 - 1) / 12}),
#' \code{2000-02-29} (60th day in 2000) --- \code{2000.1612}
#' (\code{2000 + (60 - 1) / 366}, 2000 was a leap year).
#'
#' @note
#' \code{year_frac} is not to be confused with a similarly called function
#' (\code{YEARFRAC}) found in popular spreadsheet software. In order to compute
#' differences between dates as year fractions use \code{\link{daycount_frac}}
#' function.
#'
#' @param x an object of \code{tind} class or an R object coercible to it
#'         for \code{year_frac}, a numeric vector for \code{yf2tind}.
#' @param type a character value determining time index type
#'             (\code{y} - years, \code{q} - quarters, \code{m} - months,
#'             \code{w} - weeks, \code{d} - dates, \code{t} - date-time).
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#'
#' @return A numeric vector for \code{year_frac} and \code{tind} for \code{yf2tind}.
#' Result is of the same length as argument.
#'
#' @seealso \code{\link{daycount_frac}}.
#'
#' @name year_frac
#'
#' @examples
#' year_frac(today())
#' year_frac(now())
#' yf2tind(2023.5, "y")
#' yf2tind(2023.5, "q")
#' yf2tind(2023.5, "m")
#' yf2tind(2023.5, "w")
#' yf2tind(2023.5, "d")
#' yf2tind(2023.5, "t")
#'
NULL


#' @rdname year_frac
#' @export
year_frac <- function(x)
{
    x <- as.tind(x)
    .expect_type(.get.type(x), c("y", "q", "m", "w", "d", "t"), 1L)
    type <- .get.type(x)
    tz <- .get.tz(x)

    return (do.call(paste0(".", type, "2yf"), c(list(.unclass(x)), tz)))
}


#' @rdname year_frac
#' @export
yf2tind <- function(x, type = NULL, tz = NULL)
{
    if (!is.numeric(x)) {
        mes1 <- gettextf("invalid %s argument", sQuote("x"))
        mes2 <- gettextf("expected a numeric vector")
        stop(paste0(mes1, "; ", mes2), domain = NA)
    }
    typetz <- .check_type_tz(type, tz, types = c("y", "q", "m", "w", "d", "t"))
    type <- typetz$type
    tz <- typetz$tz
    if (is.null(type)) stop("type not provided")

    if (type != "t") {
        y <- .validate_y(floor(x))
        if (type == "y") return (.tind(y, type))
        eps <- 1e-4
        if (type == "q") {
            q <- .validate_yq(y, floor((x - y + eps) * 4) + 1)
            return (.tind(q, type))
        }
        if (type == "m") {
            m <- .validate_ym(y, floor((x - y + eps) * 12) + 1)
            return (.tind(m, type))
        }
        if (type == "w") {
            w <- .validate_yw(y, floor((x - y + eps) * .weeks_in_year(y)) + 1)
            return (.tind(w, type))
        }
        if (type == "d") {
            d <- .validate_yj(y, floor((x - y + eps) * .days_in_year(y)) + 1)
            return (.tind(d, type))
        }
    }
    # t
    yr <- suppressWarnings(range(.validate_y(floor(x)), na.rm = TRUE))
    if (!is.finite(yr[1L])) return (tind(length = length(x), type = type, tz = tz))
    xs <- as.integer(seq.int(yr[1L], yr[2L] + 1L))
    xs <- .y2t(xs, tz)
    ys <- .t2yf(xs, tz)
    if (requireNamespace("stats", quietly = TRUE)) {
        res <- round(stats::approx(ys, xs, x, rule = 1L)$y, digits = 4)
    } else {
        ii <- findInterval(x, ys)
        ii[(ii < 1L) | (ii > length(ys))] <- NA
        dxs <- diff(xs)
        dys <- diff(ys)
        res <- xs[ii] + dxs[ii] * (x - ys[ii]) / dys[ii]
        res <- as.tind(round(res, digits = 4), "t", tz = tz)
    }

    return (.tind(res, "t", tz = tz))
}



# julian day numbers
# ###################################################################

#' Date and Date-Time Conversion to and from Julian Day Number (JDN)
#'
#' @description
#' \code{jdn} computes the JDNs for dates or date-time indices and
#' \code{jdn2tind} returns dates or date-time indices given JDNs.
#'
#' For date arguments \code{jdn} returns the numbers of days
#' since November 24, 4714 BC in the proleptic Gregorian calendar as
#' an integer vector. For date-time arguments day fraction in UTC is computed
#' and the return value is a numeric vector.
#'
#' For integer arguments \code{jdn2tind} return \code{tind} of type
#' \code{"d"} (date), for non-integer arguments --- \code{tind} of type
#' \code{"t"} (date-time). If \code{tz} argument is provided the return value
#' is always of type \code{"t"} (date-time).
#'
#' @note
#' For date-time indices JDN is computed based on day fraction
#' since noon UTC and not midnight, so 0.5 offset will be observable.
#'
#' @param x an object of \code{tind} class or an R object coercible to it
#'          for \code{jdn}, an integer or numeric vector for \code{jdn2tind}.
#' @param tz (optional) a character value determining the time zone (the default
#'           \code{NULL} is interpreted as the system time zone).
#'           See \code{\link{tzone}} documentation for information on time zones.
#'
#' @return
#' An integer or numeric vector for \code{jdn}, an object of \code{tind} class
#' (type \code{"d"} or \code{"t"}) for \code{jdn2tind}.
#'
#' @seealso \code{\link{date2num}} for conversion between dates and their
#' integer representations found different software packages.
#'
#' @name jdn
#'
#' @examples
#' # JDN of 2000-01-01 is 2451545
#' jdn("2000-01-01")
#' jdn2tind(2451545)
#' # JDN today, now?
#' jdn(today())
#' jdn(now())
#' # notice the .5 offset
#' jdn(today(tz = "UTC"))
#' format(jdn(as.date_time(today(tz = "UTC"), tz = "UTC")), digits = 8)
#'
NULL


#' @rdname jdn
#' @export
jdn <- function(x)
{
    x <- as.tind(x)
    .expect_type(.get.type(x), c("d", "t"), 1L)
    res <- if (.get.type(x) == "d") .d2jdn(x)
           else .t2jdn(.unclass(x))
    return (res)
}


#' @rdname jdn
#' @export
jdn2tind <- function(x, tz = NULL)
{
    if (!is.numeric(x)) {
        mes1 <- gettextf("invalid %s argument", sQuote("x"))
        mes2 <- gettextf("expected a numeric vector")
        stop(paste0(mes1, "; ", mes2), domain = NA)
    }

    n <- length(x)
    if (!n) return (if (is.integer(x) && is.null(tz)) tind(type = "d") else
                    tind(type = "t", tz = tz))

    type <- if ((is.integer(x) || !.anyTRUE(floor(x) != x)) && is.null(tz)) "d"
            else "t"

    if (type == "d")
        return (.tind(.validate_d(.jdn2d(.require_mode(x, "integer"))), "d"))

    tz <- .check_tz(tz)
    return (.tind(.validate_t(.jdn2t(x)), "t", tz))
}

