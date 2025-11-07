#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############################################################# #
# calendrical computations - business days, day count fractions #
# ############################################################# #


# business days
# ###################################################################

#' Business Days
#'
#' @description
#' \code{bizday} computes the nearest business day from the date given
#' a calendar function and one of the date rolling conventions (see Details).
#'
#' \code{bizday_advance} advances date(s) by \code{n} business days.
#'
#' \code{next_bizdays} determines the following \code{n} business days after a date.
#'
#' \code{first_bizday_in_month/quarter} and \code{last_bizday_in_month/quarter}
#' determine the first and the last business day in a month and a quarter.
#'
#' \code{bizdays_in_month},  \code{bizdays_in_quarter}, and \code{bizdays_in_year}
#' return the number of business days in particular time period.
#'
#' \code{bizday_diff} computes the number of business days between two dates.
#'
#' @details
#'
#' The vectorised implementations of \code{bizday}, \code{bizday_advance},
#' and \code{next_bizdays} work under the assumption of at least one business day
#' in a week and could return \code{NA}s for pathological calendar functions.
#'
#' \code{bizday_advance} with increment 0 will adjust the date to the first
#' preceding business day if it is not a business day (will act as
#' \code{bizday(d, "p", *)}).
#'
#' \code{next_bizdays} accepts negative arguments and returns an increasing
#' sequence of business dates prior to \code{d} of length \code{abs(n)}.
#'
#' \strong{Conventions}
#'
#' The following date rolling conventions are supported (applied when the day
#' is not a business day):
#' \describe{
#'     \item{\code{"p"}}{preceding, the previous business day,}
#'     \item{\code{"f"}}{following, the next business day,}
#'     \item{\code{"mp"}}{modified preceding, the previous business day unless
#'                        it falls in the previous month, in which case the next
#'                        business day is chosen,}
#'     \item{\code{"mf"}}{modified following, the next business day unless it
#'                        falls in the next month, in which case the the
#'                        previous business day is chosen,}
#'     \item{\code{"mf2"}}{modified following bimonthly, the next business day
#'                         unless it falls in the next month or the next half of
#'                         the month (after 15th), in which case the the previous
#'                         business day is chosen.}
#' }
#'
#' \strong{Calendar Functions}
#'
#' Calendar function should take a vector of days as an argument and return
#' a logical vector of the same length marking business days (as \code{TRUE})
#' or a list of two or three logical vectors of the same length with the first
#' marking business days. See also \link{calendars} for real-life
#' examples of calendar functions. When \code{calendar} function is not supplied,
#' Monday-Friday are marked as business days.
#'
#' @param d an object of \code{tind} class or an R object coercible to it, dates.
#' @param convention a character value determining date rolling convention, see Details.
#' @param calendar a function determining working days and holidays (see Details)
#'                 or \code{NULL}.
#' @param n an integer vector (for \code{bizday_advance}) or integer value
#'          (for \code{next_bizdays}), numebr of business days.
#' @param m an object of \code{tind} class or an R object coercible to it, months.
#' @param q an object of \code{tind} class or an R object coercible to it, quarters.
#' @param y an object of \code{tind} class or an R object coercible to it, years.
#' @param d1 an object of \code{tind} class or an R object coercible to it, start dates.
#' @param d2 an object of \code{tind} class or an R object coercible to it, end dates.
#' @param start.incl a logical value, if \code{TRUE}, the starting date is included
#'                   in computation of the number of business days between two dates
#'                   (\code{TRUE} by default).
#' @param end.incl a logical value, if \code{TRUE}, the end date is included
#'                 in computation of the number of business days between two dates
#'                 (\code{FALSE} by default).
#'
#' @return
#' \code{bizday} and \code{first/last_bizday_in_month/quarter} return
#' vectors of dates of the same length as their first argument.
#'
#' \code{bizday_advance} returns a vector of dates of length equal to length
#' of the longer of the first two arguments (\code{d} and \code{n}).
#'
#' \code{next_bizdays} returns a vector of dates of length \code{abs(n)}.
#'
#' \code{bizday_diff} and \code{bizdays_in_month/quarter/year} return integer
#' vectors.
#'
#' @seealso \code{\link{calendars}} for examples of calendar functions,
#' \code{\link{daycount_frac}} for computations of day count fractions /
#' accrual factors.
#'
#' @examples
#' # a trivial calendar function (Mon-Fri)
#' monfri <- function(d) (day_of_week(d) <= 5L)
#' # 2022-10-01 was Saturday
#' calendar("2022-10", monfri)
#' (d <- as.date("2022-10-01"))
#' bizday(d, "p", monfri)
#' bizday(d, "mp", monfri)
#' bizday(d, "f", monfri)
#' bizday(d, "mf", monfri)
#' bizday(d, "mf2", monfri)
#' # 2022-10-15 was Saturday again
#' calendar("2022-10", monfri)
#' (d <- as.date("2022-10-15"))
#' bizday(d, "p", monfri)
#' bizday(d, "mp", monfri)
#' bizday(d, "f", monfri)
#' bizday(d, "mf", monfri)
#' bizday(d, "mf2", monfri)
#' # 2022-12-31 was also Saturday
#' calendar("2022-12", monfri)
#' (d <- as.date("2022-12-31"))
#' bizday(d, "p", monfri)
#' bizday(d, "mp", monfri)
#' bizday(d, "f", monfri)
#' bizday(d, "mf", monfri)
#' bizday(d, "mf2", monfri)
#'
#' @export
bizday <- function(d, convention, calendar)
{
    .bdconv <- list(p = c("preceding"),
                    f = c("following"),
                    mp = c("modified preceding"),
                    mf = c("modified following"),
                    mf2 = c("modified following bimonthly"))
    if (missing(convention)) convention <- NULL
    convention <- .match.arg(convention, .bdconv)

    d <- .require_type(as.tind(d), "d", lower = TRUE)
    if (!length(d) || anyNA(d) && all(is.na(d))) return (d)

    d0 <- min(d, na.rm = TRUE) - 14L
    d1 <- max(d, na.rm = TRUE) + 14L
    dd <- d0 + 0L:as.integer(d1 - d0)
    if (missing(calendar)) calendar <- NULL
    bds <- dd[.eval_clndr(dd, calendar, TRUE, TRUE)]

    if (convention == "p")
        bd <- bds[.match_left(d, bds)]
    else if (convention == "f")
        bd <- bds[.match_right(d, bds)]
    else if (convention == "mp") {
        bd <- bds[.match_left(d, bds)]
        ipm <- as.month(bd) < as.month(d)
        if (anyNA(ipm)) ipm <- !is.na(ipm) & ipm
        bd[ipm] <- bds[.match_right(d[ipm], bds)]
    } else if (convention == "mf") {
        bd <- bds[.match_right(d, bds)]
        inm <- as.month(d) < as.month(bd)
        if (anyNA(inm)) inm <- !is.na(inm) & inm
        bd[inm] <- bds[.match_left(d[inm], bds)]
    } else if (convention == "mf2") {
        bd <- bds[.match_right(d, bds)]
        inhm <- (as.month(d) < as.month(bd)) | (day(d) <= 15L) & (day(bd) > 15L)
        if (anyNA(inhm)) inhm <- !is.na(inhm) & inhm
        bd[inhm] <- bds[.match_left(d[inhm], bds)]
    }
    bd <- as.date(bd)
    names(bd) <- names(d)
    return (as.date(bd))
}


#' @rdname bizday
#' @export
bizday_advance <- function(d, n = 1L, calendar)
{
    d <- .require_type(as.tind(d), "d", lower = TRUE)
    if (!is.numeric(n)) {
        mes0 <- gettextf("invalid %s argument", sQuote("n"))
        mes1 <- gettextf("expected an integer vector")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    n <- as.integer(n)
    nd <- length(d)
    nn <- length(n)
    nn <- .check_lengths(nd, nn)
    if (!nn) return (tind(type = "d"))

    dd <- suppressWarnings(d  + 7L * n)
    if (anyNA(dd) && all(is.na(dd))) return (tind(length = nn, type = "d"))
    d0 <- min(dd, d, na.rm = TRUE) - 14L
    d1 <- max(dd, d, na.rm = TRUE) + 14L
    dd <- d0 + 0L:as.integer(d1 - d0)
    if (missing(calendar)) calendar <- NULL
    bd <- .eval_clndr(dd, calendar, TRUE, TRUE)
    cb <- cumsum(bd)
    ii <- as.integer(d - d0) + 1L
    ii <- match(suppressWarnings(cb[ii] + as.integer(!bd[ii] & (n < 0L)) + n), cb[bd])
    return (dd[bd][ii])
}


#' @rdname bizday
#' @export
next_bizdays <- function(d, n = 1L, calendar)
{
    d <- .require_type(as.tind(d), "d", lower = TRUE)
    if ((length(d) != 1L) || is.na(d)) {
        mes0 <- gettextf("invalid %s argument", sQuote("d"))
        mes1 <- gettextf("single non-NA date expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    if ((length(n) != 1L) || !is.numeric(n) ||
        is.na(suppressWarnings(as.integer(n)))) {
        mes0 <- gettextf("invalid %s argument", sQuote("n"))
        mes1 <- gettextf("single non-NA integer expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    n <- as.integer(n)
    if (!n) return (d[n])
    neg <- (n < 0L)
    n <- abs(n)
    incd <- 1:as.integer(14L + 7L * n)
    dd <- if (neg) d - incd else d + incd
    if (missing(calendar)) calendar <- NULL
    bd <- .eval_clndr(dd, calendar, TRUE, TRUE)
    res <- dd[bd][seq_len(n)]
    return (if (neg) rev(res) else res)
}


#' @rdname bizday
#' @export
first_bizday_in_month <- function(m, calendar)
{
    m <- .require_type(as.tind(m), "m", lower = TRUE)
    return (if (missing(calendar)) bizday(as.date(m), "f")
            else bizday(as.date(m), "f", calendar))
}


#' @rdname bizday
#' @export
last_bizday_in_month <- function(m, calendar)
{
    return (if (missing(calendar)) bizday(last_day_in_month(m), "p")
            else bizday(last_day_in_month(m), "p", calendar))
}


#' @rdname bizday
#' @export
first_bizday_in_quarter <- function(q, calendar)
{
    q <- .require_type(as.tind(q), "q", lower = TRUE)
    return (if (missing(calendar)) bizday(as.date(q), "f")
            else bizday(as.date(q), "f", calendar))
}


#' @rdname bizday
#' @export
last_bizday_in_quarter <- function(q, calendar)
{
    return (if (missing(calendar)) bizday(last_day_in_quarter(q), "p")
            else bizday(last_day_in_quarter(q), "p", calendar))
}


#' @rdname bizday
#' @export
bizdays_in_month <- function(m, calendar)
{
    m <- .require_type(as.tind(m), "m", lower = TRUE)
    d0 <- as.date(m)
    d1 <- last_day_in_month(m)
    return (if (missing(calendar)) bizday_diff(d0, d1, start.incl = TRUE,
                                                       end.incl = TRUE)
            else bizday_diff(d0, d1, calendar, start.incl = TRUE,
                                               end.incl = TRUE))
}


#' @rdname bizday
#' @export
bizdays_in_quarter <- function(q, calendar)
{
    q <- .require_type(as.tind(q), "q", lower = TRUE)
    d0 <- as.date(q)
    d1 <- last_day_in_quarter(q)
    return (if (missing(calendar)) bizday_diff(d0, d1, start.incl = TRUE,
                                                       end.incl = TRUE)
            else bizday_diff(d0, d1, calendar, start.incl = TRUE,
                                               end.incl = TRUE))
}


#' @rdname bizday
#' @export
bizdays_in_year <- function(y, calendar)
{
    y <- .require_type(as.tind(y), "y", lower = TRUE)
    d0 <- as.date(y)
    d1 <- as.date(y + 1L) - 1L
    return (if (missing(calendar)) bizday_diff(d0, d1, start.incl = TRUE,
                                                       end.incl = TRUE)
            else bizday_diff(d0, d1, calendar, start.incl = TRUE,
                                               end.incl = TRUE))
}


#' @rdname bizday
#' @export
bizday_diff <- function(d1, d2, calendar, start.incl = TRUE, end.incl = FALSE)
{
    .checkTRUEFALSE(start.incl)
    .checkTRUEFALSE(end.incl)

    d1 <- .require_type(as.tind(d1), "d", lower = TRUE)
    d2 <- .require_type(as.tind(d2), "d", lower = TRUE)
    n1 <- length(d1)
    n2 <- length(d2)
    nn <- .check_lengths(n1, n2)
    if (!nn) return (integer())
    # we need the same length
    if (n1 < nn) d1 <- rep(d1, length = nn)
    if (n2 < nn) d2 <- rep(d2, length = nn)

    dd1 <- min(d1, d2, na.rm = TRUE) - 1L
    dd2 <- max(d1, d2, na.rm = TRUE)
    dd <- dd1 + 0L:as.integer(dd2 - dd1)
    if (missing(calendar)) calendar <- NULL
    bb <- .eval_clndr(dd, calendar, TRUE, TRUE)
    cc <- cumsum(bb)
    i1 <- as.integer(d1 - dd1) + 1L
    i2 <- as.integer(d2 - dd1) + 1L
    neg <- (d1 > d2)
    if (isTRUE(any(neg))) {
        if (anyNA(neg)) neg <- !is.na(neg) & neg
        pos <- !neg
        if (start.incl && end.incl) {
            i1[pos] <- i1[pos] - 1L
            i2[neg] <- i2[neg] - 1L
        } else if (start.incl) {
            i1[pos] <- i1[pos] - 1L
            i2[pos] <- i2[pos] - 1L
        } else if (end.incl) {
            i1[neg] <- i1[neg] - 1L
            i2[neg] <- i2[neg] - 1L
        } else {
            i2[pos] <- pmax(i2[pos] - 1L, i1[pos])
            i1[neg] <- pmax(i1[neg] - 1L, i2[neg])
        }
    } else {
        if (start.incl) i1 <- i1 - 1L
        if (!end.incl) i2 <- if (start.incl) i2 - 1L else pmax(i2 - 1L, i1)
    }

    return (suppressWarnings(cc[i2] - cc[i1]))
}



# day count fractions / accrual factors
# ###################################################################

#' Differences Between Dates as Year Fractions / Accrual Factors
#'
#' @description
#' This function computes difference between two dates as year fraction
#' given day count convention.
#'
#' @details
#' Currently, the following day count conventions are supported:
#' \describe{
#'     \item{\code{30/360}}{also known as 30/360 Bond Basis or 360/360,
#' described in ISDA 2006 Section 4.16(f). The formula is as follows:
#' \deqn{\frac{360(y_2-y_1) + 30(m_2-m_1) + (d_2-d_1)}{360},}
#' where \eqn{y} denotes year, \eqn{m} month, \eqn{d} day of month.
#' Dates are adjusted accoring to the following rules: if \eqn{d_1} is 31,
#' it is changed to 30, if \eqn{d_2} is 31 and \eqn{d_1} is 30 or 31, \eqn{d_2}
#' is changed to 30.}
#'     \item{\code{30E/360}}{also known as Eurobond basis, 30/360 ICMA or 30/360 ISMA,
#' described in ICMA Rule 251.1(ii), 251.2 and ISDA 2006 Section 4.16(g).
#' The formula used is the same as above but the adjustment of dates is different:
#' if \eqn{d_1} or \eqn{d_2} is 31, it is changed to 30.}
#'     \item{\code{ACT/ACT}}{also known as Actual/Actual and Actual/Actual ISDA,
#' described in ISDA 2006 Section 4.16(b). Days between the dates (start included,
#' end excluded) are divided into two groups: falling in leap and non-leap years.
#' The number of days in leap years is divided by 366, the number of days in
#' non-leap years is divided by 365. Finally, the two fractions are added.}
#'     \item{\code{ACT/365F}}{also known as Actual/365 Fixed, described in
#' ISDA 2006 Section 4.16(d). Difference in days between dates divided by 365. }
#'     \item{\code{ACT/360}}{also known as Actual/360, described in
#' ISDA 2006 Section 4.16(e). Difference in days between dates divided by 360. }
#' }
#'
#' @param d1 an object of \code{tind} class representing start date(s)
#'           or an R object coercible to it.
#' @param d2 an object of \code{tind} class representing end date(s)
#'           or an R object coercible to it.
#' @param convention a chacter string determining day count convention to be used,
#'                   see Details.
#'
#' @return A numeric vector.
#'
#' @seealso \code{\link{year_frac}}, \code{\link{bizday}}.
#'
#' @references
#' International Swaps and Derivatives Association, Inc.,
#' \emph{2006 ISDA Definitions}, New York, 2006.
#'
#' @examples
#' daycount_frac("2023-01-29", "2023-03-31", "30/360")
#' 1/6 + 2/360
#' daycount_frac("2023-01-29", "2023-03-31", "30E/360")
#' 1/6 + 1/360
#' daycount_frac("2023-01-29", "2023-03-31", "ACT/ACT")
#' 61 / 365
#' daycount_frac("2024-01-29", "2024-03-31", "ACT/ACT")
#' 62 / 366
#' daycount_frac("2023-01-29", "2023-03-31", "ACT/365F")
#' 61 / 365
#' daycount_frac("2024-01-29", "2024-03-31", "ACT/365F")
#' 62 / 365
#' daycount_frac("2023-01-29", "2023-03-31", "ACT/360")
#' 61 / 360
#' daycount_frac("2024-01-29", "2024-03-31", "ACT/360")
#' 62 / 360
#'
#' @export
daycount_frac <- function(d1, d2, convention)
{
    .dcconv <- list(`30/360` = c("30/360 Bond Basis", "360/360",
                                 "ISDA 2006 Section 4.16(f)"),
                    `30E/360` = c("Eurobond basis", "30/360 ICMA", "30/360 ISMA",
                                  "ICMA Rule 251.1(ii), 251.2",
                                  "ISDA 2006 Section 4.16(g)"),
                    `ACT/ACT` = c("Actual/Actual", "Actual/Actual ISDA",
                                  "ISDA 2006 Section 4.16(b)"),
                    `ACT/365F` = c("Actual/365 Fixed",
                                   "ISDA 2006 Section 4.16(d)"),
                    `ACT/360` = c("Actual/360", "ISDA 2006 Section 4.16(e)"))
    if (missing(convention)) convention <- NULL
    convention <- .match.arg(convention, .dcconv)

    d1 <- .require_type(as.tind(d1), "d", lower = TRUE)
    d2 <- .require_type(as.tind(d2), "d", lower = TRUE)
    n1 <- length(d1)
    n2 <- length(d2)
    nn <- .check_lengths(n1, n2)
    if (!nn) return (double())
    # we need the same length
    if (n1 < nn) d1 <- rep(d1, length = nn)
    if (n2 < nn) d2 <- rep(d2, length = nn)

    if (grepl("^30", convention)) {
        y1 <- year(d1); y2 <- year(d2); m1 <- month(d1); m2 <- month(d2)
        d1 <- day(d1); d2 <- day(d2)
        if (convention == "30/360") {
            d1 <- pmin(d1, 30L)
            i2 <- !is.na(d1) & (d1 == 30L)
            d2[i2] <- pmin(d2[i2], 30L)
        }
        if (convention == "30E/360") {
            d1 <- pmin(d1, 30L)
            d2 <- pmin(d2, 30L)
        }
        return (y2 - y1 + (m2 - m1) / 12 + (d2 - d1) / 360)
    }
    if (convention == "ACT/360") return (as.numeric(d2 - d1) / 360)
    if (convention == "ACT/365F") return (as.numeric(d2 - d1) / 365)
    if (convention == "ACT/ACT") return (year_frac(d2) - year_frac(d1))
}

