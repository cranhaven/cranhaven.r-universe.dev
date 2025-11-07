#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################ #
# custom calendars #
# ################ #


#' Working with Custom Calendars
#'
#' @description
#' \pkg{tind} package provides an extensive collection of functions for
#' calendrical computations allowing users to write custom calendar functions
#' that can be used to mark business days, holidays, and other observances.
#' See \emph{Writing custom calendar functions} for an introduction to such
#' functions and Examples for two real-life examples. These functions can
#' later be used for pretty printing calendars on the console (using
#' \code{calendar} function), quick identification of business days, holidays,
#' and other observances (using \code{eval_calendar} function) and business day
#' computations (see \code{\link{bizday}}).
#'
#' \code{calendar} pretty prints a calendar for year(s) or month(s)
#' on the console using user-provided calendar function determining business
#' days and holidays.
#'
#' \code{eval_calendar} applies user-provided calendar function to a sequence
#' of dates. It is designed to be used by developers who wish to implement
#' new applications of custom calendars.
#'
#' @details
#' \code{calendar} uses \code{crayon} package (when available) for highlighting
#' dates. When calendar function (\code{calendar} argument) is not provided,
#' Monday-Friday are marked as working days. Current date is marked by square
#' brackets (\code{[]}).
#'
#' For months, \code{calendar} additionally prints information about observances
#' in a month provided that values in the list returned by the function
#' passed as \code{calendar} argument are named.
#'
#' @section Writing Custom Calendar Functions:
#' Calendar function should take a vector of dates as an argument and return
#' logical vector of the same length marking business days (as \code{TRUE})
#' or a list of two or three logical vectors of the same length with the first
#' marking business days (as \code{TRUE}), the second marking holidays (as \code{TRUE}),
#' and the third marking other observances / events (as \code{TRUE}).
#' The second and the third returned logical vectors can be named indicating
#' which observances are marked.
#'
#' The 4 basic functions to be used when writing calendars are: \code{\link{year}},
#' \code{\link{month}}, \code{\link{day}}, and \code{\link{day_of_week}}.
#' These can be used to mark fixed observances and weekends.
#'
#' \code{\link{nth_dw_in_month}} and \code{\link{last_dw_in_month}} can be used
#' to determine dates of movable observances falling on the nth or the last occurrence
#' of a day of week in a particular month.
#'
#' \code{\link{easter}} function can be used to determine date of Easter in
#' a year as well as of other movable observances with a fixed distance from
#' Easter.
#'
#' Two examples of calendar functions are provided below. These two functions
#' can be used as templates for developing custom calendar functions. In the
#' examples, one will also find a programming trick to easily name holidays
#' and other observances.
#'
#' \strong{A Note on the Design}
#'
#' One could argue that a design in which calendar functions should return
#' logical vectors or lists of logical vectors is not intuitive and \code{tind}
#' (vectors of dates) should be returned instead (thus making \code{eval_calendar}
#' function redundant). Firstly, logical vectors are easy and fast to work with.
#' By definition, a business day or a holiday must satisfy some conditions,
#' which leads to logical values. Secondly, in some applications (for example
#' counting business days) one would have to use matching to get integers
#' or logical values back from time indices.
#'
#' @note
#' \pkg{tind} package does not provide a calendar for any country, region,
#' or market. Instead, it gives users all the tools necessary to create
#' customised calendars. See Examples section below for real-life examples
#' of calendar functions, which could be used as templates.
#'
#' @param d an object of \code{tind} class or an R object coercible to it
#'          representing consecutive dates.
#' @param calendar a function determining working days and holidays (see Details),
#'                 or \code{NULL}.
#' @param ym an object of \code{tind} class or an R object coercible to it
#'           determining the year or month for which calendar is to be printed,
#'           current month (and preceding or following months) is the default.
#' @param name (optional) a character value (a short string) to be printed beside
#'             the year or month (or \code{NULL}).
#' @param locale (optional) a character value determining locale or \code{NULL}
#'               (the default, interpreted as the current system locale),
#'               see \link{calendar-names} for information on locale settings.
#'
#' @return
#' \code{calendar} returns invisible \code{NULL} and is used for its side effects.
#' \code{eval_calendar} returns a 3-element list of \code{tind} objects representing
#' business days (\code{$bizdays}), holidays (\code{$holidays}), and other
#' observances (\code{$otherobs}).
#'
#' @seealso
#' \link{time-index-components}, \link{calendrical-computations},
#' \link{Ops}, \code{\link{bizday}}.
#'
#' @name calendars
#'
#' @examples
#' # US (federal) calendar with holiday names
#' calendar_US <- function(dd)
#' {
#'     dd <- as.tind(dd)
#'     y <- year(dd)
#'     m <- month(dd)
#'     d <- day(dd)
#'     newyear <- (m == 1) & (d == 1)
#'     martinlking <- (y >= 2000) & (m == 1) & (dd == nth_dw_in_month(3, 1, dd))
#'     presidentsday <- (m == 2) & (dd == nth_dw_in_month(3, 1, dd))
#'     memorialday <- (m == 5) & (dd == last_dw_in_month(1, dd))
#'     juneteenth <- (y >= 2021) & (m == 6) & (d == 19)
#'     independence <- (m == 7) & (d == 4)
#'     labor <- (m == 9) & (dd == nth_dw_in_month(1, 1, dd))
#'     columbus <- (m == 10) & (dd == nth_dw_in_month(2, 1, dd))
#'     veterans <- (m == 11) & (d == 11)
#'     thanksgiving <- (m == 11) & (dd == nth_dw_in_month(4, 4, dd))
#'     christmas <- (m == 12) & (d == 25)
#'     holiday <- newyear | martinlking | presidentsday |
#'                memorialday | juneteenth | independence |
#'                labor | columbus | veterans | thanksgiving |
#'                christmas
#'     # holiday names - a programming trick
#'     # names of holnms should be the same as names of logical vectors above
#'     names(holiday) <- rep("", length(holiday))
#'     holnms <- c(newyear = "New Year's Day",
#'                 martinlking = "Birthday of Martin Luther King, Jr.",
#'                 presidentsday = "Washington's Birthday",
#'                 memorialday = "Memorial Day",
#'                 juneteenth = "Juneteenth National Independence Day",
#'                 independence = "Independence Day",
#'                 labor = "Labor Day",
#'                 columbus = "Columbus Day",
#'                 veterans = "Veterans Day",
#'                 thanksgiving = "Thanksgiving Day",
#'                 christmas = "Christmas Day")
#'     lapply(names(holnms), function(nm) names(holiday)[get(nm)] <<- holnms[nm])
#'     # business days
#'     business <- !holiday & (day_of_week(dd) %in% 1:5)
#'     return (list(bizdays = business, holiday = holiday))
#' }
#'
#' # Polish calendar from 1990 on with holiday names as well as other
#' # observances named
#' calendar_PL <- function(dd)
#' {
#'     dd <- as.tind(dd)
#'     y <- year(dd)
#'     m <- month(dd)
#'     d <- day(dd)
#'     # public holidays
#'     newyear <- (m == 1L) & (d == 1L)
#'     epiphany <- (y >= 2011L) & (m == 1L) & (d == 6L)
#'     easterd <- easter(dd) == dd
#'     eastermon <- easter(dd) + 1L == dd
#'     labour <- (m == 5L) & (d == 1L)
#'     constitution <- (m == 5L) & (d == 3L)
#'     pentecost <- easter(dd) + 49L == dd
#'     corpuschristi <- easter(dd) + 60L == dd
#'     assumption <- (m == 8L) & (d == 15L)
#'     allsaints <- (m == 11L) & (d == 1L)
#'     independence <- (m == 11L) & (d == 11L)
#'     christmaseve <- (m == 12L) & (d == 24L) & (y >= 2025)
#'     christmas <- (m == 12L) & (d == 25L)
#'     christmas2 <- (m == 12L) & (d == 26L)
#'     holiday <- newyear | epiphany |
#'                easterd | eastermon |
#'                labour | constitution |
#'                pentecost | corpuschristi |
#'                assumption |
#'                allsaints | independence |
#'                christmaseve | christmas | christmas2
#'     # holiday names
#'     names(holiday) <- rep("", length(holiday))
#'     holnms <- c(newyear = "New Year", epiphany = "Epiphany",
#'                 easterd = "Easter", eastermon = "Easter Monday",
#'                 labour = "Labour Day", constitution = "Constitution Day",
#'                 pentecost = "Pentecost", corpuschristi = "Corpus Christi",
#'                 assumption = "Assumption of Mary",
#'                 allsaints = "All Saints Day", independence = "Independence Day",
#'                 christmaseve = "Christmas Eve",
#'                 christmas = "Christmas", christmas2 = "Christmas (2nd day)")
#'     lapply(names(holnms), function(nm) names(holiday)[get(nm)] <<- holnms[nm])
#'     # working/business days
#'     work <- !holiday & (day_of_week(dd) <= 5L)
#'     # other observances
#'     fatthursday <- easter(dd) - 52L == dd
#'     shrovetuesday <- easter(dd) - 47L == dd
#'     ashwednesday <- easter(dd) - 46L == dd
#'     goodfriday <- easter(dd) - 2L == dd
#'     primaaprilis <- (m == 4L) & (d == 1L)
#'     flagday <- (m == 5L) & (d == 2L)
#'     mothersday <- (m == 5L) & (d == 26L)
#'     childrensday <- (m == 6L) & (d == 1L)
#'     saintjohnseve <- (m == 6L) & (d == 23L)
#'     allsoulsday <- (m == 11L) & (d == 2L)
#'     saintandrewseve <- (m == 11L) & (d == 29L)
#'     saintnicholasday <- (m == 12L) & (d == 6L)
#'     christmaseve <- (m == 12L) & (d == 24L) & (y < 2025)
#'     newyeareve <- (m == 12L) & (d == 31L)
#'     other <- fatthursday | shrovetuesday | ashwednesday |
#'              goodfriday |
#'              primaaprilis |
#'              flagday |
#'              mothersday | childrensday | saintjohnseve |
#'              allsoulsday |
#'              saintandrewseve |
#'              saintnicholasday | christmaseve |
#'              newyeareve
#'     names(other) <- rep("", length(other))
#'     othernms <- c(fatthursday = "Fat Thursday",
#'                   shrovetuesday = "Shrove Tuesday",
#'                   ashwednesday = "Ash Wednesday",
#'                   goodfriday = "Good Friday",
#'                   primaaprilis = "All Fool's Day",
#'                   flagday = "Flag Day",
#'                   mothersday = "Mother's Day",
#'                   childrensday = "Children's Day",
#'                   saintjohnseve = "Saint John's Eve",
#'                   allsoulsday = "All Souls' Day",
#'                   saintandrewseve = "Saint Andrew's Eve",
#'                   saintnicholasday = "Saint Nicholas Day",
#'                   christmaseve = "Christmas Eve",
#'                   newyeareve = "New Year's Eve")
#'     lapply(names(othernms), function(nm) names(other)[get(nm)] <<- othernms[nm])
#'
#'     return (list(work = work, holiday = holiday, other = other))
#' }
#'
#' # print the calendar for the current and the previous/next month and the current year
#' # (Mon-Fri marked as working days)
#' calendar()
#' calendar(as.year(today()))
#'
#' # print Polish and US calendars for 2020 and the current year
#' calendar(2020, calendar = calendar_PL)
#' calendar(2020, calendar = calendar_US)
#' calendar(as.year(today()), calendar = calendar_PL)
#' calendar(as.year(today()), calendar = calendar_US)
#'
#' # print Polish and US calendars for 2020-01 and the current and the previous/next month
#' calendar("2020-01", calendar = calendar_PL)
#' calendar("2020-01", calendar = calendar_US)
#' calendar(calendar = calendar_PL)
#' calendar(calendar = calendar_US)
#'
#' # get list of business days, holidays for 2020-01 and the current month
#' # using Polish and US calendars
#' d202001 <- seq(as.date("2020-01-01"), "2020-01-31")
#' dcurrmnth <- seq(floor_t(today(), "m"), last_day_in_month(today()))
#' eval_calendar(d202001, calendar_PL)
#' eval_calendar(d202001, calendar_US)
#' eval_calendar(dcurrmnth, calendar_PL)
#' eval_calendar(dcurrmnth, calendar_US)
#'
#' # print calendars with names
#' calendar(calendar = calendar_PL, name = "PL")
#' calendar(calendar = calendar_US, name = "US (federal)")
#'
#' # print Polish calendar using Polish locale
#' try(
#' calendar(calendar = calendar_PL, locale = "pl_PL.UTF-8")
#' )
#'
NULL


#' @rdname calendars
#' @export
calendar <- function(ym, calendar = NULL, name = NULL, locale = NULL)
{
    if (missing(ym)) {
        td <- today()
        ym <- as.month(td)
        ym <- ym + if (as.week(td) - as.week(as.date(as.month(td))) < 2L) -1L:0L
                   else 0L:1L
    } else {
        ym <- as.tind(ym)
        tp <- .get.type(ym)
        if (tp %in% c("d", "t")) { ym <- as.month(ym); tp <- "m" }
        if (!length(ym) || anyNA(ym) || !(tp %in% c("y", "m"))) {
            mes <- gettextf("invalid %s argument", sQuote("ym"))
            stop(mes, domain = NA)
        }
        ym <- sort(unique(ym))
    }

    for (i in seq_along(ym))
        .calendar.print(ym[[i]], calendar = calendar, name, locale)

    return (invisible(NULL))
}


# actual printing function
.calendar.print <- function(ym, calendar, name, locale)
{
    tp <- .get.type(ym)
    dd <- as.date(ym) + (seq_len(if (tp == "y") days_in_year(ym)
                                 else days_in_month(ym)) - 1L)
    clndr <- .eval_clndr(dd, calendar, FALSE, FALSE)

    # colours
    have_crayon <- requireNamespace("crayon", quietly = TRUE) && crayon::has_color()
    if (have_crayon) {
        .mark_tdbr <- function(x) crayon::bgRed(crayon::white(x))
        .mark_work <- function(x) crayon::bold(x)
        .mark_nwork <- function(x) crayon::blue(x)
        .mark_hol <- function(x) crayon::red(crayon::bold(x))
        .mark_other <- function(x) crayon::underline(x)
        .mark_wkd <- function(x) crayon::silver(x)
        .mark_ymon <- function(x) crayon::bold(x)
    } else .mark_tdbr <- .mark_work <- .mark_nwork <- .mark_hol <-
                         .mark_other <-.mark_wkd <- .mark_ymon <- function(x) x

    # titles
    .mk_title <- function(x, name)
    {
        if (is.null(name)) return (x)
        if (!is.character(name) || (length(name) != 1L)) {
            mes0 <- gettextf("invalid %s argument", sQuote("name"))
            mes1 <- gettextf("character string expected")
            stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
        }
        wd <- nchar(x) + 3L
        if (wd + nchar(name) > 21L) {
            tr <- 22L - wd
            name <- paste0(substr(name, 1L, tr - 3L), "...")
        }
        x <- paste0(x, " [", name, "]")

    }

    # mark nonbusiness days, holidays, and other observances / events
    dds <- as.character(day(dd))
    wrk <- clndr[[1L]]
    hol <- clndr[[2L]]
    other <- clndr[[3L]]
    dds[!wrk & !hol] <- .mark_nwork(dds[!wrk & !hol])
    dds[hol] <- .mark_hol(dds[hol])
    dds[other] <- .mark_other(dds[other])
    dds[day(dd) < 10L] <- paste0(" ", dds[day(dd) < 10L])
    # mark today
    td <- dd == today()
    ddst <- dds
    dds[td] <- paste0("[", ddst[td], "]")

    # create calendar
    if (tp == "m") { # month
        dms <- c(rep("  ", day_of_week(dd[1L]) - 1L), dds,
                 rep("  ", 7L - day_of_week(dd[length(dd)])))
        dms <- matrix(dms, ncol = 7L, byrow = TRUE)
        wk <- formatC(unique(week(dd)), width = 2L, flag = "0")
        wk <- .mark_wkd(wk)
        dms <- cbind(wk, dms)
        wd <- substr(weekday_names(locale = locale), 1L, 2L)
        wd <- .mark_wkd(wd)
        dms <- rbind(c("  ", wd), dms)
        dms <- do.call(paste, as.data.frame(dms))
        mn <- format(ym, "%b %Y", locale = locale)
        mn <- .mk_title(mn, name)
        mnc <- nchar(mn)
        mn <- c(rep(" ", max(0L, (26L - mnc) %/% 2L)), mn,
                rep(" ", max(0L, (21L - mnc) %/% 2L)))
        mn <- paste(mn, collapse = "")
        mn <- .mark_ymon(mn)
        dms <- c(mn, dms)
        # holidays and events / other observances
        if (!is.null(names(hol)) && !is.null(names(other))) {
            names(dd)[hol & other] <- paste0(names(hol)[hol & other], " / ",
                                             names(other)[hol & other])
            names(dd)[hol & !other] <- names(hol)[hol & !other]
            names(dd)[!hol & other] <- names(other)[!hol & other]
            ddho <- dd[hol | other]
        } else if (!is.null(names(hol))) {
            ddho <- dd[hol]
            names(ddho) <- names(hol)[hol]
        } else if (!is.null(names(other))) {
            ddho <- dd[other]
            names(ddho) <- names(other)[other]
        } else ddho <- dd[0L]
        if (length(ddho)) {
            ddhon <- paste0(ddst[dd %in% ddho], " - ", names(ddho))
            ww <- as.integer(as.week(ddho) - as.week(dd[1L])) + 1L
            ddhon <- tapply(ddhon, ww, toString)
            ddhon <- gsub(",  ", ", ",  ddhon, fixed = TRUE)
            ww <- as.integer(names(ddhon))
            info <- rep("", as.integer(as.week(dd[length(dd)]) - as.week(dd[1L])) + 1L)
            info[ww] <-  paste0("  | ", ddhon)
            dms <- paste0(dms, c("", "", info))
        }
        res <- dms
    } else { # year
        # loop over months
        mnsl <- list()
        for (m in 1L:12L) {
            im <- month(dd) == m
            dm <- dd[im]
            dms <- dds[im]
            dms <- c(rep("  ", day_of_week(dm[1L]) - 1L), dms,
                     rep("  ", 7L - day_of_week(dm[length(dm)])))
            dms <- c(dms, rep("  ", 42L - length(dms)))
            dms <- matrix(dms, ncol = 7L, byrow = TRUE)
            wk <- formatC(unique(week(dm)), width = 2L, flag = "0")
            wk <- .mark_wkd(wk)
            wk <- c(wk, rep("  ", 6L - length(wk)))
            dms <- cbind(wk, dms)
            wd <- substr(weekday_names(locale = locale), 1L, 2L)
            wd <- .mark_wkd(wd)
            dms <- rbind(c("  ", wd), dms)
            dms <- do.call(paste, as.data.frame(dms))
            mn <- month_names(abbreviate = TRUE, locale = locale)[m]
            mnc <- nchar(mn)
            mn <- .mark_ymon(mn)
            mn <- c(rep(" ", (26L - mnc) %/% 2L), mn, rep(" ", (21L - mnc) %/% 2L))
            mn <- paste(mn, collapse = "")
            dms <- c(mn, dms)
            mnsl[[m]] <- dms
        }
        # check terminal width and put everything together
        nc <- (getOption("width", 80L) + 1L) %/% 26L
        nc <- c(1L:4L, 6L, 12L)[.match_left(nc, c(1L:4L, 6L, 12L))]
        nr <- 12L %/% nc
        for (r in 1L:nr) {
            ri <- nc * (r - 1L) + (1L:nc)
            nl <- max(sapply(mnsl[ri], function(x) sum(grepl("[^ ]", x))))
            mnsl[ri] <- lapply(mnsl[ri], function(x) x[1L:nl])
        }
        ci <- 1L + ((1L:nr) - 1L) * nc
        res <- do.call(c, mnsl[ci])
        if (nc > 1L) {
            sep <- rep("   ", length(res))
            for (cl in 2L:nc) {
                ci <- ci + 1L
                res <- paste0(res, sep, do.call(c, mnsl[ci]))
            }
        }
        # add year info
        year <- as.character(ym)
        year <- .mk_title(year, name)
        wd <- nchar(year)
        year <- .mark_ymon(year)
        year <- paste(c(rep(" ", (nc * 26L - wd - 2L) %/% 2L), year,
                        rep(" ", (nc * 26L - wd - 3L) %/% 2L)), collapse = "")
        res <- c(year, res)
    }
    # rm redundant space around '[]' marking today and colour '[]'
    res[-1L] <- sub("\\]( |$)", .mark_tdbr("]"),
                    sub(" \\[", .mark_tdbr("["), res[-1L]))
    # finalise
    res <- paste0(paste0("  ", res, "\n"), collapse = "")
    cat(res)
}


#' @rdname calendars
#' @export
eval_calendar <- function(d, calendar)
{
    d <- as.tind(d)
    .expect_type(.get.type(d), "d")
    if (anyNA(d) || length(d) > 2L && !isTRUE(all(diff(d) == 1L))) {
        mes0 <- gettextf("invalid %s argument", sQuote("d"))
        mes1 <- gettextf("expected a sequence of consecutive dates")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    bho <- .eval_clndr(d, calendar)
    bd <- d[bho[[1L]]]
    hd <- d[bho[[2L]]]
    names(hd) <- names(bho[[2L]])[bho[[2L]]]
    ot <- d[bho[[3L]]]
    names(ot) <- names(bho[[3L]])[bho[[3L]]]
    return (list(bizdays = bd, holidays = hd, otherobs = ot))
}


# internal calendar evaluation
.eval_clndr <- function(d, calendar, warn.null = FALSE, bd.only = FALSE)
{
    if (is.null(calendar)) {
        if (warn.null) {
            mes0 <- gettextf("%s argument missing", sQuote("calendar"))
            mes1 <- gettextf("using default settings")
            mes2 <- paste0("(", paste0(.calendar_names(NULL)[c(32L, 36L)],
                                       collapse = "-"), ")")
            mes <- paste0(mes0, "; ", mes1, " ", mes2)
            warning(mes, call. = FALSE, domain = NA)
        }
        calendar <- function(d) (day_of_week(d) <= 5L)
    } else if (!is.function(calendar)) {
        mes0 <- gettextf("invalid %s argument", sQuote("calendar"))
        mes1 <- gettextf("expected a function")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    nad <- is.na(d)
    clndr <- tryCatch(calendar(d), error = function(e) e)
    if (inherits(clndr, "error")) {
        mes <- gettextf("error while evaluating function given as %s: %s",
                        sQuote("calendar"), clndr$message)
        stop(mes, call. = FALSE, domain = NA)
    }

    if (is.list(clndr)) {
        ok <- (length(clndr) %in% 2L:3L) &&
              all(sapply(clndr, is.logical)) &&
              all(sapply(clndr, length) == length(d)) &&
              all(sapply(clndr, function(x) !any(xor(nad, is.na(x)))))
    } else if (is.logical(clndr)) {
        ok <- (length(clndr) == length(d)) && !any(xor(nad, is.na(clndr)))
    } else ok <- FALSE
    if (!ok) {
        mes <- gettextf("invalid return from function given as %s", sQuote("calendar"))
        stop(mes, call. = FALSE, domain = NA)
    }

    if (bd.only) {
        if (is.list(clndr)) clndr <- clndr[[1L]]
        names(clndr) <- NULL
        return (clndr)
    }

    if (!is.list(clndr)) {
        clndr <- list(clndr, rep(FALSE, length(d)), rep(FALSE, length(d)))
    } else if (length(clndr) == 2L) {
        clndr <- c(clndr, list(rep(FALSE, length(d))))
    }

    return (clndr)
}

