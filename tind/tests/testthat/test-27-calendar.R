context("calendrical computations")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test sample size
NN <- 100L

# test samples
y <- sample(1990L:2020L, NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
d <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(.validate_ym(y, m)))
tt <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 1)

dd <- suppressWarnings(tind(y = y, m = m, d = d))
DD <- as.Date(dd)
yy <- as.year(dd)
qq <- as.quarter(dd)
mm <- as.month(dd)
ww <- as.week(dd)
tz <- sample(tzs, 1L)
tt <- as.date_time(tt, tz = tz)
hh <- as.time(tt)

ii <- as.tind(as.integer(runif(NN, )), type = "i")
nn <- as.tind(runif(NN, -3e7, 3e7), type = "n")



# error message - invalid casts
errcast <- paste0("^cast from time index type ", dQuote("[a-z]"),
                  " \\([- a-z]+\\) to type ", dQuote("[a-z]"), " \\([- a-z]+\\) in ",
                  sQuote("[\\._a-z]+"), " not possible$")
# error message - invalid types
errtype <- paste0("^invalid time index type in ", sQuote("[_a-z]+") ,": ", dQuote("[a-z]"),
                  " \\([- a-z]+\\); expected one of the following: ",
                  dQuote("[a-z]"), " \\([- a-z]+\\)(, ", dQuote("[a-z]"),
                  " \\([- a-z]+\\))+$")
# warning NAs introduced
warnna <- paste0("^NAs introduced; first occurrence: ([a-z]+\\[[0-9]+\\] = [-0-9]+)",
                 "(, [a-z]+\\[[0-9]+\\] = [-0-9]+)+$")


test_that("'today' and 'now' work correctly", {
    nw <- now()
    td <- today()
    expect_true(is.tind(nw))
    expect_equal(.get.type(nw), "t")
    expect_equal(length(nw), 1L)
    expect_equal(.get.tz(nw), Sys.timezone())
    expect_true(is.tind(td))
    expect_equal(.get.type(td), "d")
    expect_equal(length(td), 1L)
    for (tz in tzs) {
        td <- today(tz)
        nw <- now(tz)
        expect_true(is.tind(nw))
        expect_equal(.get.type(nw), "t")
        expect_equal(length(nw), 1L)
        expect_equal(.get.tz(nw), tz)
        expect_true(is.tind(td))
        expect_equal(.get.type(td), "d")
        expect_equal(length(td), 1L)
    }
})


test_that("'month_names', 'weekday_names', and 'ampm_indicators' work correctly", {
    expect_silent(mn <- month_names())
    expect_true(is.character(mn))
    expect_equal(length(mn), 12L)
    expect_silent(mn <- month_names(abbreviate = FALSE))
    expect_true(is.character(mn))
    expect_equal(length(mn), 12L)
    expect_silent(wn <- weekday_names())
    expect_true(is.character(wn))
    expect_equal(length(wn), 7L)
    expect_silent(wn <- weekday_names(abbreviate = FALSE))
    expect_true(is.character(wn))
    expect_equal(length(wn), 7L)
    ap <- suppressWarnings(ampm_indicators())
    expect_true(is.character(ap))
    expect_equal(length(ap), 2L)

    skip_on_cran()

    cl <- Sys.getlocale("LC_TIME")
    if (is.null(cl)) skip("locale information not available")

    en_locales <- c("C", "en_US", "en_GB", "English_United States",
                    "English_United Kingdom")
    for (lc in en_locales) {
        suppressWarnings(en_locale <- Sys.setlocale("LC_TIME", lc))
        if (en_locale != "") break
    }
    Sys.setlocale("LC_TIME", cl)

    if (en_locale != "") {
        mn_ena <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                    "Oct", "Nov", "Dec")
        mn_enf <- c("January", "February", "March", "April", "May", "June", "July",
                    "August", "September", "October", "November", "December")
        wn_ena <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        wn_enf <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                    "Saturday",  "Sunday")
        ap_en <- c("am", "pm")
        expect_silent(mn <- month_names(locale = en_locale))
        expect_equal(mn, mn_ena)
        expect_silent(mn <- month_names(locale = en_locale, abbreviate = FALSE))
        expect_equal(mn, mn_enf)
        expect_silent(wn <- weekday_names(locale = en_locale))
        expect_equal(wn, wn_ena)
        expect_silent(wn <- weekday_names(locale = en_locale, abbreviate = FALSE))
        expect_equal(wn, wn_enf)
        expect_equal(wn, wn_enf)
        expect_silent(ap <- suppressWarnings(ampm_indicators(locale = en_locale)))
        expect_equal(tolower(ap), ap_en)
    }

    pl_locales <- c("pl", "pl_PL", "Polish", "Polish_Poland")
    for (lc in pl_locales) {
        suppressWarnings(pl_locale <- Sys.setlocale("LC_TIME", lc))
        if (pl_locale != "") break
    }
    Sys.setlocale("LC_TIME", cl)

    if (pl_locale != "") {
        mn_pla <- c("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz",
                    "paź", "lis", "gru")
        mn_plf <- c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec",
                    "lipiec", "sierpień", "wrzesień", "październik", "listopad",
                    "grudzień")
        wn_pla <- c("pon", "wto", "śro", "czw", "pią", "sob", "nie")
        wn_plf <- c("poniedziałek", "wtorek", "środa", "czwartek", "piątek",
                    "sobota", "niedziela")

        expect_silent(mn <- month_names(locale = pl_locale))
        # limit check to names with ASCII-only characters
        allascii <- c(1L:9L, 11:12L)
        expect_equal(mn[allascii], mn_pla[allascii])
        expect_silent(mn <- month_names(locale = pl_locale, abbreviate = FALSE))
        allascii <- c(2L:3L, 5L:7L, 11L)
        # subtr to handle different gramatical forms
        expect_equal(substr(mn[allascii], 0L, 3L),
                         substr(mn_plf[allascii], 0L, 3L))
        expect_silent(wn <- weekday_names(locale = pl_locale))
        allascii <- c(1L:2L, 4L, 6L:7L)
        expect_equal(wn[allascii], wn_pla[allascii])
        expect_silent(wn <- weekday_names(locale = pl_locale, abbreviate = FALSE))
        allascii <- c(2L, 4L, 6L:7L)
        expect_equal(wn[allascii], wn_plf[allascii])
        warn <- "AM/PM indicators not available in the selected / current locale"
        expect_warning(ap <- ampm_indicators(locale = pl_locale), warn, fixed = TRUE)
        expect_true(is.character(ap) && (length(ap) == 2L) && all(is.na(ap)))
    }
})


test_that("'year', 'quarter', 'month', 'week', 'day', 'day_of_week', and 'day_of_year' work correctly", {
    expect_equal(year(dd), as.integer(format(DD, "%Y")))
    expect_equal(year(tind(type = "t")), integer())
    expect_equal(year(tind(type = "d")), integer())
    expect_equal(year(as.tind(NA_integer_, type = "d")), NA_integer_)
    expect_equal(year(ww), as.integer(format(DD, "%G")))
    expect_equal(year(tind(type = "w")), integer())
    expect_equal(year(mm), as.integer(format(DD, "%Y")))
    expect_equal(year(tind(type = "m")), integer())
    expect_equal(year(qq), as.integer(format(DD, "%Y")))
    expect_equal(year(tind(type = "q")), integer())
    expect_equal(year(yy), as.integer(format(DD, "%Y")))
    expect_equal(year(tind(type = "y")), integer())
    expect_error(year(tind(type = "n")), errcast)
    expect_error(year(tind(type = "h")), errcast)

    expect_equal(quarter(dd), 1L + (as.integer(format(DD, "%m")) - 1L) %/% 3L)
    expect_equal(quarter(tind(type = "t")), integer())
    expect_equal(quarter(tind(type = "d")), integer())
    expect_equal(quarter(as.tind(NA_integer_, type = "d")), NA_integer_)
    expect_error(quarter(ww), errcast)
    expect_equal(quarter(mm), 1L + (as.integer(format(DD, "%m")) - 1L) %/% 3L)
    expect_equal(quarter(tind(type = "m")), integer())
    expect_equal(quarter(qq), 1L + (as.integer(format(DD, "%m")) - 1L) %/% 3L)
    expect_equal(quarter(tind(type = "q")), integer())
    expect_error(quarter(yy), errcast)
    expect_error(quarter(tind(type = "n")), errcast)
    expect_error(quarter(tind(type = "h")), errcast)

    expect_equal(month(dd), as.integer(format(DD, "%m")))
    expect_equal(month(tind(type = "t")), integer())
    expect_equal(month(tind(type = "d")), integer())
    expect_equal(month(as.tind(NA_integer_, type = "d")), NA_integer_)
    expect_equal(month(mm), as.integer(format(DD, "%m")))
    expect_equal(as.character(month(mm, labels = TRUE, abbreviate = TRUE)),
                     format(DD, "%b"))
    expect_equal(as.character(month(mm, labels = TRUE, abbreviate = FALSE)),
                     format(DD, "%B"))
    expect_equal(month(tind(type = "m")), integer())
    expect_error(month(ww), errcast)
    expect_error(month(qq), errcast)
    expect_error(month(yy), errcast)
    expect_error(month(tind(type = "n")), errcast)
    expect_error(month(tind(type = "h")), errcast)

    expect_equal(week(dd), as.integer(format(DD, "%V")))
    expect_equal(week(tind(type = "t")), integer())
    expect_equal(week(tind(type = "d")), integer())
    expect_equal(week(as.tind(NA_integer_, type = "d")), NA_integer_)
    expect_equal(week(ww), as.integer(format(DD, "%V")))
    expect_equal(week(tind(type = "w")), integer())
    expect_error(week(mm), errcast)
    expect_error(week(qq), errcast)
    expect_error(week(yy), errcast)
    expect_error(week(tind(type = "n")), errcast)
    expect_error(week(tind(type = "h")), errcast)

    expect_equal(day(dd), as.integer(format(DD, "%d")))
    expect_equal(day(tind(type = "t")), integer())
    expect_equal(day(tind(type = "d")), integer())
    expect_equal(day(as.tind(NA_integer_, type = "d")), NA_integer_)
    expect_error(day(ww), errcast)
    expect_error(day(mm), errcast)
    expect_error(day(qq), errcast)
    expect_error(day(yy), errcast)
    expect_error(day(tind(type = "n")), errcast)
    expect_error(day(tind(type = "h")), errcast)

    expect_equal(day_of_week(dd), as.integer(format(DD, "%u")))
    expect_equal(as.character(day_of_week(dd, labels = TRUE, abbreviate = TRUE)),
                     format(DD, "%a"))
    expect_equal(as.character(day_of_week(dd, labels = TRUE, abbreviate = FALSE)),
                     format(DD, "%A"))
    expect_equal(day_of_week(tind(type = "t")), integer())
    expect_equal(day_of_week(tind(type = "d")), integer())
    expect_equal(day_of_week(as.tind(NA_integer_, type = "d")), NA_integer_)
    expect_error(day_of_week(ww), errcast)
    expect_error(day_of_week(mm), errcast)
    expect_error(day_of_week(qq), errcast)
    expect_error(day_of_week(yy), errcast)
    expect_error(day_of_week(tind(type = "n")), errcast)
    expect_error(day_of_week(tind(type = "h")), errcast)

    expect_equal(day_of_year(dd), as.integer(format(DD, "%j")))
    expect_equal(day_of_year(tind(type = "t")), integer())
    expect_equal(day_of_year(tind(type = "d")), integer())
    expect_equal(day_of_year(as.tind(NA_integer_, type = "d")), NA_integer_)
    expect_error(day_of_year(ww), errcast)
    expect_error(day_of_year(mm), errcast)
    expect_error(day_of_year(qq), errcast)
    expect_error(day_of_year(yy), errcast)
    expect_error(day_of_year(tind(type = "n")), errcast)
    expect_error(day_of_year(tind(type = "h")), errcast)

    for (tz in tzs) {
        plt <- as.POSIXlt(tt, origin = "1970-01-01 00:00:00", tz = tz)
        tit <- as.tind(tt, tz = tz)
        expect_equal(year(tit), as.integer(format(plt, "%Y")))
        expect_equal(quarter(tit), 1L + (as.integer(format(plt, "%m")) - 1L) %/% 3L)
        expect_equal(month(tit), as.integer(format(plt, "%m")))
        expect_equal(as.character(month(tit, labels = TRUE, abbreviate = TRUE)),
                         format(plt, "%b"))
        expect_equal(as.character(month(tit, labels = TRUE, abbreviate = FALSE)),
                         format(plt, "%B"))
        expect_equal(week(tit), as.integer(format(plt, "%V")))
        expect_equal(day(tit), as.integer(format(plt, "%d")))
        expect_equal(day_of_week(tit), as.integer(format(plt, "%u")))
        expect_equal(as.character(day_of_week(tit, labels = TRUE, abbreviate = TRUE)),
                         format(plt, "%a"))
        expect_equal(as.character(day_of_week(tit, labels = TRUE, abbreviate = FALSE)),
                         format(plt, "%A"))
        expect_equal(day_of_year(tit), as.integer(format(plt, "%j")))
    }
})


test_that("methods 'weekdays', 'months', 'quarters' from base work correctly", {
    DD[1L] <- NA
    dd[1L] <- NA
    expect_equal(weekdays(dd), weekdays(DD))
    expect_equal(weekdays(dd, TRUE), weekdays(DD, TRUE))
    expect_equal(weekdays(dd, FALSE), weekdays(DD, FALSE))
    expect_equal(months(dd), months(DD))
    expect_equal(months(dd, TRUE), months(DD, TRUE))
    expect_equal(months(dd, FALSE), months(DD, FALSE))
    expect_equal(quarters(dd), quarters(DD))
})


test_that("'hour', 'am', 'pm', 'minute', and 'second' work correctly", {
    for (tp in c("d", "w", "m", "q", "y", "n")) {
        expect_error(hour(tind(type = tp)), errtype)
        expect_error(minute(tind(type = tp)), errtype)
        expect_error(second(tind(type = tp)), errtype)
    }

    for (tz in tzs) {
        tit <- as.tind(tt, tz = tz)
        plt <- as.POSIXlt(tit)
        expect_true(is.integer(hour(tit)))
        expect_equal(hour(tit), plt$hour)
        expect_equal(hour(tit), hour(plt))
        expect_true(is.logical(am(tit)))
        expect_true(is.logical(pm(tit)))
        expect_equal(am(tit), plt$hour < 12)
        expect_equal(pm(tit), plt$hour >= 12)
        expect_equal(am(tit), !pm(tit))
        expect_true(is.integer(minute(tit)))
        expect_equal(minute(tit), plt$min)
        expect_equal(minute(tit), minute(plt))
        expect_true(is.double(second(tit)))
        expect_equal(second(tit), round(plt$sec, 6L))
        expect_equal(second(tit), second(plt))
        expect_equal(hour(tind(type = "t", tz = tz)), integer())
        expect_equal(minute(tind(type = "t", tz = tz)), integer())
        expect_equal(second(tind(type = "t", tz = tz)), numeric())
        expect_equal(am(tind(type = "t", tz = tz)), logical())
        expect_equal(pm(tind(type = "t", tz = tz)), logical())

        tih <- as.time(tit)
        expect_equal(hour(tih), as.integer(plt$hour))
        expect_equal(am(tih), plt$hour < 12)
        expect_equal(pm(tih), plt$hour >= 12)
        expect_equal(am(tih), !pm(tih))
        expect_equal(minute(tih), as.integer(plt$min))
        expect_equal(second(tih), plt$sec)
        expect_equal(hour(tind(type = "h")), integer())
        expect_equal(minute(tind(type = "h")), integer())
        expect_equal(second(tind(type = "h")), numeric())
    }
})


test_that("'is.leap_year' and 'days_in_year' work correctly", {
    yy <- c(1900L, 2000L:2019)
    dy <- c(365L, rep(c(366L, 365L, 365L, 365L), 5L))
    ly <- dy == 366L
    expect_equal(is.leap_year(yy), ly)
    expect_equal(days_in_year(yy), dy)

    expect_error(is.leap_year(tind(type = "n")), errcast)
    expect_error(days_in_year(tind(type = "n")), errcast)
})


test_that("'weeks_in_year' works correctly", {
    yy <- c(1900L, 2000L:2019)
    wy <- rep(52L, 21L)
    names(wy) <- yy
    wy[c("2004", "2009", "2015")] <- 53L
    names(wy) <- NULL

    expect_equal(weeks_in_year(as.tind(yy, type = "y")), wy)

    expect_error(weeks_in_year(tind(type = "n")), errcast)
})


test_that("'days_in_quarter' and 'days_in_month' work correctly", {
    ymd1 <- tind(y = 2000, m = (1L:12L), d = sample.int(28L, size = 12L, replace = TRUE))
    ymd2 <- tind(y = 2019, m = (1L:12L), d = sample.int(28L, size = 12L, replace = TRUE))
    ymd3 <- tind(y = 1900, m = (1L:12L), d = sample.int(28L, size = 12L, replace = TRUE))
    dm1 <- c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    dm2 <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    dm3 <- dm2
    dq1 <- as.integer(kronecker(tapply(dm1, kronecker(1:4, rep(1, 3)), sum), rep(1, 3)))
    dq2 <- as.integer(kronecker(tapply(dm2, kronecker(1:4, rep(1, 3)), sum), rep(1, 3)))
    dq3 <- dq2

    expect_equal(days_in_quarter(as.tind(ymd1, type = "q")), dq1)
    expect_equal(days_in_quarter(as.tind(ymd1, type = "d")), dq1)
    expect_equal(days_in_quarter(as.tind(ymd2, type = "q")), dq2)
    expect_equal(days_in_quarter(as.tind(ymd2, type = "d")), dq2)
    expect_equal(days_in_quarter(as.tind(ymd3, type = "q")), dq3)
    expect_equal(days_in_quarter(as.tind(ymd3, type = "d")), dq3)

    expect_equal(days_in_month(as.tind(ymd1, type = "m")), dm1)
    expect_equal(days_in_month(ymd1), dm1)
    expect_equal(days_in_month(as.tind(ymd2, type = "m")), dm2)
    expect_equal(days_in_month(ymd2), dm2)
    expect_equal(days_in_month(as.tind(ymd3, type = "m")), dm3)
    expect_equal(days_in_month(ymd3), dm3)

    for (tp in c("y", "q", "w", "i", "n")) {
        expect_error(days_in_month(tind(type = tp)), errcast)
    }
    for (tp in c("y", "w", "i", "n")) {
        expect_error(days_in_quarter(tind(type = tp)), errcast)
    }
})


test_that("'hours_in_day' and 'is.dst' work correctly", {
    dd <- as.tind(c(20190310, 20190331, 20191027, 20191103))
    expect_equal(hours_in_day(dd, "UTC"), rep(24., 4))

    tt <- rep(date_time(dd, tz = "UTC"), each = 25L) + c(0:23, NA) * 3600
    expect_equal(is.dst(tt), rep(c(rep(FALSE, 24L), NA), 4L))
    expect_equal(is.dst(tt), is.dst(as.POSIXlt(tt)))

    if ((tz <- "Europe/Warsaw") %in% OlsonNames()) {
        hd <- c(24., 23, 25, 24)
        expect_equal(hours_in_day(dd, tz), hd)
        expect_equal(hours_in_day(as.tind(dd, tz = tz)), hd)
        expect_equal(hours_in_day(as.tind(dd, tz = tz), tz), hd)
        tt <- rep(date_time(dd, tz = tz), each = 25L) + c(0:23, NA) * 3600
        expect_equal(is.dst(tt), c(rep(FALSE, 24L), NA,
                                       rep(FALSE, 2L), rep(TRUE, 22L), NA,
                                       rep(TRUE, 3L), rep(FALSE, 21L), NA,
                                       rep(FALSE, 24L), NA))
        expect_equal(is.dst(tt), is.dst(as.POSIXlt(tt)))
    }
    if ((tz <- "America/New_York") %in% OlsonNames()) {
        hd <- c(23., 24, 24, 25)
        expect_equal(hours_in_day(dd, tz), hd)
        expect_equal(hours_in_day(as.tind(dd, tz = tz)), hd)
        expect_equal(hours_in_day(as.tind(dd, tz = tz), tz), hd)
        tt <- rep(date_time(dd, tz = tz), each = 25L) + c(0:23, NA) * 3600
        expect_equal(is.dst(tt), is.dst(as.POSIXlt(tt)))
    }

    expect_error(hours_in_day(tind(type = "y")), errtype)
    expect_error(hours_in_day(tind(type = "q")), errtype)
    expect_error(hours_in_day(tind(type = "w")), errtype)
    expect_error(hours_in_day(tind(type = "n")), errtype)
})


test_that("'nth_day_of_year' works correctly", {
    expect_error(nth_day_of_year(1, tind(type = "n")), errcast)
    expect_equal(nth_day_of_year(366, 2020), as.tind(20201231))
    expect_warning(nth_day_of_year(366, 2019), warnna)
    expect_warning(nth_day_of_year(0, 2019), warnna)
})


test_that("'last_day_in_month' and 'last_day_in_quarter' work correctly", {
    expect_error(last_day_in_month(tind(type = "y")), errcast)
    expect_error(last_day_in_month(tind(type = "q")), errcast)
    expect_error(last_day_in_month(tind(type = "n")), errcast)

    expect_equal(last_day_in_month(201902), as.tind(20190228))
    expect_equal(last_day_in_month(202002), as.tind(20200229))
    expect_equal(last_day_in_month(202003), as.tind(20200331))

    expect_error(last_day_in_quarter(tind(type = "y")), errcast)
    expect_error(last_day_in_quarter(tind(type = "n")), errcast)

    mmti <- as.month(mm)
    ddti <- as.date(dd)
    expect_equal(last_day_in_quarter(mmti),
                     last_day_in_month(as.month(as.quarter(mmti) + 1) - 1))
    expect_equal(last_day_in_quarter(mmti), last_day_in_quarter(ddti))
})


test_that("'nth_dw_in_month' works correctly", {
    expect_error(nth_dw_in_month(1, 1, tind(type = "y")), errcast)
    expect_error(nth_dw_in_month(1, 1, tind(type = "q")), errcast)
    expect_error(nth_dw_in_month(1, 1, tind(type = "n")), errcast)

    expect_equal(nth_dw_in_month(1, c(5, 6, 7), 202005), as.tind(20200501 + 0:2))
    expect_equal(nth_dw_in_month(2, 1:4, 202005), as.tind(20200511 + 0:3))
    expect_warning(nth_dw_in_month(1, 8, 202005), warnna)
    expect_warning(nth_dw_in_month(1, 0, 202005), warnna)
    expect_warning(nth_dw_in_month(0, 1, 202005), warnna)
    expect_warning(nth_dw_in_month(5, 1, 202005), warnna)
})


test_that("'last_dw_in_month' works correctly", {
    expect_error(last_dw_in_month(1, tind(type = "y")), errcast)
    expect_error(last_dw_in_month(1, tind(type = "q")), errcast)
    expect_error(last_dw_in_month(1, tind(type = "n")), errcast)

    expect_equal(last_dw_in_month(1:7, 202005), as.tind(20200524 + 1:7))
    expect_warning(last_dw_in_month(8, 202005), warnna)
    expect_warning(last_dw_in_month(0, 202005), warnna)
})


test_that("'easter' works correctly", {
    expect_error(easter(tind(type = "n")), errcast)
    eas <- c(20180401L, 20190421L, 20200412L, 20210404L, 20220417L, 20230409L)
    expect_equal(easter(eas %/% 10000L), as.tind(eas))
})


test_that("'date_time' and 'date_time_split' work correctly", {
    if ((tz <- "Europe/Warsaw") %in% tzs) {
        wrn24 <- paste0("NAs introduced; first occurrence: d[1] = 2021-03-27, H[1] = 24, M[1] = 30, S[1] = 0")
        expect_warning(date_time(20210327, c(24, 1:3), 30, 0, tz, FALSE),
                       wrn24, fixed = TRUE)
        wrn2 <- paste0("NAs introduced; first occurrence: d[1] = 2021-03-28, H[2] = 2, M[1] = 30; time zone: Europe/Warsaw")
        expect_warning(date_time(20210328, c(1:3), 30, tz = tz),
                       wrn2, fixed = TRUE)
    }

    y <- 2020
    mon <- c(11, 3)
    d <- c(1, 29)
    ymdc <- c("2020-11-01", "2020-03-29")
    ymd <- as.tind(ymdc)
    h <- 20
    m <- 4
    s <- c(44, 19.66)
    hmsc <- c("20:04:44", "20:04:19.66")
    hms <- as.tind(hmsc)

    for (tz in tzs) {
        dt0 <- date_time(ymd, hms, tz = tz)
        dt1 <- date_time(ymdc, hmsc, tz = tz)
        dt2 <- date_time(ymd, h, m, s, tz = tz)
        dt3 <- date_time(ymdc, h, m, s, tz = tz)
        expect_true(identical(dt0, dt1) && identical(dt1, dt2) && identical(dt2, dt3))
        expect_true(all(year(dt0) == y) && all(month(dt0) == mon) && all(day(dt0) == d) &&
                    all(hour(dt0) == h) && all(minute(dt0) == m) && all(second(dt0) == s))
        expect_equal(date_time(ymd, h, tz = tz) + 60 * m + s,
                     date_time(ymd, h, m, s, tz = tz))
        expect_equal(date_time(ymd, h, m, tz = tz) + s,
                     date_time(ymd, h, m, s, tz = tz))
        expect_equal(date_time(ymd, tz = tz), as.date_time(as.date(ymd), tz = tz))
        dtsplt <- date_time_split(dt0)
        expect_equal(dtsplt$date, ymd)
        expect_equal(dtsplt$time, hms)
    }

    errmish <- paste0(sQuote("M"), " or ", sQuote("S"),
                      " argument provided without ", sQuote("H"))
    errmism <- paste0(sQuote("S"), " argument provided without ", sQuote("M"))
    expect_error(date_time(ymd, M = m, S = s), errmish, fixed = TRUE)
    expect_error(date_time(ymd, M = m), errmish, fixed = TRUE)
    expect_error(date_time(ymd, S = s), errmish, fixed = TRUE)
    expect_error(date_time(ymd, H = h, S = s), errmism, fixed = TRUE)
    errnonnumh <- paste0(sQuote("M"), " or ", sQuote("S"),
                         " argument provided with nonnumeric ", sQuote("H"))
    expect_error(date_time(ymd, H = hms, M = m, S = s), errnonnumh, fixed = TRUE)
    expect_error(date_time(ymd, H = hms, M = m), errnonnumh, fixed = TRUE)
    expect_error(date_time(ymd, H = hms, S = s), errnonnumh, fixed = TRUE)

    warn <- "longer object length is not a multiple of shorter object length"
    ymd <- tind(y = 2023, m = 3, d = 26) + -3:1
    h <- tind(H = 0:23)

    tz <- "UTC"
    expect_warning(date_time(ymd, h, tz = tz), warn, fixed = TRUE)
    expect_warning(date_time(ymd, h, tz = tz, grid = FALSE), warn, fixed = TRUE)
    expect_warning(date_time(ymd[1L:3L], h[1L:2L], tz = tz), warn, fixed = TRUE)
    expect_silent(dt <- date_time(ymd, h, tz = tz, grid = TRUE))
    expect_equal(diff(dt), as.tdiff(rep(1, 119), "h"))
    if ((tz <- "Europe/Warsaw") %in% tzs) {
        expect_warning(date_time(ymd, h, tz = tz), warn, fixed = TRUE)
        expect_warning(date_time(ymd, h, tz = tz, grid = FALSE), warn, fixed = TRUE)
        warn <- "NAs introduced; first occurrence: d[4] = 2023-03-26, H[3] = 02:00; time zone: Europe/Warsaw"
        expect_warning(date_time(ymd, h, tz = tz, grid = TRUE), warn, fixed = TRUE)
    }

    for (tz in tzs)
        expect_equal(date_time(ymd, H = 24, tz = tz), date_time(ymd + 1, H = 0, tz = tz))
})


test_that("'year_frac' and 'yf2tind' work correctly", {
    types <- .ti_type(long = FALSE)

    for (tp in types) {
        x <- get(paste0(tp, tp))
        if (tp %in% c("h", "i", "n")) {
            expect_error(year_frac(x), errtype)
        } else {
            x[sample.int(NN, NN %/% 10L)] <- NA
            yf <- year_frac(x)
            expect_true(is.numeric(yf))
            expect_equal(floor(yf), as.numeric(as.tind(x, type = "y")))
            expect_equal(year_frac(tind(type = tp)), numeric())
            expect_equal(year_frac(tind(length = NN, type = tp)), rep(NA_real_, NN))
            if (tp != "t") {
                expect_equal(yf2tind(yf, tp), x)
            } else {
                expect_equal(yf2tind(yf, tp, tz), x)
            }
            yf[] <- NA
            expect_equal(yf2tind(yf, tp), tind(length = NN, type = tp))
        }
    }

    yy[1] <- NA
    expect_equal(year_frac(yy), as.numeric(yy))
    # errors
    errtype <- paste0("^invalid ", sQuote("type"), " argument; expected one of the following: ",
                  dQuote("[a-z]"), " \\([- a-z]+\\)(, ", dQuote("[a-z]"),
                  " \\([- a-z]+\\))+$")
    for (tp in c("h", "i", "n")) expect_error(yf2tind(numeric(), tp), errtype)
    expect_error(yf2tind(numeric()), "type not provided", fixed = TRUE)
    errx <- paste0("invalid ", sQuote("x"), " argument; expected a numeric vector")
    expect_error(yf2tind(dd, tp), errx, fixed = TRUE)
})


test_that("'jdn' and 'jdn2tind' work correctly", {
    expect_error(jdn(tind(type = "y")), errtype)
    expect_error(jdn(tind(type = "q")), errtype)
    expect_error(jdn(tind(type = "w")), errtype)
    expect_error(jdn(tind(type = "n")), errtype)

    expect_equal(jdn(as.tind("2000-01-01")), 2451545L)
    expect_equal(jdn(as.tind("2013-01-01 00:30Z")), 2456293.520833)
    expect_equal(jdn2tind(c(2451545L, NA)), as.tind(c("2000-01-01", NA)))
    expect_equal(jdn2tind(c(2451545., NA)), as.tind(c("2000-01-01", NA)))
    expect_equal(jdn2tind(2456293.520833333, "UTC"),
                 as.tind("2013-01-01 00:30Z", tz = "UTC"))
    expect_equal(jdn2tind(2456293.520833333), as.tind("2013-01-01 00:30Z"))

    err <- paste0("invalid ", sQuote("x"), " argument; expected a numeric vector")
    expect_error(jdn2tind(tind(type = "d")), err)
    expect_equal(jdn2tind(integer()), tind(type = "d"))
    expect_equal(jdn2tind(numeric()), tind(type = "t"))
    expect_equal(jdn2tind(numeric(), tz = "UTC"), tind(tz = "UTC"))

    # test name preservation
    y2k <- as.date(c(y2k = "2000-01-01"))
    expect_equal(jdn2tind(jdn(y2k)), y2k)
})

