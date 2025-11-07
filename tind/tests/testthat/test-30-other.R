context("other representations of time indices")
# ###################################################################

err1 <- paste0("^invalid time index type in ", sQuote("[\\.a-zA-Z]"), ": ",
               dQuote("[a-z]"), " \\([-a-z]\\); expected: ", dQuote("[a-z]"),
               " \\([-a-z]\\)$")
err1f <- paste0("^invalid time index type in ", sQuote("[\\.a-zA-Z]"), ": ",
                dQuote("[a-z]"), " \\([-a-z]\\); expected one of the following: ",
                 dQuote("[a-z]"), " \\([- a-z]+\\)(, ", dQuote("[a-z]"),
                  " \\([- a-z]+\\))+$")
err2 <- paste0("^cast from time index type ", dQuote("[a-z]"),
               " \\([- a-z]+\\) to type ", dQuote("[a-z]"), " \\([- a-z]+\\) in ",
               sQuote("[\\._a-z0-9]+"), " not possible$")
err3 <- paste0("invalid ", sQuote("format"), " argument; expected one of the following: ",
               dQuote("[a-zA-Z]"), "(, ", dQuote("[a-zA-Z]"), ")+$")



test_that("zoo::yearmon and zoo::yearqtr are handled correctly", {
    skip_on_cran()
    skip_if_not_installed("zoo")
    x <- zoo::as.yearmon(2000 + (0:23) / 12)
    expect_true(.tind_coercible(x))
    expect_equal(ti_type(x, long = FALSE), "m")
    xt <- as.tind(x)
    expect_equal(zoo::as.yearmon(year_frac(xt)), x)
    expect_equal(zoo::as.yearmon(xt), x)
    expect_null(tzone(x))
    x <- zoo::as.yearqtr(2000 + (0:7) / 4)
    expect_true(.tind_coercible(x))
    expect_equal(ti_type(x, long = FALSE), "q")
    xt <- as.tind(x)
    expect_equal(resolution_t(x), resolution_t(xt))
    expect_equal(tspan(x), tspan(xt))
    expect_equal(zoo::as.yearqtr(year_frac(xt)), x)
    expect_equal(zoo::as.yearqtr(xt), x)
    expect_null(tzone(x))
})


test_that("timeDate::timeDate is handled correctly", {
    skip_on_cran()
    skip_if_not_installed("timeDate")
    tzs <- c("Asia/Tokyo", "Europe/Warsaw", "America/New_York", "UTC")
    tzs <- intersect(intersect(tzs, timeDate::listFinCenter()), OlsonNames())

    tt <- round(1e9 + sample(c(1, 10, 20, 40), 1L) * (-3:7))
    for (tz in tzs) {
        xt <- as.tind(tt, tz = tz)
        x <- timeDate::as.timeDate(as.POSIXct(xt, tz = "UTC"), FinCenter = tz)
        expect_equal(x, timeDate::as.timeDate(xt))
        expect_true(.tind_coercible(x))
        expect_equal(ti_type(x, long = FALSE), "t")
        expect_equal(x@FinCenter, tz)
        expect_equal(as.tind(x), xt)
        expect_equal(tzone(x), tzone(xt))
    }
    if (length(tzs) >= 2L) {
        tsz <- sample(tzs, 2L)
        xt <- as.tind(tt, tz = tzs[1L])
        x <- timeDate::as.timeDate(xt)
        tzone(xt) <- tzs[2L]
        tzone(x) <- tzs[2L]
        expect_equal(xt, as.tind(x))
        xt <- as.tzone(xt, tzs[1L])
        x <- as.tzone(x, tzs[1L])
        expect_equal(xt, as.tind(x))
    }
})


test_that("chron::chron, chron::dates, and chron::times are handled correctly", {
    skip_on_cran()
    skip_if_not_installed("chron")
    tt <- as.date_time(round(1e9 + sample(c(1, 10, 20, 40, 1e5, 2e5, 3e5), 3L) * (-3:5)), tz = "UTC")
    td <- as.date(tt)
    th <- as.time(tt)
    chrt <- chron::as.chron(tt)
    expect_true(.tind_coercible(chrt))
    expect_equal(ti_type(chrt, long = FALSE), "t")
    expect_equal(tzone(chrt), "UTC")
    chrd <- chron::as.dates(td)
    expect_true(.tind_coercible(chrd))
    expect_equal(chron::as.chron(td), chrd)
    expect_equal(ti_type(chrd, long = FALSE), "d")
    chrh <- chron::as.times(th)
    expect_true(.tind_coercible(chrh))
    expect_equal(chron::as.chron(th), chrh)
    expect_equal(ti_type(chrh, long = FALSE), "h")
    expect_equal(as.character(chrt), format(tt, "(%m/%d/%y %T)"))
    expect_equal(as.character(chrd), format(td, "%m/%d/%y"))
    expect_equal(as.character(chrh), format(th, "%T"))
    tt2 <- as.tind(chrt)
    expect_equal(tzone(tt2), "UTC")
    expect_equal(tt2, tt)
    expect_equal(as.tind(chrd), td)
    expect_equal(as.tind(chrh), th)
    expect_equal(ti_type(chrt, long = FALSE), "t")
})


test_that("data.table::IDate and data.table::ITime are handled correctly", {
    skip_on_cran()
    skip_if_not_installed("data.table")
    tt <- as.date_time(round(1e9 + sample(c(1, 10, 20, 40, 1e5, 2e5, 3e5), 3L) * (-3:5)), tz = "UTC")
    td <- as.date(tt)
    th <- as.time(tt)
    ID <- data.table::as.IDate(td)
    expect_true(.tind_coercible(ID))
    expect_equal(ti_type(ID, long = FALSE), "d")
    expect_equal(as.tind(ID), td)
    expect_equal(as.Date(ID), as.Date(td))
    IT <- data.table::as.ITime(th)
    expect_true(.tind_coercible(IT))
    expect_equal(ti_type(IT, long = FALSE), "h")
    expect_equal(as.tind(IT), th)
})


test_that("'date2num' and 'num2date' work correctly", {
    # MATLAB
    dd <- 730377
    expect_equal(num2date(dd, "MATLAB"), as.tind("1999-09-14"))
    expect_equal(date2num(as.tind("1999-09-14"), "MATLAB"), as.integer(dd))
    # Excel
    dd <- 36526
    expect_equal(num2date(dd, "Excel"), as.tind("2000-01-01"))
    expect_equal(date2num(as.tind("2000-01-01"), "Excel"), as.integer(dd))
    # SAS
    dd <- 19068
    expect_equal(num2date(dd, "SAS"), as.tind("2012-03-16"))
    expect_equal(date2num(as.tind("2012-03-16"), "SAS"), as.integer(dd))
    # R
    dd <- sample.int(30000L, 1L)
    expect_equal(num2date(dd, "R"), as.tind(as.Date(dd, origin = "1970-01-01")))
    expect_equal(date2num(as.tind(as.Date(dd, origin = "1970-01-01")), "R"), dd)
    # JDN
    dd <- as.tind(as.Date(dd, origin = "1970-01-01"))
    expect_equal(date2num(dd, "JDN"), jdn(dd))
    expect_equal(num2date(date2num(dd, "JDN"), "JDN"), dd)
    # errors
    err <- paste(sQuote("format"), "argument missing")
    expect_error(num2date(730377), err, fixed = TRUE)
    expect_error(date2num(as.tind("1999-09-14")), err, fixed = TRUE)
    err <- paste0("^invalid ", sQuote("format"), " argument; expected one of the following: ",
                  dQuote("[a-zA-Z]+"), "(, ", dQuote("[a-zA-Z]+"), ")+$")
    expect_error(num2date(730377, "Qwerty"), err)
    expect_error(date2num(as.tind("1999-09-14"), "Qwerty"), err)
    expect_error(date2num(now(), "R"))
    expect_error(date2num(as.month(today()), "R"))
    err <- paste0("invalid ", sQuote("x"), " argument; expected a numeric vector")
    expect_error(num2date(tind(type = "d"), "JDN"), err)

    # test name preservation
    y2k <- as.date(c(y2k = "2000-01-01"))
    for (fmt in names(.origins))
        expect_equal(num2date(date2num(y2k, fmt), fmt), y2k)
})

