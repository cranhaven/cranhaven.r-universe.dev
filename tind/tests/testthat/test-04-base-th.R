context("base - date-time")
# ###################################################################

# test sample size
NN <- 100L
nms <- sample(letters, NN, replace = TRUE)
# NN <- 100L
# nms <- NULL

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test samples
yy <- sample(1923L:2069L, size = NN, replace = TRUE)
names(yy) <- nms
mm <- .validate_ym(yy, sample.int(12L, size = NN, replace = TRUE))
names(mm) <- nms
dd <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(mm))
dd <- .m2d(mm) + dd - 1L
names(dd) <- nms
yw <- sample(yy, size = NN, replace = TRUE)
ww <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(yw))
ww <- .validate_yw(yw, ww)
rm("yw")
names(ww) <- nms
qq <- .m2q(mm)
names(qq) <- nms
tt <- round(as.numeric(Sys.time()) + (((1L - NN) %/% 2):((NN - 1L) %/% 2)) *
                       (3600 * 23 + 61.111111), digits = 6L)
names(tt) <- nms


.valid_t0 <- function(t) (!is.na(t) & (t >= -62167165200) & (t <= 253402246800))


test_that("'.validate_t' works correctly", {
    tst <- as.numeric(as.POSIXct("0000-01-01 15:00:00", tz = "UTC"))
    ten <- as.numeric(as.POSIXct("9999-12-31 09:00:00", tz = "UTC"))
    expect_true(all(!is.na(.validate_t(c(tst, as.numeric(Sys.time()), ten)))))
    expect_equal(.validate_t(c(tst - 1, ten + 1)), c(NA_real_, NA_real_))
    ttv <- round(as.numeric(Sys.time()) + (2 * runif(NN) - 1) * 1e12, digits = 6L)
    names(ttv) <- nms
    tt <- .validate_t(ttv)
    expect_equal(!.valid_t0(ttv), is.na(tt))
    expect_equal(ttv[.valid_t0(ttv)], tt[!is.na(tt)])
    expect_equal(names(tt), nms)
})


test_that("'.t2char' works correctly", {
    expect_equal(.t2char(numeric(), "UTC"), character())
    expect_equal(.t2char(NA_real_, "UTC"), NA_character_)
    tt0 <- round(tt, 3)
    tt1 <- round(tt)
    tt2 <- as.numeric(as.POSIXct("2018-08-31 09:00:00", tz = "UTC")) + (0:(NN - 1L)) * 3600
    expect_equal(.t2char(tt2[1], "UTC", FALSE, FALSE), "2018-08-31 09:00")
    expect_equal(.t2char(tt2[1], "UTC", TRUE, FALSE), "2018-08-31 09:00Z")
    expect_equal(.t2char(tt2[1], "UTC", FALSE, TRUE), "2018-08-31 09:00 UTC")
    phms <- "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}(:[0-9]{2}(.[0-9]{1,6})?)?)"
    pz <- "(Z|[-+][0-9]{4})"
    pZ <- "([A-Z][-+a-zA-Z/]+|[-+][0-9]{2,4})"
    p00 <- paste0("^", phms, "$")
    p10 <- paste0("^", phms, pz, "$")
    p01 <- paste0("^", phms, " ", pZ, " *$")
    for (tz in tzs) {
        c00 <- .t2char(tt0, tz, FALSE, FALSE)
        expect_equal(names(c00), nms)
        expect_true(all(diff(nchar(c00)) == 0L))
        expect_true(all(grepl(p00, c00)))
        c10 <- .t2char(tt0, tz, TRUE, FALSE)
        expect_equal(names(c10), nms)
        expect_true(all(grepl(p10, c10)))
        if (tz != "UTC")
            expect_equal(as.numeric(strptime(c10, "%F %H:%M:%OS%z", tz = tz)), unname(tt0))
        if (tz != "Etc/GMT+1") {
            c01 <- .t2char(tt0, tz, FALSE, TRUE)
            expect_equal(names(c01), nms)
            expect_true(all(grepl(p01, c01)))
            expect_equal(sub(p01, "\\1", c01), c00)
        }

        c00 <- .t2char(tt1, tz, FALSE, FALSE)
        expect_equal(names(c00), nms)
        expect_true(all(grepl(p00, c00)))
        c10 <- .t2char(tt1, tz, TRUE, FALSE)
        expect_equal(names(c10), nms)
        expect_true(all(diff(nchar(c10)) == 0L))
        expect_true(all(grepl(p10, c10)))
        if (tz != "UTC")
            expect_equal(as.numeric(strptime(c10, "%F %H:%M:%S%z", tz = tz)), unname(tt1))
        if (tz != "Etc/GMT+1") {
            c01 <- .t2char(tt1, tz, FALSE, TRUE)
            expect_equal(names(c01), nms)
            expect_true(all(grepl(p01, c01)))
            expect_equal(sub(p01, "\\1", c01), c00)
        }
    }
    # check decimal places
    for (n in 1:6) {
        tx <- round(tt, n)
        tc <- .t2char(tx, "UTC", FALSE, FALSE)
        expect_equal(names(tc), nms)
        tc <- gsub("^.*(\\.[0-9]+)$", "\\1", tc)
        tx <- format(round(tx - floor(tx), n))
        tx <- gsub("^.*(\\.[0-9]+)$", "\\1", tx)
        expect_equal(tc, tx)
    }
})


test_that("'.t2hour', '.t2min', and '.t2sec' work correctly", {
    tt <- round(tt)
    for (tz in tzs) {
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00", tz = tz)
        t2h <- .t2hour(tt, tz)
        t2m <- .t2min(tt, tz)
        t2s <- .t2sec(tt, tz)
        expect_equal(names(t2h), nms)
        expect_equal(names(t2m), nms)
        expect_equal(names(t2s), nms)
        expect_equal(unname(t2h), as.integer(format(px, "%H", tz)))
        expect_equal(unname(t2m), as.integer(format(px, "%M", tz)))
        expect_equal(unname(t2s), as.numeric(format(px, "%S", tz)))
        expect_equal(.t2hour(numeric(), tz), integer())
        expect_equal(.t2min(numeric(), tz), integer())
        expect_equal(.t2sec(numeric(), tz), numeric())
    }
    tt <- -283996800 - round(runif(NN, 0, 1e9)) # before 1961-01-01 00:00Z
    for (tz in tzs) {
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00", tz = tz)
        expect_equal(.t2hour(tt, tz), as.integer(format(px, "%H", tz)))
        expect_equal(.t2min(tt, tz), as.integer(format(px, "%M", tz)))
        expect_equal(.t2sec(tt, tz), as.numeric(format(px, "%S", tz)))
        expect_equal(.t2hour(numeric(), tz), integer())
        expect_equal(.t2min(numeric(), tz), integer())
        expect_equal(.t2sec(numeric(), tz), numeric())
    }
})


test_that("'.t2d', '.t2y', '.t2q', '.t2m', '.t2w' work correctly", {
    for (tz in tzs) {
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00", tz = tz)
        t2d <- .t2d(tt, tz)
        t2d0 <- .validate_ymd(as.integer(format(px, "%Y", tz)),
                              as.integer(format(px, "%m", tz)),
                              as.integer(format(px, "%d", tz)))
        t2w <- .t2w(tt, tz)
        t2w0 <- .validate_yw(as.integer(format(px, "%G", tz)),
                             as.integer(format(px, "%V", tz)))
        t2m <- .t2m(tt, tz)
        t2m0 <- .validate_ym(as.integer(format(px, "%Y", tz)),
                             as.integer(format(px, "%m", tz)))
        t2q <- .t2q(tt, tz)
        t2q0 <- .validate_yq(as.integer(format(px, "%Y", tz)),
                             (as.integer(format(px, "%m", tz)) - 1L) %/% 3L +  1L)
        t2y <- .t2y(tt, tz)
        t2y0 <- as.integer(format(px, "%Y", tz))
        expect_equal(names(t2d), nms)
        expect_equal(names(t2w), nms)
        expect_equal(names(t2m), nms)
        expect_equal(names(t2q), nms)
        expect_equal(names(t2y), nms)
        expect_equal(unname(t2d), t2d0)
        expect_equal(unname(t2w), t2w0)
        expect_equal(unname(t2m), t2m0)
        expect_equal(unname(t2q), t2q0)
        expect_equal(unname(t2y), t2y0)
    }
})


test_that("'.d2t' works correctly", {
    for (tz in tzs) {
        expect_equal(.d2t(integer(), tz), numeric())
        expect_equal(.d2t(NA_integer_, tz), NA_real_)
        tt <- .d2t(c(NA_integer_, dd), tz)
        expect_equal(unname(tt[1L]), NA_real_)
        tt <- tt[-1L]
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00")
        pxd <- .validate_ymd(as.integer(format(px, "%Y", tz)),
                             as.integer(format(px, "%m", tz)),
                             as.integer(format(px, "%d", tz)))
        pxd1 <- .validate_ymd(as.integer(format(px - 1, "%Y", tz)),
                              as.integer(format(px - 1, "%m", tz)),
                              as.integer(format(px - 1, "%d", tz)))
        expect_true(all(pxd1 < dd) && identical(unname(dd), pxd))
    }
    warn <- "results for dates before 1923-01-01 might be incorrect"
    dd <- .validate_ymd(1922, 10, 10)
    for (tz in tzs) {
        if ((tz == "UTC") || grepl("^Etc", tz)) expect_silent(tt <- .d2t(dd, tz))
        else expect_warning(tt <- .d2t(dd, tz), warn, fixed = TRUE)
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00")
        pxd <- .validate_ymd(as.integer(format(px, "%Y", tz)),
                             as.integer(format(px, "%m", tz)),
                             as.integer(format(px, "%d", tz)))
        pxd1 <- .validate_ymd(as.integer(format(px - 1, "%Y", tz)),
                              as.integer(format(px - 1, "%m", tz)),
                              as.integer(format(px - 1, "%d", tz)))
        expect_true(all(pxd1 < dd) && identical(unname(dd), pxd))
    }
})


test_that("'.d2t' and '.t2d' work correctly - random time zones", {
    skip_on_cran() # in case new time zones with some peculiarities appear
                   # this is also slow...
    tzs2 <- setdiff(OlsonNames(), tzs)
    if (length(tzs2) < 5L) skip("too few time zones for further tests")
    dd <- 0L:as.integer(Sys.Date() + 1000 - as.Date("1923-01-01"))
    dd <- .validate_ymd(1923, 1, 1) + as.integer(dd)
    wrn <- "^NAs introduced; invalid date \\([-0-9]+\\) for time zone [_a-zA-Z/]+$"
    for (tz in sample(tzs2, 5L)) {
        ddt <- dd
        if (tz %in% names(.tz_missing_days())) {
            expect_warning(tt <- .d2t(ddt, tz), wrn)
            expect_equal(tt[ddt %in% .tz_missing_days()[[tz]]], NA_real_)
            expect_false(anyNA(tt[!(ddt %in% .tz_missing_days()[[tz]])]))
            ddt <- ddt[!(ddt %in% .tz_missing_days()[[tz]])]
        }
        tt <- .d2t(ddt, tz)
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00")
        pxd <- .validate_ymd(as.integer(format(px, "%Y", tz)),
                             as.integer(format(px, "%m", tz)),
                             as.integer(format(px, "%d", tz)))
        pxd1 <- .validate_ymd(as.integer(format(px - 1, "%Y", tz)),
                              as.integer(format(px - 1, "%m", tz)),
                              as.integer(format(px - 1, "%d", tz)))
        expect_true(all(pxd1 < ddt) && identical(ddt, pxd))
        dd2 <- .t2d(tt, tz)
        expect_equal(dd2, ddt)
        if (!(tz %in% names(.tz_missing_days()))) {
            expect_equal(.t2d(tt - 1, tz), ddt - 1L)
        }
    }
})


test_that("'.w2t' works correctly", {
    for (tz in tzs) {
        tt <- .w2t(ww, tz)
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00")
        pxw <- .validate_yw(as.integer(format(px, "%G", tz)),
                            as.integer(format(px, "%V", tz)))
        pxw1 <- .validate_yw(as.integer(format(px - 1, "%G", tz)),
                             as.integer(format(px - 1, "%V", tz)))
        expect_true(all(pxw1 < ww) && identical(unname(ww), pxw))
        expect_equal(names(tt), nms)
    }
})


test_that("'.m2t' works correctly", {
    for (tz in tzs) {
        tt <- .m2t(mm, tz)
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00")
        pxm <- .validate_ym(as.integer(format(px, "%Y", tz)),
                            as.integer(format(px, "%m", tz)))
        pxm1 <- .validate_ym(as.integer(format(px - 1, "%Y", tz)),
                             as.integer(format(px - 1, "%m", tz)))
        expect_true(all(pxm1 < mm) && identical(unname(mm), pxm))
        expect_equal(names(tt), nms)
    }
})


test_that("'.q2t' works correctly", {
    for (tz in tzs) {
        tt <- .q2t(qq, tz)
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00")
        pxq <- .validate_yq(as.integer(format(px, "%Y", tz)),
                            (as.integer(format(px, "%m", tz)) - 1L) %/% 3L +  1L)
        pxq1 <- .validate_yq(as.integer(format(px - 1, "%Y", tz)),
                             (as.integer(format(px - 1, "%m", tz)) - 1L) %/% 3L +  1L)
        expect_true(all(pxq1 < qq) && identical(unname(qq), pxq))
        expect_equal(names(tt), nms)
    }
})


test_that("'.y2t' works correctly", {
    for (tz in tzs) {
        tt <- .y2t(yy, tz)
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00")
        pxy <- as.integer(format(px, "%Y", tz))
        pxy1 <- as.integer(format(px - 1, "%Y", tz))
        expect_true(all(pxy1 < yy) && identical(unname(yy), pxy))
        expect_equal(names(tt), nms)
    }
})


test_that("'.t2yf' works correctly", {
    for (tz in tzs) {
        yf <- .t2yf(tt, tz)
        expect_equal(names(yf), nms)
        px <- as.POSIXct(tt, origin = "1970-01-01 00:00:00", tz = tz)
        expect_equal(as.integer(floor(yf)),
                         as.integer(format(px, format = "%Y", tz = tz)))
        y0 <- .t2y(tt, tz)
        y1 <- y0 + 1L
        t0 <- .y2t(y0, tz)
        t1 <- .y2t(y1, tz)
        yf0 <- y0 + (tt - t0) / (t1 - t0)
        expect_equal(yf, yf0)
    }
    expect_equal(.t2yf(1e9, "UTC"), 2001.687874175545403)
    expect_equal(.t2yf(numeric(), "UTC"), numeric())
})


test_that("'.t2jdn' and '.jdn2t' work correctly", {
    expect_equal(names(.t2jdn(tt)), nms)
    expect_equal(names(.jdn2t(.t2jdn(tt))), nms)
    expect_equal(.jdn2t(.t2jdn(tt)), tt)
    tt <- .d2t(dd, "UTC")
    expect_equal(.d2jdn(dd) - .5, .t2jdn(tt))
})


test_that("'.hours_in_day' and '.isdst_t' work correctly", {
    hd0 <- rep(24., length(dd))
    hd <- .hours_in_day(dd, "UTC")
    expect_equal(names(hd), nms)
    expect_equal(unname(hd), hd0)
    expect_equal(.hours_in_day(numeric(), "UTC"), numeric())

    ## NOTE: in the EU (and the majority of European countries) until 2024
    ## changes to DST occurred on the last Sunday of March and changes back
    ## on the last Sunday of October
    tzs_eu <- c("Europe/Amsterdam", "Europe/Berlin", "Europe/Budapest",
                "Europe/Copenhagen", "Europe/Dublin", "Europe/Helsinki",
                "Europe/Lisbon", "Europe/London", "Europe/Paris",
                "Europe/Prague", "Europe/Rome", "Europe/Vienna",
                "Europe/Warsaw", "Europe/Zurich")
    tzs_eu <- intersect(OlsonNames(), tzs_eu)
    dd <- .validate_ymd(2000, 1, 1):.validate_ymd(2024, 12, 31)
    todst <- .last_dw_in_month(7L, .validate_ym(2000:2024, 3L))
    fromdst <- .last_dw_in_month(7L, .validate_ym(2000:2024, 10L))
    hd0 <- rep(24., length(dd))
    hd0[dd %in% todst] <- 23
    hd0[dd %in% fromdst] <- 25
    for (tz in tzs_eu) {
        hd <- .hours_in_day(dd, tz = tz)
        expect_equal(hd, hd0)
        if (!(tz %in% c("Europe/Dublin", "Europe/Helsinki", "Europe/Lisbon", "Europe/London"))) {
            # changes 1:59 -> 3:00 and 2:59 -> 2:00
            t_todst <- rep(.d2t(todst, tz), each = 25L) + c(0:23, NA) * 3600
            expect_equal(.isdst_t(t_todst, tz),
                            rep(c(rep(FALSE, 2L), rep(TRUE, 22L), NA), length(todst)))
            t_fromdst <- rep(.d2t(fromdst, tz), each = 25L) + c(0:23, NA) * 3600
            expect_equal(.isdst_t(t_fromdst, tz),
                            rep(c(rep(TRUE, 3L), rep(FALSE, 21L), NA), length(fromdst)))
        }
    }
    for (tz in intersect(tzs, c("UTC", "Etc/GMT+1"))) {
        expect_equal(.isdst_t(t_todst, tz),
                         rep(c(rep(FALSE, 24L), NA), length(todst)))
    }

    ## NOTE: in the US and Canada (most areas) in years 2007--2024 changes
    ## to DST occurred on the 2nd Sunday of March and changes back on the 1st
    ## Sunday of November
    tzs_uscan <- c("America/Chicago", "America/Denver", "America/Detroit",
                   "America/Los_Angeles", "America/New_York", "America/Toronto",
                   "America/Vancouver", "America/Winnipeg")
    tzs_uscan <- intersect(OlsonNames(), tzs_uscan)
    dd <- .validate_ymd(2007, 1, 1):.validate_ymd(2024, 12, 31)
    todst <- .nth_dw_in_month(2L, 7L, .validate_ym(2007:2024, 3))
    fromdst <- .nth_dw_in_month(1L, 7L, .validate_ym(2007:2024, 11))
    hd <- rep(24., length(dd))
    hd[dd %in% todst] <- 23
    hd[dd %in% fromdst] <- 25
    for (tz in tzs_uscan) expect_equal(.hours_in_day(dd, tz = tz), hd)

    wrn <- "^NAs introduced; invalid date \\([-0-9]+\\) for time zone [_a-zA-Z/]+$"
    for (tz in intersect(OlsonNames(), names(.tz_missing_days()))) {
        dd <- .tz_missing_days()[[tz]] + (-1L:1L)
        ddt <- .tz_missing_days()[[tz]] + c(-1L, 1L)
        expect_warning(.hours_in_day(dd, tz), wrn)
        expect_silent(.hours_in_day(ddt, tz))
    }
    if ((tz <- "Africa/Monrovia") %in% OlsonNames()) {
        hd <- .hours_in_day(.validate_ymd(1972, 1, 7), tz)
        expect_equal(hd, 24 - (44 * 60 + 30) / 3600)
    }
    if ((tz <- "Etc/GMT-1") %in% OlsonNames()) {
        dd <- .validate_ymd(1970, 1, 1):.validate_ymd(2022, 12, 31)
        hd <- .hours_in_day(dd, tz)
        expect_true(all(hd == 24))
    }
})


test_that("'.dhz2t' works correctly", {
    for (tz in tzs) {
        dd <- .validate_ymd(2020, 10, c(1, NA_integer_))
        h <-  20
        m <- 9
        s <- 16.194
        dt <- .dhz2t(dd, .validate_hms(h, m, s), integer(), tz)
        plt <- as.POSIXlt(dt[1L], origin = "1970-01-01 00:00", tz = tz)
        expect_equal(dd, c(.validate_ymd(plt$year + 1900, plt$mon + 1, plt$mday),
                               NA_integer_))
        expect_equal(h, plt$hour)
        expect_equal(m, plt$min)
        expect_equal(s, plt$sec)
    }

    dd <- .validate_ymd(1990, 1, 1):.validate_ymd(2020, 12, 31)
    nn <- length(dd)
    names(dd) <- if (is.null(nms)) nms else rep_len(nms, nn)
    h <- sample(0:23, nn, replace = TRUE)
    m <- sample(0:59, nn, replace = TRUE)
    s <- round(runif(nn, 0, 60), 3)
    s[s == 60] <- 60 - 1e-3
    for (tz in tzs) {
        dt <- .dhz2t(dd, .validate_hms(h, m, s), integer(), tz, 0L)
        expect_equal(names(dt), names(dd))
        plt <- as.POSIXlt(dt, origin = "1970-01-01 00:00", tz = tz)
        ii <- !is.na(dt)
        plt <- plt[ii]
        expect_equal(unname(dd[ii]), as.integer(as.Date(plt)))
        expect_equal(plt$hour, h[ii])
        expect_equal(plt$min, m[ii])
        expect_equal(round(plt$sec, digits  = 6), s[ii])
    }

    if ((tz <- "Europe/Warsaw") %in% OlsonNames()) {
        # DST change
        expect_equal(is.na(.dhz2t(.validate_ymd(2021, 3, 28),
                                      .validate_hms(c(1:3, 24), 30, 0),
                                      integer(), tz, 0L)),
                         c(FALSE, TRUE, FALSE, TRUE))
        expect_equal(.dhz2t(.validate_ymd(2021, 3, 28),
                                .validate_hms(24, 0, 0), integer(), tz, 0L),
                         .d2t(.validate_ymd(2021, 3, 29), tz))
    }
})


test_that("'.astz' works correctly", {
    if (length(tzs) < 2L) skip("too few time zones to test")
    tt <- round(tt)
    tz01 <- sample(tzs, 2L)
    tz <- tz01[1L]
    tz1 <- tz01[2L]
    tt1 <- suppressWarnings(.astz(tt, tz, tz1))
    tt2 <- suppressWarnings(.astz(tt1, tz1, tz))
    expect_equal(names(tt1), names(tt))
    ok <- !is.na(tt1)
    expect_equal(.t2d(tt1[ok], tz1), .t2d(tt[ok], tz))
    expect_equal(.t2h(tt1[ok], tz1), .t2h(tt[ok], tz))
    expect_equal(.t2d(tt2[ok], tz), .t2d(tt[ok], tz))
    expect_equal(.t2h(tt2[ok], tz), .t2h(tt[ok], tz))
})


test_that("'.astz' works correctly for longer vectors", {
    skip_on_cran()
    if (length(tzs) < 2L) skip("too few time zones to test")
    tt <- round(runif(2e5, 8e8, 1.7e9))
    names(tt) <- rep_len(nms, length(tt))
    tz01 <- sample(tzs, 2L)
    tz <- tz01[1L]
    tz1 <- tz01[2L]
    tt1 <- suppressWarnings(.astz(tt, tz, tz1))
    tt2 <- suppressWarnings(.astz(tt1, tz1, tz))
    expect_equal(names(tt1), names(tt))
    ok <- !is.na(tt1)
    expect_equal(.t2d(tt1[ok], tz1), .t2d(tt[ok], tz))
    expect_equal(.t2h(tt1[ok], tz1), .t2h(tt[ok], tz))
    expect_equal(.t2d(tt2[ok], tz), .t2d(tt[ok], tz))
    expect_equal(.t2h(tt2[ok], tz), .t2h(tt[ok], tz))
})


test_that("'.floor_t_/.ceiling_t_ h/min/s' work correctly", {
    tt <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 6)
    nh <- sample(c(1L:4L, 6L, 12L, 24L), 3L)
    nm <- sample(c(1L:6L, 10L, 12L, 15L, 20L, 30L), 3L)
    ns <- c(sample(c(.1, .2, .5), 1L),
            sample(c(1L:6L, 10L, 12L, 15L, 20L, 30L), 3L))
    expect_equal(.floor_t_h(numeric(), 1, "UTC"), numeric())
    expect_equal(.floor_t_h(c(0, NA_real_), 1, "UTC"), c(0, NA_real_))
    expect_equal(.floor_t_h(NA_real_, 1, "UTC"), NA_real_)
    expect_equal(.floor_t_min(numeric(), 1, "UTC"), numeric())
    expect_equal(.floor_t_min(c(0, NA_real_), 1, "UTC"), c(0, NA_real_))
    expect_equal(.floor_t_min(NA_real_, 1, "UTC"), NA_real_)
    expect_equal(.floor_t_s(numeric(), 1, "UTC"), numeric())
    expect_equal(.floor_t_s(c(0, NA_real_), 1, "UTC"), c(0, NA_real_))
    expect_equal(.floor_t_s(NA_real_, 1, "UTC"), NA_real_)
    for (tz in tzs) {
        ttt <- c(tt, .floor_t_h(tt, 1, tz))
        for (n in nh) {
            ft <- .floor_t_h(ttt, n, tz)
            ct <- .ceiling_t_h(ttt, n, tz)
            expect_equal(.floor_t_h(ft, n, tz), ft)
            expect_equal(.ceiling_t_h(ct, n, tz), ct)
            expect_equal(ft == ttt, ttt == ct)
            expect_true(all(ft <= ttt & ttt <= ct) &&
                        all(ct <= .d2t(.t2d(ttt, tz) + 1L, tz)) &&
                        all(ft >= .d2t(.t2d(ttt, tz), tz)))
        }
        ttt <- c(tt, .floor_t_min(tt, 1, tz))
        for (n in nm) {
            ft <- .floor_t_min(ttt, n, tz)
            ct <- .ceiling_t_min(ttt, n, tz)
            expect_equal(.floor_t_min(ft, n, tz), ft)
            expect_equal(.ceiling_t_min(ct, n, tz), ct)
            expect_equal(ft == ttt, ttt == ct)
            expect_true(all(ft <= ttt & ttt <= ct) &&
                        all(ct <= .d2t(.t2d(ttt, tz) + 1L, tz)) &&
                        all(ft >= .d2t(.t2d(ttt, tz), tz)))
        }
        ttt <- c(tt, .floor_t_s(tt, 1, tz))
        for (n in ns) {
            ft <- .floor_t_s(ttt, n, tz)
            ct <- .ceiling_t_s(ttt, n, tz)
            expect_equal(.floor_t_s(ft, n, tz), ft)
            expect_equal(.ceiling_t_s(ct, n, tz), ct)
            expect_equal(ft == ttt, ttt == ct)
            expect_true(all(ft <= ttt & ttt <= ct))
            expect_true(all(ct <= .d2t(.t2d(ttt, tz) + 1L, tz)))
            expect_true(all(ft >= .d2t(.t2d(ttt, tz), tz)))
        }
    }

    if ((tz <- "Europe/Warsaw") %in% tzs) {
        expect_equal(.floor_t_h(numeric(), 1, tz), numeric())
        expect_equal(.floor_t_h(c(zero = 0, na = NA_real_), 1, tz),
                                  c(zero = 0, na = NA_real_))
        expect_equal(.floor_t_h(NA_real_, 1, tz), NA_real_)
        expect_equal(.floor_t_min(numeric(), 1, tz), numeric())
        expect_equal(.floor_t_min(c(zero = 0, na = NA_real_), 1, tz),
                                    c(zero = 0, na = NA_real_))
        expect_equal(.floor_t_min(NA_real_, 1, tz), NA_real_)
        expect_equal(.floor_t_s(numeric(), 1, tz), numeric())
        expect_equal(.floor_t_s(c(zero = 0, na = NA_real_), 1, tz),
                                  c(zero = 0, na = NA_real_))
        expect_equal(.floor_t_s(NA_real_, 1, tz), NA_real_)
        tt <- as.numeric(as.POSIXct("2020-05-31 18:28:32", tz = tz))
        for (n in nh) {
            fl <- floor((tt + 7200) / (3600 * n)) * 3600 * n - 7200
            cl <- ceiling((tt + 7200) / (3600 * n)) * 3600 * n - 7200
            expect_equal(.floor_t_h(tt, n, tz), fl)
            expect_equal(.ceiling_t_h(tt, n, tz), cl)
        }
        for (n in nm) {
            fl <- floor(tt / (60 * n)) * 60 * n
            cl <- ceiling(tt / (60 * n)) * 60 * n
            expect_equal(.floor_t_min(tt, n, tz), fl)
            expect_equal(.ceiling_t_min(tt, n, tz), cl)
        }
        for (n in ns) {
            fl <- floor(tt / n) * n
            cl <- ceiling(tt / n) * n
            expect_equal(.floor_t_s(tt, n, tz), fl)
            expect_equal(.ceiling_t_s(tt, n, tz), cl)
        }

        tt <- as.numeric(as.POSIXct(c(# DST change, 25h-long day
                                      "2020-10-25 03:28:32",
                                      # DST change, 23h-long day
                                      "2021-03-28 03:28:32"), tz = tz))
        nh <- c(1, 2, 3, 4, 6, 12, 24)
        # DST change 25h-long day
        f1 <- c(3, 2, 3, 0, 0,  0,  0)
        c1 <- c(4, 4, 6, 4, 6, 12,  0)
        # DST change 23h-long day
        f2 <- c(3, 0, 3, 0, 0,  0,  0)
        c2 <- c1
        fha <- fhb <- cha <- chb <- numeric(length(nh))
        fma <- fmb <- cma <- cmb <- numeric(length(nh))
        fsa <- fsb <- csa <- csb <- numeric(length(nh))
        for (i in seq_along(nh)) {
            ftplt <- as.POSIXlt(.floor_t_h(tt, nh[i], tz),
                                origin = "1970-01-01 00:00", tz = tz)
            ctplt <- as.POSIXlt(.ceiling_t_h(tt, nh[i], tz),
                                origin = "1970-01-01 00:00", tz = tz)
            fha[i] <- ftplt$hour[1]
            fhb[i] <- ftplt$hour[2]
            cha[i] <- ctplt$hour[1]
            chb[i] <- ctplt$hour[2]
            fma[i] <- ftplt$min[1]
            fmb[i] <- ftplt$min[2]
            cma[i] <- ctplt$min[1]
            cmb[i] <- ctplt$min[2]
            fsa[i] <- ftplt$sec[1]
            fsb[i] <- ftplt$sec[2]
            csa[i] <- ctplt$sec[1]
            csb[i] <- ctplt$sec[2]
        }
        expect_true(all(fma == 0) && all(fmb == 0) &&
                    all(cma == 0) && all(cmb == 0) &&
                    all(fsa == 0) && all(fsb == 0) &&
                    all(csa == 0) && all(csb == 0))
        expect_equal(fha, f1)
        expect_equal(cha, c1)
        expect_equal(fhb, f2)
        expect_equal(chb, c2)
    }

    for (tz in intersect(OlsonNames(), names(.tz_missing_days()))) {
        dd <- .tz_missing_days()[[tz]] + (-2L:-1L)
        tt <- .dhz2t(dd, 12 * 3600, integer(), tz)
        expect_equal(.floor_t_h(tt, 6L, tz), tt)
        expect_equal(.floor_t_h(tt, 12L, tz), tt)
        expect_equal(.floor_t_min(tt, 30L, tz), tt)
        expect_equal(.floor_t_s(tt, 30L, tz), tt)
    }
})


# test sample (1995-2008), will add +/- 10 years
tt <- round(runif(NN, .8e9, 1.2e9))
names(tt) <- nms
dd <- as.integer(runif(NN, -3650, 3650))
dm <- as.integer(runif(NN, -120, 120))

test_that("'.inc_t_by_d' works correctly", {
    for (tz in tzs) {
        expect_equal(.inc_t_by_d(c(1e9, NA), 1L, tz), c(1000086400, NA))
    }
    for (tz in tzs) {
        d0 <- .t2d(tt, tz)
        tt1 <- .inc_t_by_d(tt, dd, tz)
        expect_equal(names(tt1), nms)
        d1 <- .t2d(tt1, tz)
        expect_equal(unname(d1 - d0), dd)
        pct0 <- as.POSIXct(tt, origin = "1970-01-01 00:00:00", tz = tz)
        pct1 <- as.POSIXct(tt1, origin = "1970-01-01 00:00:00", tz = tz)
        fmt <- "%M:%S"
        expect_equal(format(pct0, fmt), format(pct1, fmt))
        fmt <- "%H"
        h0 <- as.integer(format(pct0, fmt))
        h1 <- as.integer(format(pct1, fmt))
        ii <- ((.hours_in_day(d0, tz) == 24) & (.hours_in_day(d1, tz) == 24))
        expect_true(all(h0[ii] == h1[ii]) && all(h1[h0 != h1] == h0[h0 != h1] + 1L))
    }
    for (tz in intersect(OlsonNames(), names(.tz_missing_days()))) {
        dd <- unname(.tz_missing_days()[[tz]]) + (-2L:-1L)
        dd2 <- dd + 1L:2L
        tt <- .dhz2t(dd, 12 * 3600, integer(), tz)
        expect_equal(.t2d(.inc_t_by_d(tt, 1L, tz), tz), dd2)
    }
})


test_that("'.inc_t_by_m' works correctly", {
    dm <- as.integer(sample(-200L:200L, NN, replace = TRUE))
    for (tz in tzs) {
        d0 <- .t2d(tt, tz)
        m0 <- .d2m(d0)
        tt1 <- .inc_t_by_m(tt, dm, tz)
        expect_equal(names(tt1), nms)
        d1 <- .t2d(tt1, tz)
        m1 <- .d2m(d1)
        expect_equal(d1, .inc_d_by_m(d0, dm))
        pct0 <- as.POSIXct(tt, origin = "1970-01-01 00:00:00", tz = tz)
        pct1 <- as.POSIXct(tt1, origin = "1970-01-01 00:00:00", tz = tz)
        fmt <- "%M:%S"
        expect_equal(format(pct0, fmt), format(pct1, fmt))
        fmt <- "%H"
        h0 <- as.integer(format(pct0, fmt))
        h1 <- as.integer(format(pct1, fmt))
        ii <- ((.hours_in_day(d0, tz) == 24) & (.hours_in_day(d1, tz) == 24))
        expect_true(all(h0[ii] == h1[ii]) && all(h1[h0 != h1] == h0[h0 != h1] + 1L))
    }
})


context("base - time of day")
# ###################################################################

hh <- round(runif(NN, 0, 86400), digits = 6L)
names(hh) <- nms


.valid_h0 <- function(h) (!is.na(h) & (h >= 0) & (h <= 86400))

test_that("'.validate_h' works correctly", {
    expect_true(all(!is.na(.validate_h(c(0, as.numeric(Sys.time()) %% 86400, 86400)))))
    expect_equal(.validate_h(c(-1e-6, 86400 + 1e-6)), c(NA_real_, NA_real_))
    hhv <- round(runif(NN, -1000, 100000), digits = 6L)
    hh <- .validate_t(hhv)
    expect_equal(!.valid_t0(hhv), is.na(hh))
    expect_equal(hhv[.valid_t0(hhv)], hh[!is.na(hh)])
    expect_equal(.validate_h(double()), double())
    expect_equal(.validate_h(integer()), double())
    expect_equal(.validate_h(7200L), 7200.)
})


test_that("'.validate_hms' works correctly", {
    expect_equal(.validate_hms(c(1:3, 24), 30, 0), c(1:3, NA) * 3600 + 1800)
    expect_equal(.validate_hms(c(1:3, 24), 0, 30), c(1:3, NA) * 3600 + 30)
    expect_equal(.validate_hms(c(1:3, 24), 0, 0), c(1:3, 24) * 3600)
    H <- sample.int(25L, size = NN, replace = TRUE) - 1L
    M <- sample.int(62L, size = NN, replace = TRUE) - 1L
    S <- floor(runif(NN, 0, 62))
    ina <- sample.int(NN - 2L, 3L) + 2L
    H[ina[1L]] <- NA_integer_
    M[ina[2L]] <- NA_integer_
    S[ina[3L]] <- NA_real_

    jj <- sample.int(3L, 1L)
    if (jj == 1L) {
        names(H) <- nms
    } else if (jj == 2L) {
        names(M) <- nms
    } else {
        names(S) <- nms
    }

    .validate_hms0 <- function(h, m, s)
    {
        ok <- !is.na(h) & !is.na(m) & !is.na(s) &
              ((h >= 0) & (h < 24) & (m >= 0) & (m < 60) & (s >= 0) & (s < 60) |
              (h == 24) & (m == 0) & (s == 0))
        res <- round(3600 * h + 60 * m + s, 6L)
        res[!ok] <- NA_real_
        return (res)
    }

    expect_equal(.validate_hms(H, M, S), .validate_hms0(H, M, S))
    expect_equal(.validate_hms(H, M[1L], NA_real_), .validate_hms0(H, M[1L], NA_real_))
    expect_equal(.validate_hms(H, NA_integer_, S[1L]), .validate_hms0(H, NA_integer_, S[1L]))
    expect_equal(.validate_hms(H[1L], M, NA_real_), .validate_hms0(H[1L], M, NA_real_))
    expect_equal(.validate_hms(NA_integer_, M, S[1L]), .validate_hms0(NA_integer_, M, S[1L]))
    expect_equal(.validate_hms(NA_integer_, M[1L], S), .validate_hms0(NA_integer_, M[1L], S))
    expect_equal(.validate_hms(H[1L], NA_integer_, S), .validate_hms0(H[1L], NA_integer_, S))
    expect_equal(.validate_hms(H[1L], M, S[1L]), .validate_hms0(H[1L], M, S[1L]))
    expect_equal(.validate_hms(H, M[1L:2L], S[1L:2L]), .validate_hms0(H, M[1L:2L], S[1L:2L]))
    expect_equal(.validate_hms(H[1L:2L], M, S[1L:2L]), .validate_hms0(H[1L:2L], M, S[1L:2L]))
    expect_equal(.validate_hms(H[1L:2L], M[1L:2L], S), .validate_hms0(H[1L:2L], M[1L:2L], S))
})


test_that("'.h2char' works correctly", {
    expect_equal(.h2char(numeric()), character())
    expect_equal(.h2char(NA_real_), NA_character_)
    expect_equal(.h2char(1), "00:00:01")
    expect_equal(.h2char(3600), "01:00")
    expect_equal(.h2char(43932.12), "12:12:12.12")
    hh[1] <- 13.111111
    for (digits in 6L:1L) {
        hh <- round(hh, digits)
        hh[hh == 86400] <- 0
        phms <- paste0("^[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{", digits, "}$")
        expect_true(all(grepl(phms, .h2char(hh))))
    }
    expect_equal(names(.h2char(hh)), nms)
    hh  <- round(hh)
    hh[hh == 86400] <- 0
    phms <- "^[0-9]{2}:[0-9]{2}:[0-9]{2}$"
    expect_true(all(grepl(phms, .h2char(hh))))
    hh <- (hh %/% 60) * 60
    phms <- "^[0-9]{2}:[0-9]{2}$"
    expect_true(all(grepl(phms, .h2char(hh))))
    hh <- (hh %/% 3600) * 3600
    expect_true(all(grepl(phms, .h2char(hh))))
})


test_that("'.h2hour', '.h2min', and '.h2sec' work correctly", {
    hour <- sample(0L:23L, NN, replace = TRUE)
    min <- sample(0L:59L, NN, replace = TRUE)
    sec <- round(runif(NN, 0, 60 - 1e-6), 6L)
    hms <- 3600 * hour + 60 * min + sec
    names(hms) <- nms
    h2h <- .h2hour(hms)
    h2m <- .h2min(hms)
    h2s <- .h2sec(hms)
    expect_equal(names(h2h), nms)
    expect_equal(names(h2m), nms)
    expect_equal(names(h2s), nms)
    expect_equal(unname(h2h), hour)
    expect_equal(unname(h2m), min)
    expect_equal(unname(h2s), sec)
    expect_equal(.h2hour(numeric()), integer())
    expect_equal(.h2min(numeric()), integer())
    expect_equal(.h2sec(numeric()), numeric())
})


test_that("'.floor_h_/.ceiling_h_ h/min/s' work correctly", {
    nh <- sample(c(1L:4L, 6L, 12L, 24L), 3L)
    nm <- sample(c(1L:6L, 10L, 12L, 15L, 20L, 30L), 3L)
    ns <- c(sample(c(.1, .2, .5), 1L),
            sample(c(1L:6L, 10L, 12L, 15L, 20L, 30L), 3L))
    expect_equal(.floor_h_h(numeric(), 1), numeric())
    expect_equal(.floor_h_h(c(0, NA_real_), 1), c(0, NA_real_))
    expect_equal(.floor_h_h(NA_real_, 1), NA_real_)
    expect_equal(.floor_h_min(numeric(), 1), numeric())
    expect_equal(.floor_h_min(c(0, NA_real_), 1), c(0, NA_real_))
    expect_equal(.floor_h_min(NA_real_, 1), NA_real_)
    expect_equal(.floor_h_s(numeric(), 1), numeric())
    expect_equal(.floor_h_s(c(0, NA_real_), 1), c(0, NA_real_))
    expect_equal(.floor_h_s(NA_real_, 1), NA_real_)

    for (n in nh) {
        fh <- .floor_h_h(hh, n)
        ch <- .ceiling_h_h(hh, n)
        expect_equal(.floor_h_h(fh, n), fh)
        expect_equal(.ceiling_h_h(ch, n), ch)
        expect_equal(fh == hh, hh == ch)
        expect_true(all(fh <= hh & hh <= ch))
        expect_equal(fh, .floor_t_h(hh, n, "UTC"))
        expect_equal(ch, .ceiling_t_h(hh, n, "UTC"))
        expect_equal(names(fh), nms)
        expect_equal(names(ch), nms)
    }
    for (n in nm) {
        fh <- .floor_h_min(hh, n)
        ch <- .ceiling_h_min(hh, n)
        expect_equal(.floor_h_min(fh, n), fh)
        expect_equal(.ceiling_h_min(ch, n), ch)
        expect_equal(fh == hh, hh == ch)
        expect_true(all(fh <= hh & hh <= ch))
        expect_equal(fh, .floor_t_min(hh, n, "UTC"))
        expect_equal(ch, .ceiling_t_min(hh, n, "UTC"))
        expect_equal(names(fh), nms)
        expect_equal(names(ch), nms)
    }
    for (n in ns) {
        fh <- .floor_h_s(hh, n)
        ch <- .ceiling_h_s(hh, n)
        expect_equal(.floor_h_s(fh, n), fh)
        expect_equal(.ceiling_h_s(ch, n), ch)
        expect_equal(fh == hh, hh == ch)
        expect_true(all(fh <= hh & hh <= ch))
        expect_equal(fh, .floor_t_s(hh, n, "UTC"))
        expect_equal(ch, .ceiling_t_s(hh, n, "UTC"))
        expect_equal(names(fh), nms)
        expect_equal(names(ch), nms)
    }
})

