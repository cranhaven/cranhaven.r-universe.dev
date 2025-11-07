context("tzone, tzone<-, and as.tzone methods")
# ###################################################################

errimpl <- paste0("^", sQuote("[-<\\.a-z]+"), " method not defined for class ",
                  dQuote("[a-zA-Z/]+"), "$")
errclnt <- paste0("^", dQuote("[a-zA-Z/]+"), " is not recognised as a class representing time indices$")

errtype <- paste0("^invalid time index type in `[-\\.<_a-z]+`: ", dQuote("[a-z]"),
                  " \\([- a-z]+\\); expected: ", dQuote("t"), " \\(date-time\\)$")


test_that("'tzone' and 'tzone<-' methods work correctly", {
    ctpc <- Sys.time()
    ctpl <- as.POSIXlt(ctpc)
    ct <- as.tind(ctpc)
    cti <- tinterval(ct, ct + 1)
    expect_equal(tzone(ct), Sys.timezone())
    expect_equal(tzone(ctpc), Sys.timezone())
    expect_equal(tzone(ctpl), Sys.timezone())
    expect_equal(tzone(cti), Sys.timezone())
    for (tz in sample(OlsonNames(), min(5L, length(OlsonNames())))) {
        tzone(ct) <- tz
        tzone(ctpc) <- tz
        tzone(ctpl) <- tz
        tzone(cti) <- tz
        expect_equal(tzone(ct), tz)
        expect_equal(tzone(ctpc), tz)
        expect_equal(tzone(ctpl), tz)
        expect_equal(tzone(cti), tz)
        expect_equal(tzone(cti), tzone(cti$start))
        expect_equal(tzone(cti), tzone(cti$end))
        expect_equal(ct, as.tind(ctpc))
        expect_equal(ct, as.tind(ctpl))
    }

    types <- setdiff(.ti_type(long = FALSE), "t")
    for (tp in types) {
        xx <- tind(type = tp)
        expect_equal(tzone(xx), NULL)
        expect_error(tzone(xx) <- tz, errtype)
    }
    x <- 1
    expect_error(tzone(x), errclnt)
    expect_error(tzone(x) <- "UTC", errclnt)
    DD <- Sys.Date()
    expect_null(tzone(DD))
    expect_error(tzone(DD) <- "UTC", errimpl)
    expect_error(tzone(""))
    # NULL
    expect_null(tzone(today()))
})


# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("as.tzone method work correctly", {
    x <- 1
    expect_error(as.tzone(x, "UTC"), errclnt)
    x <- Sys.Date()
    expect_error(as.tzone(x, "UTC"), errimpl)

    NN <- 1000L
    tt <- round(as.numeric(Sys.time()) + (((1L - NN) %/% 2):((NN - 1L) %/% 2)) *
                           (3600 * 23 + 61.00001), digits = 2)
    xt <- as.tind(tt, type = "t")
    xplt <- as.POSIXlt(xt)
    xti <- tinterval(xt[1L:(NN %/% 2L)], xt[(1L + NN %/% 2L):NN])
    for (tz in setdiff(tzs, Sys.timezone())) {
        suppressWarnings(yt <- as.tzone(xt, tz))
        expect_equal(tzone(yt), tz)
        nna <- !is.na(yt)
        expect_equal(as.date(xt[nna]), as.date(yt[nna]))
        expect_equal(as.time(xt[nna]), as.time(yt[nna]))

        xt2 <- as.POSIXct(xt)
        suppressWarnings(yt2 <- as.tzone(xt2, tz))
        expect_equal(tzone(yt2), tz)
        nna <- !is.na(yt2)
        xpltn <- xplt[nna]
        ypltn <- as.POSIXlt(yt2[nna])
        expect_equal((xpltn$year + 1900) * 10000 + (xpltn$mon + 1) * 100 + xpltn$mday,
                     (ypltn$year + 1900) * 10000 + (ypltn$mon + 1) * 100 + ypltn$mday)
        expect_equal(xpltn$hour * 3600 + xpltn$min * 60 + xpltn$sec,
                     ypltn$hour * 3600 + ypltn$min * 60 + ypltn$sec)

        xt3 <- as.POSIXlt(xt)
        suppressWarnings(yt3 <- as.tzone(xt3, tz))
        expect_equal(tzone(yt3), tz)
        nna <- !is.na(yt3)
        xpltn <- xplt[nna]
        ypltn <- yt3[nna]
        expect_equal((xpltn$year + 1900) * 10000 + (xpltn$mon + 1) * 100 + xpltn$mday,
                     (ypltn$year + 1900) * 10000 + (ypltn$mon + 1) * 100 + ypltn$mday)
        expect_equal(xpltn$hour * 3600 + xpltn$min * 60 + xpltn$sec,
                     ypltn$hour * 3600 + ypltn$min * 60 + ypltn$sec)
    }
    # check corner case - DST change
    if ((tz <- "Europe/Warsaw") %in% tzs) {
        dtU <- date_time(20230326L, 0:3, tz = "UTC")
        dtW <- date_time(20230326L, c(0:1, 3), tz = tz)

        warn <- "NAs introduced; first position 3: 2023-03-26 02:00; time zone: Europe/Warsaw"
        expect_warning(res <- as.tzone(dtU, tz = tz), warn, fixed = TRUE)
        expect_equal(res, dtW[c(1L:2L, NA_integer_, 3L)])

        res <- as.tzone(dtU[1] %--% dtU[2], tz = tz)
        expect_equal(res, dtW[1] %--% dtW[2])
        res <- as.tzone(dtU[2] %--% dtU[4], tz = tz)
        expect_equal(res, dtW[2] %--% dtW[3])
        warn <- "end times adjusted; first position 1: 2023-03-26 02:00; time zone: Europe/Warsaw"
        expect_warning(res <- as.tzone(dtU[2] %--% dtU[3], tz = tz), warn, fixed = TRUE)
        expect_equal(res, dtW[2] %--% dtW[3])
        warn <- "starting times adjusted; first position 1: 2023-03-26 02:00; time zone: Europe/Warsaw"
        expect_warning(res <- as.tzone(dtU[3] %--% dtU[4], tz = tz), warn, fixed = TRUE)
        expect_equal(res, dtW[3] %--% dtW[3])
    }
})

