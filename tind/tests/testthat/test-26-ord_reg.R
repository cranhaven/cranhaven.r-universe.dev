context("ordered and regularly spaced time indices")
# ###################################################################

# test sample size
NN <- 200L
# test samples
y <- sample(1990L:2020L, NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
d <- pmin(sample.int(31L, size = NN, replace = TRUE),
          .days_in_month(.validate_ym(y, m)))
dd <- tind(y = y, m = m, d = d)

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("'is.ordered_t' method works correctly", {
    .isorderedt0 <- function (x) !anyNA(x) && !is.unsorted(x, strictly = TRUE)
    tt <- as.date_time(dd, "UTC")
    expect_equal(is.ordered_t(dd), .isorderedt0(dd))
    expect_equal(is.ordered_t(tt), .isorderedt0(tt))
    dd <- sort(dd)
    tt <- as.date_time(dd, "UTC")
    expect_equal(is.ordered_t(dd), .isorderedt0(dd))
    expect_equal(is.ordered_t(tt), .isorderedt0(tt))
    dd <- unique(dd)
    tt <- as.date_time(dd, "UTC")
    expect_equal(is.ordered_t(dd), TRUE)
    expect_equal(is.ordered_t(tt), TRUE)
    expect_equal(is.ordered_t(as.POSIXct(tt)), TRUE)
    dd[sample.int(length(dd), 1L)] <- NA
    tt <- as.date_time(dd, "UTC")
    expect_equal(is.ordered_t(dd), FALSE)
    expect_equal(is.ordered_t(tt), FALSE)
    expect_equal(is.ordered_t(as.Date(dd)), FALSE)
    expect_equal(is.ordered_t(as.POSIXct(dd)), FALSE)
    err <- paste0(dQuote("character"), " is not recognised as a class representing ",
                  "time indices")
    expect_error(is.ordered_t(as.character(dd)), err, fixed = TRUE)
})


errordexp <- "ordered time indices expected"
errresna <- "resolution cannot be determined"

test_that("'is.regular' and 'as.regular' work correctly", {
    # d, w, m, q, y
    for (tp in c("d", "w", "m", "q", "y")) {
        x0 <- seq(as.tind(dd[1L], tp), length.out = sample(NN:(2L * NN), 1L))
        expect_true(is.regular(x0))
        expect_true(is.regular(as.Date(x0)))
        expect_equal(as.regular(x0), x0)
        mlt <- setdiff(.mults(tp), 1)
        mlt <- mlt[mlt < 50]
        mlt <- if (length(mlt) == 1L) mlt else sample(mlt, 1L)
        x1 <- floor_t(x0, mlt)
        expect_false(is.regular(x1))
        expect_false(is.regular(as.Date(x1)))
        expect_error(as.regular(x1), errordexp, fixed = TRUE)
        x11 <- sample(x1, 1L)
        expect_true(is.regular(x11))
        expect_equal(as.regular(x11), x11)
        x2 <- unique(x1)
        expect_true(is.regular(x2))
        expect_true(is.regular(as.Date(x2)))
        expect_equal(as.regular(x2), x2)
        x3 <- x2[-sample(2L:(length(x2) - 1L), 1L)]
        expect_false(is.regular(x3))
        expect_false(is.regular(as.Date(x3)))
        x4 <- as.regular(x3)
        expect_equal(x4, x2)
        expect_equal(as.regular(x4), x4)
        expect_equal(as.regular(as.Date(x3)), as.date(x4))
        x5 <- x2[-sample(2L:(length(x2) - 1L), 1L)]
        expect_false(is.regular(x5))
        expect_false(is.regular(as.Date(x5)))
        x6 <- as.regular(x5)
        expect_equal(x6, x2)
        expect_equal(as.regular(x6), x6)
        for (hftp in setdiff(.hi_res_cast(tp), "t")) {
            x0a <- as.tind(x0, hftp)
            expect_true(is.regular(x0a))
            expect_equal(as.regular(x0a), x0a)
            x1a <- as.tind(x1, hftp)
            expect_false(is.regular(x1a))
            x2a <- as.tind(x2, hftp)
            expect_true(is.regular(x2a))
            expect_equal(as.regular(x2a), x2a)
            x3a <- as.tind(x3, hftp)
            expect_false(is.regular(x3a))
            x4a <- as.regular(x3a)
            expect_equal(x4a, x2a)
            expect_equal(as.regular(x4a), x4a)
            x5a <- as.tind(x5, hftp)
            x6a <- as.regular(x5a)
            expect_equal(x6a, x2a)
            expect_equal(as.regular(x6a), x6a)
        }
        tz <- sample(tzs, 1L)
        x0a <- as.tind(x0, tz = tz)
        expect_true(is.regular(x0a))
        expect_equal(as.regular(x0a), x0a)
        x1a <- as.tind(x1, tz = tz)
        expect_false(is.regular(x1a))
        x2a <- as.tind(x2, tz = tz)
        expect_true(is.regular(x2a))
        expect_equal(as.regular(x2a), x2a)
        x3a <- as.tind(x3, tz = tz)
        expect_false(is.regular(x3a))
        x4a <- as.regular(x3a)
        expect_equal(x4a, x2a)
        expect_equal(as.regular(x4a), x4a)
        x5a <- as.tind(x5, tz = tz)
        x6a <- as.regular(x5a)
        expect_equal(x6a, x2a)
        expect_equal(as.regular(x6a), x6a)
    }
    # t
    for (tz in tzs) {
        x00 <- as.tind("2025-03-29 23:00", tz = tz)
        for (mult in 1:3) {
            inc <- seq(0, by = 1, length.out = sample(NN:(2L * NN), 1L))
            inc <- as.tdiff(mult * inc, "h")
            x0 <- unique(floor_t(x00 + inc, as.tdiff(mult, "h")))
            isrx0 <- is.regular(x0)
            if ((mult == 1) || tz %in% c("UTC", "Etc/GMT+1"))
                expect_true(isrx0)
            if (isrx0) {
                expect_true(is.regular(as.POSIXct(x0)))
                expect_equal(as.regular(x0), x0)
                x1 <- x0[-sample(2L:(length(x0) - 1L), 1L)]
                expect_false(is.regular(x1))
                expect_false(is.regular(as.POSIXct(x1)))
                x2 <- as.regular(x1)
                expect_equal(x2, x0)
                expect_equal(as.regular(x2), x2)
                expect_equal(as.regular(as.POSIXct(x1)), x2)
            } else if ((tz == "Europe/Warsaw") && (mult == 2)){
                expect_error(as.regular(x0), "failed to construct regularly spaced time indices")
            }
        }
    }
    # h
    H <- sort(sample(0:23, 2L))
    H <- seq(H[1L], H[2L])
    M <- sort(sample(0:59, 2L))
    M <- seq(M[1L], M[2L])
    S <- sort(sample(0:59, 2L))
    S <- seq(S[1L], S[2L])
    x0 <- tind(H = H)
    expect_true(is.regular(x0))
    if (length(x0) >= 4L) {
        x1 <- x0[-sample(2L:(length(x0) - 1L), 1L)]
        expect_false(is.regular(x1))
        x2 <- as.regular(x1)
        expect_equal(x2, x0)
        expect_equal(as.regular(x2), x2)
    }
    x0 <- tind(H = H[1L], M = M)
    expect_true(is.regular(x0))
    if (length(x0) >= 4L) {
        x1 <- x0[-sample(2L:(length(x0) - 1L), 1L)]
        expect_false(is.regular(x1))
        x2 <- as.regular(x1)
        expect_equal(x2, x0)
        expect_equal(as.regular(x2), x2)
    }
    x0 <- tind(H = H[1L], M = M[1L], S = S)
    expect_true(is.regular(x0))
    if (length(x0) >= 4L) {
        x1 <- x0[-sample(2L:(length(x0) - 1L), 1L)]
        expect_false(is.regular(x1))
        x2 <- as.regular(x1)
        expect_equal(x2, x0)
        expect_equal(as.regular(x2), x2)
    }
    # i
    x0 <- as.tind(seq(sample(NN, 1L), by = 1, length.out = sample(NN:(2L * NN), 1L)), "i")
    expect_true(is.regular(x0))
    expect_equal(as.regular(x0), x0)
    mlt <- sample(2L:7L, 1L)
    x1 <- floor_t(x0, mlt)
    expect_false(is.regular(x1))
    expect_error(as.regular(x1), errordexp, fixed = TRUE)
    x11 <- sample(x1, 1L)
    expect_true(is.regular(x11))
    expect_equal(as.regular(x11), x11)
    x2 <- unique(x1)
    expect_true(is.regular(x2))
    expect_equal(as.regular(x2), x2)
    x3 <- x0[-sample(2L:(length(x2) - 1L), 1L)]
    expect_false(is.regular(x3))
    x4 <- as.regular(x3)
    expect_equal(x4, x0)
    expect_equal(as.regular(x4), x4)
    x5 <- x2[-sample(2L:(length(x2) - 1L), 1L)]
    expect_false(is.regular(x5))
    x6 <- as.regular(x5)
    expect_equal(x6, x2)
    expect_equal(as.regular(x6), x6)
    # n
    x0na <- as.tind(1 / (10:1), "n")
    expect_false(is.regular(x0na))
    expect_error(as.regular(x0na), errresna, fixed = TRUE)
    mlt0 <- sample(4L:10L, 1L)
    mlt <- sample(2L:(mlt0 - 1L), 1L)
    x0 <- as.tind(seq(sample(NN, 1L) / mlt0, by = 1 / mlt0, length.out = sample(NN:(2L * NN), 1L)), "n")
    expect_true(is.regular(x0))
    expect_equal(as.regular(x0), x0)
    x1 <- floor_t(x0, mlt / mlt0)
    expect_false(is.regular(x1))
    expect_error(as.regular(x1), errordexp, fixed = TRUE)
    x11 <- sample(x1, 1L)
    expect_true(is.regular(x11))
    expect_equal(as.regular(x11), x11)
    x2 <- unique(x1)
    expect_true(is.regular(x2))
    expect_equal(as.regular(x2), x2)
    x3 <- x0[-sample(2L:(length(x2) - 1L), 1L)]
    expect_false(is.regular(x3))
    x4 <- as.regular(x3)
    expect_equal(x4, x0)
    expect_equal(as.regular(x4), x4)
    x5 <- x2[-sample(2L:(length(x2) - 1L), 1L)]
    expect_false(is.regular(x5))
    x6 <- as.regular(x5)
    expect_equal(x6, x2)
    expect_equal(as.regular(x6), x6)
})



test_that("'extend_regular' work correctly", {
    warnnrg <- "original time indices are not regular"
    # d, w, m, q, y
    for (tp in c("d", "w", "m", "q", "y")) {
        x0 <- seq(as.tind(dd[1L], tp), length.out = sample(NN:(2L * NN), 1L))
        mlt <- .mults(tp)
        mlt <- mlt[mlt < 50]
        mlt <- sample(mlt, 1L)
        x0 <- unique(floor_t(x0, mlt))
        MM <- sample(2L:50L, 1L)
        enext0 <- extend_regular(x0, MM)
        eprev0 <- extend_regular(x0, -MM)
        expect_equal(length(enext0), MM)
        expect_equal(length(eprev0), MM)
        expect_equal(resolution_t(enext0), resolution_t(x0))
        expect_equal(resolution_t(eprev0), resolution_t(x0))
        expect_true(is.regular(c(x0[length(x0)], enext0)))
        expect_true(is.regular(c(eprev0, x0[1L])))
        expect_equal(extend_regular(x0, 0), x0[0L])
        x1 <- x0[-sample(2L:(length(x0) - 1L), 1L)]
        expect_warning(enext1 <- extend_regular(x1, MM), warnnrg, fixed = TRUE)
        expect_warning(eprev1 <- extend_regular(x1, -MM), warnnrg, fixed = TRUE)
        expect_equal(enext1, enext0)
        expect_equal(eprev1, eprev0)
        for (hftp in setdiff(.hi_res_cast(tp), "t")) {
            x0a <- as.tind(x0, hftp)
            enext0a <- extend_regular(x0a, MM)
            eprev0a <- extend_regular(x0a, -MM)
            expect_equal(enext0a, as.tind(enext0, hftp))
            expect_equal(eprev0a, as.tind(eprev0a, hftp))
        }
        tz <- sample(tzs, 1L)
        x0a <- as.tind(x0, tz = tz)
        enext0a <- extend_regular(x0a, MM)
        expect_equal(enext0a, as.tind(enext0, tz = tz))
        if (eprev0[1L] >= as.year(1923)) {
            eprev0a <- extend_regular(x0a, -MM)
            expect_equal(eprev0a, as.tind(eprev0a, tz = tz))
        }
    }
    # errors
    expect_error(extend_regular(rev(x0), MM), errordexp, fixed = TRUE)
    errlen0 <- "argument of length 0"
    expect_error(extend_regular(x0[0L], MM), errlen0, fixed = TRUE)
    errn <- paste("invalid", sQuote("n"), "argument; single non-NA integer expected")
    expect_error(extend_regular(x0, 1:2), errn, fixed = TRUE)
    expect_error(extend_regular(x0, NA_integer_), errn, fixed = TRUE)
    errfail <- "failed to regularly extend time indices"
    # t
    tz <- "UTC"
    x0 <- date_time("2025-03-30", H = c(4:8), tz = tz)
    x1 <- date_time("2025-03-30", H = c(9:12), tz = tz)
    expect_equal(extend_regular(x0, 4), x1)
    x1 <- date_time("2025-03-30", H = c(0:3), tz = tz)
    expect_equal(extend_regular(x0, -4), x1)
    x0 <- date_time("2025-03-30", H = c(4, 6, 8), tz = tz)
    x1 <- date_time("2025-03-30", H = c(10, 12, 14, 16), tz = tz)
    expect_equal(extend_regular(x0, 4), x1)
    x1 <- date_time("2025-03-30", H = c(0, 2), tz = tz)
    expect_equal(extend_regular(x0, -2), x1)
    if ((tz <- "Europe/Warsaw") %in% OlsonNames()) {
        x0 <- date_time("2025-03-30", H = c(4:8), tz = tz)
        x1 <- date_time("2025-03-30", H = c(9:12), tz = tz)
        expect_equal(extend_regular(x0, 4), x1)
        x1 <- date_time("2025-03-30", H = c(0:1, 3), tz = tz)
        expect_equal(extend_regular(x0, -3), x1)
        x0 <- date_time("2025-03-30", H = c(4, 6, 8), tz = tz)
        x1 <- date_time("2025-03-30", H = c(10, 12, 14, 16), tz = tz)
        expect_equal(extend_regular(x0, 4), x1)
        expect_error(extend_regular(x0, -2), errfail, fixed = TRUE)
        x0 <- date_time("2025-03-30", H = c(12, 18), tz = tz)
        x1 <- date_time("2025-03-30", H = c(0, 6), tz = tz)
        expect_equal(extend_regular(x0, -2), x1)
    }
    # h
    x0 <- tind(H = 20:22)
    expect_equal(extend_regular(x0, 2), tind(H = 23:24))
    expect_error(extend_regular(x0, 3), errfail, fixed = TRUE)
    x0 <- tind(H = 0, M = 15 * (1:3))
    expect_equal(extend_regular(x0, -1), tind(H = 0))
    expect_error(extend_regular(x0, -2), errfail, fixed = TRUE)
    # i
    x0 <- as.tind(1:NN, "i")
    expect_equal(extend_regular(x0, MM), as.tind((NN + 1L):(NN + MM), "i"))
    expect_equal(extend_regular(x0, -MM), as.tind((-MM + 1L):0L, "i"))
    # n
    x0 <- as.tind(1 / (10:1), "n")
    expect_error(extend_regular(rev(x0), MM), errordexp, fixed = TRUE)
    expect_error(extend_regular(x0, MM), errresna, fixed = TRUE)
    MM <- sample(2L:50L, 1L)
    x0 <- as.tind(1:NN / MM, "n")
    expect_equal(extend_regular(x0, MM), as.tind((NN + 1L):(NN + MM) / MM, "n"))
})

