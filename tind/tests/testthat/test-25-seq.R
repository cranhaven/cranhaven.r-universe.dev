context("seq method")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("'seq' method works correctly", {
    err <- paste(sQuote("from"), "and", sQuote("to"), "arguments missing")
    expect_error(seq.tind(), err, fixed = TRUE)
    expect_error(seq.tind(length.out = 5), err, fixed = TRUE)
    expect_error(seq.tind(by = 1, length.out = 5), err, fixed = TRUE)

    from0 <- "2024-11-08"
    from1 <- as.tind("2024-11-08")
    to0 <- "2025-01-19"
    to1 <- as.tind("2025-01-19")
    err <- paste("invalid", sQuote("from"), "argument; single non-NA index expected")
    expect_error(seq.tind(from = from0[c(1L, 1L)]), err, fixed = TRUE)
    expect_error(seq.tind(from0[c(1L, 1L)]), err, fixed = TRUE)
    expect_error(seq(from = from1[NA_integer_]), err, fixed = TRUE)
    expect_error(seq(from1[NA_integer_]), err, fixed = TRUE)
    err <- paste("invalid", sQuote("to"), "argument; single non-NA index expected")
    expect_error(seq.tind(to = to0[c(1L, 1L)]), err, fixed = TRUE)
    expect_error(seq(to = to1[NA_integer_]), err, fixed = TRUE)

    err <- paste(sQuote("from"), "and", sQuote("length.out"), "arguments missing")
    expect_error(seq.tind(to = to0), err, fixed = TRUE)
    expect_error(seq(to = to1), err, fixed = TRUE)
    err <- paste(sQuote("to"), "and", sQuote("length.out"), "arguments missing")
    expect_error(seq.tind(from = from0), err, fixed = TRUE)
    expect_error(seq.tind(from0), err, fixed = TRUE)
    expect_error(seq(from = from1), err, fixed = TRUE)
    expect_error(seq(from1), err, fixed = TRUE)

    err <- paste0(sQuote("from"), ", ", sQuote("to"), ", and ", sQuote("length.out"),
                  " arguments provided")
    expect_error(seq.tind(from = from0, to = to0, length.out = 7L), err, fixed = TRUE)
    expect_error(seq.tind(from0, to0, length.out = 7L), err, fixed = TRUE)
    expect_error(seq(from = from1, to = to1, length.out = 7L), err, fixed = TRUE)
    expect_error(seq(from1, to1, length.out = 7L), err, fixed = TRUE)

    err <- paste("invalid", sQuote("length.out"), "argument; non-negative integer expected")
    expect_error(seq.tind(from = from0, length.out = 1L:2L), err, fixed = TRUE)
    expect_error(seq.tind(from0, length.out = NA), err, fixed = TRUE)
    expect_error(seq.tind(from0, length.out = -1L), err, fixed = TRUE)
    expect_error(seq(from = from1, length.out = 1L:2L), err, fixed = TRUE)
    expect_error(seq(from1, length.out = NA), err, fixed = TRUE)
    expect_error(seq.tind(to = to0, length.out = 1L:2L), err, fixed = TRUE)
    expect_error(seq(to = to1, length.out = 1L:2L), err, fixed = TRUE)
    expect_error(seq(to = to1, length.out = -2L), err, fixed = TRUE)

    err <- paste("invalid", sQuote("by"), "argument; single non-NA value expected")
    expect_error(seq(from1, to1, by = as.tdiff(NA_integer_, "m")), err, fixed = TRUE)
    expect_error(seq(from1, to1, by = NA_integer_), err, fixed = TRUE)
    expect_error(seq(from1, to1, by = Inf), err, fixed = TRUE)
    expect_error(seq(from1, to1, by = "1x"))

    # from
    lo <- sample(5L:15L, 1L)
    sD <- seq(as.Date(from0), by = 1, length.out = lo)
    expect_equal(seq.tind(from0, by = 1, length.out = lo), as.tind(sD))
    expect_equal(seq(from1, by = 1, length.out = lo), as.tind(sD))
    expect_equal(seq(from1, by = "1d", length.out = lo), as.tind(sD))
    expect_equal(seq(from1, by = 1, along.with = complex(lo)), as.tind(sD))
    expect_equal(seq(from1, by = 1, length.out = lo - 1L, along.with = complex(lo)), as.tind(sD))
    sD <- seq(as.Date(from0), by = "1 month", length.out = lo)
    expect_equal(seq.tind(from0, by = "1m", length.out = lo), as.tind(sD))
    expect_equal(seq(from1, by = "1 month", length.out = lo), as.tind(sD))
    expect_equal(seq(from1, by = "m", length.out = lo), as.tind(sD))
    sD <- seq(as.Date(from0), by = "-2 months", length.out = lo)
    expect_equal(seq.tind(from0, by = "-2m", length.out = lo), as.tind(sD))
    expect_equal(seq(from1, by = "-2 months", length.out = lo), as.tind(sD))
    expect_equal(seq(from1, by = "-2m", length.out = lo), as.tind(sD))

    # to
    lo <- sample(5L:15L, 1L)
    sD <- rev(seq(as.Date(to0), by = -1, length.out = lo))
    expect_equal(seq.tind(to = to0, by = 1, length.out = lo), as.tind(sD))
    expect_equal(seq(to = to1, by = 1, length.out = lo), as.tind(sD))
    expect_equal(seq(to = to1, by = "1d", length.out = lo), as.tind(sD))
    expect_equal(seq(to = to1, by = 1, along.with = double(lo)), as.tind(sD))
    expect_equal(seq(to = to1, by = 1, length.out = lo - 1L, along.with = double(lo)), as.tind(sD))
    sD <- rev(seq(as.Date(to0), by = "-1 week", length.out = lo))
    expect_equal(seq.tind(to = to0, by = "1w", length.out = lo), as.tind(sD))
    expect_equal(seq(to = to1, by = "1 week", length.out = lo), as.tind(sD))
    expect_equal(seq(to = to1, by = "w", length.out = lo), as.tind(sD))
    sD <- rev(seq(as.Date(to0), by = "2 weeks", length.out = lo))
    expect_equal(seq.tind(to = to0, by = "-2w", length.out = lo), as.tind(sD))
    expect_equal(seq(to = to1, by = "-2 weeks", length.out = lo), as.tind(sD))
    expect_equal(seq(to = to1, by = "-2w", length.out = lo), as.tind(sD))


    # from, to
    err <- paste(sQuote("by"), "argument equals 0")
    expect_error(seq(from1, to1, by = 0), err, fixed = TRUE)
    expect_error(seq(from1, to1, by = as.tdiff(0, "w")), err, fixed = TRUE)
    err <- paste("wrong sign in", sQuote("by"), "argument")
    expect_error(seq(from1, to1, by = -1), err, fixed = TRUE)
    expect_error(seq(from1, to1, by = "-2w"), err, fixed = TRUE)
    expect_error(seq(from1, to1, by = "-1m"), err, fixed = TRUE)
    expect_error(seq(to1, from1, by = 1), err, fixed = TRUE)
    expect_error(seq(to1, from1, by = "2w"), err, fixed = TRUE)
    expect_error(seq(to1, from1, by = "1m"), err, fixed = TRUE)
    sD <- seq(as.Date(from0), as.Date(to0), by = 1)
    expect_equal(seq(from1, to1, by = 1), as.tind(sD))
    expect_equal(seq(from1, to1, by = "1d"), as.tind(sD))
    sD <- seq(as.Date(from0), as.Date(to0), by = "1 month")
    expect_equal(seq(from1, to1, by = "1m"), as.tind(sD))
    sD <- seq(as.Date(to0), as.Date(from0), by = "-2 weeks")
    expect_equal(seq(to1, from1, by = as.tdiff(-2, "w")), as.tind(sD))

    # from, to with conversion
    s0 <- seq(as.month(from1), to1, by = "2w")
    s1 <- seq(floor_t(from1, "m"), to1, by = "2w")
    expect_equal(s0, s1)
    s0 <- seq(to1, as.month(from1), by = as.tdiff(-2, "w"))
    s1 <- seq(to1, floor_t(from1, "m"), by = as.tdiff(-2, "w"))
    expect_equal(s0, s1)
    expect_true(tail(s0, 1L) >= as.month(from1))
    expect_false(tail(s0, 1L) %-w% 2 >= as.month(from1))

    # from, to with different tz
    if (length(tzs) > 1L) {
        tzs12 <- sample(tzs, 2L)
        tz1 <- tzs12[1L]
        tz2 <- tzs12[2L]
        warn <- "^different time zones of arguments"
        froma <- as.tind(1e9, tz = tz1)
        fromb <- as.tind(1e9, tz = tz2)
        toa <- as.tind(1e9 + 13000, tz = tz1)
        tob <- as.tind(1e9 + 13000, tz = tz2)
        s0 <- froma + seq(0, 13000, by = 20)
        s1 <- seq(froma, toa, by = "20s")
        expect_warning(s2 <- seq(froma, tob, by = "20s"), warn)
        expect_equal(s0, s1)
        expect_equal(s0, s2)
        s0 <- froma + seq(0, 13000, by = 1200)
        s1 <- seq(froma, toa, by = "20min")
        expect_warning(s2 <- seq(froma, tob, by = as.tdiff(20, "min")), warn)
        expect_equal(s0, s1)
        expect_equal(s0, s2)
        s0 <- tob - seq(0, 13000, by = 10)
        s1 <- seq(tob, fromb, by = "-10s")
        expect_warning(s2 <- seq(tob, froma, by = "-10s"), warn)
        expect_equal(s0, s1)
        expect_equal(s0, s2)
        s0 <- tob - seq(0, 13000, by = 7200)
        s1 <- seq(tob, fromb, by = as.tdiff(-2, "h"))
        expect_warning(s2 <- seq(tob, froma, by = as.tdiff(-2, "h")), warn)
        expect_equal(s0, s1)
        expect_equal(s0, s2)
    }

    # corner case - trimming results
    s1 <- seq(as.tind("2025-12-30 23:00", tz = "UTC"), "2025", by = "5h")
    s2 <- seq(as.tind("2025"), as.tind("2025-12-30 23:00", tz = "UTC"), by = "-5h")
    expect_equal(s1, rev(s2))
    expect_true(all(s1 %in_t% "2025"))
})








