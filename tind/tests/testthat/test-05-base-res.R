context("base - resolution of time indices")
# ###################################################################

# supported units of time
units <- c("y", "q", "m", "w", "d", "h", "min", "s")


test_that("'.mults' and '.check_mult' work correctly", {
    .divisors <- function(n)
    {
        dv <- 1L:(n - 1L)
        return (dv[!(n %% dv)])
    }
    for (u in units) {
        if (u == "y") {
            expect_equal(.mults(u),
                             as.integer(c(1, 2, 5, 10^(1:2) %x% c(1, 2, 2.5, 5), 10^3)))
        } else if (u == "d") {
            expect_equal(.mults(u), c(1L, 15L))
            expect_equal(.mults(u, TRUE), c(1L:3L, 7L, 15L, 30L))
        } else if (u == "s") {
            expect_equal(.mults(u),
                             c(round(10^(-6:-1) %x% c(1, 2, 5), 6L), .divisors(60L)))
            expect_equal(.mults(u, TRUE),
                             c(round(10^(-6:-1) %x% c(1, 2, 5), 6L), .divisors(60L), 60L))
        } else {
            ny <- switch(u, q = 4L, m = 12L, w = 52L, h = 24L, min = 60L)
            expect_equal(.mults(u), .divisors(ny))
            expect_equal(.mults(u, TRUE), c(.divisors(ny), ny))
        }
        f <- sample(.mults(u), 1L)
        expect_silent(.check_mult(f, u))
        f <- sample(c(17, 29, 31), 1L)
        err <- paste0("^invalid multiplier for time unit ",
                      dQuote(u), " \\([- a-z]+\\)",
                      "; admissible values: (0\\.)?[-e0-9]+(, (0\\.)?[-e0-9]+)+$")
        expect_error(.check_mult(f, u), err)
    }
})


# test sample size
NN <- 100L
# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test sample
yy <- sample(0L:9999L, NN, replace = TRUE)
yy[2L] <- 3L # this forces resolution 1

test_that("'.res_y' works correctly", {
    expect_equal(.res_y(yy[1L]), list(n = 1L, unit = "y"))
    expect_equal(.res_y(2000L), list(n = 1L, unit = "y"))
    expect_equal(.res_y(NA_integer_), list(n = 1L, unit = "y"))
    for (f in .mults("y")) {
        yy_ <- .floor_y(yy, f)
        expect_equal(.res_y(yy_), list(n = f, unit = "y"))
    }
})


# test sample
q <- sample.int(4L, size = NN, replace = TRUE)
qq <- .validate_yq(yy, q)
qq[2L] <- .validate_yq(2022, 4) # this forces resolution 1

test_that("'.res_q' works correctly", {
    expect_equal(.res_q(qq[1L]), list(n = 1L, unit = "q"))
    expect_equal(.res_q(.validate_yq(2000, 1)), list(n = 1L, unit = "q"))
    expect_equal(.res_q(NA_integer_), list(n = 1L, unit = "q"))
    for (f in .mults("q")) {
        qq_ <- .floor_q(qq, f)
        expect_equal(.res_q(qq_), list(n = f, unit = "q"))
    }
    for (f in .mults("y")) {
        qq_ <- .y2q(.floor_y(yy, f))
        expect_equal(.res_q(qq_), list(n = f, unit = "y"))
    }
})


# test sample
m <- sample.int(12L, size = NN, replace = TRUE)
mm <- .validate_ym(yy, m)
mm[2L] <- .validate_ym(2022, 12) # this forces resolution 1

test_that("'.res_m' works correctly", {
    expect_equal(.res_m(mm[1L]), list(n = 1L, unit = "m"))
    expect_equal(.res_m(200001L), list(n = 1L, unit = "m"))
    expect_equal(.res_m(NA_integer_), list(n = 1L, unit = "m"))
    for (f in .mults("m")) {
        mm_ <- .floor_m(mm, f)
        if (f %% 3L) expect_equal(.res_m(mm_), list(n = f, unit = "m"))
        else expect_equal(.res_m(mm_), list(n = f %/% 3L, unit = "q"))
    }
    for (f in .mults("q")) {
        mm_ <- .q2m(.floor_q(qq, f))
        expect_equal(.res_m(mm_), list(n = f, unit = "q"))
    }
    for (f in .mults("y")) {
        mm_ <- .y2m(.floor_y(yy, f))
        expect_equal(.res_m(mm_), list(n = f, unit = "y"))
    }
})


# test sample
w <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(yy))
ww <- .validate_yw(yy, w)
ww[2L] <- .validate_yw(2022, 52) # this forces resolution 1

test_that("'.res_w' works correctly", {
    expect_equal(.res_w(ww[1L]), list(n = 1L, unit = "w"))
    expect_equal(.res_w(200001L), list(n = 1L, unit = "w"))
    expect_equal(.res_w(NA_integer_), list(n = 1L, unit = "w"))
    for (f in .mults("w")) {
        ww_ <- .floor_w(ww, f)
        expect_equal(.res_w(ww_), list(n = f, unit = "w"))
    }
    for (f in .mults("y")) {
        ww_ <- .y2w(.floor_y(yy, f))
        expect_equal(.res_w(ww_), list(n = f, unit = "y"))
    }
})


# test sample
d <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(mm))
dd <- .validate_ymd(yy, m, d)
dd[2L] <- .validate_ymd(2022, 4, 23) # this forces resolution 1

test_that("'.res_d' works correctly", {
    expect_equal(.res_d(dd[1L]), list(n = 1L, unit = "d"))
    expect_equal(.res_d(.validate_ymd(2000, 1, 1L)), list(n = 1L, unit = "d"))
    expect_equal(.res_d(NA_integer_), list(n = 1L, unit = "d"))
    for (f in c(1L, 15L)) {
        dd_ <- .floor_d(dd, f)
        expect_equal(.res_d(dd_), list(n = f, unit = "d"))
    }
    dd_ <- .floor_d(dd, 7L)
    expect_equal(.res_d(dd_), list(n = 1L, unit = "w"))
    for (f in .mults("w")) {
        dd_ <- .w2d(.floor_w(ww, f))
        expect_equal(.res_d(dd_), list(n = f, unit = "w"))
    }
    for (f in .mults("m")) {
        dd_ <- .m2d(.floor_m(mm, f))
        if (f %% 3L) expect_equal(.res_d(dd_), list(n = f, unit = "m"))
        else expect_equal(.res_d(dd_), list(n = f %/% 3L, unit = "q"))
    }
    for (f in .mults("q")) {
        dd_ <- .q2d(.floor_q(qq, f))
        expect_equal(.res_d(dd_), list(n = f, unit = "q"))
    }
    for (f in .mults("y")) {
        dd_ <- .y2d(.floor_y(yy, f))
        expect_equal(.res_d(dd_), list(n = f, unit = "y"))
    }
    # special case...
    expect_equal(.res_d(c(.validate_ymd(2021, 2, 1),
                              .validate_ymd(2021, 2, 15),
                              .validate_ymd(2021, 3, 1))),
                     list(n = 2L, unit = "w"))
})


# test sample
tt <- round(1650002399 + (0:(NN - 1L)) * (3600 * 23 + 61.111111), digits = 6)

test_that("'.res_t' works correctly", {
    for (tz in tzs) {
        for (f in sample(.mults("s")[.mults("s") < 1], 5L)) {
            tt_ <- .floor_t_s(tt, f, tz)
            expect_equal(.res_t(tt_, tz), list(n = f, unit = "s"))
            expect_equal(.res_t(tt_[10L], tz), list(n = f, unit = "s"))
            expect_equal(.res_t(tt_[1L], tz), list(n = 1., unit = "s"))
            expect_equal(.res_t(NA_real_, tz), list(n = 1., unit = "s"))
            expect_true(is.double(.res_t(NA_real_, tz)$n))
        }
        for (f in sample(.mults("s")[.mults("s") >= 1], 5L)) {
            tt_ <- .floor_t_s(tt, f, tz)
            expect_equal(.res_t(tt_, tz), list(n = f, unit = "s"))
            expect_equal(.res_t(tt_[1L], tz), list(n = 1., unit = "s"))
            expect_true(is.double(.res_t(tt_[1L], tz)$n))
        }
        for (f in sample(.mults("min"), 5L)) {
            tt_ <- .floor_t_min(tt, f, tz)
            expect_equal(.res_t(tt_, tz), list(n = f, unit = "min"))
            expect_equal(.res_t(tt_[1L], tz), list(n = 1L, unit = "min"))
        }
        for (f in sample(.mults("h"), 5L)) {
            tt_ <- .floor_t_h(tt, f, tz)
            expect_equal(.res_t(tt_, tz), list(n = f, unit = "h"))
            expect_equal(.res_t(tt_[1L], tz), list(n = 1L, unit = "h"))
        }
        dd <- .floor_d(.t2d(tt, tz), sample(.mults("d"), 1L))
        tt_ <- .d2t(dd, tz)
        expect_equal(.res_t(tt_, tz), .res_d(dd))
    }
})


# test sample
hh <- round(tt %% 86400, digits = 6)

test_that("'.res_h' works correctly", {
    for (f in sample(.mults("s")[.mults("s") < 1], 5L)) {
        hh_ <- .floor_h_s(hh, f)
        expect_equal(.res_h(hh_), list(n = f, unit = "s"))
        expect_equal(.res_h(hh_[10L]), list(n = f, unit = "s"))
        expect_equal(.res_h(hh_[1L]), list(n = 1, unit = "s"))
        expect_equal(.res_h(NA_real_), list(n = 1., unit = "s"))
    }
    for (f in sample(.mults("s")[.mults("s") >= 1], 5L)) {
        hh_ <- .floor_h_s(hh, f)
        expect_equal(.res_h(hh_), list(n = f, unit = "s"))
        expect_equal(.res_h(hh_[1L]), list(n = 1, unit = "s"))
    }
    for (f in sample(.mults("min"), 5L)) {
        hh_ <- .floor_h_min(hh, f)
        expect_equal(.res_h(hh_), list(n = f, unit = "min"))
        expect_equal(.res_h(hh_[1L]), list(n = 1, unit = "min"))
    }
    for (f in sample(.mults("h"), 5L)) {
        hh_ <- .floor_h_h(hh, f)
        expect_equal(.res_h(hh_), list(n = f, unit = "h"))
        expect_equal(.res_h(hh_[1L]), list(n = 1, unit = "h"))
    }
    # corner cases
    expect_equal(.res_h(c(0, 0)), list(n = 1L, unit = "h"))
    expect_equal(.res_h(c(0, 86400)), list(n = 1L, unit = "h"))
})


test_that("'.res_i' works correctly", {
    expect_equal(.res_i(10L), 1L)
    expect_equal(.res_i(integer()), 1L)
    expect_equal(.res_i(NA_integer_), 1L)
    ii <- c(NA_integer_, NA_integer_, sample.int(10L, NN, replace = TRUE), 1L, NA_integer_)
    ml <- c(1L:3L, 5L, 7L:10L, 13L, 17L, 23L)
    for (m in ml) {
        expect_equal(.res_i(m * ii), m)
    }
    expect_equal(.res_i(c(NA_integer_, rep(0L, NN), NA_integer_)), 1L)
})


test_that("'.res_n' works correctly", {
    expect_equal(.res_n(numeric()), NA_real_)
    expect_equal(.res_n(0.), NA_real_)
    expect_equal(.res_n(1.), 1.)
    expect_equal(.res_n(1.5), 1.5)
    xx <- 1e9 * c(1, 2^65)
    expect_equal(.res_n(xx), 1e9)
    xx <- 1e9 * c(-1, 7, 14, 21, 49)
    expect_equal(.res_n(xx), 1e9)
    expect_equal(.res_n(xx), .res_n(xx * sample(c(-1, 1), length(xx), replace = TRUE)))
    expect_equal(.res_n(xx), .res_n(sample(xx)))
    expect_equal(.res_n(xx), .res_n(c(xx[1L], xx)))
    expect_equal(.res_n(xx), .res_n(c(xx[1L], xx, NA)))
    xx <- 1e-6 * c(-7, 7, 14, 21, 49)
    expect_equal(.res_n(xx), 7e-6)
    expect_equal(.res_n(xx), .res_n(xx * sample(c(-1, 1), length(xx), replace = TRUE)))
    expect_equal(.res_n(xx), .res_n(sample(xx)))
    expect_equal(.res_n(xx), .res_n(c(xx[1L], xx)))
    expect_equal(.res_n(xx), .res_n(c(xx[1L], xx, NA)))
    xx <- c(1/2, 1/3, 1/4, 1/7)
    expect_equal(.res_n(xx), NA_real_)
    xx <- xx[-4L]
    expect_equal(.res_n(xx), 1/12)
    expect_equal(.res_n(xx[-2L]), 1/4)
})

