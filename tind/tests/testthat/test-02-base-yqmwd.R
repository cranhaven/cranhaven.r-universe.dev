# test sample size
NN <- 100L
nms <- sample(letters, NN, replace = TRUE)
# NN <- 1000L
# nms <- NULL


context("base - years")
# ###################################################################

# test sample
yy <- sample(1800L:2100L, NN, replace = TRUE)
names(yy) <- nms


.valid_y0 <- function(y) (!is.na(y) & (y >= 0L) & (y <= 9999L))

test_that("'.validate_y' works correctly", {
    yyv <- sample(-1000L:20000L, NN)
    names(yyv) <- nms
    yy <- .validate_y(yyv)
    expect_equal(unname(!is.na(yy)), unname(.valid_y0(yyv)))
    expect_equal(unname(yy[!is.na(yy)]), unname(yyv[.valid_y0(yyv)]))
    expect_equal(names(yy), nms)
})


test_that("'.is.leap_year' works correctly", {
    .is.leap_year0 <- function(y) (!(y %% 4) & ((y %% 100) | !(y %% 400)))
    ly0 <- 365L == as.integer(as.Date(paste0(yy, "-12-31")) -
                              as.Date(paste0(yy, "-01-01")))
    ly <- .is.leap_year(yy)
    expect_equal(unname(ly), ly0)
    expect_equal(.is.leap_year(yy), .is.leap_year0(yy))
    expect_equal(.is.leap_year(NA_integer_), NA)
})


test_that("'.y2char' works correctly", {
    yychar <- .y2char(yy)
    expect_true(all(grepl("^[0-9]{4}$", yychar)))
    expect_equal(as.integer(yychar), unname(yy))
    expect_equal(.y2char(NA_integer_), NA_character_)
    expect_equal(names(yychar), nms)
})


test_that("'.y2yf' works correctly", {
    yyf <- .y2yf(yy)
    expect_equal(unname(yyf), as.double(yy))
    expect_equal(names(yyf), nms)
    expect_equal(.y2yf(NA_integer_), NA_real_)
})


test_that("'.floor_y' and '.ceiling_y' work correctly", {
    ns <- as.integer(c(10^(0:2) %x% c(1, 2, 5), 10^3))
    for (n in ns) {
        fy <- .floor_y(yy, n)
        cy <- .ceiling_y(yy, n)
        expect_equal(.floor_y(fy, n), fy)
        expect_equal(.ceiling_y(cy, n), cy)
        expect_true(all(fy %% n == 0L))
        expect_true(all(cy %% n == 0L))
        expect_true(all(fy <= yy & yy <= cy))
        expect_equal(fy == yy, yy == cy)
        if (n == 1L) {
            expect_equal(fy, yy)
            expect_equal(cy, yy)
        } else {
            expect_equal(unname(fy %% n), rep(0L, NN))
            expect_equal(unname(cy %% n), rep(0L, NN))
        }
    }
})



context("base - quarters")
# ###################################################################

# test sample
q <- sample.int(4L, size = NN, replace = TRUE)
qq <- 4L * yy + q - 1L
names(qq) <- nms

.valid_q0 <- function(q) .valid_y0(q %/% 4L)


test_that("'.validate_q' and '.validate_yq' work correctly", {
    qqv <- sample(-100L:10100L, NN, replace = TRUE) * 4L +
           sample(0L:9L, size = NN, replace = TRUE)
    qqv[(NN - 3L):NN] <- 0L:3L
    qq <- .validate_q(qqv)
    expect_equal(!is.na(qq), .valid_q0(qqv))
    expect_equal(qq[!is.na(qq)], qqv[.valid_q0(qqv)])
    expect_equal(.validate_yq(qqv %/% 4L, qqv %% 4L + 1L), qq)
})


test_that("'.q2char' works correctly", {
    qqchar <- .q2char(qq)
    expect_true(all(grepl("^[0-9]{4}Q[0-9]$", qqchar)))
    expect_equal(as.integer(sub("Q[0-9]$", "", qqchar)), as.integer(yy))
    expect_equal(as.integer(sub("^[0-9]{4}Q", "", qqchar)), as.integer(q))
    expect_equal(.q2char(NA_integer_), NA_character_)
    expect_equal(names(qqchar), nms)
})


test_that("'.q2qrtr' works correctly", {
    expect_equal(unname(.q2qrtr(qq)), q)
})


test_that("'.q2yf' works correctly", {
    expect_equal(.q2yf(qq), yy + (q - 1) / 4)
    expect_equal(.q2yf(NA_integer_), NA_real_)
})


test_that("'.q2y' works correctly", {
    expect_equal(.q2y(qq), yy)
})


test_that("'.y2q' works correctly", {
    expect_equal(unname(.q2char(.y2q(yy))), paste0(.y2char(yy), "Q1"))
})


test_that("'.floor_q' and '.ceiling_q' work correctly", {
    qq <- pmin(qq, .validate_yq(9998, 4))
    ns <- c(1L:2L, 4L)
    for (n in ns) {
        fq <- .floor_q(qq, n)
        cq <- .ceiling_q(qq, n)
        expect_true(all(.valid_q0(fq)))
        expect_true(all(.valid_q0(cq)))
        expect_equal(.floor_q(fq, n), fq)
        expect_equal(.ceiling_q(cq, n), cq)
        expect_true(all(fq <= qq & qq <= cq))
        expect_equal(fq == qq, qq == cq)
        if (n == 1L) {
            expect_equal(fq, qq)
            expect_equal(cq, qq)
        } else {
            expect_equal(unname(fq %% 4L) %% n, rep(0L, NN))
            expect_equal(unname(cq %% 4L) %% n, rep(0L, NN))
        }
    }
})



context("base - months")
# ###################################################################

# test sample
m <- sample.int(12L, size = NN, replace = TRUE)
mm <- 12L * yy + m - 1L
names(mm) <- nms


.valid_m0 <- function(m) .valid_y0(m %/% 12L)


test_that("'.validate_m' and '.validate_ym' work correctly", {
    mmv <- sample(-100L:10100L, NN, replace = TRUE) * 12L +
           sample(0L:11L, size = NN, replace = TRUE)
    mmv[(NN - 11L):NN] <- 1L:12L
    mm <- .validate_m(mmv)
    expect_equal(!is.na(mm), .valid_m0(mmv))
    expect_equal(mm[!is.na(mm)], mmv[.valid_m0(mmv)])
    expect_equal(.validate_ym(mmv %/% 12L, mmv %% 12L + 1L), mm)
})


test_that("'.m2char' works correctly", {
    mmchar <- .m2char(mm)
    expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", mmchar)))
    expect_equal(as.integer(sub("-[0-9]{2}$", "", mmchar)), as.integer(yy))
    expect_equal(as.integer(sub("^[0-9]{4}-", "", mmchar)), as.integer(m))
    expect_equal(.m2char(NA_integer_), NA_character_)
    expect_equal(names(mmchar), nms)
})


test_that("'.m2mnth' works correctly", {
    expect_equal(unname(.m2mnth(mm)), m)
})


test_that("'.m2yf' works correctly", {
    expect_equal(.m2yf(mm), yy + (m - 1) / 12)
    expect_equal(.m2yf(NA_integer_), NA_real_)
})


test_that("'.m2y' works correctly", {
    expect_equal(.m2y(mm), yy)
})


test_that("'.y2m' works correctly", {
    expect_equal(unname(.m2char(.y2m(yy))), paste0(.y2char(yy), "-01"))
})


test_that("'.q2m' works correctly", {
    expect_equal(.m2y(.q2m(qq)), yy)
    expect_equal(unname(.q2m(qq) %% 12L + 1L), (q - 1L) * 3L + 1L)
})


test_that("'.m2q' works correctly", {
    expect_equal(.q2y(.m2q(mm)), yy)
    expect_equal(.m2q(mm) %% 4L + 1L, (mm %% 12L) %/% 3L + 1L)
})


test_that("'.floor_m' and '.ceiling_m' work correctly", {
    mm <- pmin(mm, 999901L)
    ns <- c(1L:4L, 6L, 12L)
    for (n in ns) {
        fm <- .floor_m(mm, n)
        cm <- .ceiling_m(mm, n)
        expect_true(all(.valid_m0(fm)))
        expect_true(all(.valid_m0(cm)))
        expect_equal(.floor_m(fm, n), fm)
        expect_equal(.ceiling_m(cm, n), cm)
        expect_true(all(fm <= mm & mm <= cm))
        expect_equal(fm == mm, mm == cm)
        if (n == 1L) {
            expect_equal(fm, mm)
            expect_equal(cm, mm)
        } else {
            expect_equal(unname(fm %% 12L) %% n, rep(0L, NN))
            expect_equal(unname(cm %% 12L) %% n, rep(0L, NN))
        }
    }
})



context("base - weeks")
# ###################################################################


test_that("'.weeks_in_year' works correctly", {
    winy <- .weeks_in_year(yy)
    winy0 <- as.integer(format(as.Date(paste0(.y2char(yy), "-12-28")), "%V"))
    expect_equal(unname(winy), winy0)
})


# test sample
w <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(yy))


test_that("'.validate_w' and '.validate_yw' work correctly", {
    y0 <- sample(-100L:10100L, NN, replace = TRUE)
    w0 <- sample(0L:99L, size = NN, replace = TRUE)
    ww <- .validate_yw(y0, w0)
    expect_equal(is.na(ww), !.valid_y0(y0) | (w0 < 1) | (w0 > .weeks_in_year(y0)))
    expect_false(any(is.na(.validate_yw(yy, w))))
    ww <- c(.validate_yw(0, 1) + (-1:0), .validate_yw(9999, 52) + (0:1))
    expect_equal(.validate_w(ww), c(NA_integer_, ww[2L:3L], NA_integer_))
})


ww <- .validate_yw(yy, w)
names(ww) <- nms


test_that("'.w2char' works correctly", {
    wwchar <- .w2char(ww)
    expect_true(all(grepl("^[0-9]{4}-W[0-9]{2}$", wwchar)))
    expect_equal(as.integer(sub("^[0-9]{4}-W", "", wwchar)), unname(w))
    expect_equal(as.integer(sub("-W[0-9]{2}$", "", wwchar)), unname(yy))
    expect_equal(.w2char(NA_integer_), NA_character_)
    expect_equal(names(wwchar), nms)
})


test_that("'.w2week' works correctly", {
    expect_equal(unname(.w2week(ww)), w)
})


test_that("'.w2y' works correctly", {
    expect_equal(.w2y(ww), yy)
})


test_that("'.y2w' works correctly", {
    expect_equal(.y2w(yy), .validate_yw(yy, 1))
})


test_that("'.w2yf' works correctly", {
    .w2yf0 <- function(w) {
        y <- .w2y(w)
        w <- .w2week(w)
        y + (w - 1) / .weeks_in_year(y)
    }
    expect_equal(.w2yf(ww), .w2yf0(ww))
})


test_that("'.floor_w' and '.ceiling_w' work correctly", {
    ww <- pmin(ww, .y2w(9999L))
    ns <- c(1L, 2L, 4L, 13L, 26L, 52L)
    for (n in ns) {
        fw <- .floor_w(ww, n)
        cw <- .ceiling_w(ww, n)
        expect_true(all(fw <= ww & ww <= cw))
        expect_equal(fw == ww, ww == cw)
        expect_equal(.floor_w(fw, n), fw)
        expect_equal(.ceiling_w(cw, n), cw)
        if (n == 1L) {
            expect_equal(fw, ww)
            expect_equal(cw, ww)
        } else {
            expect_equal(unname(.w2week(fw) - 1L) %% n, rep(0L, NN))
            expect_equal(unname(.w2week(cw) - 1L) %% n, rep(0L, NN))
        }
    }
})


test_that("'.inc_w_by_y' works correctly", {
    dy <- as.integer(sample(-200L:200L, NN, replace = TRUE))
    ww1 <- .inc_w_by_y(ww, dy)
    y0 <- yy
    w0 <- w
    y1 <- .w2y(ww1)
    w1 <- unname(.w2week(ww1))
    expect_equal(unname(y1 - y0), dy)
    i53 <- (w0 == 53)
    expect_equal(w1[!i53], w0[!i53])
    expect_equal(w1[i53], unname(.w2week(.y2w(y0[i53] + dy[i53] + 1L) - 1L)))
})



context("base - dates")
# ###################################################################

test_that("'.days_in_year' works correctly", {
    dy <- .days_in_year(yy)
    dy0 <- as.integer(as.Date(paste0(yy, "-12-31")) -
                      as.Date(paste0(yy, "-01-01"))) + 1L
    expect_equal(unname(dy), dy0)
})


test_that("'.days_in_quarter' works correctly", {
    dq <- .days_in_quarter(qq)
    m <- (q - 1L) * 3L + 1L
    y1 <- yy
    m1 <- m + 3L
    y1[m == 10L] <- y1[m == 10L] + 1L
    m1[m == 10L] <- 1L
    dq0 <- as.integer(as.Date(paste0(.m2char(.validate_ym(y1, m1)), "-01")) -
                      as.Date(paste0(.m2char(.validate_ym(yy, m)), "-01")))
    expect_equal(unname(dq), dq0)
})


test_that("'.days_in_month' works correctly", {
    dm <- .days_in_month(mm)
    y1 <- yy
    m1 <- m + 1L
    y1[m == 12] <- y1[m == 12] + 1L
    m1[m == 12] <- 1L
    dm0 <- as.integer(as.Date(paste0(y1, "-", m1, "-01")) -
                      as.Date(paste0(yy, "-", m, "-01")))
    expect_equal(unname(dm), dm0)
})


# test sample
d <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(mm))


test_that("'.validate_ymd' works correctly", {
    ymd <- .validate_ymd(yy, m, d)
    DD <- as.Date(paste0(yy, "-", m, "-", d))
    expect_equal(unname(ymd), as.integer(DD))
})


test_that("'.validate_d' works correctly", {
    ymd <- .validate_ymd(yy, m, d)
    d00000101 <- .validate_ymd(0, 1, 1)
    d99991231 <- .validate_ymd(9999, 12, 31)
    vd <- c(d00000101 - c(1, 0), ymd, d99991231 + c(0, 1))
    vd <- .validate_d(vd)
    expect_equal(vd[3L:(NN + 2L)], ymd)
    expect_true(all(is.na(vd[c(1L, length(vd))])))
})


dd <- .validate_ymd(yy, m, d)


test_that("'.d2char' works correctly", {
    ddchar <- .d2char(dd)
    expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", ddchar)))
    expect_equal(as.integer(gsub("^([0-9]{4})-([0-9]{2})-([0-9]{2})$", "\\1", ddchar)),
                     as.integer(yy))
    expect_equal(as.integer(gsub("^([0-9]{4})-([0-9]{2})-([0-9]{2})$", "\\2", ddchar)),
                     as.integer(m))
    expect_equal(as.integer(gsub("^([0-9]{4})-([0-9]{2})-([0-9]{2})$", "\\3", ddchar)),
                     as.integer(d))
    expect_equal(.d2char(NA_integer_), NA_character_)
})


test_that("'.day_of_year' works correctly", {
    DD <- as.Date(.d2char(dd))
    expect_equal(unname(.day_of_year(dd)), as.integer(format(DD, "%j")))
})


test_that("'.day_of_week' works correctly", {
    DD <- as.Date(.d2char(dd))
    expect_equal(unname(.day_of_week(dd)), as.integer(format(DD, "%u")))
})


test_that("'.d2day' works correctly", {
    expect_equal(unname(.d2day(dd)), d)
})


test_that("'.d2y' works correctly", {
    expect_equal(.d2y(dd), yy)
})


test_that("'.d2q' works correctly", {
    expect_equal(.d2q(dd), .m2q(mm))
})


test_that("'.d2m' works correctly", {
    expect_equal(.d2m(dd), mm)
})


test_that("'.m2d' works correctly", {
    dd <- .m2d(mm)
    expect_equal(dd, .validate_ymd(yy, m, 1))
})


test_that("'.last_day_in_month' works correctly", {
    dd <- .last_day_in_month(mm)
    expect_equal(.m2d(mm) - 1L + .days_in_month(mm), dd)
})


test_that("'.y2d' works correctly", {
    dd <- .y2d(yy)
    expect_equal(dd, .validate_ymd(yy, 1, 1))
})


test_that("'.q2d' works correctly", {
    dd <- .q2d(qq)
    expect_equal(dd, .validate_ymd(yy, (q - 1) * 3 + 1, 1))
})


test_that("'.w2d' works correctly", {
    dd <- .w2d(ww)
    wD <- as.Date(.d2char(dd))
    expect_equal(names(dd), nms)
    expect_equal(unname(ww),
                     .validate_yw(as.integer(format(as.Date(wD), "%G")),
                                  as.integer(format(as.Date(wD), "%V"))))
    expect_equal(as.integer(format(wD, "%u")), rep(1L, NN))
})


test_that("'.d2w' works correctly", {
    ww <- .d2w(dd)
    wD <- as.Date(.d2char(dd))
    expect_equal(names(ww), nms)
    expect_equal(unname(ww),
                     .validate_yw(as.integer(format(as.Date(wD), "%G")),
                                  as.integer(format(as.Date(wD), "%V"))))
})


test_that("'.d2jdn' and '.jdn2d' work correctly", {
    expect_equal(.d2jdn(.validate_ymd(2000, 1, 1)), 2451545L)
    DD <- as.Date(.d2char(dd))
    expect_equal(unname(.d2jdn(dd)), 2451545L + as.integer(DD - as.Date("2000-01-01")))
    expect_equal(.jdn2d(.d2jdn(dd)), dd)
})


test_that("'.easter' works correctly", {
    es <- c(.validate_ymd(2015, 04, 05),
            .validate_ymd(2016, 03, 27),
            .validate_ymd(2017, 04, 16),
            .validate_ymd(2018, 04, 01),
            .validate_ymd(2019, 04, 21),
            .validate_ymd(2020, 04, 12),
            .validate_ymd(2021, 04, 04),
            .validate_ymd(2022, 04, 17),
            .validate_ymd(2023, 04, 09),
            .validate_ymd(2024, 03, 31),
            .validate_ymd(2025, 04, 20))
    expect_equal(.easter(.d2y(es)), es)
})


test_that("'.d2yf' works correctly", {
    .d2yf0 <- function(d) {
        y <- .d2y(dd)
        return (y + (.day_of_year(dd) - 1) / .days_in_year(y))
    }
    expect_equal(.d2yf(dd), .d2yf0(dd))
})


test_that("'.floor_d' and '.ceiling_d' work correctly", {
    ns <- c(1L:3L, 7L, 15L, 30L)
    for (n in ns) {
        fd <- .floor_d(dd, n)
        cd <- .ceiling_d(dd, n)
        expect_equal(.floor_d(fd, n), fd)
        expect_equal(.ceiling_d(cd, n), cd)
        expect_true(all(fd <= dd & dd <= cd))
        expect_equal(fd == dd, dd == cd)
        if (n == 1L) {
            expect_equal(fd, dd)
            expect_equal(cd, dd)
        } else if (n == 2) {
            expect_true(all(.day_of_week(fd) %in% c(1L, 3L, 5L)))
            expect_true(all(.day_of_week(cd) %in% c(1L, 3L, 5L)))
        } else if (n == 3) {
            expect_true(all(.day_of_week(fd) %in% c(1L, 4L)))
            expect_true(all(.day_of_week(cd) %in% c(1L, 4L)))
        } else if (n == 7) {
            expect_true(all(.day_of_week(fd) == 1L))
            expect_true(all(.day_of_week(cd) == 1L))
        } else if (n == 15L) {
            fdd <- .d2day(fd)
            cdd <- .d2day(cd)
            expect_true(all((fdd == 1L) | (fdd == 16L)))
            expect_true(all((cdd == 1L) | (cdd == 16L)))
        } else if (n == 30L) {
            expect_true(all(.d2day(fd) == 1L))
            expect_true(all(.d2day(cd) == 1L))
        }
    }
})


test_that("'.inc_d_by_m' works correctly", {
    dm <- as.integer(sample(-200L:200L, NN, replace = TRUE))
    dd1 <- .inc_d_by_m(dd, dm)
    m0 <- mm
    d0 <- d
    m1 <- .d2m(dd1)
    d1 <- unname(.d2day(dd1))
    expect_equal(unname(m1 - m0), dm)
    i28l <- (d0 <= 28L)
    expect_equal(d1[i28l], d0[i28l])
    expect_equal(d1[!i28l], unname(pmin(d0[!i28l], .days_in_month(m1[!i28l]))))
})


test_that("'.validate_yj' works correctly", {
    nn <- sample.int(365L, size = NN, replace = TRUE)
    nthD <- as.Date(.d2char(.validate_yj(yy, nn)))
    expect_equal(as.integer(format(nthD, "%Y")), unname(yy))
    expect_equal(as.integer(format(nthD, "%j")), unname(nn))
    expect_equal(.validate_yj(yy, .day_of_year(dd)), dd)
    expect_equal(.validate_yj(c(    2000L,     2016L), 366L),
                     .validate_ymd(c(2000, 2016), 12, 31))
    expect_equal(.validate_yj(c(2020L, 2020L), c(NA, 366L)),
                     .validate_ymd(c(NA, 2020), 12, 31))
    expect_equal(.validate_yj(yy, integer()), integer())
    expect_equal(.validate_yj(integer(), nn), integer())
    warn <- "longer object length is not a multiple of shorter object length"
    expect_warning(r1 <- .validate_yj(nn[1L:3L], unname(yy[1L:2L])), warn, fixed = TRUE)
    expect_equal(r1, .validate_yj(nn[1L:3L], unname(yy[c(1L, 2L, 1L)])))
    expect_warning(r1 <- .validate_yj(nn[1L:2L], unname(yy[1L:3L])), warn, fixed = TRUE)
    expect_equal(r1, .validate_yj(nn[c(1L, 2L, 1L)], unname(yy[1L:3L])))
})


test_that("'.last_dw_in_month' works correctly", {
    dw <- sample.int(7L, size = NN, replace = TRUE)
    dd <- .last_dw_in_month(dw, mm)
    expect_equal(unname(.day_of_week(dd)), dw)
    expect_true(all(.d2m(dd) == mm))
    expect_false(any(.d2m(dd + 7L) == mm))
    expect_equal(.last_dw_in_month(integer(), mm), integer())
    expect_equal(.last_dw_in_month(dw, integer()), integer())
    warn <- "longer object length is not a multiple of shorter object length"
    expect_warning(.last_dw_in_month(dw[1L:2L], mm[1L:3L]), warn, fixed = TRUE)
    expect_warning(.last_dw_in_month(dw[1L:3L], mm[1L:2L]), warn, fixed = TRUE)
    expect_silent(dd <- .last_dw_in_month(8L, mm))
    expect_equal(unname(dd), rep(NA_integer_, NN))
})


test_that("'.validate_ywu' works correctly", {
    dw <- sample(1L:7L, NN, replace = TRUE)
    dd <- .validate_ywu(yy, w, dw)
    ddD <- as.Date(.d2char(dd))
    expect_equal(.d2w(dd), ww)
    expect_equal(as.integer(format(ddD, "%u")), dw)
    expect_equal(.validate_ywu(yy, w, integer()), integer())
    expect_equal(.validate_ywu(integer(), integer(), dw), integer())
    warn <- "longer object length is not a multiple of shorter object length"
    expect_warning(.validate_ywu(yy[1L:3L], w[1L:3L], dw[1L:2L]), warn, fixed = TRUE)
    expect_warning(.validate_ywu(yy[1L:2L], w[1L:2L], dw[1L:3L]), warn, fixed = TRUE)
    expect_equal(unname(.validate_ywu(yy[1L:8L], w[1L:8L], c(dw[1L:7L], 8L))),
                     unname(c(dd[1L:7L], NA_integer_)))
})


test_that("'.nth_dw_in_month' works correctly", {
    nn <- sample.int(4L, size = NN, replace = TRUE)
    dw <- sample.int(7L, size = NN, replace = TRUE)
    dd <- .nth_dw_in_month(nn, dw, mm)
    expect_equal(unname(.day_of_week(dd)), dw)
    expect_equal(.d2m(dd), mm)
    expect_true(all(.d2m(dd - 7L * (nn - 1L)) == mm))
    expect_false(any(.d2m(dd - 7L * nn) == mm))
    expect_equal(.nth_dw_in_month(integer(), dw, mm), integer())
    expect_equal(.nth_dw_in_month(nn, integer(), mm), integer())
    expect_equal(.nth_dw_in_month(nn, dw, integer()), integer())
    warn <- "longer object length is not a multiple of shorter object length"
    expect_warning(.nth_dw_in_month(nn[1L:2L], dw[1L:3L], mm[1L:3L]), warn, fixed = TRUE)
    expect_warning(.nth_dw_in_month(nn[1L:3L], dw[1L:2L], mm[1L:3L]), warn, fixed = TRUE)
    expect_warning(.nth_dw_in_month(nn[1L:3L], dw[1L:3L], mm[1L:2L]), warn, fixed = TRUE)
    expect_equal(unname(.nth_dw_in_month(6L, dw, mm)), rep(NA_integer_, NN))
    expect_equal(unname(.nth_dw_in_month(nn, 8L, mm)), rep(NA_integer_, NN))
})

