context("base - pretty time axis ticks")
# ###################################################################

# supported units of time
units <- c("s", "min", "h", "d", "w", "m", "q", "y")

# test sample size
NN <- 5L

# test samples
y0 <- sample(1969:2068, size = NN, replace = TRUE)
dy <- as.integer(pmin(rgeom(NN, prob = .005), 1000))
y1 <- y0 + dy
q0 <- .validate_yq(sample(1998:2020, size = NN, replace = TRUE),
                   sample.int(4L, size = NN, replace = TRUE))
dq <- as.integer(pmin(rgeom(NN, prob = .005), 4000))
q1 <- q0 + dq
m0 <- .validate_ym(sample(1998:2020, size = NN, replace = TRUE),
                   sample.int(12L, size = NN, replace = TRUE))
dm <- as.integer(pmin(rgeom(NN, prob = .005), 12000))
m1 <- m0 + dm



test_that("'.pretty_[yqm]' work correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    for (tp in c("y", "q", "m")) {
        .pretty_x <- switch(tp, y = .pretty_y, q = .pretty_q, m = .pretty_m)
        .res_x <- switch(tp, y = .res_y, q = .res_q, m = .res_m)
        x0 <- get(paste0(tp, "0"))
        x1 <- get(paste0(tp, "1"))
        dx <- get(paste0("d", tp))
        ns <- c(0L:5L, 10L, 20L)
        for (n in ns) {
            for (mn in 0L:min(n, 5L)) {
                nn <- nu <- integer(NN)
                fr <- t0 <- t1 <- integer(NN)
                isin <- frok <- logical(NN)
                for (i in 1:NN) {
                    xx_ <- unique(c(x0[i], x1[i]))
                    pt <- .pretty_x(xx_, n, mn)
                    isin[i] <- is.integer(pt)
                    nn[i] <- length(pt) - 1L
                    nu[i] <- length(unique(pt)) - 1L
                    t0[i] <- min(pt)
                    t1[i] <- max(pt)
                    f <- .res_x(pt)
                    f_ <- .res_x(xx_)
                    frok[i] <- !(f$n %% f_$n) ||
                               (match(f$unit, units) > match(f_$unit, units))
                }
                expect_true(all(t0 <= x0) && all(t1 >= x1) && all(nn == nu) &&
                            all(nn >= mn) && all(frok) && all(isin))
            }
        }
    }
    yy <- c(y0[1L], y1[1L])
    if (diff(range(yy)) > 0) {
        n <- sample(c(0L:5L, 10L, 20L), 1L)
        mn <- sample(0L:min(n, 5L), 1L)
        expect_equal(.pretty_q(.y2q(yy), n, mn), .y2q(.pretty_y(yy, n, mn)))
        expect_equal(.pretty_m(.y2m(yy), n, mn), .y2m(.pretty_y(yy, n, mn)))
    }
})


# test sample
yw <- sample(1998:2020, size = NN, replace = TRUE)
# start with res == 1
w0 <- 1L:52L
w0 <- w0[as.logical((w0 - 1L) %% 2L)]
w0 <- w0[as.logical((w0 - 1L) %% 13L)]
w0 <- c(w0, 53L)
w0 <- pmin(sample(w0, size = NN, replace = TRUE), .weeks_in_year(yw))
w0 <- .validate_yw(yw, w0)
rm("yw")
dw <- as.integer(pmin(rgeom(NN, prob = .005), 52000))
w1 <- w0 + dw


test_that("'.pretty_w' works correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    ns <- c(0L:5L, 10L, 20L)
    for (n in ns) {
        for (mn in 0L:min(n, 5L)) {
            nn <- nu <- integer(NN)
            t0 <- t1 <- integer(NN)
            isin <- logical(NN)
            for (i in 1:NN) {
                ww_ <- unique(c(w0[i], w1[i]))
                pt <- .pretty_w(ww_, n, mn)
                isin[i] <- is.integer(pt)
                nn[i] <- length(pt) - 1L
                nu[i] <- length(unique(pt)) - 1L
                t0[i] <- min(pt)
                t1[i] <- max(pt)
            }
            expect_true(all(t0 <= w0) && all(t1 >= w1) && all(nn == nu) &&
                        all(nn >= mn) && all(isin))
        }
    }
    yy <- c(y0[1L], y1[1L])
    if (diff(range(yy)) > 0) {
        n <- sample(c(0L:5L, 10L, 20L), 1L)
        mn <- sample(0L:min(n, 5L), 1L)
        expect_equal(.pretty_w(.y2w(yy), n, mn), .y2w(.pretty_y(yy, n, mn)))
    }
    for (rfr in c(2L, 4L, 13L, 26L)) {
        w0 <- .floor_w(w0, rfr)
        w1 <- .ceiling_w(w1, rfr)
        dw <- w1 - w0
        for (n in c(5L:6L)) {
            mn <- n %/% 2L
            nn <- nu <- integer(NN)
            t0 <- t1 <- integer(NN)
            frok <- logical(NN)
            for (i in 1:NN) {
                ww_ <- unique(c(w0[i], w1[i]))
                f_ <- .res_w(ww_)
                pt <- .pretty_w(ww_, n, mn)
                isin[i] <- is.integer(pt)
                nn[i] <- length(pt) - 1L
                nu[i] <- length(unique(pt)) - 1L
                t0[i] <- min(pt)
                t1[i] <- max(pt)
                f <- .res_w(pt)
                frok[i] <- !(f$n %% f_$n) ||
                            (match(f$unit, units) > match(f_$unit, units))
            }
            expect_true(all(t0 <= w0) && all(t1 >= w1) && all(nn == nu) &&
                        all(nn >= mn) && all(frok) && all(isin))
        }
    }
})


# test sample
y <- sample(1998:2020, size = NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
d0 <- pmin(sample.int(31L, size = NN, replace = TRUE),
           .days_in_month(.validate_ym(y, m)))
d0 <- .validate_ymd(y, m, d0)
# don't get into .pretty_w
d0[.day_of_week(d0) == 1L] <- d0[.day_of_week(d0) == 1L] + 1L
dd <- as.integer(pmin(rgeom(NN, prob = .005), 365000))
d1 <- d0 + dd


test_that("'.pretty_d' works correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    ns <- c(0L:5L, 10L, 20L)
    for (n in ns) {
        for (mn in 0L:min(n, 5L)) {
            nn <- nu <- integer(NN)
            fr <- t0 <- t1 <- integer(NN)
            isin <- frok <- logical(NN)
            for (i in 1:NN) {
                dd_ <- unique(c(d0[i], d1[i]))
                pt <- .pretty_d(dd_, n, mn)
                isin[i] <- is.integer(pt)
                nn[i] <- length(pt) - 1L
                nu[i] <- length(unique(pt)) - 1L
                t0[i] <- min(pt)
                t1[i] <- max(pt)
                f <- .res_d(pt)
                f_ <- .res_d(dd_)
                frok[i] <- !(f$n %% f_$n) ||
                            (match(f$unit, units) > match(f_$unit, units))
            }
            expect_true(all(t0 <= d0) && all(t1 >= d1) && all(nn == nu) &&
                        all(nn >= mn) && all(frok) && all(isin))
        }
    }
    yy <- c(y0[1L], y1[1L])
    if (diff(range(yy)) > 0) {
        n <- sample(c(0L:5L, 10L, 20L), 1L)
        mn <- sample(0L:min(n, 5L), 1L)
        expect_equal(.pretty_d(.y2d(yy), n, mn), .y2d(.pretty_y(yy, n, mn)))
    }
    mm <- c(m0[1L], m1[1L])
    if (diff(range(mm)) > 0) {
        n <- sample(c(0L:5L, 10L, 20L), 1L)
        mn <- sample(0L:min(n, 5L), 1L)
        expect_equal(.pretty_d(.m2d(mm), n, mn), .m2d(.pretty_m(mm, n, mn)))
    }
    ww <- c(w0[1L], w1[1L])
    if (diff(range(ww)) > 0) {
        n <- sample(c(0L:5L, 10L, 20L), 1L)
        mn <- sample(0L:min(n, 5L), 1L)
        wd <- .w2d(ww)
        if (.res_d(wd)$unit == "w")
            expect_equal(.pretty_d(wd, n, mn), .w2d(.pretty_w(ww, n, mn)))
    }
})




test_that("'.pretty_t' works correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                     "UTC", "Etc/GMT+1",
                                     "Europe/London", "America/New_York"))
    tz <- sample(tzs, 1L)
    tt <- round(1650002399 + (0:99) * (3600 + 61.111111), digits = 6)
    fs <- c(1L:4L, 6L, 8L, 12L)
    fs <- sort(sample(fs, 2))
    ns <- c(0L:7L, 10L)
    ns <- sort(sample(ns, 2))
    for (f in c(1L:4L, 6L, 8L, 12L)) {
        tt <- .floor_t_h(tt, f, tz)
        i0 <- sample.int(100, NN, replace = TRUE)
        i1 <- sample.int(100, NN, replace = TRUE)
        for (n in ns) {
            for (mn in 0L:min(n, 4L)) {
                nn <- nu <- integer(NN)
                x0 <- x1 <- t0 <- t1 <- numeric(NN)
                isdbl <- frok <- logical(NN)
                for (i in 1:NN) {
                    tt_ <- sort(unique(tt[i0[i]:i1[i]]))
                    x0[i] <- min(tt_)
                    x1[i] <- max(tt_)
                    pt <- .pretty_t(tt_, tz, n, mn)
                    isdbl[i] <- is.double(pt)
                    nn[i] <- length(pt) - 1L
                    nu[i] <- length(unique(pt)) - 1L
                    t0[i] <- min(pt)
                    t1[i] <- max(pt)
                    f <- .res_t(pt, tz)
                    f_ <- .res_t(tt_, tz)
                    frok[i] <- (f$n >= f_$n) ||
                                (match(f$unit, units) >= match(f_$unit, units))
                }
                expect_true(all(t0 <= x0) && all(t1 >= x1) && all(nn == nu) &&
                            all(nn >= mn) && all(frok) && all(isdbl))
            }
        }
    }
    tt <- round(1650002399 + (0:99) * 61.111111, digits = 6)
    fs <- c(1L:6L, 10L, 12L, 15L, 20L, 30)
    fs <- sort(sample(fs, 2))
    ns <- c(0L:7L, 10L)
    ns <- sort(sample(ns, 2))
    for (f in fs) {
        tt <- .floor_t_min(tt, f, tz)
        i0 <- sample.int(100, NN, replace = TRUE)
        i1 <- sample.int(100, NN, replace = TRUE)
        for (n in ns) {
            for (mn in 0L:min(n, 4L)) {
                nn <- nu <- integer(NN)
                x0 <- x1 <- t0 <- t1 <- numeric(NN)
                isdbl <- frok <- logical(NN)
                for (i in 1:NN) {
                    tt_ <- sort(unique(tt[i0[i]:i1[i]]))
                    x0[i] <- min(tt_)
                    x1[i] <- max(tt_)
                    pt <- .pretty_t(tt_, tz, n, mn)
                    isdbl[i] <- is.double(pt)
                    nn[i] <- length(pt) - 1L
                    nu[i] <- length(unique(pt)) - 1L
                    t0[i] <- min(pt)
                    t1[i] <- max(pt)
                    f <- .res_t(pt, tz)
                    f_ <- .res_t(tt_, tz)
                    frok[i] <- (f$n >= f_$n) ||
                                (match(f$unit, units) >= match(f_$unit, units))
                }
                expect_true(all(t0 <= x0) && all(t1 >= x1) && all(nn == nu) &&
                            all(nn >= mn) && all(frok) && all(isdbl))
            }
        }
    }
    tt <- round(1650002399 + (0:99) * 1.111111, digits = 6)
    fs <- c(1L:6L, 10L, 12L, 15L, 20L, 30)
    fs <- sort(sample(fs, 2))
    ns <- c(0L:7L, 10L)
    ns <- sort(sample(ns, 2))
    for (f in c(1L:6L, 10L, 12L, 15L, 20L, 30)) {
        tt <- .floor_t_s(tt, f, tz)
        i0 <- sample.int(100, NN, replace = TRUE)
        i1 <- sample.int(100, NN, replace = TRUE)
        for (n in ns) {
            for (mn in 0L:min(n, 4L)) {
                nn <- nu <- integer(NN)
                x0 <- x1 <- t0 <- t1 <- numeric(NN)
                isdbl <- frok <- logical(NN)
                for (i in 1:NN) {
                    tt_ <- sort(unique(tt[i0[i]:i1[i]]))
                    x0[i] <- min(tt_)
                    x1[i] <- max(tt_)
                    pt <- .pretty_t(tt_, tz, n, mn)
                    isdbl[i] <- is.double(pt)
                    nn[i] <- length(pt) - 1L
                    nu[i] <- length(unique(pt)) - 1L
                    t0[i] <- min(pt)
                    t1[i] <- max(pt)
                    f <- .res_t(pt, tz)
                    f_ <- .res_t(tt_, tz)
                    frok[i] <- (f$n >= f_$n) ||
                                (match(f$unit, units) >= match(f_$unit, units))
                }
                expect_true(all(t0 <= x0) && all(t1 >= x1) &&
                            all(nn == nu) && all(nn >= mn) &&
                            all(frok) && all(isdbl))
            }
        }
    }
    y0 <- sample(1998:2020, 1L) + 0L:sample.int(20L, 1L)
    n <- sample(c(0L:5L, 10L, 20L), 1L)
    mn <- sample(0L:min(n, 5L), 1L)
    expect_equal(.pretty_t(.y2t(y0, tz), tz, n, mn), .y2t(.pretty_y(y0, n, mn), tz))
    m0 <- m0[1L] + 0L:sample.int(20L, 1L)
    n <- sample(c(0L:5L, 10L, 20L), 1L)
    mn <- sample(0L:min(n, 5L), 1L)
    expect_equal(.pretty_t(.m2t(m0, tz), tz, n, mn), .m2t(.pretty_m(m0, n, mn), tz))
    w0 <- w0[1L] + 0L:sample.int(20L, 1L)
    n <- sample(c(0L:5L, 10L, 20L), 1L)
    mn <- sample(0L:min(n, 5L), 1L)
    expect_equal(.pretty_t(.w2t(w0, tz), tz, n, mn), .w2t(.pretty_w(w0, n, mn), tz))
    d0 <- d0[1L] + 0L:sample.int(20L, 1L)
    n <- sample(c(0L:5L, 10L, 20L), 1L)
    mn <- sample(0L:min(n, 5L), 1L)
    expect_equal(.pretty_t(.d2t(d0, tz), tz, n, mn), .d2t(.pretty_d(d0, n, mn), tz))
})


test_that("'.pretty_h' works correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    hh <- round((1650002399 + (0:99) * (3600 + 61.111111)) %% 86400, digits = 6)
    fs <- c(1L:4L, 6L, 8L, 12L)
    fs <- sort(sample(fs, 2))
    ns <- c(0L:7L, 10L)
    ns <- sort(sample(ns, 2))
    for (f in c(1L:4L, 6L, 8L, 12L)) {
        hh <- .floor_h_h(hh, f)
        i0 <- sample.int(100, NN, replace = TRUE)
        i1 <- sample.int(100, NN, replace = TRUE)
        for (n in ns) {
            for (mn in 0L:min(n, 4L)) {
                nn <- nu <- integer(NN)
                x0 <- x1 <- h0 <- h1 <- numeric(NN)
                isdbl <- hasna <- frok <- logical(NN)
                for (i in 1:NN) {
                    hh_ <- sort(unique(hh[i0[i]:i1[i]]))
                    x0[i] <- min(hh_)
                    x1[i] <- max(hh_)
                    pt <- .validate_h(.pretty_h(hh_, n, mn))
                    isdbl[i] <- is.double(pt)
                    hasna[i] <- anyNA(pt)
                    nn[i] <- length(pt) - 1L
                    nu[i] <- length(unique(pt)) - 1L
                    h0[i] <- min(pt)
                    h1[i] <- max(pt)
                    f <- .res_h(pt)
                    f_ <- .res_h(hh_)
                    frok[i] <- (f$n >= f_$n) ||
                                (match(f$unit, units) >= match(f_$unit, units))
                }
                expect_true(all(h0 <= x0 | hasna) && all(h1 >= x1 | hasna) &&
                            all(nn == nu | hasna) && all(nn >= mn | hasna) &&
                            all(frok | hasna))
                expect_true(all(isdbl))
            }
        }
    }
    hh <- round((1650002399 + (0:99) * (3600 + 61.111111)) %% 86400, digits = 6)
    fs <- c(1L:6L, 10L, 12L, 15L, 20L, 30)
    fs <- sort(sample(fs, 2))
    ns <- c(0L:7L, 10L)
    ns <- sort(sample(ns, 2))
    for (f in fs) {
        hh <- .floor_h_min(hh, f)
        i0 <- sample.int(100, NN, replace = TRUE)
        i1 <- sample.int(100, NN, replace = TRUE)
        for (n in ns) {
            for (mn in 0L:min(n, 4L)) {
                nn <- nu <- integer(NN)
                x0 <- x1 <- h0 <- h1 <- numeric(NN)
                isdbl <- hasna <- frok <- logical(NN)
                for (i in 1:NN) {
                    hh_ <- sort(unique(hh[i0[i]:i1[i]]))
                    x0[i] <- min(hh_)
                    x1[i] <- max(hh_)
                    pt <- .validate_h(.pretty_h(hh_, n, mn))
                    isdbl[i] <- is.double(pt)
                    hasna[i] <- anyNA(pt)
                    nn[i] <- length(pt) - 1L
                    nu[i] <- length(unique(pt)) - 1L
                    h0[i] <- min(pt)
                    h1[i] <- max(pt)
                    f <- .res_h(pt)
                    f_ <- .res_h(hh_)
                    frok[i] <- (f$n >= f_$n) ||
                                (match(f$unit, units) >= match(f_$unit, units))
                }
                expect_true(all(h0 <= x0 | hasna) && all(h1 >= x1 | hasna) &&
                            all(nn == nu | hasna) && all(nn >= mn | hasna) &&
                            all(frok | hasna) && all(isdbl))
            }
        }
    }
    hh <- round((1650002399 + (0:99) * (3600 + 61.111111)) %% 86400, digits = 6)
    fs <- c(1L:6L, 10L, 12L, 15L, 20L, 30)
    fs <- sort(sample(fs, 2))
    ns <- c(0L:7L, 10L)
    ns <- sort(sample(ns, 2))
    for (f in c(1L:6L, 10L, 12L, 15L, 20L, 30)) {
        hh <- .floor_h_s(hh, f)
        i0 <- sample.int(100, NN, replace = TRUE)
        i1 <- sample.int(100, NN, replace = TRUE)
        for (n in ns) {
            for (mn in 0L:min(n, 4L)) {
                nn <- nu <- integer(NN)
                x0 <- x1 <- h0 <- h1 <- numeric(NN)
                isdbl <- hasna <- frok <- logical(NN)
                for (i in 1:NN) {
                    hh_ <- sort(unique(hh[i0[i]:i1[i]]))
                    x0[i] <- min(hh_)
                    x1[i] <- max(hh_)
                    pt <- .validate_h(.pretty_h(hh_, n, mn))
                    isdbl[i] <- is.double(pt)
                    hasna[i] <- anyNA(pt)
                    nn[i] <- length(pt) - 1L
                    nu[i] <- length(unique(pt)) - 1L
                    h0[i] <- min(pt)
                    h1[i] <- max(pt)
                    f <- .res_h(pt)
                    f_ <- .res_h(hh_)
                    frok[i] <- (f$n >= f_$n) ||
                                (match(f$unit, units) >= match(f_$unit, units))
                }
                expect_true(all(h0 <= x0 | hasna) && all(h1 >= x1 | hasna) &&
                            all(nn == nu | hasna) && all(nn >= mn | hasna) &&
                            all(frok | hasna) && all(isdbl))
            }
        }
    }
})



test_that("'.pretty_i' works correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    ii <- sample.int(1001L, 2L * NN) - 500L
    for (n in c(0L:7L, 10L)) {
        for (mn in 0L:min(n, 4L)) {
            isin <- ok <- logical(NN)
            for (i in 1L:NN) {
                ii_ <- ii[1L:2L + (i - 1L) * 2L]
                pt <- .pretty_i(ii_, n, mn)
                nn <- length(pt) - 1L
                ok[i] <- is.integer(pt) &&
                         (length(unique(pt)) == nn + 1L) && (nn >= mn) &&
                         (min(pt) <= min(ii_)) && (max(pt) >= max(ii_))
                if (min(ii) >= 0) ok[i] <- ok[i] && (min(pt) >= 0)
                if (max(ii) <= 0) ok[i] <- ok[i] && (max(pt) <= 0)
            }
            expect_true(all(ok))
        }
    }
    expect_equal(.pretty_i(0L), 0L:2L)
})


test_that("'.pretty_n' works correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    xx <- rnorm(2L * NN, sd = exp(rnorm(NN)))
    for (n in c(0L:7L, 10L)) {
        for (mn in 0L:min(n, 4L)) {
            isdbl <- ok <- logical(NN)
            for (i in 1L:NN) {
                xx_ <- xx[1L + c(0L, NN)]
                p0 <- pretty(xx_, n, mn)
                pt <- .pretty_n(xx_, n, mn)
                isdbl[i] <- is.double(pt)
                n0 <- length(p0) - 1L
                nn <- length(pt) - 1L
                ok[i] <- (isTRUE(all.equal(p0, pt)) || (abs(n - nn) < abs(n - n0)) ||
                         (diff(range(pt)) < diff(range(p0)))) &&
                         (length(unique(pt)) == nn + 1L) && (nn >= mn)
                         (min(pt) <= min(xx_)) && (max(pt) >= max(xx_))
            }
            expect_true(all(ok))
        }
    }
    expect_equal(.pretty_n(0), seq(0, 1, by = .2))
})

