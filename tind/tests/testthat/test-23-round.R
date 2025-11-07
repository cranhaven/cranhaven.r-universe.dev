context("floor_t, ceiling_t, round_t, and trunc_t")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test sample size
NN <- 100L

# test samples
y <- sample(1990L:2020L, NN, replace = TRUE)
q <- sample.int(4L, size = NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
w <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(y))
d <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(.validate_ym(y, m)))
tt <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 1)

yy <- tind(y = y)
qq <- tind(y = y, q = q)
mm <- tind(y = y, m = m)
ww <- tind(y = y, w = w)
dd <- tind(y = y, m = m, d = d)
tt <- as.date_time(tt, tz = sample(tzs, 1L))

nas <- sample.int(NN, 2L)
yy[nas] <- NA
qq[nas] <- NA
mm[nas] <- NA
ww[nas] <- NA
dd[nas] <- NA

tt[nas] <- NA
tt[sample.int(NN, 1L)] <-1703894258.1 # 2023-12-29 23:57:38.1Z to make sure resolution is .1s



test_that("'floor_t', 'ceiling_t', 'round_t', and 'trunc_t' work correctly", {
    # d, w, m, q, y
    tu <- c("d", "w", "m", "q", "y")
    for (tp in tu) {
        xx <- get(paste0(tp, tp))
        # admissible
        for (un in c(.lo_res_cast(tp), tp)) {
            nss <- if (un == "d") .mults(un)
                   else c(1L, sample(setdiff(.mults(un), 1L), 2L))
            for (ns in nss) {
                nsun <- as.tdiff(ns, un)
                fx <- floor_t(xx, nsun)
                cx <- ceiling_t(xx, nsun)
                cxf <- ceiling_t(xx, nsun, ceiling = "following")
                cxl <- ceiling_t(xx, nsun, ceiling = "last")
                rx <- round_t(xx, nsun)
                rxf <- round_t(xx, nsun, ceiling = "following")
                rxl <- round_t(xx, nsun, ceiling = "last")
                expect_equal(fx, floor_t(xx, paste0(ns, un)))
                expect_equal(fx, floor_t(xx, as.tdiff(paste0(ns, un))))
                expect_equal(cx, ceiling_t(xx, paste0(ns, un)))
                expect_equal(cx, ceiling_t(xx, as.tdiff(paste0(ns, un))))
                expect_equal(cxf, ceiling_t(xx, paste0(ns, un), ceiling = "following"))
                expect_equal(cxl, ceiling_t(xx, paste0(ns, un), ceiling = "last"))
                expect_equal(rx, round_t(xx, paste0(ns, un)))
                expect_equal(rx, round_t(xx, as.tdiff(paste0(ns, un))))
                expect_equal(rxf, round_t(xx, paste0(ns, un), ceiling = "following"))
                expect_equal(rxl, round_t(xx, paste0(ns, un), ceiling = "last"))
                if (ns == 1L) {
                    tx <- trunc_t(xx, un)
                    if (un == tp) {
                        expect_true(identical(tx, xx) &&
                                    identical(fx, xx) &&
                                    identical(cx, xx) &&
                                    identical(cxf, xx + 1L) &&
                                    identical(cxl, xx) &&
                                    identical(rx, xx) &&
                                    identical(rxf, xx) &&
                                    identical(rxl, xx))
                    } else {
                        expect_true(identical(tx, as.tind(xx, type = un)) &&
                                    identical(fx, as.tind(tx, type = tp)))
                    }
                }
                if ((ns != 1L) || (un != tp)) {
                    ina <- is.na(xx)
                    expect_true(identical(ina, is.na(fx)) &&
                                identical(ina, is.na(cx)) && identical(ina, is.na(cxf)) &&
                                identical(ina, is.na(cxl)) && identical(ina, is.na(rx)) &&
                                identical(ina, is.na(rxf)) && identical(ina, is.na(rxl)))
                    expect_true(identical(floor_t(fx, nsun), fx) &&
                                identical(ceiling_t(cx, nsun), cx) &&
                                identical(floor_t(cx, nsun), cx) &&
                                identical(floor_t(cxf, nsun), cxf) &&
                                identical(floor_t(cxl, nsun), fx))
                    expect_true(all(fx <= xx, na.rm = TRUE) && all(xx <= cx, na.rm = TRUE) &&
                                all(xx < cxf, na.rm = TRUE) && all(xx <= cxl, na.rm = TRUE) &&
                                all((rx == fx) | (rx == cx), na.rm = TRUE) &&
                                all((rxf == fx) | (rxf == cxf), na.rm = TRUE) &&
                                all((rxl == fx) | (rxl == cxl), na.rm = TRUE))
                }
            }
        }
        # errors
        un <- sample(c(.lo_res_cast(tp), tp), 1L)
        ns <- sample(setdiff(1:100, .mults(un)), 1L)
        nsun <- as.tdiff(ns, un)
        err <- "^invalid multiplier"
        expect_error(floor_t(xx, nsun), err)
        expect_error(floor_t(xx, paste0(ns, un)), err)
        expect_error(floor_t(xx, as.tdiff(paste0(ns, un)), err))
        expect_error(ceiling_t(xx, nsun), err)
        expect_error(ceiling_t(xx, paste0(ns, un)), err)
        expect_error(ceiling_t(xx, as.tdiff(paste0(ns, un)), err))
        expect_error(round_t(xx, nsun), err)
        expect_error(round_t(xx, paste0(ns, un)), err)
        expect_error(round_t(xx, as.tdiff(paste0(ns, un)), err))
        err <- "^invalid time unit"
        un <- sample(setdiff(.t_unit(long = FALSE), c(.lo_res_cast(tp), tp)), 1L)
        expect_error(trunc_t(xx, un), err)
        expect_error(floor_t(xx, un), err)
        expect_error(ceiling_t(xx, un), err)
        expect_error(round_t(xx, un), err)
        err <- paste0("^invalid ", sQuote("unit"), " argument")
        expect_error(floor_t(xx, as.tdiff(c(1, 7), "w")), err)
        expect_error(ceiling_t(xx, as.tdiff(c(1, 7), "w")), err)
        expect_error(round_t(xx, as.tdiff(c(1, 7), "w")), err)
    }
    # errors - trunc_t
    tp <- sample(c("d", "w", "m", "q", "y"), 1L)
    un <- sample(c(.lo_res_cast(tp), tp), 1L)
    err <- paste0("^invalid ", sQuote("unit"), " argument;")
    expect_error(trunc_t(xx, paste0(1, un)), err)
    expect_error(trunc_t(xx, as.tdiff(1, un)), err)
    err <- paste0(".*unit.* missing")
    expect_error(trunc_t(xx), err)
    expect_error(floor_t(xx), err)
    expect_error(ceiling_t(xx), err)
    expect_error(round_t(xx), err)
    # errors - floor_t, ceiling_t, round_t
    tp <- sample(c("d", "w", "m", "q", "y"), 1L)
    un <- sample(c(.lo_res_cast(tp), tp), 1L)
    xx <- get(paste0(tp, tp))
    # ceiling behavior
    ceiling <- c("default", "following", "last")
    err <- paste0("invalid ", sQuote("ceiling"), " argument; ",
                  "expected one of the following: ", toString(dQuote(ceiling)))
    expect_error(round_t(dd, "1m", ceiling = "qwerty"), err, fixed = TRUE)
    expect_error(round_t(dd, 15, "qwerty"), err, fixed = TRUE)
    # 0-length
    tp <- sample(setdiff(tu, "y"), 1L)
    xx <- get(paste0(tp, tp))
    un <- sample(c(.lo_res_cast(tp), tp), 1L)
    expect_equal(trunc_t(xx[0L], un), tind(type = un))
    expect_equal(trunc_t(xx[0L], tp), xx[0L])
    expect_equal(floor_t(xx[0L], un), xx[0L])
    expect_equal(ceiling_t(xx[0L], un), xx[0L])
    expect_equal(round_t(xx[0L], un), xx[0L])

    # t, h
    xx <- tt
    xxh <- as.tind(xx, type = "h")
    un <- sample(c("d", "w", "m", "q", "y"), 1L)
    expect_equal(trunc_t(xx, un), as.tind(xx, type = un))
    expect_equal(trunc_t(xx[0L], un), as.tind(xx[0L], type = un))
    expect_equal(floor_t(xx[0L], un), xx[0L])
    expect_equal(ceiling_t(xx[0L], un), xx[0L])
    expect_equal(round_t(xx[0L], un), xx[0L])
    err <- "^invalid time unit"
    expect_error(trunc_t(xxh, un), err)
    expect_error(floor_t(xxh, un), err)
    expect_error(ceiling_t(xxh, un), err)
    expect_error(round_t(xxh, un), err)
    for (ns in sample(.mults(un), 2L)) {
        nsun <- as.tdiff(ns, un)
        fx <- floor_t(xx, nsun)
        expect_true(identical(fx, floor_t(xx, paste0(ns, un))) &&
                    identical(fx, floor_t(xx, as.tdiff(paste0(ns, un)))))
        cx <- ceiling_t(xx, nsun)
        expect_true(identical(cx, ceiling_t(xx, paste0(ns, un))) &&
                    identical(cx, ceiling_t(xx, as.tdiff(paste0(ns, un)))))
        rx <- round_t(xx, nsun)
        expect_true(identical(rx, round_t(xx, paste0(ns, un))) &&
                    identical(rx, round_t(xx, as.tdiff(paste0(ns, un)))))
        expect_true(identical(cx, ceiling_t(xx, nsun, ceiling = "following")) &&
                    identical(cx, ceiling_t(xx, nsun, ceiling = "last")))
        ina <- is.na(xx)
        expect_true(identical(ina, is.na(fx)) &&
                    identical(ina, is.na(cx)) &&
                    identical(ina, is.na(rx)))
        expect_true(all(fx <= xx, na.rm = TRUE) && all(xx <= cx, na.rm = TRUE) &&
                    all((rx == fx) | (rx == cx), na.rm = TRUE))
        if (un != "q") {
            expect_true((.get_tdiff_type(resolution_t(fx)) %in% c(un, .lo_res_cast(un))) &&
                        (.get_tdiff_type(resolution_t(cx)) %in% c(un, .lo_res_cast(un))) &&
                        (.get_tdiff_type(resolution_t(rx)) %in% c(un, .lo_res_cast(un))))
        } else {
            expect_true(((resolution_t(fx) %in% (as.tdiff(1:2, "q")) || .get_tdiff_type(resolution_t(fx)) == "y")) &&
                        ((resolution_t(cx) %in% (as.tdiff(1:2, "q")) || .get_tdiff_type(resolution_t(fx)) == "y")) &&
                        ((resolution_t(rx) %in% (as.tdiff(1:2, "q")) || .get_tdiff_type(resolution_t(fx)) == "y")))
        }
    }
    for (un in c("s", "min", "h")) {
        nss <- .mults(un)
        nss <- nss[nss >= .1]
        nss <- c(1L, sample(setdiff(nss, 1L), 2L))
        for (ns in nss) {
            nsun <- as.tdiff(ns, un)
            fx <- floor_t(xx, nsun)
            expect_true(identical(fx, floor_t(xx, paste0(ns, un))) &&
                        identical(fx, floor_t(xx, as.tdiff(paste0(ns, un)))))
            cx <- ceiling_t(xx, nsun)
            expect_true(identical(cx, ceiling_t(xx, paste0(ns, un))) &&
                        identical(cx, ceiling_t(xx, as.tdiff(paste0(ns, un)))))
            rx <- round_t(xx, nsun)
            expect_true(identical(rx, round_t(xx, paste0(ns, un))) &&
                        identical(rx, round_t(xx, as.tdiff(paste0(ns, un)))))
            expect_true(identical(cx, ceiling_t(xx, nsun, ceiling = "following")) &&
                        identical(cx, ceiling_t(xx, nsun, ceiling = "last")))
            fxh <- floor_t(xxh, nsun)
            cxh <- ceiling_t(xxh, nsun)
            rxh <- round_t(xxh, nsun)
            expect_true(identical(cxh, ceiling_t(xxh, nsun, ceiling = "following")) &&
                        identical(cxh, ceiling_t(xxh, nsun, ceiling = "last")))
            if (ns == 1L) {
                tx <- trunc_t(xx, un)
                expect_equal(tx, fx)
                txh <- trunc_t(xxh, un)
                expect_equal(txh, fxh)
                expect_equal(trunc_t(xx[0L], un), xx[0L])
                expect_equal(trunc_t(xxh[0L], un), xxh[0L])
            }
            ina <- is.na(xx)
            expect_true(identical(ina, is.na(fx)) &&
                        identical(ina, is.na(cx)) &&
                        identical(ina, is.na(rx)))
            expect_true(all(xx - fx >= 0, na.rm = TRUE) && all(cx - xx >= 0, na.rm = TRUE) &&
                        all((rx == fx) | (rx == cx), na.rm = TRUE))
            expect_true(identical(ina, is.na(fxh)) &&
                        identical(ina, is.na(cxh)) &&
                        identical(ina, is.na(rxh)))
            expect_true(all(xxh - fxh >= 0, na.rm = TRUE) && all(cxh - xxh >= 0, na.rm = TRUE) &&
                        all((rxh == fxh) | (rxh == cxh), na.rm = TRUE))
            # h vs t
            d24h <- hours_in_day(xx) == 24
            d24h <- d24h & !is.na(d24h)
            ic24 <- cxh == as.time("24:00")
            ic24 <- ic24 & !is.na(ic24)
            fx2h <- as.tind(fx, "h")
            cx2h <- as.tind(cx, "h")
            if ((un != "h") || (ns == 1)) {
                expect_equal(fx2h, fxh)
            } else {
                expect_equal(fx2h[d24h], fxh[d24h])
            }
            expect_equal(cx2h[!ic24 & d24h], cxh[!ic24 & d24h])
            expect_true(all(cx2h[ic24] == as.time("00:00")))
        }
    }

    # i
    xx <- as.tind(1:10, type = "i")
    err <- paste0(sQuote("trunc_t"), " function not defined for type ", dQuote("i"),
                  " (integer index)")
    expect_error(trunc_t(xx), err, fixed = TRUE)
    err <- paste0("time unit provided for type ", dQuote("i"), " (integer index)")
    expect_error(floor_t(xx, "1d"), err, fixed = TRUE)
    expect_error(ceiling_t(xx, "1d"), err, fixed = TRUE)
    expect_equal(floor_t(xx, 2), as.tind((as.integer(xx) %/% 2L) * 2L, type = "i"))
    # n
    xx <- as.tind(.5 + 1:10, type = "n")
    err <- paste0(sQuote("trunc_t"), " function not defined for type ", dQuote("n"),
                  " (numeric index)")
    expect_error(trunc_t(xx), err, fixed = TRUE)
    err <- paste0("time unit provided for type ", dQuote("n"), " (numeric index)")
    expect_error(floor_t(xx, "d"), err, fixed = TRUE)
    expect_error(ceiling_t(xx, "1d"), err, fixed = TRUE)
    expect_equal(ceiling_t(xx, 1), as.tind(2:11, type = "n"))
})



test_that("'floor', 'ceiling', 'round', and 'trunc' methods are properly disabled", {
    xx <- as.tind(Sys.time())
    err <- paste0(sQuote("%s"), " method not defined for class ",
                  dQuote("tind"), "; use ", sQuote("%s_t"),
                  " function instead")
    for (f in c("floor", "ceiling", "round", "trunc"))
        expect_error(do.call(f, list(xx)), sprintf(err, f, f))
})

