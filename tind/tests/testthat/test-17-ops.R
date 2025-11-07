context("operators - calendar arithmetic and comparisons")
# ###################################################################

# supported time index types
types <- c("y", "q", "m", "w", "d", "t", "h", "i", "n")
# supported units of time
units <- c("y", "q", "m", "w", "d", "h", "min", "s")


# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test sample size
NN <- 99L
MM <- 10L


y <- sample(1990L:2020L, NN, replace = TRUE)
q <- sample.int(4L, size = NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
w <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(y))
d <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(.validate_ym(y, m)))
tt <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 1)
hh <- tt %% 86400

yy <- tind(y = y)
qq <- tind(y = y, q = q)
mm <- tind(y = y, m = m)
ww <- suppressWarnings(tind(y = y, w = w))
dd <- suppressWarnings(tind(y = y, m = m, d = d))
tt <- as.tind(tt, tz = sample(tzs, 1L))
hh <- as.time(hh)
ii <- as.tind(as.integer(runif(NN, )), type = "i")
nn <- as.tind(runif(NN, -3e7, 3e7), type = "n")


errd1 <- "unary operator %s not defined for "
errd2 <- "operator %s not defined for "
errtt <- "time index type mismatch in %s: "
errtu <- "time unit mismatch in %s: "
warntz <- "different time zones of arguments: "
warnls <- "longer object length is not a multiple of shorter object length"



test_that("unary operators work correctly", {
    tp <- sample(types, 1L)
    xx <- get(paste0(tp, tp))
    tx <- tinterval(xx, rev(xx))

    for (gen in c("+", "-", "!")) {
        for (cl in c("x", "t")) {
            err <- sprintf(errd1, sQuote(gen))
            oprnd <- get(paste0(cl, "x"))
            if ((gen == "!") || (cl == "t")) next
                expect_error(do.call(gen, list(oprnd)), err, fixed = TRUE)
        }
    }
})



test_that("operators for 'tind' work correctly", {
for (it in 1:3) {
    tp <- sample(types, 2L)
    if (tp[1L] == "h") tp <- rev(tp)
    t1 <- tp[1L]
    t2 <- tp[2L]
    x1 <- get(paste0(t1, t1))
    x2 <- get(paste0(t2, t2))
    un <- sample(units, 2L)
    u1 <- un[1]
    u2 <- un[2]
    d1 <- as.tdiff(runif(NN, -100, 100), u1)
    d2 <- as.tdiff(runif(NN, -100, 100), u2)
    nnn <- runif(NN, -100, 100)

    # unary
    for (gen in c("+", "-", "!")) {
        err <- sprintf(errd1, sQuote(gen))
        expect_error(do.call(gen, list(x1)), err, fixed = TRUE)
    }

    # ^ * / %% %/%
    gen <- sample(c("^", "*", "/", "%%", "%/%"), 1L)
    err <- sprintf(errd2, sQuote(gen))
    expect_error(do.call(gen, list(x1, x1)), err, fixed = TRUE)
    expect_error(do.call(gen, list(x1, x2)), err, fixed = TRUE)

    # + -
    gen <- "-"
    err <- sprintf(errtt, sQuote(gen))
    expect_error(do.call(gen, list(x1, x2)), err, fixed = TRUE)
    if (t1 %in% c("i", "n")) {
        expect_true(is.numeric(x1 - rev(x1)))
    } else {
        expect_true(is.tdiff(x1 - rev(x1)))
    }
    expect_equal((x1 - rev(x1)) + rev(x1), x1)
    expect_equal(rev(x1) + (x1 - rev(x1)), x1)
    if (length(tzs) >= 2L) {
        tt1 <- round(as.numeric(Sys.time()) + (((1L - NN) %/% 2):((NN - 1L) %/% 2)) *
                               (3600 * 23 + 61.111111), digits = 0L)
        tt2 <- rev(tt1)
        tz <- sample(tzs, 2L)
        xt1 <- as.date_time(tt1, tz[1L])
        xt2 <- as.date_time(tt2, tz[2L])
        expect_warning(res <- (xt1 - xt2) + xt2, warntz, fixed = TRUE)
        expect_equal(res, as.date_time(tt1, tz[2L]))
        expect_warning(res <- xt2 + (xt1 - xt2), warntz, fixed = TRUE)
        expect_equal(res, as.date_time(tt1, tz[2L]))
    }
    err <- sprintf(errd2, sQuote("-"))
    expect_error((x1 - x1) - x1, err, fixed = TRUE)

    gen <- "+"
    err <- sprintf(errd2, sQuote(gen))
    expect_error(do.call(gen, list(x1, x1)), err, fixed = TRUE)
    expect_error(do.call(gen, list(x1, x2)), err, fixed = TRUE)
    expect_equal(x1 + TRUE, TRUE + x1)
    expect_equal(x1 + TRUE, x1 + 1)
    expect_equal((x1 - nnn) + nnn, x1)
    expect_equal(nnn + (x1 - nnn), x1)
    expect_equal((x1 + nnn) - nnn, x1)
    expect_equal(-nnn + (x1 + nnn), x1)

    # comparisons
    for (gen in c("==", "!=")) {
        err <- sprintf(errtt, sQuote(gen))
        expect_error(do.call(gen, list(x1, x2)), err, fixed = TRUE)
    }
    expect_equal(x1 == rev(x1), !(x1 != rev(x1)))
    expect_equal(x1 >= rev(x1), !(x1 < rev(x1)))
    expect_equal(x1 <= rev(x1), !(x1 > rev(x1)))
    if (length(tzs) >= 2L) {
        ttt <- round(as.numeric(Sys.time()) + (((1L - NN) %/% 2):((NN - 1L) %/% 2)) *
                              (3600 * 23 + 61.111111), digits = 0L)
        tz <- sample(tzs, 2L)
        xt1 <- as.date_time(ttt, tz[1L])
        xt2 <- as.date_time(ttt, tz[2L])
        expect_warning(res <- xt1 >= xt2, warntz, fixed = TRUE)
        expect_true(all(res))
        expect_warning(res <- xt1 < xt2, warntz, fixed = TRUE)
        expect_false(any(res))
    }

    if ((t1 %in% .lo_res_cast(t2)) || (t2 %in% .lo_res_cast(t1)) ||
        all(c(t1, t2) %in% c("i", "n"))) {
        expect_equal(x1 >= x2, !(x1 < x2))
        expect_equal(x1 <= x2, !(x1 > x2))
    } else {
        err <- paste0("^cast from time index type ", dQuote("[a-z]"),
                      " \\([- a-z]+\\) to type ", dQuote("[a-z]"),
                      " \\([- a-z]+\\) in " , sQuote(">="), " not possible$")
        expect_error(x1 >= x2)
        err <- paste0("^cast from time index type ", dQuote("[a-z]"),
                      " \\([- a-z]+\\) to type ", dQuote("[a-z]"),
                      " \\([- a-z]+\\) in " , sQuote("<"), " not possible$")
        expect_error(x1 < x2)
    }
}
    # long /short warning
    i1 <- sample.int(NN, 4L)
    i2 <- sample.int(NN, 3L)
    i2r <- rep_len(i2, length(i1))
    for (gen in c("-", "<=")) {
        r0 <- do.call(gen, list(dd[i1], dd[i2r]))
        expect_warning(r1 <- do.call(gen, list(dd[i1], dd[i2])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
        r0 <- do.call(gen, list(dd[i2r], dd[i1]))
        expect_warning(r1 <- do.call(gen, list(dd[i2], dd[i1])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
    }
})



test_that("operators for 'tdiff' work correctly", {
    un <- sample(units, 2L)
    u1 <- un[1]
    u2 <- un[2]
    d1 <- as.tdiff(runif(NN, -100, 100), u1)
    d2 <- as.tdiff(runif(NN, -100, 100), u2)
    nn <- runif(NN, -100, 100)
    # unit adjusments
    u1 <- if (u1 %in% c("h", "min", "s")) "s" else u1
    u2 <- if (u2 %in% c("h", "min", "s")) "s" else u2

    # unary
    expect_equal(!(!d1), as.logical(d1))
    expect_equal(+d1, d1)
    expect_equal(-(-d1), d1)

    # ^
    err <- sprintf(errd2, sQuote("^"))
    expect_error(d1^d1, err, fixed = TRUE)
    expect_error(d1^d2, err, fixed = TRUE)
    expect_error(nn^d2, err, fixed = TRUE)
    expect_error(d1^nn, err, fixed = TRUE)

    # *
    err <- sprintf(errd2, sQuote("*"))
    expect_error(d1 * d1, err, fixed = TRUE)
    expect_error(d1 * d2, err, fixed = TRUE)
    expect_equal(d1 * nn, as.tdiff(unclass(d1) * nn, u1))
    expect_equal(nn * d2, as.tdiff(nn * unclass(d2), u2))

    # / %% %/%
    err <- sprintf(errd2, sQuote("/"))
    expect_error(d1 / d1, err, fixed = TRUE)
    expect_error(d1 / d2, err, fixed = TRUE)
    expect_error(nn / d2, err, fixed = TRUE)
    expect_equal(suppressWarnings(d1 / nn),
                 as.tdiff(suppressWarnings(unclass(d1) / nn), u1))
    err <- sprintf(errd2, sQuote("%%"))
    expect_error(d1 %% d1, err, fixed = TRUE)
    expect_error(d1 %% d2, err, fixed = TRUE)
    expect_error(nn %% d2, err, fixed = TRUE)
    expect_equal(suppressWarnings(d1 %% nn),
                 as.tdiff(suppressWarnings(unclass(d1) %% nn), u1))
    err <- sprintf(errd2, sQuote("%/%"))
    expect_error(d1 %/% d1, err, fixed = TRUE)
    expect_error(d1 %/% d2, err, fixed = TRUE)
    expect_error(nn %/% d2, err, fixed = TRUE)
    expect_equal(suppressWarnings(d1 %/% nn),
                 as.tdiff(suppressWarnings(unclass(d1) %/% nn), u1))

    # + -
    for (gen in c("+", "-")) {
        err <- sprintf(errtu, sQuote(gen))
        if (u1 != u2) expect_error(do.call(gen, list(d1, d2)), err, fixed = TRUE)
        expect_equal(do.call(gen, list(d1, rev(d1))),
                     as.tdiff(do.call(gen, list(unclass(d1), rev(unclass(d1)))), u1))
        expect_equal(do.call(gen, list(d1, nn)),
                     as.tdiff(do.call(gen, list(unclass(d1), nn)), u1))
        expect_equal(do.call(gen, list(nn, d2)),
                     as.tdiff(do.call(gen, list(nn, unclass(d2))), u2))
    }
    nnn <- if (u1 %in% c("h", "min", "s")) nn else round(nn)
    expect_equal((d1 - nnn) + nnn, d1)
    expect_equal(nnn + (d1 - nnn), d1)
    expect_equal((d1 + nnn) - nnn, d1)
    expect_equal(-nnn + (d1 + nnn), d1)

    # unit handling
    expect_equal(hours(3) + 1, hours(4))
    expect_equal(1 + hours(3), hours(4))
    expect_equal(mins(3) + 1, mins(4))
    expect_equal(1 + mins(3), mins(4))
    expect_equal(mins(30) * 4, hours(2))
    expect_equal(4 * mins(30), hours(2))
    expect_equal(mins(30), hours(2) / 4)

    # comparisons
    for (gen in c("==", "!=", "<=", "<", ">=", ">")) {
        err <- sprintf(errtu, sQuote(gen))
        if (u1 != u2) expect_error(do.call(gen, list(d1, d2)), err, fixed = TRUE)
        expect_equal(do.call(gen, list(d1, rev(d1))),
                     do.call(gen, list(unclass(d1), rev(unclass(d1)))), u1)
        expect_equal(do.call(gen, list(d1, nn)),
                     do.call(gen, list(unclass(d1), nn)))
        expect_equal(do.call(gen, list(nn, d2)),
                     do.call(gen, list(nn, unclass(d2))))
    }

    # long /short warning
    i1 <- sample.int(NN, 4L)
    i2 <- sample.int(NN, 3L)
    i2r <- rep_len(i2, length(i1))
    for (gen in c("-", ">", "*")) {
        r0 <- do.call(gen, list(d1[i1], nn[i2r]))
        expect_warning(r1 <- do.call(gen, list(d1[i1], nn[i2])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
        r0 <- do.call(gen, list(d1[i2r], nn[i1]))
        expect_warning(r1 <- do.call(gen, list(d1[i2], nn[i1])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
        r0 <- do.call(gen, list(nn[i1], d1[i2r]))
        expect_warning(r1 <- do.call(gen, list(nn[i1], d1[i2])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
        r0 <- do.call(gen, list(nn[i2r], d1[i1]))
        expect_warning(r1 <- do.call(gen, list(nn[i2], d1[i1])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
    }
})



test_that("operators for 'tinterval' work correctly", {
    tp <- sample(setdiff(types, "h"), 1L)
    xx <- get(paste0(tp, tp))
    tixx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                      sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    tdxx <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE) -
            sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE)

    # unary
    for (gen in c("+", "-")) {
        err <- sprintf(errd1, sQuote(gen))
        expect_error(do.call(gen, list(tixx)), err, fixed = TRUE)
    }

    # binary
    gen <- sample(c("^", "*", "/", "%%", "%/%", "==", "!=", "<=", "<", ">=", ">"), 1L)
    err <- sprintf(errd2, sQuote(gen))
    expect_error(do.call(gen, list(tixx, tixx)), err, fixed = TRUE)

    # + -
    gen <- sample(c("-", "+"), 1L)
    err <- sprintf(errd2, sQuote(gen))
    expect_error(do.call(gen, list(tixx, tixx)), err, fixed = TRUE)
    expect_error(do.call(gen, list(xx, tixx)), err, fixed = TRUE)

    res <- tixx + tdxx
    expect_equal(res$start, tixx$start + tdxx)
    expect_equal(res$end, tixx$end + tdxx)
    res <- tdxx + tixx
    expect_equal(res$start, tixx$start + tdxx)
    expect_equal(res$end, tixx$end + tdxx)
    res <- tixx - tdxx
    expect_equal(res$start, tixx$start - tdxx)
    expect_equal(res$end, tixx$end - tdxx)
    tdxx <- as.numeric(tdxx)
    res <- tixx + tdxx
    expect_equal(res$start, tixx$start + tdxx)
    expect_equal(res$end, tixx$end + tdxx)
    res <- tixx - tdxx
    expect_equal(res$start, tixx$start - tdxx)
    expect_equal(res$end, tixx$end - tdxx)

    # long /short warning
    i1 <- sample.int(NN, 4L)
    i2 <- sample.int(NN, 3L)
    i2r <- rep_len(i2, length(i1))
    for (gen in c("-", "+")) {
        r0 <- do.call(gen, list(tixx[i1], tdxx[i2r]))
        expect_warning(r1 <- do.call(gen, list(tixx[i1], tdxx[i2])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
        r0 <- do.call(gen, list(tixx[i2r], tdxx[i1]))
        expect_warning(r1 <- do.call(gen, list(tixx[i2], tdxx[i1])), warnls, fixed = TRUE)
        expect_equal(r0, r1)
        if (gen == "+") {
            r0 <- do.call(gen, list(tdxx[i1], tixx[i2r]))
            expect_warning(r1 <- do.call(gen, list(tdxx[i1], tixx[i2])), warnls, fixed = TRUE)
            expect_equal(r0, r1)
            r0 <- do.call(gen, list(tdxx[i2r], tixx[i1]))
            expect_warning(r1 <- do.call(gen, list(tdxx[i2], tixx[i1])), warnls, fixed = TRUE)
            expect_equal(r0, r1)
        }
    }
})



test_that("operators %+-x% work correctly", {
    nnn <- round(runif(NN, -100, 100))
    funcs1 <- c("years", "qrtrs", "mnths", "weeks", "days")
    funcs2 <- c("hours", "mins", "secs")
    for (f in c(funcs1, funcs2)) {
        u <- if (f == "mins") "min" else substr(f, 1L, 1L)
        for (neg in c(FALSE, TRUE)) {
            op0 <- if (neg) "-" else "+"
            op1 <- paste0("%", op0, u, "%")
            expect_equal(do.call(op1, list(tt, nnn)),
                         do.call(op0, list(tt, do.call(f, list(nnn)))))
            if (f %in% funcs1)
            expect_equal(do.call(op1, list(dd, nnn)),
                         do.call(op0, list(dd, do.call(f, list(nnn)))))
            expect_warning(r1 <- do.call(op1, list(tt[1L:3L], nnn[1L:2L])))
            expect_equal(r1, do.call(op1, list(tt[1L:3L], nnn[c(1L, 2L, 1L)])))
        }
    }
    expect_error(dd %+m% (dd - dd))

    # tinterval
    nnn <- round(runif(MM, -100, 100))
    f <- sample(funcs1, 1L)
    u <- substr(f, 1L, 1L)
    op0 <- if (runif(1L) < .5) "-" else "+"
    op1 <- paste0("%", op0, u, "%")
    tixx <- tinterval(sort(dd[sample.int(NN, MM, TRUE)], na.last = FALSE),
                      sort(dd[sample.int(NN, MM, TRUE)], na.last = TRUE))
    res <- do.call(op1, list(tixx, nnn))
    expect_equal(res$start, do.call(op1, list(tixx$start, nnn)))
    expect_equal(res$end, do.call(op1, list(tixx$end, nnn)))
    expect_warning(r1 <- do.call(op1, list(tixx[1L:3L], nnn[1L:2L])))
    expect_equal(r1, do.call(op1, list(tixx[1L:3L], nnn[c(1L, 2L, 1L)])))
})



test_that("'diff' methods work correctly", {
    for (tp in types) {
        xx <- get(paste0(tp, tp))
        for (lag in c(1L, sample(2L:5L, 1L))) {
            for (diffs in c(1L, sample(2L:5L, 1L))) {
                res <- diff(xx, lag, diffs)
                if (tp %in% c("i", "n")) {
                    expect_equal(unclass(res), diff(unclass(xx), lag, diffs))
                } else {
                    res0 <- diff(unclass(xx), lag, diffs)
                    res0 <- as.tdiff(res0, if (tp %in% c("h", "t")) "s" else tp)
                    expect_equal(res, res0)
                }
            }
        }
    }

    tp <- sample(setdiff(types, c("i", "n")), 1L)
    xx <- get(paste0(tp, tp))
    expect_equal(diff(xx[1:2], 2), xx[0] - xx[0])
    errl <- paste0("invalid ", sQuote("lag"), " argument; positive integer expected")
    errd <- paste0("invalid ", sQuote("differences"), " argument; positive integer expected")
    expect_error(diff(xx, 0, 1), errl, fixed = TRUE)
    expect_error(diff(xx, 1, 0), errd, fixed = TRUE)
})



test_that("'mean' and 'median' methods for 'tind' work correctly", {
    dd <- today() + -1:1
    expect_equal(mean(dd), dd[2L])
    expect_equal(median(dd), dd[2L])
    dd <- c(dd, NA)
    expect_equal(mean(dd), dd[4L])
    expect_equal(median(dd), dd[4L])
    expect_equal(mean(dd, na.rm = TRUE), dd[2L])
    expect_equal(median(dd, na.rm = TRUE), dd[2L])
    dd <- dd[0L]
    expect_equal(mean(dd), dd[1L])
    expect_equal(median(dd), dd[1L])
})

