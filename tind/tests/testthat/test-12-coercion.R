context("tind - coercion")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test sample size
NN <- 100L
# test samples
yy <- sample(1924L:2100L, size = NN, replace = TRUE)
qq <- sample(1924L:2100L, size = NN, replace = TRUE) * 10L +
      sample.int(4L, size = NN, replace = TRUE)
mm <- sample(1924L:2100L, size = NN, replace = TRUE) * 100L +
      sample.int(12L, size = NN, replace = TRUE)
w <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(yy))
ww <- 100L * yy + w
dd <- pmin(sample.int(31L, size = NN, replace = TRUE),
           .days_in_month(.validate_ym(mm %/% 100, mm %% 100)))
dd <- mm * 100L + dd
nt <- 1e9 + c(0, round(runif(NN - 1L, min = 0, max = 1e9)))
ni <- sample.int(.Machine$integer.max, NN)
nas <- sample.int(NN, NN %/% 10L)
yy[nas] <- NA_integer_
qq[nas] <- NA_integer_
mm[nas] <- NA_integer_
dd[nas] <- NA_integer_
dd[1L] <- 20221111
nt[nas] <- NA_real_
ni[nas] <- NA_integer_
rm("nas")


test_that("'.tind_base_coercible', '.tind_coercible' and '.check_tind_coercible' work correctly", {
    expect_true(.tind_base_coercible(integer()))
    expect_true(.tind_base_coercible(double()))
    expect_true(.tind_base_coercible(character()))
    expect_true(.tind_base_coercible(as.factor(character())))
    expect_true(.tind_base_coercible(logical()))
    expect_false(.tind_base_coercible(Sys.Date()))
    expect_false(.tind_base_coercible(Sys.time()))
    expect_false(.tind_base_coercible(as.POSIXlt(Sys.time())))
    expect_false(.tind_coercible(integer()))
    expect_false(.tind_coercible(double()))
    expect_false(.tind_coercible(character()))
    expect_false(.tind_coercible(as.factor(character())))
    expect_false(.tind_coercible(logical()))
    expect_true(.tind_coercible(Sys.Date()))
    expect_true(.tind_coercible(Sys.time()))
    expect_true(.tind_coercible(as.POSIXlt(Sys.time())))
    expect_false(.tind_base_coercible(sum))
    expect_false(.tind_coercible(sum))
    err <- paste0(dQuote("function"), " is not recognised as a class representing time indices")
    expect_error(.check_tind_coercible(sum), err)
})


test_that("'as.tind.default' works correctly", {
    errno <- paste0(sQuote("as.tind"), " method not defined for class ",
                    dQuote("function"))
    expect_error(as.tind(sum), errno, fixed = TRUE)
    errnt <- paste0("time index type could not be automatically inferred; provide ",
                    sQuote("type"), " argument")
    expect_error(as.tind(NULL), errnt, fixed = TRUE)
    expect_equal(as.tind(NULL, "d"), tind(type = "d"))
})


test_that("as.tind.tind works correctly", {
    xy <- as.tind(yy)
    xq <- as.tind(qq)
    xm <- as.tind(mm)
    xw <- as.tind(ww, type = "w")
    xd <- as.tind(dd)
    xh <- as.tind(nt %% 86400, type = "h")
    xn <- as.tind(nt, type = "n")
    xi <- as.tind(ni, type = "i")
    types <- .ti_type(long = FALSE)

    tz12 <- if (length(tzs) >= 2L) sample(tzs, 2L) else tzs[c(1L, 1L)]
    tz1 <- tz12[1L]
    tz2 <- tz12[2L]

    for (tp1 in types) {
        if (tp1 == "t") {
            ti1 <- as.tind(nt, tz = tz1)
        } else ti1 <- get(paste0("x", tp1))
        expect_equal(ti1, as.tind(ti1))
        for (tp2 in types) {
            if (tp1 == tp2) {
                ti2 <- as.tind(ti1, tp2)
                expect_equal(ti1, ti2)
                if (tp1 == "t") {
                    ti2 <- as.tind(ti1, tz = tz2)
                    expect_equal(as.numeric(ti1), as.numeric(ti2))
                    expect_equal(.get.tz(ti2), tz2)
                }
                next
            }
            nok <- (tp1 %in% c("i", "n")) && !(tp2 %in% c("i", "n")) ||
                   !(tp1 %in% c("i", "n")) && (tp2 %in% c("i", "n")) ||
                   (tp1 == "w") && (tp2 %in% c("q", "m")) ||
                   (tp1 %in% c("q", "m")) && (tp2 == "w") ||
                   (tp1 == "n") && (tp2 == "i") ||
                   (tp1 != "t") && (tp2 == "h") ||
                   (tp1 == "h")
            if (nok) {
                err <- paste0("cast from time index type ", .ti_type2char(tp1),
                              " to type ", .ti_type2char(tp2), " in ",
                              sQuote("as.tind"), " not possible")
                expect_error(as.tind(ti1, tp2), err, fixed = TRUE)
                next
            }
            ti2 <- as.tind(ti1, tp2)
            expect_equal(.get.type(ti2), tp2)
            if (tp2 == "t") {
                ti2 <- as.tind(ti1, tz = tz2)
                expect_equal(.get.type(ti2), tp2)
                expect_equal(.get.tz(ti2), tz2)
            } else expect_null(.get.tz(ti2))
        }
    }
})


## NOTE: 'as.year', ... are tested indirectly below

test_that("'as.tind.(double|integer)' and 'as.(double|integer).tind' work correctly", {
    x <- as.tind(yy)
    expect_equal(x, as.year(yy))
    expect_equal(.get.type(x), "y")
    expect_equal(as.integer(x), as.integer(yy))
    expect_equal(as.double(x), as.double(yy))

    x <- as.tind(qq)
    expect_equal(x, as.quarter(qq))
    expect_equal(.get.type(x), "q")
    expect_equal(as.integer(x), as.integer(qq))
    expect_equal(as.double(x), as.double(qq %% 10 - 1 + 4 * (qq %/% 10)))

    x <- as.tind(mm)
    expect_equal(x, as.month(mm))
    expect_equal(.get.type(x), "m")
    expect_equal(as.integer(x), as.integer(mm))
    expect_equal(as.double(x), as.double(mm %% 100 - 1 + 12 * (mm %/% 100)))

    err <- paste0("time index type could not be automatically inferred; provide ",
                  sQuote("type"), " argument")
    expect_error(as.tind(ww), err, fixed = TRUE)
    x <- as.tind(ww, "w")
    expect_equal(x, as.week(ww))
    expect_equal(.get.type(x), "w")
    expect_equal(as.integer(x), as.integer(ww))
    expect_equal(as.double(x), as.double(.validate_yw(ww %/% 100, ww %% 100)))

    x <- as.tind(dd)
    expect_equal(x, as.date(dd))
    expect_equal(.get.type(x), "d")
    expect_equal(as.integer(x), as.integer(dd))
    expect_equal(as.double(x),
                     as.double(.validate_ymd(dd %/% 10000, dd %% 10000 %/% 100, dd %% 100)))

    expect_error(as.tind(nt), err, fixed = TRUE)
    x <- as.tind(nt, "t")
    expect_equal(x, as.date_time(nt))
    expect_equal(.get.type(x), "t")
    stz <- Sys.timezone()
    if (!is.na(stz) && stz != "") expect_equal(.get.tz(x), stz)
    for (tz in tzs) {
        x <- as.tind(nt, tz = tz)
        expect_equal(.get.type(x), "t")
        expect_equal(.get.tz(x), tz)
        expect_equal(as.numeric(x), nt)
    }

    nh <- round(nt %% 86400)
    x <- as.tind(nh, "h")
    expect_equal(x, as.time(nh))
    expect_equal(.get.type(x), "h")
    expect_equal(as.numeric(x), nh)
})


test_that("'as.tind.logical' and 'as.logical.tind' work correctly", {
    expect_equal(as.logical(tind(y = c(0, 2022, NA))), c(TRUE, TRUE, NA))
    warn <- "NAs introduced by coercion"
    expect_silent(ll <- as.tind(NA, "y"))
    expect_equal(ll, tind(length = 1L, type = "y"))
    expect_warning(ll <- as.tind(c(TRUE, FALSE, NA), "d"), warn, fixed = TRUE)
    expect_equal(ll, tind(length = 3L, type = "d"))
})


test_that("'as.tind.character' and 'as.character.tind' work correctly", {
    fmts <- list(y = c("%Y", "%y"),
                 q = c("%YQ%q", "%Y.%q"),
                 m = c("%Y-%m", "%b %Y"),
                 w = c("%G-W%V", "%g-w%V"),
                 d = c("%Y-%m-%d", "%b %d, %Y"),
                 t = c("%Y-%m-%d %H:%M:%S%z", "%Y-%m-%d %H:%M:%S %Z"),
                 h = c("%H:%M:%S", "%I:%M:%S %p"))
    ords <- list(y = c("y", "y"),
                 q = c("yq", "yq"),
                 m = c("ym", "my"),
                 w = c("yw", "yw"),
                 d = c("ymd", "mdy"),
                 t = c("ymdHMSz", "ymdHMSz"),
                 h = c("HMS", "IMSp"))
    tz <- sample(setdiff(tzs, "Etc/GMT+1"), 1L)
    xx <- as.tind(nt, tz = tz)
    for (tp in names(fmts)) {
        x <- as.tind(xx, type = tp)
        xc <- as.character(x)
        expect_equal(as.character(xx[0L]), character())
        expect_equal(xc, format(x, fmts[[tp]][[1L]]))
        for (i in seq_along(fmts[[tp]])) {
            fmt <- fmts[[tp]][[i]]
            ord <- ords[[tp]][[i]]
            xc <- as.character(x, fmt)
            expect_equal(xc, format(x, fmt))
            if (tp == "t") {
                expect_equal(as.tind(xc, tz = tz), x)
                if (i == 1L) expect_true(is.tind(as.tind(xc)))
                expect_equal(as.tind(xc, format = fmt, tz = tz), x)
                expect_equal(as.tind(xc, order = ord, tz = tz), x)
            } else {
                if (i == 1L) expect_equal(as.tind(xc), x)
                expect_equal(as.tind(xc, format = fmt), x)
                expect_equal(as.tind(xc, order = ord), x)
            }
        }
    }
    # errors
    err <- paste0(sQuote("format"), " provided together with ", sQuote("order"))
    expect_error(as.tind(xc, format = fmts[[tp]][[1L]], order = ords[[tp]][[1L]]), err, fixed = TRUE)
    # i, n
    x <- as.tind(ni, type = "i")
    xc <- as.character(x)
    err <- paste0("^format specification for type ", dQuote("i"),
                  " \\(integer index\\) is not supported")
    expect_error(as.character(x, ""), err)
    err <- paste0(sQuote("format"), " or ", sQuote("order"), " provided for type ",
                  dQuote("i"), " (integer index)")
    expect_error(as.tind(xc, type = "i", format = ""), err, fixed = TRUE)
    expect_error(as.tind(xc, type = "i", order = ""), err, fixed = TRUE)
    expect_equal(as.tind(xc, type = "i"), x)
    err <- paste0("^time index type could not be automatically inferred; ",
                  "provide ", sQuote("type"), ", ", sQuote("format"), ", or ",
                  sQuote("order"), " argument$")
    expect_error(as.tind(xc), err)
    x <- as.tind(nt, type = "n")
    xc <- as.character(x)
    err <- paste0("^format specification for type ", dQuote("n"),
                  " \\(numeric index\\) is not supported")
    expect_error(as.character(x, ""), err)
    err <- paste0(sQuote("format"), " or ", sQuote("order"), " provided for type ",
                  dQuote("n"), " (numeric index)")
    expect_error(as.tind(xc, type = "n", format = ""), err, fixed = TRUE)
    expect_error(as.tind(xc, type = "n", order = ""), err, fixed = TRUE)
    expect_equal(as.tind(xc, type = "n"), x)
    err <- paste0("^time index type could not be automatically inferred; ",
                  "provide ", sQuote("type"), ", ", sQuote("format"), ", or ",
                  sQuote("order"), " argument$")
    expect_error(as.tind(xc), err)
})


test_that("'as.tind.factor' works correctly", {
    xx <- as.tind(dd)
    ff <- as.factor(xx)
    expect_equal(ff, as.factor(as.character(xx)))
    expect_equal(as.tind(ff), xx)
})


test_that("'as.Date.tind' and 'as.tind.Date' work correctly", {
    d <- as.tind(dd)
    names(d) <- sample(letters, NN, replace = TRUE)

    deffmts <- c(y = "%Y", q = "%YQ%q", m = "%Y-%m", w = "%G-W%V", d = "%Y-%m-%d")

    for (tp in names(deffmts)) {
        x <- as.tind(d, tp)
        Dx <- as.Date(x)
        expect_true(inherits(Dx, "Date"))
        if (tp == "q") {
            qchar <- paste0(format(Dx, "%Y"), "Q",
                            (as.integer(format(Dx, "%m")) - 1L) %/% 3 + 1L)
            expect_equal(unname(as.character(x)[!is.na(d)]), qchar[!is.na(d)])
        } else {
            expect_equal(as.character(x), format(Dx, deffmts[tp]))
        }
    }

    for (tp in c("h", "i", "n")) {
        x <- tind(type = tp)
        err <- paste0("cast from time index type ", .ti_type2char(tp), " to type ",
                      .ti_type2char("d"), " in ", sQuote("as.Date"),
                      " not possible")
        expect_error(as.Date(x), err, fixed = TRUE)
    }

    expect_equal(as.tind(as.Date(d)), d)
})


test_that("'as.POSIX[cl]t.tind' and 'as.tind.POSIX[cl]t' work correctly", {
    tz <- sample(setdiff(tzs, "Etc/GMT+1"), 1L)
    tt <- as.tind(nt, type = "t", tz = tz)

    deffmts <- c(y = "%Y", q = "%YQ%q", m = "%Y-%m", w = "%G-W%V", d = "%Y-%m-%d",
                 t = "%F %T %Z")

    for (tp in names(deffmts)) {
        x <- as.tind(tt, tp)
        Pxc <- as.POSIXct(x)
        expect_true(inherits(Pxc, "POSIXct"))
        Pxl <- as.POSIXlt(x)
        expect_true(inherits(Pxl, "POSIXlt"))
        if (tp == "t") {
            expect_equal(attr(Pxc, "tzone"), tz)
            expect_equal(attr(Pxl, "tzone")[1L], tz)
            expect_equal(format(x), format(Pxc, deffmts[tp]))
            expect_equal(format(x), format(Pxl, deffmts[tp]))
            expect_equal(as.tind(Pxc), tt)
            expect_equal(as.tind(Pxl), tt)
        } else if (tp == "q") {
            qchar <- paste0(format(Pxc, "%Y"), "Q",
                            (as.integer(format(Pxc, "%m")) - 1L) %/% 3 + 1L)
            expect_equal(as.character(x)[!is.na(tt)], qchar[!is.na(tt)])
            qchar <- paste0(format(Pxl, "%Y"), "Q",
                            (as.integer(format(Pxl, "%m")) - 1L) %/% 3 + 1L)
            expect_equal(as.character(x)[!is.na(tt)], qchar[!is.na(tt)])
        } else {
            expect_equal(as.character(x), format(Pxc, deffmts[tp]))
            expect_equal(as.character(x), format(Pxl, deffmts[tp]))
        }
    }

    for (tp in c("h", "i", "n")) {
        x <- tind(type = tp)
        err <- paste0("cast from time index type ", .ti_type2char(tp), " to type ",
                      .ti_type2char("t"), " in ", sQuote("as.POSIXct"),
                      " not possible")
        expect_error(as.POSIXct(x), err, fixed = TRUE)
        err <- paste0("cast from time index type ", .ti_type2char(tp), " to type ",
                      .ti_type2char("t"), " in ", sQuote("as.POSIXlt"),
                      " not possible")
        expect_error(as.POSIXlt(x), err, fixed = TRUE)
    }
})


test_that("'as.data.frame.tind' and 'as.tind.data.frame' work correctly", {
    xd <- as.tind(dd)
    xt <- as.tind(round(nt), "t")
    dfd <- as.data.frame(xd)
    dft <- as.data.frame(xt)
    expect_true(is.data.frame(dfd))
    expect_equal(nrow(dfd), length(xd))
    expect_equal(ncol(dfd), 1L)
    expect_equal(dfd[[1L]], xd)
    expect_equal(as.tind(dfd), xd)
    expect_true(is.data.frame(dft))
    expect_equal(nrow(dft), length(xt))
    expect_equal(ncol(dft), 1L)
    expect_equal(dft[[1L]], xt)
    expect_equal(as.tind(dft), xt)

    xt <- xt[!is.na(xt)]
    chd <- format(xt, "%F")
    chh <- format(xt, "%T%z")
    dft2 <- data.frame(chd, chh)
    expect_equal(xt, as.tind(dft2))

    expect_equal(as.tind(dft2[, 1L]), as.tind(xt, "d"))

    err <- paste0("trying to convert a data frame with no columns to ", sQuote("tind"))
    expect_error(as.tind(dft2[, 0L]), err, fixed = TRUE)
})


test_that("'as.list.tind' works correctly", {
    ld <- as.list(dd)
    xx <- as.tind(dd)
    expect_equal(as.list(xx), lapply(ld, as.date))
    # names
    dd10 <- dd[1L:10L]
    names(dd10) <- letters[1L:10L]
    ld10 <- as.list(dd10)
    xx10 <- as.tind(dd10)
    expect_equal(as.list(xx10), lapply(ld10, as.date))
})

