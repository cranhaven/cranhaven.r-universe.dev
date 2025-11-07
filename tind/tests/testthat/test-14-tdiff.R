context("tdiff class")
# ###################################################################

# supported units of time
units <- c("y", "q", "m", "w", "d", "h", "min", "s")

# test sample size
NN <- 99L

# test samples
yy <- sample(1990L:2020L, NN, replace = TRUE)
qq <- .validate_yq(yy, sample.int(4L, size = NN, replace = TRUE))
mm <- .validate_ym(yy, sample.int(12L, size = NN, replace = TRUE))
mm <- .validate_ym(yy, sample.int(12L, size = NN, replace = TRUE))
ww <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(yy))
ww <- .validate_yw(yy, ww)
dd <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(mm))
dd <- .validate_ymd(.m2y(mm), .m2mnth(mm), dd)
yy <- sample(0L:9999L, NN, replace = TRUE)
tt0 <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 3)
tt1 <- round(tt0)
tt2 <- round(tt0 / 60) * 60
tt3 <- round(tt0 / 3600) * 3600


test_that("'.validate_tdiff' works correctly", {
    rng <- c(y = 9999,
             q = .validate_yq(9999, 4) - .validate_yq(0, 1),
             m = .validate_ym(9999, 12) - .validate_ym(0, 1),
             w = .validate_yw(9999, 52) - .validate_yw(0, 1),
             d = .validate_ymd(9999, 12, 31) - .validate_ymd(0, 1, 1),
             t = 253402246800 - -62167165200)

    for (un in setdiff(names(rng), "t")) {
        rt <- rng[un]
        x <- c(-rt - 1, -rt, rt + 0:2)
        vx <- .validate_tdiff(x, un)
        expect_equal(vx[!is.na(vx)], x[2L:4L])
    }
    rt <- rng["t"]
    x <- c(-rt - 1, -rt, rt + 1)
    vx <- .validate_tdiff(x, "t")
    expect_equal(vx[!is.na(vx)], x[2L])
})



test_that("'as.tdiff' works correctly", {
    # default, NULL, numeric
    err <- ".*unit.* missing"
    expect_error(as.tdiff(1), err)
    expect_error(as.tdiff(NULL), err)
    un <- sample(units, 1L)
    expect_equal(as.tdiff(NULL, un), as.tdiff(numeric(), un))
    err <- paste0(sQuote("as.tdiff"), " method not defined")
    expect_error(as.tdiff(today()), err, fixed = TRUE)
    warn <- "NAs introduced"
    expect_warning(res <- as.tdiff(9999:10001, "y"), warn, fixed = TRUE)
    expect_equal(res, as.tdiff(c(9999:10000, NA), "y"))

    # character, factor
    expect_equal(as.tdiff("1d"), as.tdiff(1, "d"))
    expect_equal(as.tdiff("d"), as.tdiff(1, "d"))
    expect_equal(as.tdiff(as.factor(paste0(1:3, "d"))), as.tdiff(1:3, "d"))
    err <- "parse error / could not recognise format"
    expect_error(as.tdiff("dd"), err, fixed = TRUE)

    # difftime
    xx <- sample(-100:100, NN, replace = TRUE)
    map <- c(s = "secs", min = "mins", h = "hours", d = "days", w = "weeks")
    for (un in names(map)) {
        expect_equal(as.tdiff(as.difftime(xx, units = map[un])), as.tdiff(xx, un))
    }
    warn <- "NAs introduced"
    dtw <- as.difftime(51:53 * 10000, units = "weeks")
    expect_warning(tdw <- as.tdiff(dtw), warn, fixed = TRUE)
    expect_equal(tdw, as.tdiff(c(51:52 * 10000, NA), "w"))
})


test_that("'years', ..., 'secs' work correctly", {
    funcs <- c("years", "qrtrs", "mnths", "weeks", "days", "hours", "mins", "secs")
    for (f in funcs) {
        u <- if (f == "mins") "min" else substr(f, 1L, 1L)
        xx <- sample(-100:100, NN, replace = TRUE)
        expect_equal(do.call(f, list(xx)), as.tdiff(xx, u))
    }
})


test_that("'as.character.tdiff' and 'format.tdiff' work correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    # basic R implementations
    .as.character.tdiff0 <- function(x)
    {
        nms <- names(x)
        type <- .get_tdiff_type(x)
        if (anyna <- anyNA(x)) {
            nna <- !is.na(x)
            res <- rep(NA_character_, length(x))
            names(res) <- nms
            if (!any(nna)) { names(res) <- nms; return (res) }
            x <- x[nna]
        }
        x <- as.vector(x)
        if (type == "t") {
            ax <- abs(x)
            nx <- (x < 0)
            s <- round(ax %% 60, 6)
            m <- as.integer((ax %% 3600) %/% 60)
            h <- as.integer((ax %% 86400) %/% 3600)
            d <- as.integer(ax %/% 86400)
            if (all(x == 0)) {
                sflag <- TRUE
                mflag <- FALSE
            } else {
                sflag <- any(s != 0)
                mflag <- any(m) || sflag && (any(h) || any(d))
            }
            xf <- rep("", length(x))
            xf[nx] <- paste0("-", xf[nx])
            # days
            iid <- as.logical(d)
            xf[iid] <- paste0(xf[iid], d[iid], "d")
            # hours
            xf[iid] <- paste0(xf[iid], formatC(h[iid], width = 2L, format = "d", flag = "0"))
            iih <- !iid & (as.logical(h) | as.logical(m) | !mflag && !sflag |
                   mflag && !sflag)
            xf[iih] <- paste0(xf[iih], h[iih])
            iih <- iid | iih
            xf[iih] <- paste0(xf[iih], "h")
            # minutes
            if (mflag) {
                iim <- !iih & (as.logical(m) | !sflag)
                xf[iim] <- paste0(xf[iim], m[iim])
                xf[iih] <- paste0(xf[iih], formatC(m[iih], width = 2L, format = "d", flag = "0"))
                iim <- iim | iih
                xf[iim] <- paste0(xf[iim], "m")
            } else iim <- rep(FALSE, length(x))
            # seconds
            if (sflag) {
                sf <- paste0(s %/% 10, format(round(s %% 10, 6)))
                xf[iim] <- paste0(xf[iim], sf[iim])
                xf[!iim] <- paste0(xf[!iim], gsub("^0([0-9])", "\\1", sf[!iim]))
                xf <- paste0(xf, "s")
            }
        } else {
            xf <- paste0(x, type)
        }
        if (anyna) { res[nna] <- xf; return (res) }
        names(xf) <- nms
        return (xf)
    }

    .format.tdiff0 <- function(x)
    {
        xf <- format(.as.character.tdiff0(x), justify = "right")

        type <- .get_tdiff_type(x)
        nms <- names(x)
        if (type == "y") { names(xf) <- nms; return (xf) }
        attributes(x) <- NULL
        xfaux <- rep("", length(x))
        if (type == "t") { x <- round(x / 86400); type <- "d" }
        if ((type == "q") && (aux <- .anyTRUE(addi <- (abs(x) >= 4L)))) {
            addi[is.na(addi)] <- FALSE
            xfaux[addi] <- paste0("(", round(x[addi] / 4, 2), "y)")
        }
        if ((type == "m") && (aux <- .anyTRUE(addi <- (abs(x) >= 12L)))) {
            addi[is.na(addi)] <- FALSE
            xfaux[addi] <- paste0("(", round(x[addi] / 12, 2), "y)")
        }
        if ((type == "w") && (aux <- .anyTRUE(addi <- (abs(x) >= 52L)))) {
            addi[is.na(addi)] <- FALSE
            xfaux[addi] <- paste0("(~", round(x[addi] / 52.1775, 1), "y)")
        }
        if ((type == "d") && (aux <- .anyTRUE(addi <- (abs(x) >= 30L)))) {
            addi[is.na(addi)] <- FALSE
            addi <- which(addi)
            mm <- round(x[addi] / 30.44, 1)
            iy <- (abs(mm) >= 12)
            xfaux[addi[!iy]] <- paste0("(~", mm[!iy], "m)")
            xfaux[addi[iy]] <- paste0("(~", round(x[addi[iy]] / 365.2425, 1), "y)")
        }
        if (aux) xf <- paste(xf, format(xfaux, justify = "left"))
        names(xf) <- nms
        return (xf)
    }

    # actual tests
    for (un in c("y", "q", "m", "w", "d")) {
        xx <- get(paste0(un, un))
        dxx <- as.tdiff(xx - rev(xx), un)
        expect_equal(as.character(dxx), .as.character.tdiff0(dxx))
        expect_equal(format(dxx), .format.tdiff0(dxx))
        expect_true(all(diff(nchar(format(dxx))) == 0L))
        dxx <- dxx[1L:4L]
        names(dxx) <- letters[1L:4L]
        expect_equal(as.character(dxx), .as.character.tdiff0(dxx))
        expect_equal(format(dxx), .format.tdiff0(dxx))
    }
    for (ttv in 0:3) {
        xx <- get(paste0("tt", ttv))
        dxx <- as.tdiff(xx - rev(xx), "s")
        expect_equal(as.character(dxx), .as.character.tdiff0(dxx))
        expect_true(all(diff(nchar(format(dxx))) == 0L))
        expect_equal(format(dxx), .format.tdiff0(dxx))
        dxx <- dxx[1L:4L]
        names(dxx) <- letters[1L:4L]
        expect_equal(as.character(dxx), .as.character.tdiff0(dxx))
        expect_equal(format(dxx), .format.tdiff0(dxx))
    }
    # empty
    un <- sample(units, 1L)
    xx <- as.tdiff(numeric(), un)
    expect_equal(as.character(xx), character())
    expect_equal(format(xx), character())
})



test_that("'as.integer.tdiff' and 'as.double.tdiff' work correctly", {
    un <- sample(head(units, -3L), 1L)
    td <- as.tdiff(-1:1, un)
    expect_equal(as.integer(td), -1L:1L)
    expect_equal(as.double(td), as.double(-1:1))
    td <- hours(-1:1)
    expect_equal(as.integer(td), -1L:1L)
    expect_equal(as.double(td), 3600. * (-1:1))
    td <- mins(-1:1)
    expect_equal(as.integer(td), -1L:1L)
    expect_equal(as.double(td), 60. * (-1:1))
    td <- secs(-1:1)
    expect_equal(as.integer(td), -1L:1L)
    expect_equal(as.double(td), as.double(-1:1))
})



test_that("'as.list.tdiff' works correctly", {
    xx <- sample(-100:100, NN, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)
    expect_equal(as.list(td), lapply(xx, function(x) as.tdiff(x, un)))
    xx10 <- xx[1L:10L]
    names(xx10) <- letters[1L:10L]
    lx10 <- as.list(xx10)
    dx10 <- as.tdiff(xx10, un)
    expect_equal(as.list(dx10), lapply(lx10, function(x) as.tdiff(x, un)))
})


test_that("'as.data.frame.tdiff' works correctly", {
    xx <- sample(-100:100, NN, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)

    df <- as.data.frame(td)
    expect_true(is.data.frame(df))
    expect_equal(nrow(df), length(td))
    expect_equal(ncol(df), 1L)
    expect_equal(df[[1L]], td)
})



test_that("tdiff '[', '[[', '[<-', and '[[<-' methods work correctly", {
    xx <- sample(-100:100, NN, replace = FALSE)
    un <- sample(units, 1L)
    dx <- as.tdiff(xx, un)
    ii <- sample(1L:NN, 10)
    expect_equal(dx[ii], as.tdiff(xx[ii], un))

    ii <- sample(1L:NN, 10L)
    jj <- sample(1L:NN, 10L)
    dx[ii] <- dx[jj]
    xx[ii] <- xx[jj]
    expect_equal(dx, as.tdiff(xx, un))
    dx[] <- 2000
    expect_true(all(dx == 2000))

    xx <- sample(-100:100, NN, replace = FALSE)
    dx <- days(xx)
    expect_silent(dx[NN + (-2L:0L)] <- NA)
    expect_equal(is.na(dx), c(rep(FALSE, NN - 3L), rep(TRUE, 3L)))
    expect_silent(dx[] <- "-1d")
    expect_true(all(dx == -1))
    expect_silent(dx[1L] <- "2d")
    expect_true(dx[1L] == 2)
    expect_true(all(dx[-1L] == -1))
    expect_silent(dx[1L:2L] <- as.difftime(3, units = "days"))
    expect_true(dx[1L] == dx[2L])
    expect_true(dx[2L] == dx[[2L]])
    expect_true(dx[[1L]] == dx[[2L]])
    expect_silent(dx[[NN - 1L]] <- as.difftime(-5, units = "days"))
    expect_silent(dx[[NN]] <- "-4d")
    expect_true(all(dx[NN + -1:0] == -5:-4))
    expect_silent(dx[[NN]] <- NA)
    expect_true(is.na(dx[NN]))

    err <- paste0("time unit mismatch in ", sQuote("[<-.tdiff"),
                  ": ", .t_unit2char("d"), ", ", .t_unit2char("w"))
    expect_error(dx[1L:3L] <- weeks(1:3), err, fixed = TRUE)
    err <- paste0("time unit mismatch in ", sQuote("[[<-.tdiff"),
                  ": ", .t_unit2char("d"), ", ", .t_unit2char("m"))
    expect_error(dx[[3L]] <- mnths(7), err, fixed = TRUE)
})



test_that("'length<-' works correctly", {
    NN <- sample((NN %/% 2):NN, 1L)
    MM <- sample(2L:5L, 1L)
    xx <- sample(-100:100, NN, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)
    length(xx) <- NN - MM
    length(td) <- NN - MM
    expect_equal(as.tdiff(xx, un), rep(td))

    NN <- sample((NN %/% 2):NN, 1L)
    MM <- sample(2L:5L, 1L)
    xx <- sample(-100:100, NN, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)
    length(xx) <- NN + MM
    length(td) <- NN + MM
    expect_equal(as.tdiff(xx, un), rep(td))
})


test_that("'rep' works correctly", {
    NN <- sample((NN %/% 2):NN, 1L)
    MM <- sample(2L:5L, 1L)
    xx <- sample(-100:100, NN, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)
    expect_equal(as.tdiff(rep(xx, MM), un), rep(td, MM))
})



test_that("'c.tdiff' works correctly", {
    xx <- sample(-100:100, NN, replace = FALSE)
    un <- sample(units, 1L)
    dx <- as.tdiff(xx, un)

    nn <- sample.int(NN - 1L, 1L)
    x1 <- head(xx, nn)
    x2 <- tail(xx, -nn)
    d1 <- as.tdiff(x1, un)
    d2 <- as.tdiff(x2, un)
    expect_equal(dx, c(d1, d2))
    if (!(un %in% c("h", "min"))) expect_equal(dx, c(d1, x2))

    expect_equal(c(days(7), "8d"), days(7:8))
    expect_equal(c(days(7), 8), days(7:8))

    err <- paste("time unit mismatch in", sQuote("c.tdiff"))
    un2 <- if (un %in% tail(units, 3L)) sample(head(units, -3L), 1L)
           else sample(setdiff(units, un), 1L)
    expect_error(c(d1, as.tdiff(x2, un2)), err, fixed = TRUE)
    expect_error(c(as.tdiff(x1, un2), d2), err, fixed = TRUE)
})



test_that("'Math.tind', 'Summary.tind', and 'Complex.tind' methods work correctly", {
    xx <- sample(-100:100, NN, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)

    # math w/0 sign
    for (f in c("abs", "cummin", "cummax")) {
        expect_equal(do.call(f, list(td)),
                         as.tdiff(do.call(f, list(xx)), un))
    }

    # summary + sign
    for (f in c("all", "any", "sign")) {
        expect_equal(do.call(f, list(td)),
                         do.call(f, list(xx)))
    }
    for (f in c("min", "max", "range", "sum")) {
        expect_equal(do.call(f, list(td)),
                         as.tdiff(do.call(f, list(xx)), un))
        expect_equal(do.call(f, list(td)),
                         do.call(f, list(td[1L:10L], td[11L:NN])))
    }

    # math2
    xx <- round(runif(NN, -1, 10), digits = 3)
    un <- "s"
    td <- as.tdiff(xx, un)
    expect_equal(signif(td, 1), as.tdiff(signif(xx, 1), un))
    expect_equal(round(td, 1), as.tdiff(round(xx, 1), un))

    # errors
    err <- paste0(" method not defined for class ", dQuote("tdiff"))
    expect_error(prod(td), paste0(sQuote("prod"), err), fixed = TRUE)
    expect_error(cos(td), paste0(sQuote("cos"), err), fixed = TRUE)
    expect_error(Arg(td), paste0(sQuote("Arg"), err), fixed = TRUE)
})




test_that("'is.unsorted', 'sort', and 'order' tind methods work correctly", {
    xx <- sample(-1:1, 10, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)

    expect_equal(is.unsorted(td), is.unsorted(xx))
    expect_false(is.unsorted(sort(td)))
    expect_equal(sort(td), as.tdiff(sort(xx), un))
})


test_that("'unique', 'duplicated', and 'anyDuplicated' tdiff methods work correctly", {
    xx <- sample(-1:1, 10, replace = TRUE)
    un <- sample(units, 1L)
    td <- as.tdiff(xx, un)

    expect_equal(unique(td), as.tdiff(unique(xx), un))
    expect_equal(unique(as.tdiff(sort(xx), un)), as.tdiff(sort(unique(xx)), un))
    expect_equal(duplicated(td), duplicated(xx))
    expect_equal(duplicated(td, fromLast = TRUE), duplicated(xx, fromLast = TRUE))
    expect_equal(anyDuplicated(td), anyDuplicated(xx))
    expect_equal(anyDuplicated(unique(td)), 0L)
    expect_equal(anyDuplicated(td, fromLast = TRUE), anyDuplicated(xx, fromLast = TRUE))
})





