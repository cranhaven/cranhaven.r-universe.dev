context("set operations on time indices/intervals and time index matching")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test sample size
NN <- 50L
MM <- 10L

# test samples
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
ii <- as.tind(as.integer(runif(NN, -3e2, 3e2)), type = "i")
nn <- as.tind(runif(NN, -3e2, 3e2), type = "n")

.sample2 <- function(x) if (length(x) <= 2L) x else x[sample.int(length(x), 2L)]


test_that("set operations for 'tinterval' work correctly", {
    # simple (and inefficient) R implementation of intersect_t
    .intersect_t0 <- function(x, y)
    {
        x <- unique(x)
        y <- unique(y)
        nx <- length(x)
        ny <- length(y)
        if (!nx) return (x)
        if (!ny) return (y)
        tp <- .get.type(x$start)
        tz <- .get.tz(x$start)

        xstart <- x$start
        xend <- x$end
        ystart <- y$start
        yend <- y$end

        ij <- expand.grid(1:nx, 1:ny)
        i <- ij[[1L]]
        j <- ij[[2L]]
        ok <- if (.is.instant(tp)) (xstart[i] < yend[j]) else (xstart[i] <= yend[j])
        ok[is.na(ok)] <- TRUE
        i <- i[ok]; j <- j[ok]
        ok <- if (.is.instant(tp)) (ystart[j] < xend[i]) else (ystart[j] <= xend[i])
        ok[is.na(ok)] <- TRUE
        i <- i[ok]; j <- j[ok]

        .pminmax <- function(x, y, max)
        {
            xy <- if (max) pmax(x, y) else pmin(x, y)
            nax <- is.na(x)
            xy[nax] <- y[nax]
            nay <- is.na(y)
            xy[nay] <- x[nay]
            return (xy)
        }

        nstart <- .pminmax(xstart[i], ystart[j], TRUE)
        nend <- .pminmax(xend[i], yend[j], FALSE)

        return (.tinterval(nstart, nend))
    }

    for (tp in c("y", "q", "m", "w", "d", "h", "i", "n")) {
        all <- tinterval(tind(length = 1L, type = tp), tind(length = 1L, type = tp))
        none <- tinterval(tind(length = 0L, type = tp), tind(length = 0L, type = tp))
        xx <- get(paste0(tp, tp))
        x0 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
        x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE)
        tixx <- tinterval(x0, x1)
        utixx <- unique(tixx)
        ntixx <- !tixx
        tixx1 <- utixx[1L]
        ntixx1 <- !tixx1
        expect_equal(utixx, unique(utixx))
        expect_equal(ntixx, unique(ntixx))
        expect_equal(ntixx1, unique(ntixx1))
        expect_equal(!ntixx, utixx)
        expect_equal(union_t(tixx, tixx), utixx)
        expect_equal(union_t(tixx, ntixx), all)
        expect_equal(union_t(tixx1, ntixx1), all)
        expect_equal(union_t(utixx, ntixx), all)
        expect_equal(intersect_t(tixx, tixx), utixx)
        expect_equal(intersect_t(tixx, ntixx), none)
        expect_equal(intersect_t(tixx1, ntixx1), none)
        expect_equal(intersect_t(tixx, all), utixx)
        expect_equal(intersect_t(tixx, none), none)
        x0b <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
        x1b <- sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE)
        tixxb <- tinterval(x0b, x1b)
        expect_equal(intersect_t(tixx, tixxb), .intersect_t0(tixx, tixxb))
        expect_equal(intersect_t(setdiff_t(tixx, tixxb), setdiff_t(tixxb, tixx)), none)
        expect_equal(intersect_t(setdiff_t(tixx, tixxb), intersect_t(tixxb, tixx)), none)
        expect_equal(intersect_t(intersect_t(tixx, tixxb), setdiff_t(tixxb, tixx)), none)
        expect_equal(union_t(tixx, tixxb), unique(c(setdiff_t(tixx, tixxb),
                                                        intersect_t(tixx, tixxb),
                                                        setdiff_t(tixxb, tixx))))
        for (tp2 in .sample2(.lo_res_cast(tp))) {
            xx2 <- get(paste0(tp2, tp2))
            x20 <- sort(xx2[sample.int(NN, MM, TRUE)], na.last = FALSE)
            x21 <- sort(xx2[sample.int(NN, MM, TRUE)], na.last = TRUE)
            tixx2 <- tinterval(x20, x21)
            tixx2conv <- as.tinterval(tixx2, tp)
            expect_equal(union_t(tixx, tixx2), union_t(tixx, tixx2conv))
            expect_equal(union_t(tixx, tixx2conv), union_t(tixx, tixx2))
            expect_equal(intersect_t(tixx, tixx2), intersect_t(tixx, tixx2conv))
            expect_equal(intersect_t(tixx, tixx2conv), intersect_t(tixx, tixx2))
            expect_equal(setdiff_t(tixx, tixx2), setdiff_t(tixx, tixx2conv))
            expect_equal(setdiff_t(tixx, tixx2conv), setdiff_t(tixx, tixx2))
            expect_equal(setdiff_t(tixx2, tixx), setdiff_t(tixx2conv, tixx))
            expect_equal(setdiff_t(tixx2conv, tixx), setdiff_t(tixx2, tixx))
        }
    }

    # date-time
    for (tz in .sample2(tzs)) {
        all <- tinterval(tind(length = 1L, tz = tz), tind(length = 1L, tz = tz))
        none <- tinterval(tind(length = 0L, tz = tz), tind(length = 0L, tz = tz))
        xx <- as.tind(tt, tz = tz)
        x0 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
        x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE)
        tixx <- tinterval(x0, x1)
        utixx <- unique(tixx)
        ntixx <- !tixx
        tixx1 <- utixx[1L]
        ntixx1 <- !tixx1
        expect_equal(utixx, unique(utixx))
        expect_equal(ntixx, unique(ntixx))
        expect_equal(ntixx1, unique(ntixx1))
        expect_equal(!ntixx, unique(tixx))
        expect_equal(union_t(tixx, tixx), utixx)
        expect_equal(union_t(tixx, ntixx), all)
        expect_equal(union_t(utixx, ntixx), all)
        expect_equal(intersect_t(tixx, tixx), utixx)
        expect_equal(intersect_t(tixx, ntixx), none)
        expect_equal(intersect_t(tixx, all), utixx)
        expect_equal(intersect_t(tixx, none), none)
        xx2 <- as.tind(tt, tz = tz)
        x0b <- sort(xx2[sample.int(NN, MM, TRUE)], na.last = FALSE)
        x1b <- sort(xx2[sample.int(NN, MM, TRUE)], na.last = TRUE)
        tixxb <- tinterval(x0b, x1b)
        expect_equal(intersect_t(tixx, tixxb), .intersect_t0(tixx, tixxb))
        expect_equal(intersect_t(setdiff_t(tixx, tixxb), setdiff_t(tixxb, tixx)), none)
        expect_equal(intersect_t(setdiff_t(tixx, tixxb), intersect_t(tixxb, tixx)), none)
        expect_equal(intersect_t(intersect_t(tixx, tixxb), setdiff_t(tixxb, tixx)), none)
        expect_equal(union_t(tixx, tixxb), unique(c(setdiff_t(tixx, tixxb),
                                                        intersect_t(tixx, tixxb),
                                                        setdiff_t(tixxb, tixx))))
        for (tp2 in .sample2(setdiff(.lo_res_cast("t"), "h"))) {
            xx2 <- as.tind(xx, tp2)
            x20 <- sort(xx2[sample.int(NN, MM, TRUE)], na.last = FALSE)
            x21 <- sort(xx2[sample.int(NN, MM, TRUE)], na.last = TRUE)
            tixx2 <- tinterval(x20, x21)
            tixx2conv <- as.tinterval(tixx2, tz = tz)
            expect_equal(union_t(tixx, tixx2), union_t(tixx, tixx2conv))
            expect_equal(union_t(tixx, tixx2conv), union_t(tixx, tixx2))
            expect_equal(intersect_t(tixx, tixx2), intersect_t(tixx, tixx2conv))
            expect_equal(intersect_t(tixx, tixx2conv), intersect_t(tixx, tixx2))
            expect_equal(setdiff_t(tixx, tixx2), setdiff_t(tixx, tixx2conv))
            expect_equal(setdiff_t(tixx, tixx2conv), setdiff_t(tixx, tixx2))
            expect_equal(setdiff_t(tixx2, tixx), setdiff_t(tixx2conv, tixx))
            expect_equal(setdiff_t(tixx2conv, tixx), setdiff_t(tixx2, tixx))
        }
    }

    # date-time with different time zones
    if (length(tzs) >= 2L) {
        tz12 <- sample(tzs, 2L)
        tz1 <- tz12[1L]
        tz2 <- tz12[2L]
        xx <- as.tind(tt, tz = tz1)
        x0 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
        x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE)
        tixx <- tinterval(x0, x1)
        xx <- as.tind(tt, tz = tz2)
        x0 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
        x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE)
        tixx2 <- tinterval(x0, x1)
        tixx3 <- tinterval(as.tind(x0, tz = tz1), as.tind(x1, tz = tz1))
        warntz <- "different time zones of arguments: "
        expect_warning(resu <- union_t(tixx, tixx2), warntz, fixed = TRUE)
        expect_equal(resu, union_t(tixx, tixx3))
        expect_warning(resi <- intersect_t(tixx, tixx2), warntz, fixed = TRUE)
        expect_equal(resi, intersect_t(tixx, tixx3))
        expect_warning(resd <- setdiff_t(tixx, tixx2), warntz, fixed = TRUE)
        expect_equal(resd, setdiff_t(tixx, tixx3))
    }
})


test_that("set operations for 'tind' work correctly", {
    for (tp in c("y", "q", "m", "w", "d", "t", "h", "i", "n")) {
        xx <- get(paste0(tp, tp))
        x0 <- xx[sample.int(NN, MM, TRUE)]
        x1 <- xx[sample.int(NN, MM, TRUE)]
        ux <- union_t(x0, x1)
        expect_equal(.unclass(ux), union(.unclass(x0), .unclass(x1)))
        ix <- intersect_t(x0, x1)
        expect_equal(.unclass(ix), intersect(.unclass(x0), .unclass(x1)))
        sx <- setdiff_t(x0, x1)
        expect_equal(.unclass(sx), setdiff(.unclass(x0), .unclass(x1)))
        # zero vectors
        expect_equal(intersect_t(x0[0L], x1), xx[0L])
        expect_equal(intersect_t(x0, x1[0L]), xx[0L])
        expect_equal(setdiff_t(x0[0L], x1), xx[0L])
        expect_equal(setdiff_t(x0, x1[0L]), unique(x0))
        expect_equal(union_t(x0, x1[0L]), unique(x0))
        expect_equal(union_t(x0[0L], x1), unique(x1))

        # sorted versions
        x0 <- sort(x0)
        x1 <- sort(x1)
        ux <- union_t(x0, x1)
        expect_equal(.unclass(ux), sort(union(.unclass(x0), .unclass(x1))))
        ix <- intersect_t(x0, x1)
        expect_equal(.unclass(ix), sort(intersect(.unclass(x0), .unclass(x1))))
        sx <- setdiff_t(x0, x1)
        expect_equal(.unclass(sx), sort(setdiff(.unclass(x0), .unclass(x1))))
        # zero vectors
        expect_equal(intersect_t(x0[0L], x1), xx[0L])
        expect_equal(intersect_t(x0, x1[0L]), xx[0L])
        expect_equal(setdiff_t(x0[0L], x1), xx[0L])
        expect_equal(setdiff_t(x0, x1[0L]), unique(x0))
        expect_equal(union_t(x0, x1[0L]), unique(x0))
        expect_equal(union_t(x0[0L], x1), unique(x1))
    }

    # date-time with different time zones
    if (length(tzs) >= 2L) {
        tz12 <- sample(tzs, 2L)
        tz1 <- tz12[1L]
        tz2 <- tz12[2L]
        xx <- as.tind(tt, tz = tz1)
        x0 <- xx[sample.int(NN, MM, TRUE)]
        x1 <- as.tind(xx[sample.int(NN, MM, TRUE)], tz = tz2)
        warntz <- "different time zones of arguments: "
        expect_warning(ux <- union_t(x0, x1), warntz, fixed = TRUE)
        expect_equal(.unclass(ux), union(.unclass(x0), .unclass(x1)))
        expect_warning(ix <- intersect_t(x0, x1), warntz, fixed = TRUE)
        expect_equal(.unclass(ix), intersect(.unclass(x0), .unclass(x1)))
        expect_warning(sx <- setdiff_t(x0, x1), warntz, fixed = TRUE)
        expect_equal(.unclass(sx), setdiff(.unclass(x0), .unclass(x1)))
    }

    # mismatch
    tps <- sample(c("y", "q", "m", "w", "d", "t", "h", "i", "n"), 2L)
    tp1 <- tps[1L]
    tp2 <- tps[2L]
    x0 <- get(paste0(tp1, tp1))
    x1 <- get(paste0(tp2, tp2))
    err <- "time index type mismatch"
    expect_error(union_t(x0, x1), err, fixed = TRUE)
    expect_error(intersect_t(x0, x1), err, fixed = TRUE)
    expect_error(setdiff_t(x0, x1), err, fixed = TRUE)
    err <- "invalid arguments "
    expect_error(union_t(x0 - x0, x0), err, fixed = TRUE)
    expect_error(union_t(x0, x0 - x0), err, fixed = TRUE)
    expect_error(intersect_t(x0 - x0, x0), err, fixed = TRUE)
    expect_error(intersect_t(x0, x0 - x0), err, fixed = TRUE)
    expect_error(setdiff_t(x0 - x0, x0), err, fixed = TRUE)
    expect_error(setdiff_t(x0, x0 - x0), err, fixed = TRUE)
})



test_that("'match_t' and '%in_t%' work correctly", {
    types <- c("y", "q", "m", "w", "d", "t", "h", "i", "n")
    types0 <- c("q", "m", "w", "d")
    types1 <- c("q", "m", "w", "d", "t")

    .match_t0 <- function(x, table, nomatch = NA_integer_)
    {
        res0 <- logical(length(x))
        for (i in seq_along(x)) {
            res0a <- x[i] >= table$start
            res0b <- x[i] < table$end + !.is.instant(.get.type(x))
            res0a[is.na(res0a)] <- TRUE
            res0b[is.na(res0b)] <- TRUE
            res0i <- res0a & res0b
            res0[i] <- if (any(res0i)) which.max(res0i) else nomatch
        }
        return (res0)
    }

    .in_t0 <- function(x, table) .match_t0(x, table, nomatch = 0L) > 0L

    for (tp in c("t", sample(types0, 1L))) {
        # tind
        xx <- get(paste0(tp, tp))
        tab <- xx[sample.int(NN, MM, FALSE)]
        expect_equal(match_t(xx, tab), match(unclass(xx), unclass(tab)))
        expect_equal(xx %in_t% tab, match_t(xx, tab, 0L) > 0L)
        xx1 <- sort(xx, na.last = TRUE)
        tab1 <- sort(tab, na.last = TRUE)
        expect_equal(match_t(xx1, tab1), match(unclass(xx1), unclass(tab1)))
        expect_equal(xx1 %in_t% tab1, match_t(xx1, tab1, 0L) > 0L)
        # 0's never matched
        xna <- xx
        xna[sample.int(NN, 1L)] <- NA
        tabna <- tab
        tabna[sample.int(NN, 1L)] <- NA
        match0 <- match(unclass(xna), unclass(tabna))
        match0[is.na(xna)] <- NA_integer_
        expect_equal(match_t(xna, tabna), match0)
        expect_equal(xna %in_t% tabna, match_t(xna, tabna, 0L) > 0L)
        # tinterval
        xna <- xx
        xna[sample.int(NN, 1L)] <- NA
        tab2 <- tinterval(sort(xna[sample.int(NN, MM, TRUE)], na.last = FALSE),
                          sort(xna[sample.int(NN, MM, TRUE)], na.last = TRUE))
        expect_equal(match_t(xx, tab2), .match_t0(xx, tab2))
        expect_equal(xx %in_t% tab2, .in_t0(xx, tab2))
        expect_equal(match_t(xx1, tab2), .match_t0(xx1, tab2))
        expect_equal(xx1 %in_t% tab2, .in_t0(xx1, tab2))
    }

    expect_equal(match_t(xx, tab), match_t(as.character(xx), as.character(tab)))
    expect_equal(xx %in_t% tab, as.character(xx) %in_t% as.character(tab))
    expect_equal(match_t(xx, tab1), match_t(as.character(xx), as.character(tab1)))
    expect_equal(xx %in_t% tab1, as.character(xx) %in_t% as.character(tab1))
    expect_equal(match_t(xx, tab2), match_t(as.character(xx), as.character(tab2)))
    expect_equal(xx %in_t% tab2, as.character(xx) %in_t% as.character(tab2))

    # different tz
    if (length(tzs) >= 2L) {
        tz12 <- sample(tzs, 2L)
        tz1 <- tz12[1L]
        tz2 <- tz12[2L]
        xx_ <- as.tind(tt, tz = tz1)
        tab_ <- tinterval(sort(tt[sample.int(NN, MM, TRUE)], na.last = FALSE),
                          sort(tt[sample.int(NN, MM, TRUE)], na.last = TRUE))
        tab_ <- as.tinterval(tab_, tz = tz2)
        expect_warning(xx_ %in_t% tab_)
        expect_warning(match_t(xx_, tab_))
    }

    # conversion
    xx_ <- tt
    tab_ <- tinterval(sort(dd[sample.int(NN, MM, TRUE)], na.last = FALSE),
                      sort(dd[sample.int(NN, MM, TRUE)], na.last = TRUE))
    expect_equal(match_t(xx_, tab_), match_t(as.date(xx_), tab_))
    expect_equal(xx_ %in_t% tab_, as.date(xx_) %in_t%  tab_)

    # errors
    expect_error(match_t(xx, xx - xx))
    expect_error(match_t(xx, tab2, 1:2))
    expect_error(xx %in_t% (xx - xx))
    xx_ <- dd
    tab_ <- tinterval(sort(tt[sample.int(NN, MM, TRUE)], na.last = FALSE),
                      sort(tt[sample.int(NN, MM, TRUE)], na.last = TRUE))
    expect_error(match_t(xx_, tab_))
    expect_error(xx %in_t% tab_)
})


























