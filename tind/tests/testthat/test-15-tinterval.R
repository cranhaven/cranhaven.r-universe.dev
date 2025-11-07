context("tinterval class")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# types
types <- c("y", "q", "m", "w", "d", "t", "i", "n")

# test sample size
NN <- 99L
MM <- 10L

# test samples
nas <- runif(NN) < .1
y <- sample(1990L:2020L, NN, replace = TRUE)
q <- sample.int(4L, size = NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
w <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(y))
d <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(.validate_ym(y, m)))

yy <- tind(y = y)
yy[nas] <- NA
qq <- tind(y = y, q = q)
qq[nas] <- NA
mm <- tind(y = y, m = m)
mm[nas] <- NA
ww <- suppressWarnings(tind(y = y, w = w))
ww[nas] <- NA
dd <- suppressWarnings(tind(y = y, m = m, d = d))

tt0 <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 3)
tt0[nas] <- NA
tt1 <- round(tt0)
tt2 <- round(tt0 / 60) * 60
tt3 <- round(tt0 / 3600) * 3600
tt0 <- as.date_time(tt0)
tt1 <- as.date_time(tt1)
tt2 <- as.date_time(tt2)
tt3 <- as.date_time(tt3)
hh0 <- as.time(tt0)
hh1 <- as.time(tt1)
hh2 <- as.time(tt2)
hh3 <- as.time(tt3)

ii <- as.tind(sample(-1000:1000, NN, replace = TRUE), type = "i")
nn <- as.tind(runif(NN, -3e7, 3e7), type = "n")



test_that("'tinterval', '%--%', and 'is.tinterval' work correctly", {
    # errors
    expect_error(tinterval())
    expect_error(tinterval(start = NULL))
    expect_error(tinterval(end = NULL))
    expect_error(tinterval(start = NULL, end = NULL))
    expect_error(tinterval(start = NULL))
    expect_error(tinterval(end = NULL))
    err <- paste0("different lengths of arguments: ", NN, ", ", NN - 1L)
    expect_error(dd %--% dd[-1L], err, fixed = TRUE)
    # one-sided
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    xx <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    expect_equal(tinterval(xx), tinterval(start = xx))
    expect_equal(tinterval(xx), tinterval(xx, xx[rep(NA_integer_, MM)]))
    expect_equal(tinterval(end = xx), tinterval(xx[rep(NA_integer_, MM)], xx))
    # conversion
    expect_equal(tinterval("2024-09-02", 2024),
                     tinterval("2024-09-02", "2024-12-31"))
    expect_equal(tinterval(2024, "2024-09-02"),
                     tinterval("2024-01-01", "2024-09-02"))
    # conversion error
    expect_error(tinterval(today(), as.time(now())))
    expect_error(tinterval(as.time(now()), today()))
    # time zones
    if (length(tzs) >= 2L) {
        tz12 <- sample(tzs, 2L)
        expect_warning(tinterval(now(tz = tz12[1L]), now(tz = tz12[2L])))
    }
    # names
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    x2 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    n1 <- sample(c(NA_character_, letters), MM)
    n2 <- sample(c(NA_character_, letters), MM)
    ti <- tinterval(x1, x2)
    expect_null(names(ti$start))
    expect_null(names(ti$end))
    names(x1) <- n1
    ti <- tinterval(x1, x2)
    expect_equal(names(ti$start), n1)
    expect_equal(names(ti$end), n1)
    names(x1) <- NULL
    names(x2) <- n2
    ti <- tinterval(x1, x2)
    expect_equal(names(ti$start), n2)
    expect_equal(names(ti$end), n2)
    names(x1) <- n1
    names(x2) <- n2
    n12 <- paste0(n1, ".", n2)
    ii <- n1 == n2
    ii[is.na(ii)] <- FALSE
    n12[ii] <- n1[ii]
    ti <- tinterval(x1, x2)
    expect_equal(names(ti$start), n12)
    expect_equal(names(ti$end), n12)
})



test_that("'as.tinterval.default' works correctly", {
    err <- paste(sQuote("as.tinterval"), "method not defined for class", dQuote("function"))
    expect_error(as.tinterval(sum), err, fixed = TRUE)
})


test_that("'as.character/format.tinterval' and 'as.tinterval.character' work correctly", {
    skip_on_cran() # in case of corner cases, this is also slow...
    # basic R implementations
    .as.character.tinterval0 <- function(x)
    {
        if (!(n <- length(x))) return (character())
        nms <- names(x$start)
        se <- c(x$start, x$end)
        sech <- as.character(se)
        sech[is.na(se)] <- "..."
        sech <- format(sech, justify = "centre")
        res <- paste0(sech[1L:n], " -- ", sech[(n + 1L):(2L * n)])
        names(res) <- nms
        return (res)
    }

    .format.tinterval0 <- function(x)
    {
        if (!(n <- length(x))) return (character())
        nms <- names(x$start)
        se <- c(x$start, x$end)
        sech <- format(se)
        sech[is.na(se)] <- "..."
        sech <- format(sech, justify = "centre")
        xf <- paste0(sech[1L:n], " -- ", sech[(n + 1L):(2L * n)])
        sp <- x$end - x$start + !.is.instant(.get.type(x$start))
        sp[sp <= 0] <- 0L
        spf <- format(sp)
        spf <- sub("^.*\\(([^\\)]+)\\) *$", "\\1", spf)
        spf <- sub("^ *([^ ]+) *$", "\\1", spf)
        spf[is.na(sp)] <- "..."
        spf[sp <= 0] <- "-"
        spf <- format(paste0("(", spf, ")"), justify = "left")
        res <- paste(xf, spf)
        names(res) <- nms
        return (res)
    }

    # actual tests
    for (tp in setdiff(types, c("t", "n"))) {
        xx <- get(paste0(tp, tp))
        xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                        sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
        expect_equal(as.character(xx), .as.character.tinterval0(xx))
        expect_true(all(diff(nchar(as.character(xx))) == 0L))
        expect_equal(format(xx), .format.tinterval0(xx))
        expect_true(all(diff(nchar(format(xx))) == 0L))
    }
    for (tz in tzs) {
        for (ttv in 0:3) {
            xx <- as.tind(get(paste0("tt", ttv)), tz = tz)
            xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                            sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
            expect_equal(as.character(xx), .as.character.tinterval0(xx))
            expect_true(all(diff(nchar(as.character(xx))) == 0L))
            expect_equal(format(xx), .format.tinterval0(xx))
            expect_true(all(diff(nchar(format(xx))) == 0L))
        }
    }

    expect_equal(as.character(xx, sep = " / "), format(xx, sep = " / "))
    expect_equal(as.character(xx[0L]), character())
    expect_equal(format(xx[0L]), character())

    # errors
    errs <- paste("invalid", sQuote("sep"), "argument; nonempty character string expected")
    erro <- paste("invalid", sQuote("open"), "argument; character string expected")
    erre <- paste("invalid", sQuote("empty"), "argument; character string expected")
    expect_error(format(xx, sep = ""), errs, fixed = TRUE)
    expect_error(format(xx, sep = letters), errs, fixed = TRUE)
    expect_silent(format(xx, open = ""))
    expect_error(format(xx, open = letters), erro, fixed = TRUE)
    expect_silent(format(xx, empty = ""))
    expect_error(format(xx, empty = letters), erre, fixed = TRUE)

    # aux
    expect_error(format(xx, aux = NA))
    haux <- format(xx, aux = TRUE)
    naux <- format(xx, aux = FALSE)
    expect_equal(sub(" \\(.*\\) *$", "", haux), naux)

    # as.tinterval.character
    tp <- sample(setdiff(types, c("i", "n")), 1L)
    xx <- if (tp == "t") get(paste0("tt", 1)) else get(paste0(tp, tp))
    xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    xxc <- as.character(xx)
    xxf <- format(xx)
    expect_equal(as.tinterval(xxc), xx)
    expect_equal(as.tinterval(xxf), xx)
    expect_error(as.tinterval(xxf, sep = ""))
    expect_error(as.tinterval(xxf, sep = c(" -- ", " / ")))
    xxf <- format(xx, sep = " / ")
    expect_equal(as.tinterval(xxf, sep = " / "), xx)
})


test_that("'as.tinterval.tinterval' works correctly", {
    xx <- tinterval(sort(dd[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(dd[sample.int(NN, MM, TRUE)], na.last = TRUE))
    expect_equal(as.tinterval(xx), xx)
    expect_equal(as.tinterval(xx, "d"), xx)

    tz <- sample(tzs, 1L)
    expect_error(as.tinterval(xx, "d", tz))
    expect_equal(as.tinterval(xx, "t", tz), as.tinterval(xx, tz = tz))
    expect_error(as.tinterval(xx, "h"))
    expect_equal(as.tinterval(2024 %--% 2025, "d"), "2024-01-01" %--% "2025-12-31")

    expect_error(as.tinterval(as.tinterval(xx, "t", tz), "h"))
})


test_that("'as.list.tinterval' and 'as.tinterval.list' work correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    alxx <- as.list(xx)
    expect_true(is.list(alxx))
    expect_equal(names(alxx), c("start", "end"))
    expect_equal(as.tinterval(alxx), xx)
    err <- "expected a 2-element list"
    expect_error(as.tinterval(alxx[1L]), err, fixed = TRUE)
    expect_error(as.tinterval(alxx[c(1L, 2L, 2L)]), err, fixed = TRUE)
})


test_that("'as.data.frame.tinterval' and 'as.tinterval.data.frame' work correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    adfxx <- as.data.frame(xx)
    expect_true(is.data.frame(adfxx))
    expect_equal(names(adfxx), c("start", "end"))
    expect_equal(as.list(adfxx), as.list(xx))
    expect_equal(as.tinterval(adfxx), xx)
    err <- "expected a 2-column data frame"
    expect_error(as.tinterval(adfxx[1L]), err, fixed = TRUE)
    expect_error(as.tinterval(adfxx[c(1L, 2L, 2L)]), err, fixed = TRUE)
    names(xx) <- sample(letters, MM)
    adfxx <- as.data.frame(xx)
    expect_true(is.data.frame(adfxx))
    expect_equal(names(adfxx), c("start", "end"))
    expect_equal(rownames(adfxx), names(xx))
    expect_equal(as.tinterval(adfxx), xx)
})


test_that("'ti_type.tinterval' works correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    st <- xx$start
    en <- xx$end
    expect_equal(ti_type(xx), ti_type(xx, long = TRUE))
    expect_equal(ti_type(xx, long = FALSE), ti_type(st, long = FALSE))
    expect_equal(ti_type(xx, long = FALSE), ti_type(en, long = FALSE))
    expect_equal(ti_type(xx, long = TRUE), ti_type(st, long = TRUE))
    expect_equal(ti_type(xx, long = TRUE), ti_type(en, long = TRUE))
    expect_equal(ti_type(xx, long = TRUE, valid = TRUE),
                     ti_type(st, long = TRUE, valid = TRUE))
    expect_equal(ti_type(xx, long = TRUE, valid = TRUE),
                     ti_type(en, long = TRUE, valid = TRUE))
})


test_that("'names.tinterval' and 'names<-.tinterval' work correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    expect_null(names(xx))
    nms <- sample(letters, MM)
    names(xx) <- nms
    expect_equal(names(xx), nms)
    expect_equal(names(xx$start), nms)
    expect_equal(names(xx$end), nms)
})


test_that("'length.tinterval' and 'length<-.tinterval' work correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    expect_equal(length(xx), MM)
    err <- paste(sQuote("length<-"), "method not defined for class", dQuote("tinterval"))
    expect_error(length(xx) <- MM - 1L, err, fixed = TRUE)
})


test_that("tinterval '[', '[[', '[<-', and '[[<-' methods work correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    x2 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    xx <- tinterval(x1, x2)

    expect_equal(xx[], xx)
    i <- sample(1L:MM, MM %/% 2L)
    j <- sample(1L:MM, MM %/% 2L)
    xx[i] <- xx[j]
    x1[i] <- x1[j]
    x2[i] <- x2[j]
    expect_equal(tinterval(x1, x2), xx)

    i1 <- i[1L]
    xx[] <- xx[i1]
    expect_true(all(sapply(seq_along(xx), function(i)
                                isTRUE(all.equal(unclass(xx[[i]]), unclass(xx[[i1]]))))))

    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    x2 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    xx <- tinterval(x1, x2)
    ii <- sample(1L:MM, MM %/% 2L)
    jj <- sample(1L:MM, MM %/% 2L)
    i1 <- ii[1L]
    j1 <- jj[1L]
    expect_silent(xx[[i1]] <- xx[[j1]])
    expect_true(isTRUE(all.equal(unclass(xx[[i1]]), unclass(xx[[j1]]))))
    expect_error(xx[[ii[1L:2L]]] <- xx[[j1]])
    expect_error(xx[[i1]] <- xx[[jj[1L:2L]]])

    # conversion errors
    xx <- dd
    x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    x2 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    xx <- tinterval(x1, x2)
    expect_error(xx[1L] <- "09:00" %--% "17:00")
    expect_error(xx[[1L]] <- "09:00" %--% "17:00")

    # diff tz
    if (length(tzs) >= 2L) {
        tz12 <- sample(tzs, 2L)
        tz1 <- tz12[1L]
        tz2 <- tz12[2L]
        x1 <- as.tind(tt1, tz = tz1)
        x2 <- as.tind(tt1, tz = tz2)
        i1 <- sample.int(NN, MM, TRUE)
        i2 <- sample.int(NN, MM, TRUE)
        xx1 <- tinterval(x1[i1], x1[i2])
        xx2 <- tinterval(x2[i1], x2[i2])
        ii <- sample(1L:MM, MM %/% 2L)
        jj <- sample(1L:MM, MM %/% 2L)
        warn  <- "^different time zones"
        expect_warning(xx1[[ii[1L]]] <- xx2[jj[1L]])
        xx2[ii[1L]] <- xx2[jj[1L]]
        expect_equal(xx1, as.tinterval(xx2, tz = tz1))
        expect_warning(xx1[ii] <- xx2[jj])
        xx2[ii] <- xx2[jj]
        expect_equal(xx1, as.tinterval(xx2, tz = tz1))
    }
})


test_that("'[[<-.tinterval' works correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    x2 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    xx <- tinterval(x1, x2)

    # warnPartialMatchDollar
    xxx <- xx
    options(warnPartialMatchDollar = TRUE)
    warn <- paste("partial match of", sQuote("st"), "to", sQuote("start"))
    expect_warning(xxx$st <- xx$end, warn, fixed = TRUE)
    warn <- paste("partial match of", sQuote("en"), "to", sQuote("end"))
    expect_warning(xxx$en <- xx$start, fixed = TRUE)
    expect_equal(xxx, tinterval(x2, x1))
    xxx <- xx
    options(warnPartialMatchDollar = FALSE)
    expect_silent(xxx$st <- xx$end)
    expect_silent(xxx$end <- xx$start)
    expect_equal(xxx, tinterval(x2, x1))

    # NULL
    xxx <- xx
    xxx$start <- NULL
    expect_equal(xxx, tinterval(NULL, x2))
    xxx <- xx
    xxx$end <- NULL
    expect_equal(xxx, tinterval(x1, NULL))

    # diff lengths
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    x2 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    xx <- tinterval(x1, x2)
    err <- paste0("replacement is of length ", MM - 1L, ", number of intervals is ", MM)
    expect_error(xx$start <- xx$start[-MM])
    expect_error(xx$end <- xx$end[-MM])

    # conversion errors
    xx <- mm
    x1 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    x2 <- sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE)
    xx <- tinterval(x1, x2)
    yy <- dd
    y1 <- sort(yy[sample.int(NN, MM, TRUE)], na.last = FALSE)
    y2 <- sort(yy[sample.int(NN, MM, TRUE)], na.last = FALSE)
    yy <- tinterval(y1, y2)
    expect_silent(yy$start <- xx$start)
    expect_silent(yy$end <- xx$end)
    expect_equal(yy, tinterval(as.date(xx$start), as.date(xx$end + 1L) - 1L))
    expect_error(xx$start <- yy$start)
    expect_error(xx$end <- yy$end)

    # diff tz
    if (length(tzs) >= 2L) {
        tz12 <- sample(tzs, 2L)
        tz1 <- tz12[1L]
        tz2 <- tz12[2L]
        x1 <- as.tind(tt1, tz = tz1)
        x2 <- as.tind(tt1, tz = tz2)
        i1 <- sample.int(NN, MM, TRUE)
        i2 <- sample.int(NN, MM, TRUE)
        xx1 <- tinterval(x1[i1], x1[i2])
        xx2 <- tinterval(x2[i1], x2[i2])
        warn  <- "^different time zones"
        expect_warning(xx1$start <- xx2$start)
        expect_equal(xx1, as.tinterval(xx2, tz = tz1))
        expect_warning(xx1$end <- xx2$end)
        expect_equal(xx1, as.tinterval(xx2, tz = tz1))
    }
})

test_that("'c.tinterval' works correctly", {
    tp <- sample(types, 1L)
    xx <- if (tp == "t") get(paste0("tt", sample(0:3, 1L))) else get(paste0(tp, tp))
    xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                    sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
    names(xx) <- sample(letters, MM)

    expect_equal(c(xx), xx)
    err <- paste("expected all arguments to be of", dQuote("tinterval"),
                 "class in", sQuote("c.tinterval"))
    expect_error(c(xx, dd), err, fixed = TRUE)

    expect_equal(c(2024 %--% 2025, "2025-02-01" %--% "2025-02-28"),
                     c("2024-01-01" %--% "2025-12-31", "2025-02-01" %--% "2025-02-28"))
})


test_that("methods are are properly disabled for tinterval", {
    ti <- today() %--% today()
    err <- paste0(" method not defined for class ", dQuote("tinterval"))
    expect_error(rep(ti), paste0(sQuote("rep"), err), fixed = TRUE)
    expect_error(sum(ti), paste0(sQuote("sum"), err), fixed = TRUE)
    expect_error(cos(ti), paste0(sQuote("cos"), err), fixed = TRUE)
    expect_error(Arg(ti), paste0(sQuote("Arg"), err), fixed = TRUE)
    expect_error(mean(ti), paste0(sQuote("mean"), err), fixed = TRUE)
})







