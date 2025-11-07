context("base - parsing")
# ###################################################################

# test sample size
NN <- 100L
nms <- sample(letters, NN, replace = TRUE)
# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("'.parse_num' works correctly", {
    warn0 <- paste0("^NAs introduced; first position [0-9]+: (([-0-9]+)|(-?Inf)|NA|NaN)")
    warn <- paste0(warn0, "; representation: [A-Z]+$")
    warn0 <- paste0(warn0, "; type: ", dQuote("[in]"), " \\([- a-z]+\\)$")
    expect_equal(.parse_num(numeric(), NULL), NULL)
    expect_equal(.parse_num(integer(), NULL), NULL)

    # i, n
    expect_warning(res <- .parse_num(2^32, "i"), warn0)
    expect_equal(res, NA_integer_)
    expect_warning(res <- .parse_num(Inf, "n"), warn0)
    expect_equal(res, NA_real_)

    # y
    expect_equal(.parse_num(numeric(), "y"), integer())
    expect_equal(.parse_num(integer(), "y"), integer())
    yy0 <- c(1800, 2000:2009, NA, 2010:2019, 2199);
    names(yy0) <- rep_len(nms, length(yy0))
    yy0r <- as.integer(yy0); names(yy0r) <- names(yy0)
    yy1 <- unname(c(yy0, 10000))
    yy1r <- as.integer(c(yy0, NA))
    yy2a <- unname(c(yy0, 2200))
    yy2ar <- as.integer(yy2a)
    yy2b <- unname(c(1799, yy0))
    yy2br <- as.integer(yy2b)
    expect_equal(.parse_num(yy0, NULL), list(yy0r, "y"))
    expect_equal(.parse_num(yy0, "y"), yy0r)
    expect_equal(.parse_num(yy2a, NULL), NULL)
    expect_equal(.parse_num(yy2a, "y"), yy2ar)
    expect_equal(.parse_num(yy2b, NULL), NULL)
    expect_equal(.parse_num(yy2b, "y"), yy2br)
    expect_equal(.parse_num(1995.5, NULL), NULL)
    expect_equal(.parse_num(1995.5, "y"), 1995L)
    expect_warning(res <- .parse_num(yy1, "y"), warn)
    expect_equal(res, yy1r)
    names(yy0) <- NULL

    # q
    expect_equal(.parse_num(numeric(), "q"), integer())
    expect_equal(.parse_num(integer(), "q"), integer())
    qs <- sample.int(4L, size = length(yy0), replace = TRUE)
    qq0 <- yy0 * 10 + qs
    qq0r <- .validate_yq(yy0, qs)
    expect_equal(.parse_num(qq0, NULL), list(qq0r, "q"))
    expect_equal(.parse_num(qq0, "q"), qq0r)
    expect_warning(res <- .parse_num(20225, "q"), warn)
    expect_equal(res, NA_integer_)

    # m
    expect_equal(.parse_num(numeric(), "m"), integer())
    expect_equal(.parse_num(integer(), "m"), integer())
    ms <- sample.int(12L, size = length(yy0), replace = TRUE)
    mm0 <- yy0 * 100 + ms
    mm0r <- .validate_ym(yy0, ms)
    expect_equal(.parse_num(mm0, NULL), list(mm0r, "m"))
    expect_equal(.parse_num(mm0, "m"), mm0r)
    expect_warning(res <- .parse_num(202213, "m"), warn)
    expect_equal(res, NA_integer_)

    # w
    expect_equal(.parse_num(numeric(), "w"), integer())
    expect_equal(.parse_num(integer(), "w"), integer())
    ws <- sample.int(52L, size = length(yy0), replace = TRUE)
    ww0 <- yy0 * 100 + ws
    ww0r <- .validate_yw(yy0, ws)
    expect_equal(.parse_num(ww0, "w"), ww0r)
    expect_warning(res <- .parse_num(202253, "w"), warn)
    expect_equal(res, NA_integer_)

    # d
    expect_equal(.parse_num(numeric(), "d"), integer())
    expect_equal(.parse_num(integer(), "d"), integer())
    ds <- sample.int(28L, size = length(mm0), replace = TRUE)
    dd0 <- mm0 * 100 + ds
    dd0r <- .validate_ymd(yy0, ms, ds)
    expect_equal(.parse_num(dd0, NULL), list(dd0r, "d"))
    expect_equal(.parse_num(dd0, "d"), dd0r)
    expect_warning(res <- .parse_num(202213, "d"), warn)
    expect_equal(res, NA_integer_)

    # t
    warn <- paste0("^NAs introduced; first position [0-9]+: [-+eE0-9]+",
                   "; representation: seconds since the Epoch$")
    expect_equal(.parse_num(numeric(), "t"), double())
    expect_equal(.parse_num(integer(), "t"), double())
    expect_equal(.parse_num(1e9, "t"), 1e9)
    expect_warning(res <- .parse_num(-1e11, "t"), warn)
    expect_equal(res, NA_real_)

    # h
    warn <- paste0("^NAs introduced; first position [0-9]+: [-+eE0-9]+",
                   "; representation: seconds since midnight$")
    expect_equal(.parse_num(numeric(), "h"), double())
    expect_equal(.parse_num(integer(), "h"), double())
    expect_equal(.parse_num(7200., "h"), 7200.)
    expect_warning(res <- .parse_num(-1, "h"), warn)
    expect_equal(res, NA_real_)
})


test_that("'.parse' works correctly", {
    expect_equal(.parse(character(), NULL, NULL, NULL), NULL)

    warn0 <- paste0("^NAs introduced; first position [0-9]+: ",
                   dQuote("[-+ :0-9A-Z]+"),
                   "; type: ", dQuote("[dth]"), " \\([- a-z]+\\)")
    warntz <- "time zone: [-+/a-zA-Z]+"
    warnnofmt <- paste0("consider providing ", sQuote("format"), " or ",
                       sQuote("order"), " argument$")
    warn0 <- paste0(warn0, "; ", warnnofmt)
    warn1 <- paste0(warn0, "; ", warntz, "; ", warnnofmt)

    yy <- sample(1969L:2068L, size = NN, replace = TRUE)
    q <- sample.int(4L, size = NN, replace = TRUE)
    qq <- .validate_yq(yy, q)
    m <- sample.int(12L, size = NN, replace = TRUE)
    mm <- .validate_ym(yy, m)
    d <- pmin(sample.int(31L, size = NN, replace = TRUE),
              .days_in_month(mm))
    dd <- .validate_ymd(yy, m, d)
    w <- pmin(sample.int(53L, size = NN, replace = TRUE), .weeks_in_year(yy))
    ww <- .validate_yw(yy, w)
    tt <- round(as.numeric(Sys.time()) + (((1L - NN) %/% 2):((NN - 1L) %/% 2)) *
                        (3600 * 23 + 61.111111), digits = 6)
    names(yy) <- nms

    # y
    expect_equal(.parse(character(), "y", NULL, NULL),
                     list(integer(), "y", NULL))
    xx <- .y2char(yy)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(yy, "y", NULL))
    xx <- paste0(" ", xx, " ")
    expect_equal(.parse(xx, NULL, NULL, NULL), NULL)
    expect_equal(.parse(xx, "y", NULL, NULL),
                     list(unname(yy), "y", NULL))
    names(yy) <- NULL

    # q
    expect_equal(.parse(character(), "q", NULL, NULL),
                     list(integer(), "q", NULL))
    xx <- .q2char(qq)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(qq, "q", NULL))
    xx <- sub("Q", "q", xx, fixed = TRUE)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(qq, "q", NULL))
    xx <- sub("q", ".", xx, fixed = TRUE)
    expect_equal(.parse(xx, NULL, NULL, NULL), NULL)
    expect_equal(.parse(xx, "q", NULL, NULL),
                     list(qq, "q", NULL))

    # m
    expect_equal(.parse(character(), "m", NULL, NULL),
                     list(integer(), "m", NULL))
    xx <- .m2char(mm)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(mm, "m", NULL))
    xx <- sub("-", " ", xx, fixed = TRUE)
    expect_equal(.parse(xx, NULL, NULL, NULL), NULL)
    xx <- sub(" ", "", xx, fixed = TRUE)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(mm, "m", NULL))
    xx <- .m2char(mm)
    expect_equal(.parse(xx, "m", NULL, NULL),
                     list(mm, "m", NULL))
    xx <- sub("-", " ", xx, fixed = TRUE)
    expect_equal(.parse(xx, "m", NULL, NULL),
                     list(mm, "m", NULL))

    # w
    expect_equal(.parse(character(), "w", NULL, NULL),
                     list(integer(), "w", NULL))
    xx <- .w2char(ww)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(ww, "w", NULL))
    xx <- sub("-", " ", xx, fixed = TRUE)
    expect_equal(.parse(xx, NULL, NULL, NULL), NULL)
    xx <- .w2char(ww)
    expect_equal(.parse(xx, "w", NULL, NULL),
                     list(ww, "w", NULL))
    xx <- sub("-", " ", xx, fixed = TRUE)
    expect_equal(.parse(xx, "w", NULL, NULL),
                     list(ww, "w", NULL))

    # d
    expect_equal(.parse(character(), "d", NULL, NULL),
                     list(integer(), "d", NULL))
    xx <- .d2char(dd)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(dd, "d", NULL))
    xx <- gsub("-", " ", xx, fixed = TRUE)
    expect_equal(.parse(xx, NULL, NULL, NULL), NULL)
    xx <- gsub(" ", "", xx, fixed = TRUE)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(dd, "d", NULL))
    xx <- .d2char(dd)
    expect_equal(.parse(xx, "d", NULL, NULL),
                     list(dd, "d", NULL))
    xx <- gsub("-", " ", xx, fixed = TRUE)
    expect_equal(.parse(xx, "d", NULL, NULL),
                     list(dd, "d", NULL))
    # US format
    xx <- .d2char(dd)
    xx <- sub("([0-9]{2})([0-9]{2})-([0-9]{2})-([0-9]{2})", "\\3/\\4/\\2", xx)
    expect_equal(.parse(xx, NULL, NULL, NULL),
                     list(dd, "d", NULL))
    # NAs
    xx <- paste0("2024-02-", 28:31)
    expect_warning(res <- .parse(xx, "d", NULL, NULL), warn0)
    expect_equal(res, list(.validate_ymd(2024, 2, 28:31), "d", NULL))

    # t
    expect_equal(.parse(character(), "t", NULL, NULL),
                     list(double(), "t", .check_tz(NULL)))
    xx <- c(foo = "2021-03-28 02:30")
    for (tz in tzs) {
        if (tz == "Europe/Warsaw") { # invalid hour due to DST change
            expect_warning(res <- .parse(xx, "t", NULL, tz))
            expect_equal(res, list(c(foo = NA_real_), "t", tz))
        } else {
            expect_silent(res <- .parse(xx, "t", NULL, tz))
            expect_equal(names(res[[1L]]), "foo")
            expect_true(grepl(paste0("^", xx), .t2char(res[[1L]], tz)))
        }
    }
    # UTC offsets
    expect_equal(.parse("2021-03-28 02:30+01", "t", NULL, NULL),
                 .parse("2021-03-28 02:30+0100", "t", NULL, NULL))
    expect_equal(.parse("2021-03-28 02:30+01:00", "t", NULL, NULL),
                 .parse("2021-03-28 02:30+0100", "t", NULL, NULL))
    expect_equal(.parse("2021-03-28 02:30-01", "t", NULL, NULL),
                 .parse("2021-03-28 02:30-0100", "t", NULL, NULL))
    expect_equal(.parse("2021-03-28 02:30-01:00", "t", NULL, NULL),
                 .parse("2021-03-28 02:30-0100", "t", NULL, NULL))
    expect_equal(.parse("2021-03-28 02:30+0100", "t", NULL, NULL)[[1L]] -
                 .parse("2021-03-28 02:30-0100", "t", NULL, NULL)[[1L]], -7200)

    # h
    H <- c(10, 17, 23, 24)
    M <- sample(10:59, 4)
    S <- sample(10:59, 4)
    xx <- paste0(H, ":", M)
    expect_warning(res <- .parse(xx, "h", NULL, NULL), warn0)
    expect_equal(res,
                 list(c(H[-4L] * 3600 + 60 * M[-4L], NA_real_), "h", NULL))
    xx <- paste0(H, ":", M, ":", S)
    expect_warning(res <- .parse(xx, "h", NULL, NULL), warn0)
    expect_equal(res,
                 list(c(H[-4L] * 3600 + 60 * M[-4L] + S[-4L], NA_real_), "h", NULL))
    xx <- c(foo = "09:38:54")
    expect_equal(.parse(xx, "h", NULL, NULL),
                 list(c(foo = 9 * 3600 + 60 * 38 + 54), "h", NULL))
    xx <- c(bar = "09:38:54 pm")
    expect_equal(.parse(xx, "h", NULL, NULL),
                 list(c(bar = 21 * 3600 + 60 * 38 + 54), "h", NULL))

    # i, n
    warn <- paste0("^NAs introduced; first position [0-9]+: ",
                   dQuote("(([-0-9]+)|(-?Inf)|NA|NaN)"),
                   "; type: ", dQuote("[in]"), " \\([- a-z]+\\)$")
    expect_warning(res <- .parse("4294967296", "i", NULL, NULL), warn)
    expect_equal(res, list(NA_integer_, "i", NULL))
    expect_warning(res <- .parse("Inf", "n", NULL, NULL), warn)
    expect_equal(res, list(NA_real_, "n", NULL))
    expect_equal(.parse("26", "i", NULL, NULL),
                     list(26L, "i", NULL))
    expect_equal(.parse("1.234e3", "n", NULL, NULL),
                     list(1234., "n", NULL))
})


test_that("'.parse_tdiff' works correctly", {
    expect_equal(.parse_tdiff(character()), NULL)

    ws <- sample(c("", " ", "  ", "   "), NN, replace = TRUE)
    smth <- sample(c("", "(smth)"), NN, replace = TRUE)

    ym <- 9999
    yy <- round(runif(NN, -ym, ym))
    ys <- sample(c("y", "year", "years"), NN, replace = TRUE)
    yych <- paste0(formatC(yy, format = "d"), ws, ys, ws)
    names(yych) <- nms
    yych1 <- paste0(formatC(c(-ym - 1, yy[-1L]), format = "d"), ys, ws)
    yych2 <- paste0(formatC(c(yy[-1L], ym + 2), format = "d"), ys, ws)
    expect_equal(names(.parse_tdiff(yych)[[1L]]), nms)
    expect_equal(lapply(.parse_tdiff(yych), unname), list(as.integer(yy), "y"))
    expect_equal(.parse_tdiff(yych1), list(c(NA_integer_, as.integer(yy[-1L])), "y"))
    expect_equal(.parse_tdiff(yych2), list(c(as.integer(yy[-1L]), NA_integer_), "y"))
    expect_equal(.parse_tdiff(paste(yych, smth)), NULL)

    qm <- 39999
    qq <- round(runif(NN, -qm, qm))
    qs <- sample(c("q", "quarter", "quarters"), NN, replace = TRUE)
    qqch <- paste0(formatC(qq, format = "d"), qs, ws, smth, ws)
    qqch1 <- paste0(formatC(c(-qm - 1, qq[-1L]), format = "d"), qs, ws)
    qqch2 <- paste0(formatC(c(qq[-1L], qm + 2), format = "d"), qs, ws)
    expect_equal(.parse_tdiff(qqch), list(as.integer(qq), "q"))
    expect_equal(.parse_tdiff(qqch1), list(c(NA_integer_, as.integer(qq[-1L])), "q"))
    expect_equal(.parse_tdiff(qqch2), list(c(as.integer(qq[-1L]), NA_integer_), "q"))

    mmx <- 119999
    mm <- round(runif(NN, -mmx, mmx))
    ms <- sample(c("m", "month", "months"), NN, replace = TRUE)
    mmch <- paste0(formatC(mm, format = "d"), ms, ws, smth, ws)
    mmch1 <- paste0(formatC(c(-mmx - 1, mm[-1L]), format = "d"), ms, ws, smth)
    mmch2 <- paste0(formatC(c(mm[-1L], mmx + 2), format = "d"), ms, ws, smth)
    expect_equal(.parse_tdiff(mmch), list(as.integer(mm), "m"))
    expect_equal(.parse_tdiff(mmch1), list(c(NA_integer_, as.integer(mm[-1L])), "m"))
    expect_equal(.parse_tdiff(mmch2), list(c(as.integer(mm[-1L]), NA_integer_), "m"))

    wm <- 521774
    ww <- round(runif(NN, -wm, wm))
    wws <- sample(c("w", "week", "weeks"), NN, replace = TRUE)
    wwch <- paste0(formatC(ww, format = "d"), wws, ws, smth, ws)
    wwch1 <- paste0(formatC(c(-wm - 1, ww[-1L]), format = "d"), wws, ws, smth)
    wwch2 <- paste0(formatC(c(ww[-1L], wm + 2), format = "d"), wws, ws, smth)
    expect_equal(.parse_tdiff(wwch), list(as.integer(ww), "w"))
    expect_equal(.parse_tdiff(wwch1), list(c(NA_integer_, as.integer(ww[-1L])), "w"))
    expect_equal(.parse_tdiff(wwch2), list(c(as.integer(ww[-1L]), NA_integer_), "w"))

    dm <- 3652424
    dd <- round(runif(NN, -dm, dm))
    ds <- sample(c("d", "day", "days"), NN, replace = TRUE)
    ddch <- paste0(formatC(dd, format = "d"), ds, ws, smth, ws)
    ddch1 <- paste0(formatC(c(-dm - 1, dd[-1L]), format = "d"), ds, ws, smth)
    ddch2 <- paste0(formatC(c(dd[-1L], dm + 2), format = "d"), ds, ws, smth)
    expect_equal(.parse_tdiff(ddch), list(as.integer(dd), "d"))
    expect_equal(.parse_tdiff(ddch1), list(c(NA_integer_, as.integer(dd[-1L])), "d"))
    expect_equal(.parse_tdiff(ddch2), list(c(as.integer(dd[-1L]), NA_integer_), "d"))

    sgn <- sample(c("", "-"), NN, replace = TRUE)
    ss <- round(runif(NN, 0, 59.9999), 3L)
    secs <- sample(c("s", "sec", "secs", "seconds"), NN, replace = TRUE)
    ms <- sample(0:59, NN, replace = TRUE)
    mins <- sample(c("min", "mins", "minute", "minutes"), NN, replace = TRUE)
    hs <- sample(0:23, NN, replace = TRUE)
    hours <- sample(c("h", "hour", "hours"), NN, replace = TRUE)
    expect_equal(.parse_tdiff(paste0(sgn, ss, ws, secs)), list(ss * (1 - 2 * nchar(sgn)), "t"))
    expect_equal(.parse_tdiff(paste0(sgn, ms, ws, mins)), list(ms * 60 * (1 - 2 * nchar(sgn)), "t"))
    expect_equal(.parse_tdiff(paste0(sgn, hs, ws, hours)), list(hs * 3600 * (1 - 2 * nchar(sgn)), "t"))
    expect_equal(.parse_tdiff("0h3m"), list(180, "t"))
    expect_equal(.parse_tdiff(c(a = "-3.3566s", b = NA)), list(c(a = -3.3566, b = NA), "t"))
    expect_equal(.parse_tdiff("1d23h59m59s"), list(2 * 86400 - 1, "t"))
    expect_equal(.parse_tdiff("1d 23h 59m 59s"), list(2 * 86400 - 1, "t"))
    expect_equal(.parse_tdiff("1 d 23 h 59 m 59 s"), list(2 * 86400 - 1, "t"))
    expect_equal(.parse_tdiff("1 d 23 h 59 m 59 s (smth) "), list(2 * 86400 - 1, "t"))
    expect_equal(.parse_tdiff("1 d 23 h 59 m 59 s ()smth "), NULL)
    expect_equal(.parse_tdiff("23:59:59 "), list(86400 - 1, "t"))
    expect_equal(.parse_tdiff("23:59:59  (smth) "), NULL)
    expect_equal(.parse_tdiff("23:59:59  ()smth "), NULL)
})


test_that("'.parse_tinterval' works correctly", {
    st <- c("1998", "", "1998")
    en <- c("2002", "2002", "")
    aux <- sample(c("", "", "()"))
    ti <- paste0(st, "/", en, " ", aux)
    st[st == ""] <- NA
    en[en == ""] <- NA
    names(ti) <- letters[1:3]
    names(st) <- letters[1:3]
    names(en) <- letters[1:3]
    pti <- .parse_tinterval(ti, "/")
    pti <- lapply(pti, function(x) gsub("(^ +| +$)", "", x))
    expect_equal(pti, list(start = st, end = en))
    expect_null(.parse_tinterval(ti, "--"))
    ti <- sub("/", "--", ti, fixed = TRUE)
    expect_null(.parse_tinterval(ti, "/"))
    pti <- .parse_tinterval(ti, "--")
    pti <- lapply(pti, function(x) gsub("(^ +| +$)", "", x))
    expect_equal(pti, list(start = st, end = en))
})

