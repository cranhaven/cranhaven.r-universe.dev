context("calendrical computations - business days, day count fractions")
# ###################################################################

# test sample size
NN <- 100L
# test sample
y <- sample(1990L:2020L, NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
d <- sample.int(31L, size = NN, replace = TRUE)
dd <- suppressWarnings(tind(y = y, m = m, d = d))
mm <- as.month(dd)
qq <- as.quarter(dd)
yy <- as.year(dd)


errmiscnv <- paste0("^", sQuote("convention"), " argument missing; ",
                    "expected one of the following: ", dQuote("[a-zA-Z0-9/]+"),
                    " \\([- a-zA-Z0-9/,\\.\\(\\)]+\\)(, ", dQuote("[a-zA-Z0-9/]+"),
                    " \\([- a-zA-Z0-9/,\\.\\(\\)]+\\))+$")
errinvcnv <- paste0("^invalid ", sQuote("convention"), " argument; ",
                    "expected one of the following: ", dQuote("[a-zA-Z0-9/]+"),
                    " \\([- a-zA-Z0-9/,\\.\\(\\)]+\\)(, ", dQuote("[a-zA-Z0-9/]+"),
                    " \\([- a-zA-Z0-9/,\\.\\(\\)]+\\))+$")
warnclnd <- paste0("^", sQuote("calendar"), " argument missing; using default settings ",
                    "\\([- _[:alnum:]]+\\)$")
warnlsmult <- "longer object length is not a multiple of shorter object length"


# Polish calendar for tests
.calendar_PL <- function(dd)
{
    y <- year(dd)
    m <- month(dd)
    d <- day(dd)
    # public holidays
    newyear <- (m == 1) & (d == 1)
    epiphany <- (y >= 2011) & (m == 1) & (d == 6)
    easter <- easter(dd) == dd
    eastermon <- easter(dd) + 1 == dd
    labour <- (m == 5) & (d == 1)
    constitution <- (m == 5) & (d == 3)
    pentecost <- easter(dd) + 49 == dd
    corpuschristi <- easter(dd) + 60 == dd
    assumption <- (m == 8) & (d == 15)
    allsaints <- (m == 11) & (d == 1)
    independence <- (m == 11) & (d == 11)
    christmas <- (m == 12) & (d == 25)
    christmas2 <- (m == 12) & (d == 26)
    holiday <- newyear | epiphany |
               easter | eastermon |
               labour | constitution |
               pentecost | corpuschristi |
               assumption |
               allsaints | independence |
               christmas | christmas2
    # holiday names
    names(holiday) <- rep("", length(holiday))
    holnms <- c(newyear = "New Year", epiphany = "Epiphany",
                easter = "Easter", eastermon = "Easter Monday",
                labour = "Labour Day", constitution = "Constitution Day",
                pentecost = "Pentecost", corpuschristi = "Corpus Christi",
                assumption = "Assumption of Mary",
                allsaints = "All Saints Day", independence = "Independence Day",
                christmas = "Christmas", christmas2 = "2nd day of Christmas")
    lapply(names(holnms), function(nm) names(holiday)[get(nm)] <<- holnms[nm])
    # working days
    work <- !holiday & (day_of_week(dd) <= 5L)

    return (list(work = work, holiday = holiday))
}

# Mon-Fri
.calendar_MonFri <- function(d) (day_of_week(d) <= 5L)


test_that("'bizday' works correctly", {
    expect_equal(bizday(tind(type = "d"), "p"), tind(type = "d"))
    expect_equal(bizday(tind(length = 1L, type = "d"), "p"),
                     tind(length = 1L, type = "d"))
    ina <- is.na(dd)
    ibd <- !ina & .calendar_PL(dd)[[1L]]
    inb <- !ina & !.calendar_PL(dd)[[1L]]
    for (conv in c("p", "mp", "f", "mf", "mf2")) {
        bd <- bizday(dd, conv, .calendar_PL)
        expect_equal(is.na(bd), ina)
        expect_equal(bd[ibd], dd[ibd])
        expect_true(all(.calendar_PL(bd[!ina])[[1L]]))
    }
    bdp <- bizday(dd, "p", .calendar_PL)[!ina]
    bdmp <- bizday(dd, "mp", .calendar_PL)[!ina]
    bdf <- bizday(dd, "f", .calendar_PL)[!ina]
    bdmf <- bizday(dd, "mf", .calendar_PL)[!ina]
    bdmf2 <- bizday(dd, "mf2", .calendar_PL)[!ina]
    dd <- dd[!ina]
    # preceding
    expect_true(all(bdp <= dd))
    expect_false(any(bizday_diff(bdp, dd, .calendar_PL, FALSE, TRUE) != 0))
    # following
    expect_true(all(bdf >= dd))
    expect_false(any(bizday_diff(dd, bdf, .calendar_PL, TRUE, FALSE) != 0))
    # modified preceding
    impf <- (bdmp == bdf) & (bdmp != dd)
    expect_true(all(bdmp[!impf] == bdp[!impf]))
    expect_true(all(month(bdp[impf]) != month(dd[impf])))
    # modified following
    imfp <- (bdmf == bdp) & (bdmf != dd)
    expect_true(all(bdmf[!imfp] == bdf[!imfp]))
    expect_true(all(month(bdf[imfp]) != month(dd[imfp])))
    # modified following bimonthly
    imf2p <- (bdmf2 == bdp) & (bdmf2 != dd)
    expect_true(all(bdmf2[!imf2p] == bdf[!imf2p]))
    expect_true(all((month(bdf[imf2p]) != month(dd[imf2p])) |
                    (day(bdf[imf2p]) > 15L) & (day(dd[imf2p]) <= 15L)))

    expect_error(bizday(dd), errmiscnv)
    expect_error(bizday(dd, "x"), errinvcnv)
    expect_warning(bdd <- bizday(dd, "p"), warnclnd)
    expect_equal(bdd, bizday(dd, "p", .calendar_MonFri))
    err <- paste0("^invalid ", sQuote("convention"), " argument; ",
                  "expected one of the following: ", dQuote("[a-z]"),
                   " \\([- a-z]+\\)(, ", dQuote("[a-z0-9]+"), " \\([- a-z]+\\))+$")
    expect_error(bizday(dd, 1), err)
})


test_that("'bizday_advance' works correctly", {
    nn <- sample(-NN:NN, NN, replace = TRUE)
    nn[sample.int(NN, 1L)] <- NA_integer_
    err <- paste0("invalid ", sQuote("n"), " argument; expected an integer vector")
    expect_error(bizday_advance(dd, as.character(nn)), err, fixed = TRUE)
    expect_warning(bda <- bizday_advance(dd[1L:3L], nn[1L:2L], .calendar_PL), warnlsmult)
    expect_equal(bda, bizday_advance(dd[1L:3L], nn[c(1L:2L, 1L)], .calendar_PL))
    expect_warning(bda <- bizday_advance(dd[1L:2L], nn[1L:3L], .calendar_PL), warnlsmult)
    expect_equal(bda, bizday_advance(dd[c(1L:2L, 1L)], nn[1L:3L], .calendar_PL))
    expect_warning(bda <- bizday_advance(dd, nn), warnclnd)
    expect_equal(bda, bizday_advance(dd, nn, .calendar_MonFri))
    expect_equal(bizday_advance(dd, nn[0L], .calendar_PL), tind(type = "d"))
    expect_equal(bizday_advance(dd[0L], nn, .calendar_PL), tind(type = "d"))
    expect_equal(bizday_advance(dd, nn[NA_integer_], .calendar_PL),
                     tind(length = NN, type = "d"))
    expect_equal(bizday_advance(dd[NA_integer_], nn, .calendar_PL),
                     tind(length = NN, type = "d"))
    bda <- bizday_advance(dd, nn, .calendar_PL)
    idb <- unname(.calendar_PL(dd)[[1L]])
    expect_equal(bizday_diff(dd, bda, .calendar_PL, FALSE, TRUE),
                     nn[ifelse(is.na(dd), NA, TRUE)] - (!nn & !idb))
})


test_that("'next_bizdays' works correctly", {
    errd <- paste0("invalid ", sQuote("d"), " argument; single non-NA date expected")
    errn <- paste0("invalid ", sQuote("n"), " argument; single non-NA integer expected")
    expect_error(next_bizdays(today() + 0:1, 1L), errd, fixed = TRUE)
    expect_error(next_bizdays(today()[NA], 1L), errd, fixed = TRUE)
    expect_error(next_bizdays(today(), -1L:1L), errn, fixed = TRUE)
    expect_error(next_bizdays(today(), NA), errn, fixed = TRUE)
    for (n in c(-sample.int(NN, 2L), 0L, sample.int(NN, 2L))) {
        td <- today()
        nb <- next_bizdays(td, n, .calendar_PL)
        expect_equal(length(nb), abs(n))
        if (n > 0) {
            expect_false(is.unsorted(nb, strictly = TRUE))
            sd <- seq(td + 1L, nb[length(nb)])
            expect_equal(sum(.calendar_PL(sd)[[1L]]), n)
        }
        if (n < 0) {
            expect_false(is.unsorted(nb, strictly = TRUE))
            sd <- seq(nb[1L], td - 1L)
            expect_equal(sum(.calendar_PL(sd)[[1L]]), -n)
        }
    }
    expect_warning(nbd <- next_bizdays(td, n), warnclnd)
    expect_equal(nbd, next_bizdays(td, n, .calendar_MonFri))
})


test_that("'first/last_bizday_in_month/quarter' work correctly", {
    fbm <- first_bizday_in_month(mm, .calendar_PL)
    lbm <- last_bizday_in_month(mm, .calendar_PL)

    expect_equal(as.month(fbm), mm)
    expect_equal(as.month(lbm), mm)
    expect_equal(is.na(fbm), is.na(mm))
    expect_equal(is.na(lbm), is.na(mm))
    expect_true(all(.calendar_PL(fbm[!is.na(fbm)])[[1L]]))
    expect_true(all(.calendar_PL(lbm[!is.na(fbm)])[[1L]]))

    iif <- fbm == as.date(mm)
    iif <- !is.na(iif) & iif
    expect_true(all(.calendar_PL(as.date(mm)[iif])[[1L]]))
    iil <- lbm == last_day_in_month(mm)
    iil <- !is.na(iil) & iil
    expect_true(all(.calendar_PL((as.date(mm + 1L) - 1L)[iil])[[1L]]))

    # quarters
    expect_equal(first_bizday_in_quarter(qq, .calendar_PL),
                     first_bizday_in_month(as.month(qq), .calendar_PL))
    expect_equal(last_bizday_in_quarter(qq, .calendar_PL),
                     last_bizday_in_month(as.month(qq + 1L) - 1L, .calendar_PL))
})


test_that("'bizday_diff' works correctly", {
    expect_equal(bizday_diff(dd, tind(type = "d"), .calendar_PL), integer())
    expect_equal(bizday_diff(tind(type = "d"), dd, .calendar_PL), integer())
    expect_equal(bizday_diff(dd, tind(length = 1L, type = "d"), .calendar_PL),
                                 rep(NA_integer_, NN))
    expect_equal(bizday_diff(tind(length = 1L, type = "d"), dd, .calendar_PL),
                                 rep(NA_integer_, NN))
    expect_warning(bdd <- bizday_diff(dd[1L:2L], dd[1L:3L], .calendar_PL), warnlsmult)
    expect_equal(bdd, bizday_diff(dd[c(1L:2L, 1L)], dd[1L:3L], .calendar_PL))
    expect_warning(bdd <- bizday_diff(dd[1L:3L], dd[1L:2L], .calendar_PL), warnlsmult)
    expect_equal(bdd, bizday_diff(dd[1L:3L], dd[c(1L:2L, 1L)], .calendar_PL))
    expect_warning(bizday_diff(dd, dd), warnclnd)

    # simple, slow scalar implementation assuming `calendar` returns a list
    .bday_diff0 <- function(d0, d1, calendar, start.incl, end.incl) {
        if (is.na(d0) || is.na(d1)) return (NA_integer_)
        neg <- d0 > d1
        dd <- d0 + 0L:as.integer(d1 - d0)
        bd <- calendar(dd)[[1L]]
        if (!start.incl) bd <- tail(bd, -1L)
        if (!end.incl) bd <- head(bd, -1L)
        return (if (neg) -sum(bd) else sum(bd))
    }
    M <- 7L
    d0 <- head(dd, M)
    d1 <- tail(dd, M)
    for (si in c(FALSE, TRUE)) {
        for (ei in c(FALSE, TRUE)) {
            bddf <- sapply(1L:M,
                         function(i) .bday_diff0(d0[i], d1[i], .calendar_PL, si, ei))
            expect_equal(bizday_diff(d0, d1, .calendar_PL, si, ei), bddf)
            expect_equal(bizday_diff(d0, d1, .calendar_PL, si, ei),
                             ifelse(!is.na(d0) & (d0 == d1), 1L, -1L) *
                             bizday_diff(d1, d0, .calendar_PL, ei, si))
            if (si && ei) {
                expect_equal(bizday_diff(d0, d0, .calendar_PL, si, ei),
                                 as.integer(.calendar_PL(d0)[[1L]]))
            } else {
                expect_equal(bizday_diff(d0, d0, .calendar_PL, si, ei),
                                 ifelse(is.na(d0), NA_integer_, 0L))
            }
        }
    }
})


test_that("'bizdays_in_month/quarter/year' work correctly", {
    M <- 7L
    bdm <- integer(M)
    for (i in 1:M) {
        d0 <- as.date(mm[i])
        d1 <- as.date(mm[i] + 1L) - 1L
        dd <- if (!is.na(d0) && !is.na(d0)) d0 + 0L:as.integer(d1 - d0) else as.date(NA)
        bdm[i] <- sum(.calendar_PL(dd)[[1L]])
    }
    expect_equal(bdm, bizdays_in_month(mm[1L:M], .calendar_PL))

    bdq <- integer(M)
    for (i in 1:M) {
        d0 <- as.date(qq[i])
        d1 <- as.date(qq[i] + 1L) - 1L
        dd <- if (!is.na(d0) && !is.na(d0)) d0 + 0L:as.integer(d1 - d0) else as.date(NA)
        bdq[i] <- sum(.calendar_PL(dd)[[1L]])
    }
    expect_equal(bdq, bizdays_in_quarter(qq[1L:M], .calendar_PL))

    bdy <- integer(M)
    for (i in 1:M) {
        d0 <- as.date(yy[i])
        d1 <- as.date(yy[i] + 1L) - 1L
        dd <- if (!is.na(d0) && !is.na(d0)) d0 + 0L:as.integer(d1 - d0) else as.date(NA)
        bdy[i] <- sum(.calendar_PL(dd)[[1L]])
    }
    expect_equal(bdy, bizdays_in_year(yy[1L:M], .calendar_PL))
})


test_that("'daycount_frac' works correctly", {
    d1 <- dd[-NN]
    d2 <- dd[-1L]
    expect_error(daycount_frac(d1, d2), errmiscnv)
    expect_error(daycount_frac(d1, d2, "44/777"), errinvcnv)

    # 30/360, 30E/360
    expect_equal(bdd <- daycount_frac(tind(type = "d"), d2, "30/360"), numeric())
    expect_equal(bdd <- daycount_frac(d1, tind(type = "d"), "30/360"), numeric())
    expect_equal(bdd <- daycount_frac(tind(length = 1L, type = "d"), d2, "30/360"),
                     rep(NA_real_, NN - 1L))
    expect_equal(bdd <- daycount_frac(d1, tind(length = 1L, type = "d"), "30/360"),
                     rep(NA_real_, NN - 1L))
    expect_warning(bdd <- daycount_frac(d1[1L:2L], d2[1L:3L], "30/360"), warnlsmult)
    expect_equal(bdd, daycount_frac(d1[c(1L:2L, 1L)], d2[1L:3L], "30/360"))
    expect_warning(bdd <- daycount_frac(d1[1L:3L], d2[1L:2L], "30/360"), warnlsmult)
    expect_equal(bdd, daycount_frac(d1[1L:3L], d2[c(1L:2L, 1L)], "30/360"))

    df30u <- daycount_frac(d1, d2, "30/360 Bond Basis")
    df30e <- daycount_frac(d1, d2, "Eurobond basis")
    ii30na <- (day(d1) <= 30) & (day(d2) <= 30) | is.na(d1) | is.na(d2)
    expect_equal(df30u[ii30na], df30e[ii30na])
    expect_equal(daycount_frac("2023-01-31", "2023-03-31", "30/360"), 1/6)
    expect_equal(daycount_frac("2023-01-31", "2023-03-31", "30E/360"), 1/6)
    expect_equal(daycount_frac("2023-01-31", "2023-03-30", "30/360"), 1/6)
    expect_equal(daycount_frac("2023-01-31", "2023-03-30", "30E/360"), 1/6)
    expect_equal(daycount_frac("2023-01-29", "2023-03-31", "30/360"), 1/6 + 2/360)
    expect_equal(daycount_frac("2023-01-29", "2023-03-31", "30E/360"), 1/6 + 1/360)
    # ACT/360, ACT/365F
    df30a0 <- daycount_frac(d1, d2, "ACT/360")
    df30a5 <- daycount_frac(d1, d2, "ACT/365F")
    expect_equal(df30a0, df30a5 * (365 / 360))
    expect_equal(df30a0 * 360, as.numeric(d2 - d1))
    # ACT/ACT
    # trivial non-vectorised implementation of ACT/ACT
    .actact <- function(d1, d2)
    {
        if (is.na(d1) || is.na(d2)) return (NA_real_)
        if (d1 == d2) return (0)
        if (d2 < d1) return (-.actact(d2, d1))
        dd <- d1 + 0L:as.integer(d2 - d1 - 1L)
        dd6 <- is.leap_year(dd)
        dd5 <- !dd6
        return (sum(dd6) / 366 + sum(dd5) / 365)
    }
    M <- 7L
    dfaa <- numeric(M)
    for (i in 1L:M) dfaa[i] <- .actact(dd[i], dd[i + 1L])
    expect_equal(dfaa, daycount_frac(dd[1L:M], dd[1L + (1L:M)], "ACT/ACT"))
})

