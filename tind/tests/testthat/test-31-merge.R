context("merge method for time indices")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("'merge' method works correctly", {
    d1 <- tind(y = 2023, m = c(1, 1, 2, 2, 3, 3, 4, 4), d = c(1, 16))
    mn <- as.month("2022-12") + 0:3
    we <- as.week("2023-W01") + 0:7
    dfm <- data.frame(month = mn, mn = month(mn),
                    mch = month(mn, labels = TRUE))
    dfw <- data.frame(week = we, wn = week(we),
                    wch = format(we, "W%V"))
    for (ordered in c(TRUE, FALSE)) {
        if (ordered) {
            d2 <- d1 %+m% 1
            dfd1 <- data.frame(date = d1, d1 = as.numeric(d1),
                               dow1 = day_of_week(d1, labels = TRUE), stringsAsFactors = FALSE)
            dfd2 <- data.frame(date = d2, d2 = format(d2, "%m/%d/%y"),
                               dow2 = day_of_week(d2, labels = TRUE, abbreviate = FALSE), stringsAsFactors = FALSE)
        } else {
            d1 <- c(d1, tail(d1, 1L))
            d2 <- d1 %+m% 1
            d2 <- c(d2[1], d2, NA)
            dfd1 <- data.frame(date = d1, d1 = as.numeric(d1),
                               dow1 = day_of_week(d1, labels = TRUE),
                               dup1 = duplicated(d1), stringsAsFactors = FALSE)
            dfd2 <- data.frame(date = d2, d2 = format(d2, "%m/%d/%y"),
                               dow2 = day_of_week(d2, labels = TRUE, abbreviate = FALSE),
                               dup2 = duplicated(d2), stringsAsFactors = FALSE)
        }
        # same type ordered and unordered
        for (allxy in 0L:3L) {
            allx <- as.logical(allxy %% 2L)
            ally <- as.logical(allxy %/% 2L)
            mti <- merge(dfd1$date, dfd2$date, all.x = allx, all.y = ally)
            res <- data.frame(index = mti$index, dfd1[mti$xi, -1L], dfd2[mti$yi, -1L],
                              stringsAsFactors = FALSE)
            i1 <- !is.na(res$d1)
            i2 <- !is.na(res$d2)
            expect_equal(res$d1[i1], as.numeric(res$index[i1]))
            expect_equal(res$d2[i2], format(res$index[i2], "%m/%d/%y"))
            expect_equal(res$dow1[i1], day_of_week(res$index[i1], labels = TRUE))
            expect_equal(res$dow2[i2], day_of_week(res$index[i2], labels = TRUE, abbreviate = FALSE))
            if (ordered && !allx && !ally) expect_equal(res$index, intersect_t(dfd1$date, dfd2$date))
            if (ordered && allx && !ally) expect_equal(res$index, dfd1$date)
            if (ordered && !allx && ally) expect_equal(res$index, dfd2$date)
            if (ordered && allx && ally) expect_equal(res$index, union_t(dfd1$date, dfd2$date))

        }
        # different types
        # inner
        mti <- merge(dfd1$date, dfm$month)
        res <- data.frame(index = mti$index, dfd1[mti$xi, -1L], dfm[mti$yi, -1L])
        expect_equal(res$d1, as.numeric(res$index))
        expect_equal(res$dow1, day_of_week(res$index, labels = TRUE))
        expect_equal(res$mn, month(res$index))
        expect_equal(res$mch, month(res$index, labels = TRUE))
        mti2 <- merge(dfm$month, dfd1$date)
        expect_equal(mti$index, mti2$index)
        expect_equal(mti$xi, mti2$yi)
        expect_equal(mti$yi, mti2$xi)
        # left
        mti <- merge(dfd1$date, dfm$month, all.x = TRUE)
        res <- data.frame(index = mti$index, dfd1[mti$xi, -1L], dfm[mti$yi, -1L])
        if (ordered) expect_equal(res$index, dfd1$date)
        expect_equal(res$d1, as.numeric(res$index))
        expect_equal(res$dow1, day_of_week(res$index, labels = TRUE))
        i2 <- !is.na(res$mn)
        expect_equal(res$mn[i2], month(res$index[i2]))
        expect_equal(res$mch[i2], month(res$index[i2], labels = TRUE))
        # right
        mti <- merge(dfm$month, dfd1$date, all.y = TRUE)
        data.frame(index = mti$index, dfm[mti$xi, -1L], dfd1[mti$yi, -1L])
        if (ordered) expect_equal(res$index, dfd1$date)
        expect_equal(res$d1, as.numeric(res$index))
        expect_equal(res$dow1, day_of_week(res$index, labels = TRUE))
        i2 <- !is.na(res$mn)
        expect_equal(res$mn[i2], month(res$index[i2]))
        expect_equal(res$mch[i2], month(res$index[i2], labels = TRUE))
    }

    err <- paste0("time index type mismatch in ", sQuote("merge(x, y, all = TRUE)"),
                  ": ", .ti_type2char("w"), ", ", .ti_type2char("m"))
    expect_error(merge(dfw$week, dfm$month, all = TRUE), err, fixed = TRUE)
    err <- paste0("time index type mismatch in ", sQuote("merge(x, y, all = TRUE)"),
                  ": ", .ti_type2char("m"), ", ", .ti_type2char("w"))
    expect_error(merge(dfm$month, dfw$week, all = TRUE), err, fixed = TRUE)
    err <- paste0("cast from time index type ", .ti_type2char("w"),
                  " to type ", .ti_type2char("m"), " in ",
                  sQuote("merge(x, y, all.x = TRUE)"), " not possible")
    expect_error(merge(dfw$week, dfm$month, all.x = TRUE), err, fixed = TRUE)
    err <- paste0("cast from time index type ", .ti_type2char("m"),
                  " to type ", .ti_type2char("w"), " in ",
                  sQuote("merge(x, y, all.y = TRUE)"), " not possible")
    expect_error(merge(dfw$week, dfm$month, all.y = TRUE), err, fixed = TRUE)
    err <- paste0("cast from time index type ", .ti_type2char("w"),
                  " to type ", .ti_type2char("m"), " in ",
                  sQuote("merge(x, y, all = FALSE)"), " not possible")
    expect_error(merge(dfw$week, dfm$month, all = FALSE), err, fixed = TRUE)

    # date-time
    td1 <- as.date("2024-07-01") + 0:9
    td2 <- as.date("2024-07-01") + -5:5
    t1 <- date_time(td1, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    t2 <- date_time(td2, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    # inner
    mti0 <- merge(td1, td2)
    mti1 <- merge(t1, t2)
    expect_equal(mti1$index, date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC"))
    # left
    mti0 <- merge(td1, td2, all.x = TRUE)
    mti1 <- merge(t1, t2, all.x = TRUE)
    expect_equal(mti1$index, date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC"))
    # right
    mti0 <- merge(td1, td2, all.y = TRUE)
    mti1 <- merge(t1, t2, all.y = TRUE)
    expect_equal(mti1$index, date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC"))
    # outer
    mti0 <- merge(td1, td2, all = TRUE)
    mti1 <- merge(t1, t2, all = TRUE)
    expect_equal(mti1$index, date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC"))

    # different time zones
    if (length(tzs) < 2L) skip("too few time zones for further tests")
    tzs <- sample(tzs, 2L)
    warn <- paste0("different time zones of arguments: ", dQuote(tzs[1L]),
                   ", ", dQuote(tzs[2L]), "; assuming: ", dQuote(tzs[1L]))
    tzone(t1) <- tzs[1L]
    tzone(t2) <- tzs[2L]
    # inner
    mti0 <- merge(td1, td2)
    expect_warning(mti1 <- merge(t1, t2), warn, fixed = TRUE)
    t0 <- date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    tzone(t0) <- tzs[1L]
    expect_equal(mti1$index, t0)
    # left
    mti0 <- merge(td1, td2, all.x = TRUE)
    expect_warning(mti1 <- merge(t1, t2, all.x = TRUE), warn, fixed = TRUE)
    t0 <- date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    tzone(t0) <- tzs[1L]
    expect_equal(mti1$index, t0)
    # outer
    mti0 <- merge(td1, td2, all = TRUE)
    expect_warning(mti1 <- merge(t1, t2, all = TRUE), warn, fixed = TRUE)
    t0 <- date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    tzone(t0) <- tzs[1L]
    expect_equal(mti1$index, t0)
    # right
    warn <- paste0("different time zones of arguments: ", dQuote(tzs[2L]),
                   ", ", dQuote(tzs[1L]), "; assuming: ", dQuote(tzs[2L]))
    mti0 <- merge(td1, td2, all.y = TRUE)
    expect_warning(mti1 <- merge(t1, t2, all.y = TRUE), warn, fixed = TRUE)
    t0 <- date_time(mti0$index, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    tzone(t0) <- tzs[2L]
    expect_equal(mti1$index, t0)

    # unsorted
    warn <- paste0("different time zones of arguments: ", dQuote(tzs[1L]), ", ",
                   dQuote(tzs[2L]), "; assuming: ", dQuote(tzs[1L]))
    expect_warning(mti1 <- merge(rev(t1), t2, all = TRUE), warn, fixed = TRUE)
    tzone(t1) <- tzone(t2) <- "UTC"
    mti0 <- merge(rev(t1), t2, all = TRUE)
    expect_equal(mti0[-1L], mti1[-1L])
    tzone(t1) <- tzs[1L]
    tzone(t2) <- tzs[2L]
    expect_warning(mti1 <- merge(rev(t1), t2, all = FALSE), warn, fixed = TRUE)
    tzone(t1) <- tzone(t2) <- "UTC"
    mti0 <- merge(rev(t1), t2, all = FALSE)
    expect_equal(mti0[-1L], mti1[-1L])

})



test_that("'merge' method works correctly with 3+ arguments", {
    d1 <- tind(y = 2023, m = c(1, 1, 2, 2, 3, 3, 4, 4), d = c(1, 16))
    d2 <- d1 %+m% 1
    mn <- as.month("2022-12") + 0:3
    we <- as.week("2023-W01") + 0:7

    # all = TRUE
    m12 <- merge(d1, d2, all = TRUE)
    m122 <- merge(d1, d2, d2, all = TRUE)
    m112 <- merge(d1, d1, d2, all = TRUE)
    m1122 <- merge(d1, d1, d2, d2, all = TRUE)
    # index
    expect_equal(m122[[1L]], m12[[1L]])
    expect_equal(m112[[1L]], m12[[1L]])
    expect_equal(m1122[[1L]], m12[[1L]])
    # maps
    m12 <- m12[-1L]
    m122 <- m122[-1L]
    m112 <- m112[-1L]
    m1122 <- m1122[-1L]
    expect_equal(m122[[1L]], m12[[1L]])
    expect_equal(m122[[2L]], m12[[2L]])
    expect_equal(m122[[3L]], m12[[2L]])
    expect_equal(m112[[1L]], m12[[1L]])
    expect_equal(m112[[2L]], m12[[1L]])
    expect_equal(m112[[3L]], m12[[2L]])
    expect_equal(m1122[[1L]], m12[[1L]])
    expect_equal(m1122[[2L]], m12[[1L]])
    expect_equal(m1122[[3L]], m12[[2L]])
    expect_equal(m1122[[4L]], m12[[2L]])
    # errors
    err <- "^time index type mismatch"
    expect_error(merge(d1, d2, we, all = TRUE), err)
    expect_error(merge(mn, d1, d2, all = TRUE), err)
    expect_error(merge(d1, mn, d2, all = TRUE), err)
    expect_error(merge(mn, d1, d2, we, all = TRUE), err)

    # all = FALSE
    mdwm <- merge(d1, we, mn)
    mmdw <- merge(mn, d1, we)
    mmwd <- merge(mn, we, d1)
    mdwm_ <- merge(merge(d1, we)[[1L]], mn)
    mmdw_ <- merge(merge(mn, d1)[[1L]], we)
    mmwd_ <- merge(mn, merge(we, d1)[[1L]])
    # index
    expect_equal(mdwm[[1L]], mdwm_[[1L]])
    expect_equal(mmdw[[1L]], mmdw_[[1L]])
    expect_equal(mmwd[[1L]], mmwd_[[1L]])
    # maps
    mdwm <- unname(mdwm[-1L])
    mmdw <- unname(mmdw[-1L])
    mmwd <- unname(mmwd[-1L])
    mdwm_ <- mdwm_[-1L]
    mmdw_ <- mmdw_[-1L]
    mmwd_ <- mmwd_[-1L]
    expect_equal(mmdw[c(2, 3, 1)], mdwm)
    expect_equal(mmwd[c(3, 2, 1)], mdwm)
    expect_equal(mdwm[[3L]], mdwm_[[2L]])
    expect_equal(mdwm[[2L]], mmdw_[[2L]])
    # errors
    err <- "^cast from"
    expect_error(merge(mn, we, we), err)

    # mixed cases
    # case 1
    mdamw <- merge(d1, mn, we, all = c(T, F, F))
    # index
    expect_equal(mdamw[[1L]], d1)
    # maps
    mdamw <- mdamw[-1L]
    mdam_ <- merge(d1, mn, all.x = T)[-1L]
    mdaw_ <- merge(d1, we, all.x = T)[-1L]
    expect_equal(mdamw[[1L]], seq_along(d1))
    expect_equal(mdamw[[2L]], mdam_[[2L]])
    expect_equal(mdamw[[3L]], mdaw_[[2L]])
    # case 2
    mdamwda <- merge(d1, mn, we, d2, all = c(T, F, F, T))
    # index
    expect_equal(mdamwda[[1L]], union_t(d1, d2))
    # maps
    mdamwda <- mdamwda[-1L]
    mdada_ <- merge(d1, d2, all = T)[-1L]
    mdam_ <- merge(union_t(d1, d2), mn, all.x = T)[-1L]
    mdaw_ <- merge(union_t(d1, d2), we, all.x = T)[-1L]
    expect_equal(mdamwda[[1L]], mdada_[[1L]])
    expect_equal(mdamwda[[4L]], mdada_[[2L]])
    expect_equal(mdamwda[[2L]], mdam_[[2L]])
    expect_equal(mdamwda[[3L]], mdaw_[[2L]])
    # errors
    err <- "^cast from"
    expect_error(merge(d1, mn, we, all = c(T, T, F)))
    expect_error(merge(d1, mn, we, all = c(F, F, T)))

    # generic errors
    errallxy <- paste(sQuote("all.x"), "or", sQuote("all.y"),
                      "provided with more than two vectors of time indices")
    expect_error(merge(d1, d2, d2, all.x = TRUE), errallxy, fixed = TRUE)
    expect_error(merge(d1, d2, d2, all.y = TRUE), errallxy, fixed = TRUE)
    expect_error(merge(d1, d2, d2, all.x = TRUE, all.y = TRUE), errallxy, fixed = TRUE)
    errord <- "^ordered"
    expect_error(merge(rev(d1), d2, d2, all = FALSE), errord)
    expect_error(merge(rev(d1), d2, d2, all = TRUE), errord)
    expect_error(merge(d1, rev(d2), d2, all = FALSE), errord)
    expect_error(merge(d1, rev(d2), d2, all = TRUE), errord)
    expect_error(merge(d1, d2, rev(d2), all = FALSE), errord)
    expect_error(merge(d1, d2, rev(d2), all = TRUE), errord)

    # date-time
    t1 <- date_time(d1, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    t2 <- date_time(d2, H = 0:11 * 2, grid = TRUE, tz = "UTC")
    m12 <- merge(t1, t2, all = TRUE)
    m122 <- merge(t1, t2, t2, all = TRUE)
    m112 <- merge(t1, t1, t2, all = TRUE)
    m1122 <- merge(t1, t1, t2, t2, all = TRUE)
    # index
    expect_equal(m122[[1L]], m12[[1L]])
    expect_equal(m112[[1L]], m12[[1L]])
    expect_equal(m1122[[1L]], m12[[1L]])
    # maps
    m12 <- m12[-1L]
    m122 <- m122[-1L]
    m112 <- m112[-1L]
    m1122 <- m1122[-1L]
    expect_equal(m122[[1L]], m12[[1L]])
    expect_equal(m122[[2L]], m12[[2L]])
    expect_equal(m122[[3L]], m12[[2L]])
    expect_equal(m112[[1L]], m12[[1L]])
    expect_equal(m112[[2L]], m12[[1L]])
    expect_equal(m112[[3L]], m12[[2L]])
    expect_equal(m1122[[1L]], m12[[1L]])
    expect_equal(m1122[[2L]], m12[[1L]])
    expect_equal(m1122[[3L]], m12[[2L]])
    expect_equal(m1122[[4L]], m12[[2L]])
    # different time zones
    if (length(tzs) < 2L) skip("too few time zones for further tests")
    tzs <- sample(tzs, 2L)
    tzone(t1) <- tzs[1L]
    tzone(t2) <- tzs[2L]
    warn <- paste0("different time zones of arguments: ", dQuote(tzs[1L]), ", ",
                   dQuote(tzs[2L]), "; assuming: ", dQuote(tzs[1L]))
    expect_warning(m12dt <- merge(t1, t2, all = TRUE), warn, fixed = TRUE)
    warn <- paste0("different time zones of arguments; assuming: ",
                   dQuote(tzs[1L]))
    # inner
    expect_warning(m122dt <- merge(t1, t2, t2, all = TRUE), warn, fixed = TRUE)
    expect_warning(m112dt <- merge(t1, t1, t2, all = TRUE), fixed = TRUE)
    expect_warning(m1122dt <- merge(t1, t1, t2, t2, all = TRUE), fixed = TRUE)
    expect_equal(m12dt[-1L], m12)
    expect_equal(m122dt[-1L], m122)
    expect_equal(m112dt[-1L], m112)
    expect_equal(m1122dt[-1L], m1122)
})



test_that("'merge' method works correctly with date-time and time of day", {
    h0 <- sort(sample(0:23, 13))
    h1 <- sort(sample(0:23, 13))

    dt <- date_time(today() + -1:1, h0, grid = TRUE, tz = "UTC")
    h <- tind(H = h1)
    # 2 args
    mth <- merge(dt, h, all.x = TRUE)
    expect_equal(mth[[1L]], dt)
    nna <- !is.na(mth[[3L]])
    expect_equal(nna, hour(dt) %in% h1)
    expect_equal(as.time(dt[nna]), h[mth[[3L]]][nna])
    mht <- merge(h, dt, all.y = TRUE)
    expect_equal(mht[[1L]], mth[[1L]])
    expect_equal(mht[[2L]], mth[[3L]])
    expect_equal(mht[[3L]], mth[[2L]])
    # 3+ args
    mthh <- merge(dt, h, h, all = c(TRUE, FALSE, FALSE))
    expect_equal(mthh[[1L]], mth[[1L]])
    expect_equal(mthh[[2L]], mth[[2L]])
    expect_equal(mthh[[3L]], mth[[3L]])
    expect_equal(mthh[[4L]], mth[[3L]])
    mhth <- merge(h, dt, h, all = c(FALSE, TRUE, FALSE))
    expect_equal(mhth[[1L]], mth[[1L]])
    expect_equal(mhth[[2L]], mth[[3L]])
    expect_equal(mhth[[3L]], mth[[2L]])
    expect_equal(mthh[[4L]], mth[[3L]])
    mhht <- merge(h, h, dt, all = c(FALSE, FALSE, TRUE))
    expect_equal(mhht[[1L]], mth[[1L]])
    expect_equal(mhht[[2L]], mth[[3L]])
    expect_equal(mhht[[3L]], mth[[3L]])
    expect_equal(mhht[[4L]], mth[[2L]])

    # inner
    mth <- merge(dt, h)
    expect_equal(mth[[1L]], dt[hour(dt) %in% h1])
    expect_equal(as.time(mth[[1L]]), h[mth[[3L]]])
    mht <- merge(h, dt)
    expect_equal(mht[[1L]], mth[[1L]])
    expect_equal(mht[[2L]], mth[[3L]])
    expect_equal(mht[[3L]], mth[[2L]])
    # 3+ args
    mthh <- merge(dt, h, h)
    expect_equal(mthh[[1L]], mth[[1L]])
    expect_equal(mthh[[2L]], mth[[2L]])
    expect_equal(mthh[[3L]], mth[[3L]])
    expect_equal(mthh[[4L]], mth[[3L]])
    mhth <- merge(h, dt, h)
    expect_equal(mhth[[1L]], mth[[1L]])
    expect_equal(mhth[[2L]], mth[[3L]])
    expect_equal(mhth[[3L]], mth[[2L]])
    expect_equal(mthh[[4L]], mth[[3L]])
    mhht <- merge(h, h, dt)
    expect_equal(mhht[[1L]], mth[[1L]])
    expect_equal(mhht[[2L]], mth[[3L]])
    expect_equal(mhht[[3L]], mth[[3L]])
    expect_equal(mhht[[4L]], mth[[2L]])
})


