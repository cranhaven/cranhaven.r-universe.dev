context("tind - basic methods")
# ###################################################################

# test sample size
NN <- 100L
# test sample
y <- sample(1990L:2020L, NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
d <- pmin(sample.int(31L, size = NN, replace = TRUE),
          .days_in_month(.validate_ym(y, m)))
dd <- as.integer(suppressWarnings(tind(y = y, m = m, d = d)))

tt <- round(as.numeric(Sys.time()) + (((1L - NN) %/% 2):((NN - 1L) %/% 2)) *
                       (3600 * 23 + 61.111111), digits = 0L)

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("tind '[', '[[', '[<-', and '[[<-' methods work correctly", {
    ii <- sample(1L:NN, 10)
    expect_equal(as.tind(dd)[ii], as.tind(dd[ii]))

    yy <- 1969:2068
    ii <- sample(1L:NN, 10L)
    jj <- sample(1L:NN, 10L)
    ti <- as.tind(yy)
    ti[ii] <- ti[jj]
    yy[ii] <- yy[jj]
    expect_equal(ti, as.tind(yy))
    ti[] <- 2000
    expect_true(all(ti == 2000))

    ti <- as.tind(dd)
    expect_silent(ti[NN + (-2L:0L)] <- NA)
    expect_equal(is.na(ti), c(rep(FALSE, NN - 3L), rep(TRUE, 3L)))
    expect_silent(ti[] <- "20201231")
    expect_true(all(ti == "20201231"))
    expect_silent(ti[1L] <- "20211230")
    expect_true(ti[1L] == "20211230")
    expect_true(all(ti[-1L] == "20201231"))
    expect_silent(ti[1L:2L] <- Sys.Date())
    expect_true(ti[1L] == ti[2L])
    expect_true(ti[2L] == ti[[2L]])
    expect_true(ti[[1L]] == ti[[2L]])
    expect_silent(ti[[NN]] <- Sys.Date())
    expect_silent(ti[[NN]] <- "20221231")

    err <- paste0("time index type mismatch in ", sQuote("[<-.tind"),
                  ": ", .ti_type2char("d"), ", ", .ti_type2char("t"))
    expect_error(ti[1L:3L] <- as.tind(Sys.time()), err, fixed = TRUE)
    err <- paste0("time index type mismatch in ", sQuote("[[<-.tind"),
                  ": ", .ti_type2char("d"), ", ", .ti_type2char("t"))
    expect_error(ti[[3L]] <- as.tind(Sys.time()), err, fixed = TRUE)

    if (length(tzs) < 2L) skip("too few time zones for further tests")
    ti <- as.tind(tt, tz = "UTC")
    tz <- sample(setdiff(tzs, "UTC"), 1L)
    warn <- paste0("different time zones of arguments: ", dQuote("UTC"), ", ",
                   dQuote(tz))
    ti2 <- as.tind(tt, tz = tz)
    expect_warning(ti[1L:2L] <- ti2[3L:4L], warn, fixed = TRUE)
    expect_equal(as.numeric(ti[1L:2L]), as.numeric(ti[3L:4L]))
    expect_warning(ti[[1L]] <- ti2[[3L]], warn, fixed = TRUE)
    expect_equal(as.numeric(ti[[1L]]), as.numeric(ti[[3L]]))
})


test_that("tind 'rev', 'head', and 'tail' methods work correctly", {
    ti <- as.tind(dd)
    expect_equal(rev(ti), as.tind(rev(dd)))
    for (n in sample((1L - NN):(NN - 1L), 10)) {
        expect_equal(head(ti, n), as.tind(head(dd, n), type = "d"))
        expect_equal(tail(ti, n), as.tind(tail(dd, n), type = "d"))
    }
    expect_equal(head(ti, NN), ti)
    expect_equal(tail(ti, NN), ti)
    expect_equal(head(ti, -NN), tind(type = "d"))
    expect_equal(tail(ti, -NN), tind(type = "d"))
    expect_equal(head(ti, 0), tind(type = "d"))
    expect_equal(tail(ti, 0), tind(type = "d"))
})


test_that("tind 'length' and 'length<-' methods work correctly", {
    nn <- as.integer(min(rgeom(1, .1) + 1, NN))
    ii <- sample(1L:NN, nn)
    expect_equal(length(as.tind(dd[ii])), nn)
    expect_equal(length(tind(type = "t")), 0L)
    dd <- dd[ii]
    ti <- as.tind(dd)
    nn <- as.integer(min(rgeom(1, .1), 10 * NN))
    length(ti) <- nn
    length(dd) <- nn
    expect_equal(ti, as.tind(dd, "d"))
})


test_that("tind 'rep' method works correctly", {
    ti <- as.tind(dd)
    n <- as.integer(min(rgeom(1, prob = .2), 10))
    expect_equal(rep(ti, n), as.tind(rep(dd, n), type = "d"))
})


test_that("'c' method for tind works correctly", {
    nn <- sample.int(NN - 1L, 1L)
    dd1 <- head(dd, nn)
    dd2 <- tail(dd, -nn)
    ti <- as.tind(dd)
    ti1 <- as.tind(dd1)
    ti2 <- as.tind(dd2)
    expect_equal(ti, c(ti1, ti2))
    expect_equal(ti, c(ti1, dd2))
    err <- paste0("time index type mismatch in ", sQuote("c.tind"))
    expect_error(c(ti1, as.tind(ti2, "y")), err, fixed = TRUE)
    expect_error(c(as.tind(ti1, "y"), ti2), err, fixed = TRUE)
    for (tz in setdiff(tzs, "UTC")) {
        warn <- paste0("different time zones of arguments; assuming: ", dQuote(tz))
        expect_warning(c(as.tind(ti1, tz = tz),
                         as.tind(ti2, tz = "UTC")), warn, fixed = TRUE)
    }
    expect_equal(c(as.date("2024-03-31"), "2024-04-01"),
                       as.date(c("2024-03-31", "2024-04-01")))
})


test_that("'Math.tind', 'Summary.tind', and 'Complex.tind' methods work correctly", {
    # min, max, range
    ti <- as.tind(dd)
    expect_equal(max(ti), as.tind(max(dd)))
    expect_equal(min(ti), as.tind(min(dd)))
    expect_equal(range(ti), as.tind(range(dd)))
    expect_equal(max(ti[1L:10L], ti[11L:NN]), max(ti))
    expect_equal(min(ti[1L:10L], ti[11L:NN]), min(ti))
    expect_equal(range(ti[1L:10L], ti[11L:NN]), range(ti))

    nn <- sample(2L:NN, 1L)
    dd[nn] <- NA
    ti <- as.tind(dd)
    expect_equal(max(ti), as.tind(max(dd), type = "d"))
    expect_equal(min(ti), as.tind(min(dd), type = "d"))
    expect_equal(range(ti), as.tind(range(dd), type = "d"))
    expect_equal(max(ti, na.rm = TRUE), as.tind(max(dd, na.rm = TRUE)))
    expect_equal(min(ti, na.rm = TRUE), as.tind(min(dd, na.rm = TRUE)))
    expect_equal(range(ti, na.rm = TRUE), as.tind(range(dd, na.rm = TRUE)))
    expect_equal(range(ti, na.rm = TRUE),
                     c(min(ti, na.rm = TRUE), max(ti, na.rm = TRUE)))
    # cummax, cummin
    expect_equal(cummax(as.tind(dd)), as.tind(cummax(dd)))
    expect_equal(cummin(as.tind(dd)), as.tind(cummin(dd)))
    # errors
    err <- paste0(" method not defined for class ", dQuote("tind"))
    expect_error(cos(ti), paste0(sQuote("cos"), err), fixed = TRUE)
    expect_error(sum(ti), paste0(sQuote("sum"), err), fixed = TRUE)
    expect_error(Arg(ti), paste0(sQuote("Arg"), err), fixed = TRUE)
})


test_that("'unique', 'duplicated', and 'anyDuplicated' tind methods work correctly", {
    for (tp in c("d", "t")) {
        if (tp == "d") {
            xx <- dd[sample.int(NN, replace = TRUE)]
            ti <- as.tind(xx)
            tz <- NULL
        } else {
            xx <- tt[sample.int(NN, replace = TRUE)]
            tz <- sample(tzs, 1L)
            ti <- as.tind(xx, tz = tz)
        }
        expect_equal(unique(ti), as.tind(unique(xx), tp, tz))
        expect_equal(unique(as.tind(sort(xx), tp, tz)), as.tind(sort(unique(xx)), tp, tz))
        expect_equal(duplicated(ti), duplicated(xx))
        expect_equal(duplicated(ti, fromLast = TRUE), duplicated(xx, fromLast = TRUE))
        expect_equal(anyDuplicated(ti), anyDuplicated(xx))
        expect_equal(anyDuplicated(unique(ti)), 0L)
        expect_equal(anyDuplicated(ti, fromLast = TRUE), anyDuplicated(xx, fromLast = TRUE))
    }
})


test_that("'is.unsorted', 'sort', and 'order' tind methods work correctly", {
    ti <- as.tind(dd)
    expect_equal(is.unsorted(ti), is.unsorted(dd))
    expect_false(is.unsorted(sort(ti)))
    expect_equal(sort(ti), as.tind(sort(dd)))
})

