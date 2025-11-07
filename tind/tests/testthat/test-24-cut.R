context("cut method")
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
ww <- suppressWarnings(tind(y = y, w = w))
dd <- suppressWarnings(tind(y = y, m = m, d = d))
tz <- sample(tzs, 1L)
tt <- as.date_time(tt, tz = tz)

nas <- sample.int(NN, 2L)
yy[nas] <- NA
qq[nas] <- NA
mm[nas] <- NA
ww[nas] <- NA
dd[nas] <- NA
tt[nas] <- NA




test_that("'cut' method works correctly", {
    err <- paste0(".*breaks.* missing.*")
    expect_error(cut(today()), err)
    expect_error(cut(now(), labels = FALSE, right = TRUE), err)
    err <- paste0("invalid ", sQuote("labels"), " argument; TRUE/FALSE/NA expected")
    expect_error(cut(today(), "m", labels = 1), err, fixed = TRUE)
    err <- paste0("invalid ", sQuote("right"), " argument; TRUE/FALSE expected")
    expect_error(cut(today(), "m", right = NA), err, fixed = TRUE)

    # d, w, m, q, y
    tu <- c("d", "w", "m", "q", "y")
    for (tp in tu) {
        xx <- get(paste0(tp, tp))
        xo <- sort(xx[!is.na(xx)])
        # admissible
        for (un in c(.lo_res_cast(tp), tp)) {
            nss <- if (un == "d") .mults(un)
                   else c(1L, sample(setdiff(.mults(un), 1L), 2L))
            for (ns in nss) {
                nun <- paste0(ns, un)
                ct <- cut(xx, nun)
                ct1 <- cut(xx, nun, TRUE)
                ct2 <- cut(xx, nun, FALSE)
                ct3 <- cut(xx, nun, NA)
                expect_true(identical(ct, cut(xx, as.tdiff(nun))) &&
                            identical(ct, ct1) &&
                            identical(as.integer(ct), ct2) &&
                            identical(as.integer(ct), ct3[[1L]]) &&
                            identical(format(ct3[[2L]]), levels(ct)))
                map <- ct3[[1L]]
                lvx <- ct3[[2L]]
                trflx <- trunc_t(floor_t(xx, nun), un)
                expect_equal(lvx[map], trflx)
                cto3 <- cut(xo, nun, NA)
                expect_equal(lvx, cto3[[2L]])
                trflo <- trunc_t(floor_t(xo, nun), un)
                expect_equal(lvx[cto3[[1L]]], trflo)
            }
            br <- xo[sort(sample.int(length(xo), NN %% 10L))]
            br <- unique(trunc_t(br, un))
            ct <- cut(xx, br)
            ct1 <- cut(xx, br, TRUE)
            ct2 <- cut(xx, br, FALSE)
            ct3 <- cut(xx, br, NA)
            expect_true(identical(ct, cut(xx, br, right = FALSE)) &&
                        identical(ct, ct1) &&
                        identical(as.integer(ct), ct2) &&
                        identical(as.integer(ct), ct3[[1L]]) &&
                        identical(format(ct3[[2L]]), levels(ct)) &&
                        identical(ct3[[2L]], br))
        }
    }

    # errors
    err <- paste0(sQuote("right"), " cannot be set to TRUE if ",
                  sQuote("breaks"), " is not of ", dQuote("tind"),
                  " class")
    expect_error(cut(xx, nun, right = TRUE), err, fixed = TRUE)
    err <- paste0("^time index type mismatch")
    expect_error(cut(qq, yy, right = TRUE), err)
    err <- paste0("invalid ", sQuote("breaks"), " argument; ")
    expect_error(cut(tind(y = 2000:2020), tind(y = c(2020, 2010, 2000))), err)
    err <- "^cast from time index type"
    expect_error(cut(ww, mm), err)

    # t
    br <- unique(as.date(tt))
    br <- br[!is.na(br)]
    br <- sort(br)
    ct1 <- cut(tt, br, FALSE)
    br2 <- as.date_time(br, tz = tz)
    ct2 <- cut(tt, br2, FALSE)
    expect_equal(ct1, ct2)
    if (length(tzs) > 1L) {
        tz2 <- sample(setdiff(tzs, tz), 1L)
        br2 <- as.date_time(br2, tz = tz2)
        warn <- "^different time zones"
        expect_warning(ct3 <- cut(tt, br2), warn)
        expect_equal(ct1, ct2)
    }
})



