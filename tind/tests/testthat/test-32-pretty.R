context("pretty method")
# ###################################################################


# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test sample size
NN <- 3L

# test samples
y <- sample(1990L:2020L, NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
d <- pmin(sample.int(31L, size = NN, replace = TRUE), .days_in_month(.validate_ym(y, m)))
tt <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 1)

dd <- suppressWarnings(tind(y = y, m = m, d = d))
yy <- as.year(dd)
qq <- as.quarter(dd)
mm <- as.month(dd)
ww <- as.week(dd)
tz <- sample(tzs, 1L)
tt <- as.date_time(tt, tz = tz)
hh <- as.time(tt)

ii <- as.tind(as.integer(runif(NN, -3e3, 3e3)), type = "i")
nn <- as.tind(runif(NN, -3e7, 3e7), type = "n")


test_that("'pretty' method works correctly", {
    skip_on_cran() # in case of corner cases
    for (tp in c("y", "q", "m", "w", "d", "t", "h")) {
        x <- tind(type = tp)
        p <- pretty(x)
        expect_equal(x, p)
        x <- tind(length = sample.int(3L, 1L), type = tp)
        p <- pretty(x)
        expect_equal(x[0L], p)

        x <- get(paste0(tp, tp))
        p <- pretty(x)
        expect_equal(ti_type(x), ti_type(p))
        if (tp == "t")  expect_equal(tzone(x), tzone(p))

        expect_equal(p, pretty(c(x, NA)))

        fx <- resolution_t(x)
        fp <- resolution_t(p)

        if (tp == "h") {
            expect_true(fp >= fx)
        } else {
            expect_true(.get_tdiff_type(fp) %in% c(tp, .lo_res_cast(tp)))
            if (.get_tdiff_type(fp) == .get_tdiff_type(fx)) expect_true(fp >= fx)
        }

        if (!(tp %in% c("i", "w"))) {
            for (tp2 in .hi_res_cast(tp)) {
                if (length(unique(x)) == 1L) next
                p20 <- if (tp2 == "t") as.tind(p, tz = tz) else as.tind(p, type = tp2)
                x2 <- if (tp2 == "t") as.tind(x, tz = tz) else as.tind(x, type = tp2)
                p2 <- pretty(x2)
                expect_equal(p2, p20)
            }
        }
    }

    # errors
    err <- paste0("invalid ", sQuote("n"), " argument; nonnegative integer expected")
    expect_error(pretty(tt, -2, 2), err, fixed = TRUE)
    err <- paste0("invalid ", sQuote("min.n"), " argument; nonnegative integer expected")
    expect_error(pretty(tt, 2, -2), err, fixed = TRUE)
    err <- paste0(sQuote("min.n"), " greater than ", sQuote("n"))
    expect_error(pretty(tt, 2, 3), err, fixed = TRUE)
})

