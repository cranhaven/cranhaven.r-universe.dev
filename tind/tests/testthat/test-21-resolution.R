context("resolution_t")
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

dd <- tind(y = y, m = m, d = d)
yy <- as.year(dd)
qq <- as.quarter(dd)
mm <- as.month(dd)
ww <- as.week(dd)
tz <- sample(tzs, 1L)
tt <- as.date_time(tt, tz = tz)
hh <- as.time(tt)


test_that("'resolution_t' method works correctly", {
    for (tp in c("y", "q", "m", "w", "d")) {
        x <- get(paste0(tp, tp))
        rx <- resolution_t(x)
        expect_true(is.tdiff(rx) && length(rx) == 1L)
        urx <- .get_t_unit(rx)
        expect_true(urx %in% c(tp, .lo_res_cast(tp)))
        x1 <- x[1L]
        rx1 <- resolution_t(x1)
        expect_equal(rx1, as.tdiff(1L, tp))
        x0 <- x[0L]
        rx0 <- resolution_t(x0)
        expect_equal(rx0, as.tdiff(1L, tp))
    }

    # corner case (ambiguity - 2w vs 15d)
    x <- tind(y = 2004, m = 2, d = 16) + c(0, 14)
    expect_equal(resolution_t(x), days(15))
    expect_equal(resolution_t(x), resolution_t(as.Date(x)))

    # time of day
    x <- hh
    rx <- resolution_t(x)
    expect_true(is.tdiff(rx) && length(rx) == 1L)
    urx <- .get_t_unit(rx)
    expect_true(urx %in% c("h", "min", "s"))
    # corner case
    expect_equal(resolution_t(tind(H = c(0, 0))), hours(1))

    # date-time
    x <- tt
    rx <- resolution_t(x)
    expect_true(is.tdiff(rx) && length(rx) == 1L)
    urx <- .get_t_unit(rx)
    expect_true(urx %in% c("h", "min", "s", .lo_res_cast("t")))
    if (urx %in% c("h", "min", "s"))
        expect_equal(rx, resolution_t(as.time(x)))

    # integer
    pr <- sample(c(2L, 3L, 5L, 7L), 1L)
    x <- as.tind(pr * seq_len(NN), "i")
    rx <- resolution_t(x)
    expect_true(is.integer(rx) && length(rx) == 1L)
    expect_equal(rx, pr)
    expect_equal(resolution_t(x[0L]), 1L)
    expect_equal(resolution_t(x[1L]), 1L)

    # numeric
    x <- as.tind((0:10) / 4, "n")
    rx <- resolution_t(x)
    expect_true(is.double(rx) && length(rx) == 1L)
    expect_equal(rx, .25)
    x <- as.tind(1 / (1:10), "n")
    rx <- resolution_t(x)
    expect_true(is.double(rx) && length(rx) == 1L)
    expect_true(is.na(rx))

    # error
    err <- paste0(dQuote("function"), " is not recognised as a class representing time indices")
    expect_error(resolution_t(sum), err, fixed = TRUE)
})


