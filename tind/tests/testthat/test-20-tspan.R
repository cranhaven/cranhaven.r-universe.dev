context("tspan method")
# ###################################################################


# test sample size
NN <- 100L
# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("'tspan' method for 'tind' works correctly", {
    err <- paste0(dQuote("factor"), " is not recognised as a class representing time indices")
    expect_error(tspan(as.factor(letters)), err)

    # tind
    # y, q, m, w, d
    for (tp in c("y", "q", "m", "w", "d")) {
        rng <- range(sample((-NN:NN), 2L))
        inc <- c(rng[1L], sample(rng[1L]:rng[2L], NN, replace = TRUE), rng[2L])
        ind <- as.tind(today(), type = tp) + inc
        tsp <- tspan(ind)
        expect_equal(tsp, as.tdiff(diff(rng) + 1, unit = tp))
        expect_equal(tspan(ind[1L]), as.tdiff(1L, unit = tp))
        expect_true(tspan(ind[0L]) == 0)
        expect_equal(tspan(c(ind, NA)), as.tdiff(NA, unit = tp))
        expect_equal(tspan(c(ind, NA), na.rm = TRUE), tsp)
        expect_warning(tspan(c(ind, NA), na.rm = TRUE, qwerty = 3456))
        expect_error(tspan(c(ind, NA), 3456))
        if (tp == "d")
            expect_equal(tspan(as.Date(ind)), as.tdiff(diff(rng) + 1, unit = tp))
    }
    # t
    tt0 <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 3)
    tt1 <- round(tt0)
    tt2 <- round(tt0 / 60) * 60
    tt3 <- round(tt0 / 3600) * 3600
    tz <- sample(tzs, 1L)
    for (d in 0:3) {
        tt <- get(paste0("tt", d))
        rng <- diff(range(tt))
        ind <- as.date_time(tt, tz = tz)
        tsp <- tspan(ind)
        expect_equal(tsp, as.tdiff(rng, "s"))
        expect_equal(tspan(ind[1L]), as.tdiff(0, unit = "s"))
        expect_true(tspan(ind[0L]) == 0)
        expect_equal(tspan(c(ind, NA)), as.tdiff(NA, unit = "s"))
        expect_equal(tspan(c(ind, NA), na.rm = TRUE), tsp)
    }
    # h
    hh <- round(tt %% 86400, digits = 6)
    rng <- diff(range(hh))
    ind <- as.time(hh)
    tsp <- tspan(ind)
    expect_equal(tsp, as.tdiff(rng, "s"))
    expect_equal(tspan(ind[1L]), as.tdiff(0, unit = "s"))
    expect_true(tspan(ind[0L]) == 0)
    expect_equal(tspan(c(ind, NA)), as.tdiff(NA, unit = "s"))
    expect_equal(tspan(c(ind, NA), na.rm = TRUE), tsp)
    # i
    rng <- range(sample((-NN:NN), 2L))
    ii <- c(rng[1L], sample(rng[1L]:rng[2L], NN, replace = TRUE), rng[2L])
    ind <- as.tind(ii, "i")
    expect_equal(tspan(ind), as.integer(diff(rng)) + 1L)
    expect_true(tspan(ind[1L]) == 1)
    expect_true(tspan(ind[0L]) == 0)
    # n
    rng <- range(rnorm(2L))
    nn <- c(rng[1L], runif(NN, min = rng[1L], max = rng[2L]), rng[2L])
    ind <- as.tind(nn, "n")
    expect_equal(tspan(ind), diff(rng))
    expect_true(tspan(ind[1L]) == 0)
    expect_true(tspan(ind[0L]) == 0)
})



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
dd[nas] <- NA

tt0 <- round(as.numeric(Sys.time()) + runif(NN, -3e7, 3e7), digits = 3)
tt0[nas] <- NA
tt1 <- round(tt0)
tt2 <- round(tt0 / 60) * 60
tt3 <- round(tt0 / 3600) * 3600
tt0 <- as.date_time(tt0)
tt1 <- as.date_time(tt1)
tt2 <- as.date_time(tt2)
tt3 <- as.date_time(tt3)



test_that("'tspan' method for 'tinterval' works correctly", {
    for (tp in c("y", "q", "m", "w", "d")) {
        xx <- get(paste0(tp, tp))
        MM <- sample(2:99, 1L)
        tint_xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                             sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
        tsp <- tspan(tint_xx)
        expect_equal(length(tsp), MM)
        expect_true(is.tdiff(tsp))
        expect_equal(.get_tdiff_type(tsp), tp)
        expect_equal(pmax(unclass(tsp - 1), 0),
                     pmax(unclass(tint_xx$end - tint_xx$start), 0))
    }
    tz <- sample(tzs, 1L)
    for (ttv in 0:3) {
        xx <- as.tind(get(paste0("tt", ttv)), tz = tz)
        MM <- sample(2:99, 1L)
        tint_xx <- tinterval(sort(xx[sample.int(NN, MM, TRUE)], na.last = FALSE),
                                sort(xx[sample.int(NN, MM, TRUE)], na.last = TRUE))
        tsp <- tspan(tint_xx)
        expect_equal(length(tsp), MM)
        expect_true(is.tdiff(tsp))
        expect_true(.get_tdiff_type(tsp) == "t")
        expect_equal(unclass(tsp),
                        pmax(unclass(tint_xx$end - tint_xx$start), 0))
    }
})

