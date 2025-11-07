context("tind constructor")
# ###################################################################

# supported time index types
types <- c("y", "q", "m", "w", "d", "t", "i", "n")

# test sample size
NN <- 100L
# test samples
y <- sample(1969L:2068L, NN, replace = TRUE)
q <- sample.int(4L, size = NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
w <- sample.int(53L, size = NN, replace = TRUE)
w <- pmin(w, .weeks_in_year(y))
j <- sample.int(366L, size = NN, replace = TRUE)
j <- pmin(j, .days_in_year(y))
d <- sample.int(31L, size = NN, replace = TRUE)
d <- pmin(d, .days_in_month(.validate_ym(y, m)))
u <- sample.int(7L, size = NN, replace = TRUE)
H <- sample.int(24L, size = NN, replace = TRUE) - 1L
M <- sample.int(60L, size = NN, replace = TRUE) - 1L
S <- floor(runif(20, 0, 600)) / 10

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("'tind' (constructor) works correctly", {
    err <- "type not provided"
    expect_error(tind(), err, fixed = TRUE)
    expect_error(tind(length = 0), err, fixed = TRUE)
    err <- paste0(sQuote("length"), " provided with time index components (",
                  sQuote("..."), ") in ", dQuote("tind"), " constructor")
    expect_error(tind(y = 1999, length = 1), err, fixed = TRUE)
    err <- paste0("unnamed arguments in ", dQuote("tind"), " constructor")
    expect_error(tind(y = 1999, 1), err, fixed = TRUE)
    err <- paste0("duplicated time index components: ", dQuote("m"))
    expect_error(tind(y = 1999, m = 1, m = 2), err, fixed = TRUE)
    err <- paste0("type inferred (", .ti_type2char("y", dash = TRUE),
                  ") is different from type provided as argument (",
                  .ti_type2char("d", dash = TRUE), ")")
    expect_error(tind(y = 1999, type = "d"), err, fixed = TRUE)
    warn <- paste0("the following components will be ignored: ", .ti_comp2char("u"),
                   "; type inferred: ", .ti_type2char("y"))
    expect_warning(tind(y = 1999, u = 1), warn, fixed = TRUE)

    x <- tind(y = y)
    expect_equal(.get.type(x), "y")
    expect_equal(.get.tz(x), NULL)
    attributes(x) <- NULL
    expect_equal(x, y)
    expect_equal(tind(y = integer()), tind(type = "y"))

    warn <- "longer object length is not a multiple of shorter object length"

    expect_warning(tind(y = y[1L:2L], q = q[1L:3L]), warn, fixed = TRUE)
    expect_warning(tind(y = y[1L:3L], q = q[1L:2L]), warn, fixed = TRUE)
    x <- tind(y = y, q = q)
    expect_equal(x, tind(q = q, y = y))
    expect_equal(.get.type(x), "q")
    expect_equal(.get.tz(x), NULL)
    attributes(x) <- NULL
    expect_equal(x, .validate_yq(y, q))

    expect_warning(tind(y = y[1L:2L], m = m[1L:3L]), warn, fixed = TRUE)
    expect_warning(tind(y = y[1L:3L], m = m[1L:2L]), warn, fixed = TRUE)
    x <- tind(y = y, m = m)
    expect_equal(x, tind(m = m, y = y))
    expect_equal(.get.type(x), "m")
    expect_equal(.get.tz(x), NULL)
    attributes(x) <- NULL
    expect_equal(x, .validate_ym(y, m))

    expect_warning(tind(y = y[1L:2L], w = w[1L:3L]), warn, fixed = TRUE)
    expect_warning(tind(y = y[1L:3L], w = w[1L:2L]), warn, fixed = TRUE)
    x <- tind(y = y, w = w)
    expect_equal(x, tind(w = w, y = y))
    expect_equal(.get.type(x), "w")
    expect_equal(.get.tz(x), NULL)
    attributes(x) <- NULL
    expect_equal(x, .validate_yw(y, w))

    expect_warning(tind(y = y[1L:2L], j = j[1L:3L]), warn, fixed = TRUE)
    expect_warning(tind(y = y[1L:3L], j = j[1L:2L]), warn, fixed = TRUE)
    x <- tind(y = y, j = j)
    expect_equal(x, tind(j = j, y = y))
    expect_equal(.get.type(x), "d")
    expect_equal(.get.tz(x), NULL)
    attributes(x) <- NULL
    expect_equal(x, .y2d(y) + j - 1L)

    expect_warning(tind(y = y[1L:2L], m = m[1L:3L], d = d[1L:3L]), warn, fixed = TRUE)
    expect_warning(tind(y = y[1L:3L], m = m[1L:3L], d = d[1L:2L]), warn, fixed = TRUE)
    x <- tind(y = y, m = m, d = d)
    expect_equal(x, tind(m = m, d = d, y = y))
    expect_equal(x, tind(d = d, m = m, y = y))
    expect_equal(.get.type(x), "d")
    expect_equal(.get.tz(x), NULL)
    attributes(x) <- NULL
    expect_equal(x, .validate_ymd(y, m, d))

    expect_warning(tind(y = y[1L:2L], w = w[1L:3L], u = u[1L:3L]), warn, fixed = TRUE)
    expect_warning(tind(y = y[1L:3L], w = w[1L:3L], u = u[1L:2L]), warn, fixed = TRUE)
    x <- tind(y = y, w = w, u = u)
    expect_equal(x, tind(w = w, u = u, y = y))
    expect_equal(x, tind(u = u, w = w, y = y))
    expect_equal(.get.type(x), "d")
    expect_equal(.get.tz(x), NULL)
    attributes(x) <- NULL
    expect_equal(x, .w2d(.validate_yw(y, w)) + u - 1L)

    dd <- .unclass(tind(y = y, m = m, d = d))
    hh <- .validate_hms(H, 0, 0)
    hm <- .validate_hms(H, M, 0)
    hms <- .validate_hms(H, M, S)
    for (tz in tzs) {
        x <- suppressWarnings(tind(y = y, m = m, d = d, H = H, tz = tz))
        tt <- .dhz2t(dd, hh, integer(), tz, 0L)
        expect_equal(.get.type(x), "t")
        expect_equal(.get.tz(x), tz)
        expect_equal(as.vector(x), tt)
        x <- suppressWarnings(tind(y = y, m = m, d = d, H = H, M = M, tz = tz))
        tt <- .dhz2t(dd, hm, integer(), tz, 0L)
        expect_equal(.get.type(x), "t")
        expect_equal(.get.tz(x), tz)
        expect_equal(as.vector(x), tt)
        x <- suppressWarnings(tind(y = y, m = m, d = d, H = H, M = M, S = S, tz = tz))
        tt <- .dhz2t(dd, hms, integer(), tz, 0L)
        expect_equal(.get.type(x), "t")
        expect_equal(.get.tz(x), tz)
        expect_equal(as.vector(x), tt)
        # check correct argument recycling
        primes <- sample(c(2L, 3L, 5L, 7L))
        yp <- 2000 + seq_len(primes[1L])
        dp <- seq_len(primes[2L])
        Hp <- 10 + seq_len(primes[3L])
        Mp <- seq_len(primes[4L])
        expect_warning(dt0 <- tind(y = yp, m = 1, d = dp, H = Hp, M = Mp, tz = tz),
                       warn, fixed = TRUE)
        dd1 <- tind(y = rep_len(yp, 7L), m = 1, d = rep_len(dp, 7L))
        hh1 <- tind(H = rep_len(Hp, 7L), M = rep_len(Mp, 7L))
        expect_equal(.t2d(unclass(dt0), tz), unclass(dd1))
        expect_equal(.t2h(unclass(dt0), tz), unclass(hh1))
    }
    if ((tz <- "Europe/Warsaw") %in% OlsonNames()) {
        warn <- paste0("NAs introduced; first occurrence: ",
                       "y[1] = 2022, m[1] = 3, d[1] = 27, H[3] = 2; ",
                       "time zone: ", tz)
        # DST change
        expect_warning(tind(y = 2022, m = 3, d = 27, H = 0:4, tz = tz), warn, fixed = TRUE)
    }
})

