context("base - utility functions")
# ###################################################################

test_that("'.require_mode' works correctly", {
    expect_equal(.require_mode(1L, "integer"), 1L)
    expect_equal(.require_mode(1., "integer"), 1L)
    expect_equal(.require_mode(1., "double"), 1.)
    expect_equal(.require_mode(1L, "double"), 1.)
})


test_that("'.isTRUEFALSE' works correctly", {
    expect_true(.isTRUEFALSE(TRUE))
    expect_true(.isTRUEFALSE(FALSE))
    expect_false(.isTRUEFALSE(NA))
    expect_false(.isTRUEFALSE(c(FALSE, TRUE)))
})


test_that("'.checkTRUEFALSE' works correctly", {
    foo <- TRUE
    expect_silent(.checkTRUEFALSE(foo))
    foo <- FALSE
    expect_silent(.checkTRUEFALSE(foo))
    foo <- NA
    err <- paste0("invalid ", sQuote("foo"), " argument; TRUE/FALSE expected")
    expect_error(.checkTRUEFALSE(foo), err, fixed = TRUE)
})


test_that("'.anyTRUE' works correctly", {
    xx <- c(NA_integer_, 1L:7L, NA_integer_)
    expect_true(.anyTRUE(xx < 2))
    expect_false(.anyTRUE(xx < 1))
    expect_false(.anyTRUE(xx[0] < 1))
    expect_false(.anyTRUE(xx[1] < 1))
})


test_that("'.class2str' works correctly", {
    st <- Sys.time()
    sd <- Sys.Date()
    expect_equal(.class2str(st), dQuote("POSIXct/POSIXt"))
    expect_equal(.class2str(sd), dQuote("Date"))
})


test_that("'.match_left' works correctly", {
    expect_equal(.match_left(0:9, c(0, 4, 8)),
                     c(rep(1L, 4L), rep(2L, 4L), rep(3L, 2L)))
    expect_equal(.match_left(c(0:9, NA), c(0, 4, 8)),
                     c(rep(1L, 4L), rep(2L, 4L), rep(3L, 2L), NA_integer_))
    expect_equal(.match_left(0:9, c(1, 4, 8)),
                     c(NA_integer_, rep(1L, 3L), rep(2L, 4L), rep(3L, 2L)))
    expect_equal(.match_left(c(0:9, NA), c(1, 4, 8)),
                     c(NA_integer_, rep(1L, 3L), rep(2L, 4L), rep(3L, 2L), NA_integer_))
})


test_that("'.match_right' works correctly", {
    expect_equal(.match_right(0:9, c(1, 4, 9)),
                     c(rep(1L, 2L), rep(2L, 3L), rep(3L, 5L)))
    expect_equal(.match_right(c(0:9, NA), c(1, 4, 9)),
                     c(rep(1L, 2L), rep(2L, 3L), rep(3L, 5L), NA_integer_))
    expect_equal(.match_right(0:9, c(1, 4, 8)),
                     c(rep(1L, 2L), rep(2L, 3), rep(3L, 4L), NA_integer_))
    expect_equal(.match_right(c(0:9, NA), c(1, 4, 8)),
                     c(rep(1L, 2L), rep(2L, 3), rep(3L, 4L), NA_integer_, NA_integer_))
})


test_that("'.match_exct' works correctly", {
    x <- 0L:9L
    tab <- sort(sample(x, 4L))
    expect_equal(.match_exct(x, tab), match(x, tab))
    expect_equal(.match_exct(x, tab, nomatch = 0L),
                     match(x, tab, nomatch = 0L))
    x <- x + .5
    tab <- tab + .5
    expect_equal(.match_exct(x, tab), match(x, tab))
    expect_equal(.match_exct(x, tab, nomatch = 0L),
                     match(x, tab, nomatch = 0L))
})


test_that("'.check_digits' works correctly", {
    dok <- 0:6
    digs <- sample(c(-1:10, NA))
    err <- paste0("invalid ", sQuote("digits"), " argument; expected: 0-6")
    for (d in digs) {
        if (d %in% dok) expect_equal(.check_digits(d), d)
        else expect_error(.check_digits(d), err)
    }
    expect_error(.check_digits(dok), err)
})

