context("base - time zones")
# ###################################################################

test_that("'.check_tz' works correctly", {
    NN <- min(10L, length(OlsonNames()))
    for (tz in sample(OlsonNames(), NN)) expect_equal(tz, .check_tz(tz))
    expect_equal(.check_tz(NULL), Sys.timezone())
    expect_equal(.check_tz(""), Sys.timezone())
    expect_error(.check_tz(123), paste0("^invalid ", sQuote("tz"),
                 " argument; character string expected$"))
    expect_error(.check_tz(c("", "")), paste0("^invalid ", sQuote("tz"),
                 " argument; character string expected$"))
    expect_error(.check_tz(character()), paste0("^invalid ", sQuote("tz"),
                 " argument; character string expected$"))
    err <- paste0("unrecognised time zone: ", dQuote("Hsdfasdfs/Qwerty"),
                  "; you can find the list of time zones by running ",
                   sQuote("OlsonNames()"))
    expect_error(.check_tz("Hsdfasdfs/Qwerty"), err, fixed = TRUE)
    if (sum(grepl("^Europe", OlsonNames())) > 1L) {
        err <- paste0("^ambiguous time zone name: ", dQuote("Europe"),
                      "; possible matches: ", dQuote("Europe/[a-zA-Z]+"),
                      "(, .+)*$")
        expect_error(.check_tz("Europe"), err)
    }
    skip_on_cran() # in case new time zones with some peculiarities appear
    if ("Europe/Warsaw" %in% OlsonNames()) {
        warn <- paste0("assuming ", dQuote("Warsa"),
                       " refers to time zone ", dQuote("Europe/Warsaw"))
        expect_warning(tz <- .check_tz("Warsa"), warn, fixed = TRUE)
        expect_equal(tz, "Europe/Warsaw")
        expect_silent(tz <- .check_tz("Warsaw"))
        expect_equal(tz, "Europe/Warsaw")
    }
    if ("America/New_York" %in% OlsonNames()) {
        expect_silent(tz <- .check_tz("New_York"))
        expect_equal(tz, "America/New_York")
        expect_silent(tz <- .check_tz("New York"))
        expect_equal(tz, "America/New_York")
    }
})


test_that("'.warn_diff_tz' works correctly", {
    if (length(OlsonNames()) < 2L) skip("too few time zones for further tests")
    tzs <- sample(OlsonNames(), 2L)
    tz1 <- tzs[1L]
    tz2 <- tzs[2L]
    options(tind.warn.diff.tz = TRUE)
    warn <- "different time zones of arguments"
    expect_warning(.warn_diff_tz(tz1), warn, fixed = TRUE)
    warn <- paste0("different time zones of arguments: ", dQuote(tz1), ", ",
                   dQuote(tz2))
    expect_warning(.warn_diff_tz(tz1, tz2), warn, fixed = TRUE)
    warn <- paste0("different time zones of arguments; assuming: ", dQuote(tz1))
    expect_warning(.warn_diff_tz(tz1, first = TRUE), warn, fixed = TRUE)
    warn <- paste0("different time zones of arguments: ", dQuote(tz1), ", ",
                   dQuote(tz2), "; assuming: ", dQuote(tz1))
    expect_warning(.warn_diff_tz(tz1, tz2, TRUE), warn, fixed = TRUE)
    options(tind.warn.diff.tz = FALSE)
    expect_silent(.warn_diff_tz(tz1))
    expect_silent(.warn_diff_tz(tz1, tz2))
    expect_silent(.warn_diff_tz(tz1, first = TRUE))
    expect_silent(.warn_diff_tz(tz1, tz2, TRUE))

    # we check .tind.getOption along the way
    options(tind.warn.diff.tz = NA)
    warn <- paste0("invalid value of option ", dQuote("tind.warn.diff.tz"),
                   "; using default settings (TRUE)")
    expect_warning(wrdtz <- .tind.getOption("warn.diff.tz"), warn, fixed = TRUE)
    expect_equal(wrdtz, TRUE)
})


test_that("'.tz_fixed_offset' works correctly", {
    expect_equal(.tz_fixed_offset("Etc/GMT-1"), 3600)
    expect_equal(.tz_fixed_offset("Etc/GMT+1"), -3600)
})


test_that("'.tzshort' works correctly", {
    skip_on_cran() # in case new time zones with some peculiarities appear

    expect_true(is.character(.tzshort()))

    if ((tz <- "Europe/Warsaw") %in% OlsonNames()) {
        expect_equal(.tzshort("CEST"), list(7200, tz))
    }
    if ((tz <- "Europe/London") %in% OlsonNames()) {
        warn <- paste0("ambiguous abbreviations: ", dQuote("BST"), "; assuming: ",
                    dQuote("BST"), " - +0100 (", dQuote(tz), ")")
        expect_warning(off <- .tzshort("BST"), warn, fixed = TRUE)
        expect_equal(off, list(3600, tz))
        expect_silent(off <- .tzshort("BST", tz = tz))
        expect_equal(off, list(3600, tz))
        warn <- paste0("^NAs introduced; unrecognised abbreviations for time zone Europe/London: ",
                       dQuote("CEST"), "; recognised abbreviations: ", dQuote("[A-Z]+"),
                       "(, ", dQuote("[A-Z]+"), ")+$")
        expect_warning(off <- .tzshort("CEST", tz = tz), warn)
        expect_equal(off, list(NA_real_, tz))
    }

    warn <- paste0("NAs introduced; ambiguous abbreviations: ", dQuote("MSK"),
                   "; provide ", sQuote("tz"), " argument")
    expect_warning(off <- .tzshort("MSK"), warn, fixed = TRUE)
    expect_equal(off, list(NA_real_, .check_tz(NULL)))
    if ((tz <- "Europe/Moscow") %in% OlsonNames()) {
        warn <- paste0("ambiguous abbreviation ", dQuote("MSK"), " for time zone ",
                       "Europe/Moscow; results for earlier dates may be incorrect")
        expect_warning(off <- .tzshort("MSK", tz = tz), warn, fixed = TRUE)
        expect_true(is.finite(off[[1L]]))
        expect_equal(off[[2L]], tz)
        expect_silent(off <- .tzshort("MSK", tz = tz, d = .validate_ymd(2025, 1, 1)))
        expect_equal(off, list(10800, tz))
    }
})

