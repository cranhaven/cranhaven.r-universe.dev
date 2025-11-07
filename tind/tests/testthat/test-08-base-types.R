context("base - time index types and time units")
# ###################################################################

# supported time index types
types <- c("y", "q", "m", "w", "d", "t", "h", "i", "n")
# instant types
instypes <- c("t", "h", "n")
# supported units of time
units <- c("y", "q", "m", "w", "d", "h", "min", "s")
# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


test_that("'.ti_type' works correctly", {
    expect_equal(.ti_type(long = FALSE), types)
    expect_equal(length(.ti_type(long = TRUE)), length(types))
    for (tp in types) {
        expect_equal(tp, .ti_type(tp, long = FALSE))
        expect_true(nchar(.ti_type(tp, long = TRUE)) > 1L)
        expect_equal(.ti_type(tp, valid = TRUE, rm.names = TRUE),
                         make.names(.ti_type(tp, valid = TRUE, rm.names = TRUE)))
    }
})


test_that("'.ti_type2char' works correctly", {
    expect_true(is.character(.ti_type2char()))
    expect_true(is.character(.ti_type2char(dash = TRUE)))
    expect_equal(length(.ti_type2char()), 1L)
    expect_equal(length(.ti_type2char(dash = TRUE)), 1L)
    for (tp in types) {
        expect_true(grepl(paste0("^", dQuote(tp), " \\([- a-z]+\\)$"), .ti_type2char(tp)))
        expect_true(grepl(paste0("^", dQuote(tp), " - [- a-z]+$"), .ti_type2char(tp, dash = TRUE)))
    }
})


test_that("'.check_type' works correctly", {
    for (tp in types) expect_silent(.check_type(tp))
    err <- paste0("^invalid ", sQuote("type"), " argument; expected one of the following: ",
                  dQuote("[a-z]+"), " \\([- a-z]+\\)(, ", dQuote("[a-z]+"),
                  " \\([- a-z]+\\))+$")
    expect_error(.check_type("x"), err)
    expect_error(.check_type(7), err)
    expect_error(.check_type(c("y", "y")), err)
    for (tp in sample(setdiff(letters, types), 2L)) expect_error(.check_type(tp), err)
})


test_that("'.check_type_tz' works correctly", {
    err0 <- paste0("^invalid ", sQuote("type"), " argument; expected one of the following: ",
                   dQuote("[a-z]"), " \\([- a-z]+\\)(, ", dQuote("[a-z]"),
                   " \\([- a-z]+\\))+$")
    err1 <- paste0("time zone provided for type other than ", .ti_type2char("t"))
    tz <- sample(tzs, 1L)
    expect_error(.check_type_tz(sample(setdiff(letters, types), 1L), NULL), err0)
    expect_error(.check_type_tz(sample(setdiff(letters, types), 1L), tz), err0)
    for (tp in setdiff(types, "t")) {
        expect_equal(.check_type_tz(tp, NULL), list(type = tp, tz = NULL))
        expect_error(.check_type_tz(tp, tz), err1, fixed = TRUE)
    }
    expect_equal(.check_type_tz(NULL, tz), list(type = "t", tz = tz))
    expect_equal(.check_type_tz("t", tz), list(type = "t", tz = tz))
    expect_equal(.check_type_tz("t", NULL), list(type = "t", tz = Sys.timezone()))
})


test_that("'.is.instant' and '.mode' work correctly", {
    expect_true(all(instypes %in% types))
    for (tp in instypes) expect_true(.is.instant(tp))
    for (tp in setdiff(types, instypes)) expect_false(.is.instant(tp))
    for (tp in instypes) expect_equal(.mode(tp), "double")
    for (tp in setdiff(types, instypes)) expect_equal(.mode(tp), "integer")
})


test_that("'.limits' works correctly", {
    for (tp in c("i", "n")) expect_equal(.limits(tp), c(-Inf, Inf))
    for (tp in setdiff(types, c("i", "n"))) {
        lim <- .limits(tp)
        vfn <- paste0(".validate_", tp)
        expect_equal(lim, do.call(vfn, list(lim)))
        expect_false(any(is.na(do.call(vfn, list(lim)))))
        expect_true(all(is.na(do.call(vfn, list(lim + c(-1, 1))))))
    }
})


test_that("'.infer_type' works correctly", {
    expect_equal(.infer_type("y"), list(type = "y", ignored = character()))
    expect_equal(.infer_type(c("y", "m")), list(type = "m", ignored = character()))
    expect_equal(.infer_type(c("y", "m", "q")), list(type = "m", ignored = "q"))
    err <- paste0("^unrecognised time index components: ", dQuote("x"),
                  "\nHint: recognised components are: .*")
    expect_error(.infer_type(c("y", "m", "q", "x")), err)
    err <- paste0("cannot infer type / construct time index based on the following components: ",
                  dQuote("m"), " (month)")
    expect_error(.infer_type("m"), err, fixed = TRUE)
})


test_that("'.hi_res_cast' and '.lo_res_cast' work correctly", {
    for (tp in types) {
        hi <- .hi_res_cast(tp)
        lo <- .lo_res_cast(tp)
        expect_true(is.character(hi))
        expect_true(is.character(lo))
        expect_false(as.logical(length(intersect(hi, lo))))
        if (length(hi)) {
            expect_true(all(hi %in% types))
            expect_false(tp %in% hi)
            if (tp != "i") for (tph in hi) expect_true(tp %in% .lo_res_cast(tph))
        }
        if (length(lo)) {
            expect_true(all(lo %in% types))
            expect_false(tp %in% lo)
            if (tp != "n")
                for (tpl in setdiff(lo, "h")) expect_true(tp %in% .hi_res_cast(tpl))
        }
    }
})


test_that("'.max_res' works correctly", {
    err <- paste0("^cast from time index type ", dQuote("[a-z]"),
                  " \\([- a-z]+\\) to type ", dQuote("[a-z]"),
                  " \\([- a-z]+\\) in " , sQuote("[\\._a-z]+"), " not possible$")
    tpin <- c("i", "n")
    tpinh <- c("i", "n", "h")
    tp <- setdiff(types, tpinh)
    expect_equal(.max_res(tp), "t")
    tp <- setdiff(types, tpin)
    expect_equal(.max_res(tp, weak = TRUE), "t")
    tp <- setdiff(types, tpinh)
    tp <- setdiff(tp, "t")
    expect_equal(.max_res(tp), "d")
    tp <- setdiff(tp, "d")
    expect_error(.max_res(tp, 0L), err)
    tp <- setdiff(tp, "y")
    expect_error(.max_res(tp, 0L, weak = TRUE), err)
    erri <- paste0("^cast from time index type ", dQuote("i"),
                   " \\(integer index\\) to type ", dQuote("[a-z]"),
                   " \\([- a-z]+\\) in " , sQuote("[\\._a-z]+"), " not possible$")
    expect_error(.max_res(c("i", sample(tp, 1L)), 0L), erri)
    expect_error(.max_res(c("i", sample(tp, 1L), weak = TRUE), 0L), erri)
    errn <- paste0("^cast from time index type ", dQuote("n"),
                   " \\(numeric index\\) to type ", dQuote("[a-z]"),
                   " \\([- a-z]+\\) in " , sQuote("[\\._a-z]+"), " not possible$")
    expect_error(.max_res(c("n", sample(tp, 1L)), 0L), errn)
    expect_error(.max_res(c("n", sample(tp, 1L)), 0L, weak = TRUE), errn)
    expect_equal(.max_res(c("i", "i")), "i")
    expect_equal(.max_res(c("i", "i"), weak = TRUE), "i")
    expect_equal(.max_res(c("i", "n")), "n")
    expect_equal(.max_res(c("i", "n"), 0L, weak = TRUE), "n")
    errn <- paste0("^cast from time index type ", dQuote("d"),
                   " \\(date\\) to type ", dQuote("h"),
                   " \\(time of day\\) in " , sQuote("[\\._a-z]+"), " not possible$")
    expect_error(.max_res(c("d", "h"), 0L), errn)
    errn <- paste0("^cast from time index type ", dQuote("h"),
                   " \\(time of day\\) to type " , dQuote("d"),
                   " \\(date\\) in ", sQuote("[\\._a-z]+"), " not possible$")
    expect_error(.max_res(c("d", "h"), 0L, weak = TRUE), errn)
})


test_that(".t_unit works correctly", {
    expect_equal(.t_unit(long = FALSE), units)
    expect_equal(length(.t_unit(long = TRUE)), length(units))
    for (u in units) {
        expect_equal(u, .t_unit(u, long = FALSE))
        expect_true(nchar(.t_unit(u, long = TRUE)) > 1L)
        expect_true(grepl("^[_a-z]+$", .t_unit(u, long = TRUE, valid = TRUE)))
    }
})


test_that("'.t_unit2char' works correctly", {
    expect_true(is.character(.t_unit2char()))
    expect_equal(length(.t_unit2char()), 1L)
    for (u in units) {
        expect_true(grepl(paste0("^", dQuote(u), " \\([^\\)]+\\)$"), .t_unit2char(u)))
    }
})


test_that("'.check_unit' works correctly", {
    for (un in units) expect_silent(.check_unit(un))
    err <- paste0("^invalid ", sQuote("unit"), " argument; expected one of the following: ",
                  dQuote("[a-z]+"), " \\([- a-z]+\\)(, ", dQuote("[a-z]+"),
                  " \\([- a-z]+\\))+$")
    expect_error(.check_unit(7), err)
    expect_error(.check_unit(c("y", "y")), err)
    for (un in sample(setdiff(letters, units), 2L)) expect_error(.check_unit(un), err)
})


test_that("'.check_inc_x_by_y' works correctly", {
    for (tp in types) {
        n <- round(rnorm(sample.int(100L, 1L), 0, 100))
        expect_equal(.check_inc_x_by_y(tp, n, NULL), list(n = n, unit = NULL))
        for (u in units) {
            if (tp %in% c("i", "n")) {
                err <- paste0("^time units not supported with type ", dQuote(tp),
                              " \\([- a-z]+\\)$")
                expect_error(.check_inc_x_by_y(tp, n, u), err)
            } else {
                err <- paste0("^invalid time unit for type ",
                              dQuote(tp), " \\([- a-z]+\\); ",
                              "admissible units: ", dQuote("[a-z]+"), " \\([- a-z]+\\)",
                              "(, ", dQuote("[a-z]+"), " \\([- a-z]+\\))*$")
                if (tp %in% c("t", "h")) {
                    uok <- c("h", "min", "s")
                    uok <- c(uok, .lo_res_cast(tp))
                } else uok <- c(tp, .lo_res_cast(tp))
                if (!(u %in% uok)) {
                    expect_error(.check_inc_x_by_y(tp, n, u), err)
                } else if ((tp == u) && (tp != "h")) {
                    expect_equal(.check_inc_x_by_y(tp, n, u), list(n = n, unit = NULL))
                }
            }
        }
    }
})


test_that("'.check_fl_ceil_type_unit' works correctly", {
    # i
    errp <- paste0("positive integer expected for type ", dQuote("i"), " \\([- a-z]+\\)$")
    erru <- paste0("time unit provided for type ", dQuote("i"), " \\([- a-z]+\\)$")
    expect_error(.check_fl_ceil_type_unit("i", list(n = 1, unit = sample(units, 1L))), erru)
    n <- sample.int(10L)
    expect_error(.check_fl_ceil_type_unit("i", list(n = n, unit = NULL)), errp)
    expect_error(.check_fl_ceil_type_unit("i", list(n = NULL, unit = NULL)), errp)
    expect_equal(.check_fl_ceil_type_unit("i", list(n = n[1L], unit = NULL)),
                 list(n = n[1L], unit = "i", type = "i"))
    # n
    errp <- paste0("positive number expected for type ", dQuote("n"), " \\([- a-z]+\\)$")
    erru <- paste0("time unit provided for type ", dQuote("n"), " \\([- a-z]+\\)$")
    expect_error(.check_fl_ceil_type_unit("n", list(n = 1, unit = sample(units, 1L))), erru)
    n <- exp(rnorm(10L))
    expect_error(.check_fl_ceil_type_unit("n", list(n = n, unit = NULL)), errp)
    expect_error(.check_fl_ceil_type_unit("n", list(n = NULL, unit = NULL)), errp)
    expect_equal(.check_fl_ceil_type_unit("n", list(n = n[1L], unit = NULL)),
                 list(n = n[1L], unit = "n", type = "n"))
    # y, ..., d
    for (tp in setdiff(types, c("i", "n", "h", "t"))) {
        for (u in units) {
            n <- 17L
            if (u %in% c(tp, .lo_res_cast(tp))) {
                errn <- paste0("^invalid multiplier for time unit ",
                               dQuote(u), " \\([- a-z]+\\)",
                               "; admissible values: (0\\.)?[-e0-9]+(, (0\\.)?[-e0-9]+)+$")
                expect_error(.check_fl_ceil_type_unit(tp, list(n = n, unit = u)), errn)
                n <- sample(.mults(u), 1L)
                expect_equal(.check_fl_ceil_type_unit(tp, list(n = n, unit = u)),
                             list(n = n, unit = u, type = u))
            } else {
                erru <- paste0("^invalid time unit for type ",
                               dQuote(tp), " \\([- a-z]+\\); ",
                               "admissible units: ", dQuote("[a-z]+"), " \\([- a-z]+\\)",
                               "(, ", dQuote("[a-z]+"), " \\([- a-z]+\\))*$")
                expect_error(.check_fl_ceil_type_unit(tp, list(n = n, unit = u)), erru)
            }
        }
    }
    # t, h
    for (u in units) {
        erru <- paste0("^invalid time unit for type ",
                        dQuote("h"), " \\([- a-z]+\\); ",
                        "admissible units: ", dQuote("[a-z]+"), " \\([- a-z]+\\)",
                        "(, ", dQuote("[a-z]+"), " \\([- a-z]+\\))*$")
        if (u %in% c("h", "min", "s")) {
            n <- 17L
            errn <- paste0("^invalid multiplier for time unit ",
                            dQuote(u), " \\([- a-z]+\\)",
                            "; admissible values: (0\\.)?[-e0-9]+(, (0\\.)?[-e0-9]+)+$")
            expect_error(.check_fl_ceil_type_unit("t", list(n = n, unit = u)), errn)
            expect_error(.check_fl_ceil_type_unit("h", list(n = n, unit = u)), errn)
            n <- sample(.mults(u), 1L)
            expect_equal(.check_fl_ceil_type_unit("h", list(n = n, unit = u)),
                         list(n = n, unit = u, type = "h"))
            expect_equal(.check_fl_ceil_type_unit("t", list(n = n, unit = u)),
                         list(n = n, unit = u, type = "t"))
        } else {
            expect_error(.check_fl_ceil_type_unit("h", list(n = n, unit = u)), erru)
            n <- 17L
            errn <- paste0("^invalid multiplier for time unit ",
                            dQuote(u), " \\([- a-z]+\\)",
                            "; admissible values: (0\\.)?[-e0-9]+(, (0\\.)?[-e0-9]+)+$")
            expect_error(.check_fl_ceil_type_unit("t", list(n = n, unit = u)), errn)
            n <- sample(.mults(u), 1L)
            expect_equal(.check_fl_ceil_type_unit("t", list(n = n, unit = u)),
                         list(n = n, unit = u, type = u))
        }
    }
})

