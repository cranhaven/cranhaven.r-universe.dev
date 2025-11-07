context("ti_type, t_unit")
# ###################################################################

# supported time index types
types <- c("y", "q", "m", "w", "d", "t", "h", "i", "n")
# instant types
instypes <- c("t", "h", "n")
# supported units of time
units <- c("y", "q", "m", "w", "d", "h", "min", "s")


test_that("'ti_type' and 'is.instant' work correctly", {
    for (tp in types) {
        ti <- tind(type = tp)
        titp <- ti_type(ti)
        titp0 <- ti_type(ti, FALSE, FALSE)
        titp1 <- ti_type(ti, FALSE, TRUE)
        titp2 <- ti_type(ti, TRUE, FALSE)
        titp3 <- ti_type(ti, TRUE, TRUE)
        expect_equal(titp, titp2)
        expect_equal(titp0, titp1)
        expect_equal(titp0, tp)
        expect_true(nchar(titp2) > 1L)
        expect_true(nchar(titp3) > 1L)
        expect_equal(titp3, make.names(titp3))

        if (tp %in% instypes) expect_true(is.instant(ti))
        else expect_false(is.instant(ti))
    }

    err <- paste0(dQuote("function"), " is not recognised as a class representing time indices")
    expect_error(ti_type(sum, FALSE), err)
    expect_error(ti_type(sum, TRUE), err)

    expect_true(nchar(ti_type(Sys.Date())) > 1L)
    expect_equal(ti_type(Sys.Date(), FALSE), "d")
    expect_false(is.instant(Sys.Date()))
    expect_true(nchar(ti_type(Sys.time())) > 1L)
    expect_equal(ti_type(Sys.time(), FALSE), "t")
    expect_true(is.instant(Sys.time()))
})


test_that("'t_unit' and 'unit' methods work correctly", {
    for (un in units) {
        td <- as.tdiff(1, un)
        tu <- t_unit(td)
        tu0 <- t_unit(td, FALSE)
        tu1 <- t_unit(td, TRUE)
        tu2 <- t_unit(td, TRUE, TRUE)
        expect_equal(tu, tu1)
        expect_true(nchar(tu1) > 1L)
        expect_true(nchar(tu2) > 1L)
        if (tu1 == make.names(tu1)) expect_equal(tu1, tu2)
        expect_equal(units(td), tu1)
    }

    err <- paste0(dQuote("function"), " is not recognised as a class representing time differences")
    expect_error(t_unit(sum, FALSE), err)
    expect_error(t_unit(sum, TRUE), err)

    dt <- Sys.Date() - Sys.Date()
    expect_equal(t_unit(dt, FALSE), "d")
    expect_true(nchar(t_unit(dt)) > 1L)
    dt <- as.difftime(30, units = "mins")
    expect_equal(t_unit(dt, FALSE), "min")
    expect_true(nchar(t_unit(dt)) > 3L)

    # units<-
    err <- paste(sQuote("units<-"), "method not defined for class", dQuote("tdiff"))
    expect_error(units(td) <- "hours", err, fixed = TRUE)
})

