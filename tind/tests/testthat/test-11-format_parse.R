context("tind - formatting and parsing")
# ###################################################################

# test sample size
NN <- 100L
nms <- sample(letters, NN, replace = TRUE)
# nms <- NULL

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", #"Etc/GMT+1",
                                 "Europe/London", "America/New_York"))

# test samples
nt <- 1e9 + c(0, round(runif(NN - 1L, min = 0, max = 1e9)))
nas <- sample.int(NN, NN %/% 10L)
nt[nas] <- NA_real_
y <- sample(1999L:2035L, NN, replace = TRUE)
y[nas] <- NA_integer_
m <- sample.int(12L, size = NN, replace = TRUE)
d <- sample.int(31L, size = NN, replace = TRUE)
d <- pmin(d, .days_in_month(.validate_ym(y, m)))
H <- sample.int(24L, size = NN, replace = TRUE) - 1L
M <- sample.int(60L, size = NN, replace = TRUE) - 1L
S <- round(c(11, floor(runif(NN - 1L, 0, 599))) / 10, 1)


test_that("'format', 'strptind', and 'parse_t' work correctly", {
    # y, q, m, w, d, h
    fmts <- list()
    ords <- list()
    fmts[["y"]] <- c("%Y", "%y", "%%%Y%%")
    ords[["y"]] <- c("y",  "y",  "y")
    fmts[["q"]] <- c("%YQ%q", "%Yq%q", "%Y.%q", "%Y%t%q", "%Y%n%q", "Q%q '%y")
    ords[["q"]] <- c("yq",    "yq",    "yq",    "yq",     "yq",     "qy")
    fmts[["m"]] <- c("%Y-%m", "%Y%m", "%B %Y", "%b %Y", "%b %y")
    ords[["m"]] <- c("ym",    "ym",   "my",    "my",    "my")
    fmts[["w"]] <- c("%G-W%V", "%g-w%V")
    ords[["w"]] <- c("yw",     "yw")
    fmts[["d"]] <- c("%Y-%m-%d", "%B %d, %Y", "%b %d, %Y", "%m/%d/%y",
                     "%e %B %Y", "%e %b %Y",  "%d.%m.%Y",
                     "%G-W%V-%u", "%G-W%V %A", "%G-W%V %a",
                     "%Y-%j")
    ords[["d"]] <- c("ymd",      "mdy",       "mdy",       "mdy",
                     "dmy",      "dmy",       "dmy",
                     "ywu",      "ywu",       "ywu",
                     "yj")
    fmts[["h"]] = c("%H:%M:%S", "%I:%M:%S %p")
    ords[["h"]] = c("HMS",      "IMSp")

    tz <- sample(tzs, 1L)
    xx <- as.tind(nt, tz = tz)
    for (tp in names(fmts)) {
        x <- as.tind(xx, type = tp)
        names(x) <- nms
        expect_equal(format(x[0L]), character())
        fmt <- fmts[[tp]]
        ord <- ords[[tp]]
        for (fi in seq_along(fmt)) {
            cc <- format(x, fmt[fi])
            if (fi == 1L) {
                expect_equal(cc, format(x))
                expect_equal(x, as.tind(cc))
            }
            expect_equal(x, strptind(cc, fmt[fi]))
            expect_equal(x, parse_t(cc, ord[fi]))
        }
    }

    # some special cases: %q, %G, %V
    fmt <- "%Y.%q"
    for (tp in c("q", "m", "d")) {
        x <- as.tind(xx, type = tp)
        expect_equal(format(x, fmt), format(xx, fmt))
    }
    fmt <- "%G, week: %V"
    for (tp in c("w", "d")) {
        x <- as.tind(xx, type = tp)
        expect_equal(format(x, fmt), format(xx, fmt))
    }

    # date-time
    for (tz in tzs) {
        xx <- suppressWarnings(tind(y = y, m = m, d = d, H = H, M = M, tz = tz))
        fmt <- c("%Y-%m-%d %H:%M%z", "%Y-%m-%d %H:%M %Z", "%Y-%m-%d %H:%M",
                 "%m/%d/%y %I:%M %p")
        ord <- c("ymdHMz",           "ymdHMz",            "ymdHM",
                 "mdyIMp")
        expect_equal(format(xx[0L]), character())
        for (fi in seq_along(fmt)) {
            cc <- format(xx, fmt[fi])
            expect_equal(xx, suppressWarnings(as.tind(cc, tz = tz)))
            if (fi == 1L) {
                expect_equal(as.numeric(xx), as.numeric(strptind(cc, fmt[fi])))
                expect_equal(as.numeric(xx), as.numeric(parse_t(cc, ord[fi])))
            }
            if ((fi == 2L) && (tz != "Europe/London")) {
                expect_equal(xx, as.tind(cc, tz = tz))
            }
        }

        xx <- suppressWarnings(tind(y = y, m = m, d = d, H = H, M = M, S = floor(S), tz = tz))
        fmt <- c("%Y-%m-%d %H:%M:%S%z", "%Y-%m-%d %H:%M:%S %Z", "%Y-%m-%d %H:%M:%S",
                 "%m/%d/%y %I:%M:%S %p")
        ord <- c("ymdHMSz",             "ymdHMSz",              "ymdHMS",
                 "mdyIMSp")
        for (fi in seq_along(fmt)) {
            cc <- format(xx, fmt[fi])
            expect_equal(xx, suppressWarnings(as.tind(cc, tz = tz)))
            if (fi == 1L) {
                expect_equal(as.numeric(xx), as.numeric(strptind(cc, fmt[fi])))
                expect_equal(as.numeric(xx), as.numeric(parse_t(cc, ord[fi])))
            }
            if ((fi == 2L) && (tz != "Europe/London")) {
                expect_equal(xx, as.tind(cc, tz = tz))
            }
        }

        xx <- suppressWarnings(tind(y = y, m = m, d = d, H = H, M = M, S = S, tz = tz))
        fmt <- c("%Y-%m-%d %H:%M:%OS1%z", "%Y-%m-%d %H:%M:%OS1 %Z", "%Y-%m-%d %H:%M:%OS1",
                 "%m/%d/%y %I:%M:%OS1 %p")
        ord <- c("ymdHMSz",               "ymdHMSz",                "ymdHMS",
                 "mdyIMSp")
        for (fi in seq_along(fmt)) {
            cc <- format(xx, fmt[fi])
            expect_equal(xx, suppressWarnings(as.tind(cc, tz = tz)))
            if (fi == 1L) {
                expect_equal(as.numeric(xx),
                             as.numeric(strptind(cc, sub("%OS1", "%OS", fmt[fi],
                                                         fixed = TRUE))))
                expect_equal(as.numeric(xx), as.numeric(parse_t(cc, ord[fi])))
            }
            if ((fi == 2L) && (tz != "Europe/London")) {
                expect_equal(xx, as.tind(cc, tz = tz))
            }
        }
    }

    # long formats (buffer overload check)
    for (fmt in c("%F %T", "%B %d, %Y (%A)")) {
        nw <- as.tind(Sys.time())
        expect_equal(paste(rep(format(nw, fmt), 10L), collapse = " "),
                    format(nw, paste(rep(fmt, 10L), collapse = " ")))
    }

    # some corner cases
    if ((tz <- "Etc/GMT+1") %in% OlsonNames()) {
        xx <- tind(y = 2021, m = 1, d = 1, H = 7, tz = "Etc/GMT+1")
        warn <- "time zone abbreviation not available"
        expect_warning(res <- format(xx, "%F %R %Z"), warn)
        expect_true(grepl("\\?\\?\\?$", res))
        expect_equal(format(xx), format(xx, "%F %R%z"))
    }
    expect_equal(as.numeric(as.tind("2021-03-17 07:00-01")),
                 as.numeric(as.tind("2021-03-17") - as.tind("1970-01-01")) * 86400 + 8 * 3600)
    warn <- paste0("^format specifier ", dQuote("%y"), " \\(or ", dQuote("%g"),
                   "\\) used for years outside [0-9]{4}-[0-9]{4} range$")
    expect_warning(format(tind(y = 1899), "%y"), warn)
    expect_equal(format(tind(type = "y"), "%y"), character())

    warn <- paste0("NAs introduced; first position 1: ", dQuote("2023-02-29"),
                   "; type: ", dQuote("d"), " (date); order: ", dQuote("ymd"))
    expect_warning(parse_t("2023-02-29", "ymd"), warn, fixed = TRUE)
    warn <- paste0("NAs introduced; first position 1: ", dQuote("2023-02-29"),
                   "; type: ", dQuote("d"), " (date); format: ", dQuote("%Y-%m-%d"))
    expect_warning(strptind("2023-02-29", "%F"), warn, fixed = TRUE)

    # errors - format i n
    err <- paste0("^format specification for type ", dQuote("[in]"),
                  " \\((integer|numeric) index\\) is not supported; use ",
                  sQuote("format.+"), " or ", sQuote("formatC.+"), "$")
    expect_error(format(as.tind(1, "i"), "%i"), err)
    expect_error(format(as.tind(1, "n"), "%n"), err)

    # errors - format
    errinv <- paste0("invalid ", sQuote("format"), " argument; character string expected")
    expect_error(format(xx, 1), errinv, fixed = TRUE)
    errfmt <- paste0("invalid ", sQuote("format"), " argument; a letter or ",
                     dQuote("%"), " expected after ", dQuote("%"))
    expect_error(format(xx, "%Y%"), errfmt, fixed = TRUE)
    errspf <- paste0("unrecognised / unsupported format specifiers: ", dQuote("%K"))
    expect_error(format(xx, "%K"), errspf, fixed = TRUE)
    errspf <- paste0("^unrecognised / unsupported format specifiers: ", dQuote("%[wx]"), "\nNote: ")
    expect_error(format(xx, "%w"), errspf)
    expect_error(format(xx, "%x"), errspf)
    errspf <- paste0("^the following specifiers are not supported for type ",
                     dQuote("m"), " \\(month\\): ", dQuote("%d"), "; admissible specifiers: ",
                     dQuote("%[a-zA-Z]"), "(, ", dQuote("%[a-zA-Z]"), ")+$")
    expect_error(format(tind(type = "m"), "%d"), errspf)
    errspf <- paste0("^the following specifiers are not supported for type ",
                     dQuote("w"), " \\(week\\): ", dQuote("%y"), "; admissible specifiers: ",
                     dQuote("%[a-zA-Z]"), "(, ", dQuote("%[a-zA-Z]"), ")+\nHint:")
    expect_error(format(tind(type = "w"), "%y"), errspf)

    # errors - strptind
    errmis <- ".*format.* missing"
    expect_error(strptind(character()), errmis)
    errinv <- paste0("invalid ", sQuote("format"), " argument; character string or character vector expected")
    expect_error(strptind(character(), 1), errinv)
    expect_error(strptind(character(), character()), errinv)
    errfmt <- paste0("invalid ", sQuote("format"), " argument; a letter or ",
                     dQuote("%"), " expected after ", dQuote("%"), "; format: ", dQuote("%Y%"))
    expect_error(strptind(character(), "%Y%"), errfmt, fixed = TRUE)
    errspf <- paste0("unrecognised / unsupported format specifiers: ", dQuote("%K"))
    expect_error(strptind(character(), "%K"), errspf, fixed = TRUE)
    errspf <- paste0("^unrecognised / unsupported format specifiers: ", dQuote("%[wx]"), "\nNote: ")
    expect_error(strptind(character(), "%w"), errspf)
    expect_error(strptind(character(), "%x"), errspf)
    # format specifications
    errspf <- "no valid format specification"
    expect_error(strptind(character(), "%m%d"), errspf, fixed = TRUE)
    warnspf <- paste0("ignoring invalid format specifications: ", dQuote("%m%d"))
    expect_warning(strptind(character(), c("%Y-%j", "%m%d")), warnspf, fixed = TRUE)
    errtype <- paste0("type inferred (", dQuote("m"), " - month) ",
                      "is different from type provided as argument (", dQuote("d"),
                      " - date)")
    expect_error(strptind(character(), "%Y-%m", type = "d"), errtype, fixed = TRUE)
    errtype <- paste0("conflicting types inferred: ", dQuote("m"), " (month), ",
                      dQuote("d"), " (date)")
    expect_error(strptind(character(), c("%Y-%m", "%F")), errtype, fixed = TRUE)
    warnign <- paste0("the following components will be ignored: ", dQuote("%V"),
                      "; format: ", dQuote("%Y-%j %V"), "; type inferred: ", dQuote("d"), " (date)")
    expect_warning(strptind(character(), c("%Y-%j %V")), warnign, fixed = TRUE)
    warnign <- paste0("the following components will be ignored: ", dQuote("%V"),
                      "; format: ", dQuote("%Y-%j %V"), "; type: ", dQuote("d"), " (date)")
    expect_warning(strptind(character(), "%Y-%j %V", type = "d"), warnign, fixed = TRUE)

    # errors - parse_t
    errmis <- ".*order.* missing"
    expect_error(parse_t(character()), errmis)
    errinv <- paste0("invalid ", sQuote("order"), " argument; character string or character vector expected")
    expect_error(parse_t(character(), 1), errinv)
    expect_error(parse_t(character(), character()), errinv)
    # order specifications
    errsunr <- "unrecognised time index components"
    expect_error(parse_t(character(), "%m%d"), errsunr, fixed = TRUE)
    errsinv <- "cannot infer type / construct time index"
    expect_error(parse_t(character(), c("mdy", "md")), errsinv, fixed = TRUE)
    errtype <- paste0("type inferred (", dQuote("m"), " - month) ",
                      "is different from type provided as argument (", dQuote("d"),
                      " - date)")
    expect_error(parse_t(character(), "ym", type = "d"), errtype, fixed = TRUE)
    errtype <- paste0("conflicting types inferred: ", dQuote("m"), " (month), ",
                      dQuote("d"), " (date)")
    expect_error(parse_t(character(), c("ym", "mdy")), errtype, fixed = TRUE)
    warnign <- paste0("the following components will be ignored: ", dQuote("S"),
                      " (second); order: ", dQuote("ymdS"), "; type inferred: ", dQuote("d"), " (date)")
    expect_warning(parse_t(character(), c("ymdS")), warnign, fixed = TRUE)
    warnign <- paste0("the following components will be ignored: ", dQuote("S"),
                      " (second); order: ", dQuote("ymdS"), "; type: ", dQuote("d"), " (date)")
    expect_warning(parse_t(character(), "ymdS", type = "d"), warnign, fixed = TRUE)

    # named factors
    y1 <- parse_t(factor(c(a = "1998", b = "1999")), "y")
    y2 <- strptind(factor(c(a = "1998", b = "1999")), "%Y")
    y3 <- tind(y = c(a = 1998, b = 1999))
    expect_equal(y1, y2)
    expect_equal(y2, y3)

    # AM/PM locale
    skip_on_cran()
    cl <- Sys.getlocale("LC_TIME")
    if (is.null(cl)) skip("locale information not available")
    pl_locales <- c("pl", "pl_PL", "Polish", "Polish_Poland")
    for (lc in pl_locales) {
        suppressWarnings(pl_locale <- Sys.setlocale("LC_TIME", lc))
        if (pl_locale != "") break
    }
    if (pl_locale != "") {
        warn <- "AM/PM indicators not available"
        expect_warning(parse_t("2024-09-17 1pm", "ymdIp", locale = "pl_PL"), warn, fixed = TRUE)
    }
    Sys.setlocale("LC_TIME", cl)

})

