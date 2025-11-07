context("print, summary, and str methods")
# ###################################################################

# test sample size
NN <- 100L

types <- c("y", "q", "m", "w", "d", "t", "h", "i", "n")

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))


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
hh <- as.time(tt)
ii <- as.tind(as.integer(runif(NN, )), type = "i")
nn <- as.tind(runif(NN, -3e7, 3e7), type = "n")


omitpat <- paste0("^ \\[ reached ", sQuote("getOption\\(\"max.print\"\\)"),
                  " -- omitted [0-9]+ entries \\]$")
prpat <- paste0("^( *\\[[0-9]+\\])")


test_that("'print', 'summary', and 'str' for tind work correctly", {
    for (tp in types) {
        xind <- get(paste0(tp, tp))
        xind0 <- xind[0L]
        # print
        for (omp in c(NN - 1L, 2 * NN)) {
            options(max.print = omp)
            cap <- capture_output_lines(out <- expect_invisible(print(xind)))
            expect_equal(xind, out)
            cap0 <- capture_output_lines(out0 <- expect_invisible(print(xind0)))
            expect_equal(xind0, out0)
            header <- paste0(.ti_type(tp))
            if (tp == "t") header <- paste0(header, ", time zone: ", tz)
            header <- paste0(header, " [tind]")
            expect_equal(cap[1L], header)
            expect_equal(cap0[1L], header)
            cap <- cap[-1L]
            cap0 <- cap0[-1L]
            expect_true(grepl("^length 0$", cap0))
            if (NN > getOption("max.print")) {
                expect_true(grepl(omitpat, cap[length(cap)]))
                cap <- head(cap, -1L)
            }
            expect_true(all(grepl(prpat, cap)))
            cap <- sub(prpat, "", cap)
            cap <- paste0(cap, collapse = "")
            cap <- if (tp == "t") strsplit(cap, "  +")[[1L]] else strsplit(cap, " ")[[1L]]
            cap <- cap[!grepl("^ *$", cap)]
            cap <- if (tp == "t") as.tind(cap, tz = tz) else as.tind(cap, type = tp)
            if (!(tp %in% c("n", "t", "h"))) expect_equal(xind[1L:length(cap)], cap)
            else expect_equal(xind[1L:length(cap)], cap)
        }
        # summary
        summ <- expect_visible(summary(xind))
        nmssumm <- names(summ)
        expect_true(all(grepl("^[A-Z]", nmssumm)))
        capture_output(out <- expect_invisible(print(summ)))
        expect_equal(summ, out)
        expect_equal(length(summ),length(summary(sort(unique(xind)))))
        summ0 <- expect_visible(summary(xind0))
        expect_equal(length(summ0), 2L + (tp == "t"))
        # str
        skip_if_not_installed("utils")
        cap <- capture_output_lines(out <- expect_invisible(str(xind)))
        expect_null(out)
        expect_true(length(cap) == 1L)
        strpat <- "^ tind \\[1:[0-9]+\\](.*)(\\.\\.\\.)?$"
        expect_true(grepl(strpat, cap))
        cap0 <- capture_output_lines(out0 <- expect_invisible(str(xind0)))
        expect_null(out0)
        expect_true(length(cap0) == 1L)
        strpat0i <- "^ 'tind' int\\(0\\)$"
        strpat0d <- "^ 'tind' num\\(0\\)$"
        expect_true(grepl(if (.is.instant(tp)) strpat0d else strpat0i, cap0))
    }
})


test_that("'print', 'summary', and 'str' for 'tinterval' work correctly", {
    for (tp in types) {
        xind <- get(paste0(tp, tp))
        xtint <- tinterval(xind, rev(xind))
        xtint0 <- tinterval(xind, rev(xind))[0L]
        # print
        for (omp in c(NN - 1L, 2 * NN)) {
            options(max.print = omp)
            cap <- capture_output_lines(out <- expect_invisible(print(xtint)))
            expect_equal(xtint, out)
            cap0 <- capture_output_lines(out0 <- expect_invisible(print(xtint0)))
            expect_equal(xtint0, out0)
            if (tp == "i") {
                header <- "^integer interval"
            } else if (tp == "n") {
                header <- "^numeric interval"
            } else {
                header <- "^time interval"
                if (tp == "t") header <- paste0(header, ", time zone: ",
                                                gsub("+", "\\+", tz, fixed = TRUE))
                else if (tp == "h") header <- paste0(header, " \\(time of day\\)")
                else header <- paste0(header, " in ", tp, "[a-z]+s")
            }
            header <- paste0(header, " \\[tinterval\\]$")
            expect_true(grepl(header, cap[1L]))
            expect_true(grepl(header, cap0[1L]))
            cap <- cap[-1L]
            cap0 <- cap0[-1L]
            expect_true(grepl("^empty$", cap0))
            if (NN > getOption("max.print")) {
                expect_true(grepl(omitpat, cap[length(cap)]))
                cap <- head(cap, -1L)
            }
            expect_true(all(grepl(prpat, cap)))
            cap <- sub(prpat, "", cap)
            cap <- gsub("  +--", " --", cap)
            cap <- gsub("--  +", "-- ", cap)
            cap <- gsub("  +\\(", " \\(", cap)
            cap <- paste0(cap, collapse = "")
            cap <- strsplit(cap, "  +")[[1L]]
            cap <- cap[!grepl("^ *$", cap)]
            cap <- if (tp == "t") as.tinterval(cap, tz = tz) else as.tinterval(cap, type = tp)
            if (!(tp %in% c("n", "h", "t"))) expect_equal(xtint[1L:length(cap)], cap)
            else expect_equal(xtint[1L:length(cap)], cap)
        }
        # summary
        summ <- expect_visible(summary(xtint))
        nmssumm <- names(summ)
        expect_true(all(grepl("^[A-Z]", nmssumm)))
        capture_output(out <- expect_invisible(print(summ)))
        expect_equal(summ, out)
        summ0 <- expect_visible(summary(xtint0))
        expect_equal(length(summ0), 2L + (tp == "t"))
        # str
        skip_if_not_installed("utils")
        cap <- capture_output_lines(out <- expect_invisible(str(xtint)))
        expect_null(out)
        expect_true(length(cap) == 3L)
        expect_true(grepl("^ 'tinterval'$", cap[1L]))
        expect_true(grepl("^   \\$ start: .*$", cap[2L]))
        expect_true(grepl("^   \\$ end  : .*$", cap[3L]))
        cap <- cap[-1L]
        cap <- sub("^   \\$ (start|end  ):", "", cap)
        expect_equal(cap[1L], capture_output(str(xtint$start)))
        expect_equal(cap[2L], capture_output(str(xtint$end)))
        cap <- capture_output_lines(out <- expect_invisible(str(xtint0)))
        expect_null(out)
        expect_true(length(cap) == 3L)
        expect_true(grepl("^ 'tinterval'$", cap[1L]))
        expect_true(all(grepl("^   \\$ (start|end  ): 'tind' (num|int)\\(0\\).*$", cap[-1L])))
    }
})


types <- setdiff(types, c("i", "n"))


test_that("'print', 'summary', and 'str' for tdiff work correctly", {
    types <- union(setdiff(types, c("t", "h")), sample(c("t", "h"), 1L))
    for (tp in types) {
        xind <- get(paste0(tp, tp))
        xdiff <- xind - rev(xind)
        xdiff0 <- xdiff[0L]
        # print
        for (omp in c(NN - 1L, 2 * NN)) {
            options(max.print = omp)
            cap <- capture_output_lines(out <- expect_invisible(print(xdiff)))
            expect_equal(xdiff, out)
            cap0 <- capture_output_lines(out0 <- expect_invisible(print(xdiff0)))
            expect_equal(xdiff0, out0)
            if (tp %in% c("t", "h")) {
                header <- paste0("^time difference \\[tdiff\\]$")
            } else {
                header <- paste0("^time difference, unit: ", .t_unit(tp), " \\[tdiff\\]$")
            }
            expect_true(grepl(header, cap[1L]))
            expect_true(grepl(header, cap0[1L]))
            cap <- cap[-1L]
            cap0 <- cap0[-1L]
            expect_true(grepl("^length 0$", cap0))
            if (NN > getOption("max.print")) {
                expect_true(grepl(omitpat, cap[length(cap)]))
                cap <- head(cap, -1L)
            }
            expect_true(all(grepl(prpat, cap)))
            cap <- sub(prpat, "", cap)
            cap <- paste0(cap, collapse = "")
            cap <- strsplit(cap, " ")[[1L]]
            cap <- cap[!grepl("^ *$", cap)]
            cap <- cap[!grepl("^\\(.*\\)$", cap)] # rm aux info
            cap <- as.tdiff(cap)
            expect_equal(xdiff[1L:length(cap)], cap)
        }
        # summary
        summ <- expect_visible(summary(xdiff))
        nmssumm <- names(summ)
        expect_true(all(grepl("^[A-Z]", nmssumm)))
        capture_output(out <- expect_invisible(print(summ)))
        expect_equal(summ, out)
        summ0 <- expect_visible(summary(xdiff0))
        expect_equal(length(summ0), 2L)
        # str
        skip_if_not_installed("utils")
        cap <- capture_output_lines(out <- expect_invisible(str(xdiff)))
        expect_null(out)
        expect_true(length(cap) == 1L)
        strpat <- "^ tdiff \\[1:[0-9]+\\](.*)(\\.\\.\\.)?$"
        expect_true(grepl(strpat, cap))
        cap0 <- capture_output_lines(out0 <- expect_invisible(str(xdiff0)))
        expect_null(out0)
        expect_true(length(cap0) == 1L)
        strpat0i <- "^ 'tdiff' int\\(0\\)$"
        strpat0d <- "^ 'tdiff' num\\(0\\)$"
        expect_true(grepl(if (.is.instant(tp)) strpat0d else strpat0i, cap0))
    }
})

