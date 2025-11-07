context("time axes for plots")
# ###################################################################

# time zones for tests
tzs <- intersect(OlsonNames(), c("Asia/Tokyo", "Europe/Warsaw",
                                 "UTC", "Etc/GMT+1",
                                 "Europe/London", "America/New_York"))
tz <- sample(tzs, 1L)


# test data frame
N <- sample(30:360, 1L)
td <- today()
df <- data.frame(d = td + (-N + 1):0, y = cumsum(rnorm(N)), i = seq_len(N))
# test data for custom breaks
d24 <- seq(as.date("2024-01-01"), 2024)
q24 <- as.quarter("2024q1") + 0:4
l24 <- c("2024", paste("Qtr", 2:4), "2025")
m24 <- tind(y = 2024, m = setdiff(1:12, 1 + 3 * 0:3))
x24 <- cumsum(rnorm(366))
h24 <- tind(H = 0:24)
df24 <- data.frame(d = d24, y = x24)



test_that("'axis_t' works correctly", {
    ax0 <- axis_t(df$d)
    ax0e <- axis_t(df$d, expand = TRUE)
    ax1 <- axis_t(df$d, format = "%D")
    ax2 <- axis_t(df$d, format = function(x) format(x, "%m/%d/%y"),
                        limits = td + c(-2*N, N), n.breaks = 7L)
    axl <- list(ax0, ax0e, ax1, ax2)
    expect_true(all(sapply(axl, is.list)))
    for (ax in axl)
        expect_equal(names(ax),  c("lim", "at", "labels", "minor", "resolution", "limits"))
    expect_true(all(sapply(axl, function(ax) is.numeric(ax$lim))))
    expect_true(all(sapply(axl, function(ax) length(ax$lim) == 2L)))
    expect_true(all(sapply(axl, function(ax) is.numeric(ax$at))))
    expect_true(all(sapply(axl, function(ax) is.character(ax$labels))))
    expect_true(all(sapply(axl, function(ax) length(ax$at) == length(ax$labels))))
    expect_true(all(sapply(axl, function(ax) is.numeric(ax$minor))))
    expect_true(all(sapply(axl, function(ax) is.numeric(ax$resolution))))
    expect_true(all(sapply(axl, function(ax) length(ax$resolution) == 1L)))
    expect_true(all(sapply(axl, function(ax) is.tinterval(ax$limits))))
    expect_true(all(sapply(axl, function(ax) length(ax$limits) == 1L)))
    pat <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    expect_true(all(sapply(axl[1L:2L], function(ax) all(grepl(pat, ax$labels)))))
    pat <- "^[0-9]{2}/[0-9]{2}/[0-9]{2}$"
    expect_true(all(sapply(axl[3L:4L], function(ax) all(grepl(pat, ax$labels)))))
    expect_equal(ax0$lim, ax1$lim)
    expect_equal(ax0$at, ax1$at)
    expect_equal(as.date(ax0$labels), as.date(ax1$labels))
    expect_equal(ax0$minor, ax1$minor)
    expect_equal(diff(ax0$lim) * 1.06, diff(ax0e$lim))

    # date-time
    for (N in 0:2) {
        dt <- date_time(today() + (-N):N, 6 * (0:3), grid = TRUE, tz = tz)
        ax <- axis_t(dt)
        expect_true(is.numeric(ax$lim))
        expect_true(length(ax$lim) == 2L)
        expect_true(is.numeric(ax$at))
        expect_true(is.character(ax$labels))
        expect_true(length(ax$at) == length(ax$labels))
        expect_true(is.numeric(ax$minor))
        expect_true(is.numeric(ax$resolution))
        expect_true(length(ax$resolution) == 1L)
        expect_true(is.tinterval(ax$limits))
        expect_true(length(ax$limits) == 1L)
        pat <- "^([0-9]{4}-[0-9]{2}-[0-9]{2}|[0-9]{2}:[0-9]{2})$"
        expect_true(all(grepl(pat, ax$labels)))
    }

    # limits
    lm <- c(td - 20, td - 2)
    axl0 <- axis_t(df$d, limits = lm)
    expect_equal(axl0$limits, tinterval(lm[1L], lm[2L]))
    axl1 <- axis_t(df$d, limits = lm[1L] %--% lm[2L])
    expect_equal(axl1, axl0)
    axl2 <- axis_t(df$d, limits = as.character(lm[1L] %--% lm[2L]))
    expect_equal(axl2, axl0)
    err <- "^invalid"
    expect_error(axis_t(df$d, limits = (lm[1L] %--% lm[2L]) + 0:1), err)
    expect_error(axis_t(df$d, limits = "qwerty"), err)
    err <- "^empty"
    expect_error(axis_t(df$d, limits = lm[2L] %--% lm[1L]), err)
    err <- "^no data"
    expect_warning(axis_t(df$d, limits = (lm[1L] %--% lm[2L]) + 30), err)
    expect_error(axis_t(df$d, limits = as.tinterval("09:00 -- 17:00")))
})


test_that("'axis.tind' and 'Axis.tind' method work correctly", {
    skip_on_cran()
    skip_if_not_installed("graphics")
    expect_silent(graphics::plot(df$d, df$y, type = "l"))
    graphics::plot(df$d, df$y, type = "l", xaxt = "n")
    expect_silent(axis.tind(1, df$d, format = "%m/%d/%y", n.breaks = 7L,
                            cex.axis = .9))
    expect_silent(graphics::plot(df$i, df$d, type = "l"))
    graphics::plot(df$i, df$d, type = "l", yaxt = "n")
    expect_silent(axis.tind(2, df$d, format = "%m/%d/%y", n.breaks = 7L,
                            cex.axis = .9))

    # custom breaks and labels
    graphics::plot(d24, x24, type = "l", xaxt = "n")
    expect_silent(axis.tind(1, d24, at = q24))
    graphics::plot(d24, x24, type = "l", xaxt = "n")
    expect_silent(axis.tind(1, d24, at = q24, labels = l24))
    expect_silent(axis.tind(1, d24, at = m24, labels = FALSE, lwd = 0, lwd.ticks = .5))
    expect_error(axis.tind(1, d24, at = h24))
})


test_that("'scale_*_tind' and 'scale_type.tind' method work correctly", {
    skip_on_cran()
    skip_if_not_installed("ggplot2")
    expect_silent(show(ggplot2::ggplot(df) + ggplot2::geom_line(ggplot2::aes(x = d, y = y))))
    expect_silent(show(ggplot2::ggplot(df) + ggplot2::geom_line(ggplot2::aes(x = d, y = y)) +
                       scale_x_tind(limits = today() + c(-2 * N, N))))
    expect_silent(show(ggplot2::ggplot(df) + ggplot2::geom_line(ggplot2::aes(x = i, y = d))))
    expect_silent(show(ggplot2::ggplot(df) + ggplot2::geom_line(ggplot2::aes(x = i, y = d)) +
                       scale_y_tind(limits = today() + c(-2 * N, N))))
    # custom breaks and labels
    expect_silent(show(ggplot2::ggplot(df24) + ggplot2::geom_line(ggplot2::aes(x = d, y = y)) +
                       scale_x_tind(breaks = q24, labels = l24)))
    expect_silent(show(ggplot2::ggplot(df24) + ggplot2::geom_line(ggplot2::aes(x = d, y = y)) +
                       scale_x_tind(breaks = q24, labels = l24, minor_breaks = m24)))
    expect_error(show(ggplot2::ggplot(df24) + ggplot2::geom_line(ggplot2::aes(x = d, y = y)) +
                      scale_x_tind(breaks = q24, labels = l24, minor_breaks = h24)))
    expect_error(show(ggplot2::ggplot(df24) + ggplot2::geom_line(ggplot2::aes(x = d, y = y)) +
                      scale_x_tind(breaks = h24, labels = l24)))
})

