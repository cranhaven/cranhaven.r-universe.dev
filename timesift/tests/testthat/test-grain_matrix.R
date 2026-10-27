hourly_series <- function(units = c("a", "b"), from = "2021-09-01", hours = 24 * 400) {
  t <- seq(as.POSIXct(from, tz = "UTC"), by = "hour", length.out = hours)
  set.seed(1L)
  data.frame(
    plot = rep(units, each = length(t)),
    t = rep(t, length(units)),
    temp = as.numeric(replicate(length(units), rnorm(length(t)))),
    stringsAsFactors = FALSE
  )
}

# An independent reduction, written the slow obvious way, to check the fast one against.
brute <- function(d, key, fun) {
  out <- tapply(d$temp, list(d$plot, key), fun)
  out[order(rownames(out)), order(colnames(out)), drop = FALSE]
}

test_that("bins tile the record with no gap and no overlap", {
  d <- hourly_series()
  for (w in c("native", "halfday", "day", "week", "month", "season", "year")) {
    x <- grain_matrix(d, plot, t, temp, grain = w)
    n <- attr(x, "bin_n")
    expect_equal(unname(rowSums(n)), c(24 * 400, 24 * 400),
                 info = paste("grain", w))
    expect_true(all(n > 0L), info = paste("grain", w))
  }
})

test_that("the mean matches an independent reduction", {
  d <- hourly_series()
  x <- grain_matrix(d, plot, t, temp, grain = "day", stats = "mean")
  ref <- brute(d, as.Date(d$t, tz = "UTC"), mean)
  expect_equal(as.numeric(x[, , "mean"]), as.numeric(ref))
})

test_that("min and max match an independent reduction", {
  d <- hourly_series()
  key <- format(d$t, "%Y-%m", tz = "UTC")
  x <- grain_matrix(d, plot, t, temp, grain = "month", stats = c("min", "max"))
  expect_equal(as.numeric(x[, , "min"]), as.numeric(brute(d, key, min)))
  expect_equal(as.numeric(x[, , "max"]), as.numeric(brute(d, key, max)))
})

test_that("an extreme day is not an extreme reading", {
  # One hour of day 2 is far colder than anything else, but day 3 is the coldest day on average.
  t <- seq(as.POSIXct("2021-09-06", tz = "UTC"), by = "hour", length.out = 24 * 7)
  temp <- rep(c(10, 5, 0, 5, 10, 10, 10), each = 24)
  temp[24 + 5] <- -50
  d <- data.frame(plot = "a", t = t, temp = temp)
  x <- grain_matrix(d, plot, t, temp, grain = "week",
                     stats = c("min", "cold_day", "mean", "warm_day", "max"))
  expect_equal(as.numeric(x[1, 1, "min"]), -50)
  expect_equal(as.numeric(x[1, 1, "cold_day"]), 0)
  expect_equal(as.numeric(x[1, 1, "warm_day"]), 10)
  expect_equal(as.numeric(x[1, 1, "max"]), 10)
})

test_that("cold_day, warm_day and mean coincide at the daily grain", {
  d <- hourly_series(units = "a", hours = 24 * 30)
  x <- grain_matrix(d, plot, t, temp, grain = "day",
                     stats = c("cold_day", "mean", "warm_day"))
  expect_equal(as.numeric(x[, , "cold_day"]), as.numeric(x[, , "mean"]))
  expect_equal(as.numeric(x[, , "warm_day"]), as.numeric(x[, , "mean"]))
})

test_that("coarse bins follow the calendar rather than a fixed hour count", {
  d <- hourly_series(units = "a", hours = 24 * 400)
  x <- grain_matrix(d, plot, t, temp, grain = "month")
  n <- as.integer(attr(x, "bin_n")[1, ])
  # September has 30 days, October 31, February 28. A drifting 730-hour block would give 730.
  expect_equal(n[1], 30L * 24L)
  expect_equal(n[2], 31L * 24L)
  expect_true(any(n == 28L * 24L))
})

test_that("the hydrological year boundary moves with year_start", {
  t <- seq(as.POSIXct("2021-08-25", tz = "UTC"), by = "hour", length.out = 24 * 20)
  d <- data.frame(plot = "a", t = t, temp = 1)
  sep <- grain_matrix(d, plot, t, temp, grain = "year", year_start = "09-01")
  jan <- grain_matrix(d, plot, t, temp, grain = "year", year_start = "01-01")
  expect_equal(dim(sep)[2], 2L)
  expect_equal(dim(jan)[2], 1L)
  expect_equal(dimnames(sep)[[2]], c("2020-09-01T00:00:00Z", "2021-09-01T00:00:00Z"))
  expect_equal(dimnames(jan)[[2]], "2021-01-01T00:00:00Z")
})

test_that("seasons are three calendar months counted from year_start", {
  d <- hourly_series(units = "a", hours = 24 * 400)
  x <- grain_matrix(d, plot, t, temp, grain = "season", year_start = "09-01")
  starts <- format(attr(x, "bin_start"), "%Y-%m-%d", tz = "UTC")
  expect_equal(starts[1:3], c("2021-09-01", "2021-12-01", "2022-03-01"))
})

test_that("a bin the record does not fill is reported and can be dropped", {
  # 1 September 2021 is a Wednesday, so three hydrological years from it fill every month, season
  # and year of the calendar and neither the first nor the last week of it.
  aligned <- hourly_series(units = "a", from = "2021-09-01 00:00:00", hours = 26304)
  for (w in c("native", "halfday", "day", "month", "season", "year")) {
    x <- grain_matrix(aligned, plot, t, temp, grain = w)
    expect_false(any(attr(x, "bin_partial")), info = w)
  }
  week <- grain_matrix(aligned, plot, t, temp, grain = "week")
  expect_equal(which(attr(week, "bin_partial")), c(1L, 157L))
  expect_equal(dim(grain_matrix(aligned, plot, t, temp, grain = "week",
                                 partial = "drop"))[2], 155L)

  # A logger deployed on no boundary at all carries one at the start of every grain that
  # aggregates, and the bin it starts is the one holding fewer readings than its neighbours.
  offset <- hourly_series(units = "a", from = "2021-10-17 05:00:00", hours = 24 * 300)
  for (w in c("halfday", "day", "week", "month", "season")) {
    p <- attr(grain_matrix(offset, plot, t, temp, grain = w), "bin_partial")
    expect_true(p[1L], info = w)
    expect_false(any(p[-c(1L, length(p))]), info = w)
  }
  m <- grain_matrix(offset, plot, t, temp, grain = "month")
  n <- attr(m, "bin_n")[1, ]
  expect_lt(n[1], n[2])
  dropped <- grain_matrix(offset, plot, t, temp, grain = "month", partial = "drop")
  expect_equal(dim(dropped)[2], dim(m)[2] - sum(attr(m, "bin_partial")))
  expect_false(any(attr(dropped, "bin_partial")))
  expect_equal(as.numeric(dropped), as.numeric(m[, !attr(m, "bin_partial"), , drop = FALSE]))
})

test_that("a caller-supplied calendar owns its own bin lengths", {
  d <- hourly_series(units = "a", from = "2021-09-01 00:00:00", hours = 24 * 200)
  # Cut at the equinox rather than on the first of a month, with the first edge at the record's
  # own start: the leading bin is three weeks against a season's three months, and that is the
  # calendar the caller asked for rather than a bin the record failed to fill.
  edges <- as.POSIXct(c("2021-09-01", "2021-09-22", "2021-12-21", "2022-03-20"), tz = "UTC")
  astronomical <- function(when) edges[findInterval(as.numeric(when), as.numeric(edges))]
  x <- grain_matrix(d, plot, t, temp, grain = astronomical)
  expect_equal(dim(x)[2], 3L)
  expect_false(any(attr(x, "bin_partial")))
  expect_lt(attr(x, "bin_n")[1, 1], attr(x, "bin_n")[1, 2])

  # The same record on the named grain counts three calendar months from the anniversary, so it
  # is a different rule and a different number of bins, not a different implementation of one.
  expect_equal(dim(grain_matrix(d, plot, t, temp, grain = "season"))[2], 3L)
  expect_equal(format(attr(grain_matrix(d, plot, t, temp, grain = "season"), "bin_start"),
                      "%Y-%m-%d", tz = "UTC"),
               c("2021-09-01", "2021-12-01", "2022-03-01"))
})

test_that("a column can be named bare or as a string", {
  d <- hourly_series(units = "a", hours = 48)
  bare <- grain_matrix(d, plot, t, temp, grain = "day")
  quoted <- grain_matrix(d, "plot", "t", "temp", grain = "day")
  expect_equal(as.numeric(bare), as.numeric(quoted))
})

test_that("the representation refuses input it cannot reduce honestly", {
  d <- hourly_series(units = "a", hours = 48)

  gap <- d[-c(1:24), ]
  gap <- rbind(gap, transform(d[1:24, ], plot = "b"))
  expect_error(grain_matrix(gap, plot, t, temp, grain = "day"), "no readings")

  dup <- rbind(d, d[1, ])
  expect_error(grain_matrix(dup, plot, t, temp, grain = "day"), "duplicated")

  na <- d
  na$temp[3] <- NA
  expect_error(grain_matrix(na, plot, t, temp, grain = "day"), "not a finite number")

  na <- d
  na$plot[3] <- NA
  expect_error(grain_matrix(na, plot, t, temp, grain = "day"), "missing values")

  expect_error(grain_matrix(d, plot, t, temp, grain = "native", stats = "cold_day"),
               "not defined")
  expect_error(grain_matrix(d, plot, t, temp, grain = "day", stats = c("mean", "mean")),
               "twice")
  expect_error(grain_matrix(d, plot, t, temp, grain = "day", stats = "median"),
               "unknown statistic")
  expect_error(grain_matrix(d, plot, t, temp, grain = "day", year_start = "9-1"),
               "MM-DD")
  expect_error(grain_matrix(d, plot, nope, temp, grain = "day"), "not in the data")
  expect_error(grain_matrix(transform(d, t = as.character(t)), plot, t, temp),
               "must be POSIXct")
})

test_that("channel order follows the order asked for", {
  d <- hourly_series(units = "a", hours = 24 * 14)
  x <- grain_matrix(d, plot, t, temp, grain = "week",
                     stats = c("warm_day", "mean", "cold_day"))
  expect_equal(dimnames(x)[[3]], c("warm_day", "mean", "cold_day"))
  expect_true(all(x[, , "warm_day"] >= x[, , "cold_day"]))
})

test_that("units and bins come back sorted, whatever order they arrived in", {
  d <- hourly_series(units = c("b", "a"), hours = 24 * 5)
  x <- grain_matrix(d[sample(nrow(d)), ], plot, t, temp, grain = "day")
  expect_equal(dimnames(x)[[1]], c("a", "b"))
  expect_false(is.unsorted(attr(x, "bin_start")))
})

test_that("the average daily extremes are an average over days, not over readings", {
  # Day 1 swings between -10 and +10, day 2 sits at 0. The week's coldest reading is -10; its
  # average daily minimum is the mean of -10 and 0.
  t <- seq(as.POSIXct("2021-09-06", tz = "UTC"), by = "hour", length.out = 48)
  temp <- c(rep(c(-10, 10), each = 12), rep(0, 24))
  d <- data.frame(plot = "a", t = t, temp = temp)
  x <- grain_matrix(d, plot, t, temp, grain = "week",
                     stats = c("min", "mean_daily_min", "mean", "mean_daily_max", "max"))
  expect_equal(as.numeric(x[1, 1, "min"]), -10)
  expect_equal(as.numeric(x[1, 1, "mean_daily_min"]), -5)
  expect_equal(as.numeric(x[1, 1, "mean"]), 0)
  expect_equal(as.numeric(x[1, 1, "mean_daily_max"]), 5)
  expect_equal(as.numeric(x[1, 1, "max"]), 10)
})

test_that("the average daily extremes match an independent reduction", {
  d <- hourly_series()
  day <- format(d$t, "%Y-%m-%d", tz = "UTC")
  month <- format(d$t, "%Y-%m", tz = "UTC")
  daily_min <- tapply(d$temp, list(d$plot, day), min)
  key <- substr(colnames(daily_min), 1L, 7L)
  ref <- t(apply(daily_min, 1L, function(v) tapply(v, key, mean)))
  x <- grain_matrix(d, plot, t, temp, grain = "month", stats = "mean_daily_min")
  expect_equal(as.numeric(x[, , "mean_daily_min"]), as.numeric(ref[order(rownames(ref)), ]))
})

test_that("at the daily grain an average daily extreme is that day's extreme", {
  d <- hourly_series(units = "a", hours = 24 * 30)
  x <- grain_matrix(d, plot, t, temp,
                     grain = "day", stats = c("min", "mean_daily_min", "mean_daily_max", "max"))
  expect_equal(as.numeric(x[, , "mean_daily_min"]), as.numeric(x[, , "min"]))
  expect_equal(as.numeric(x[, , "mean_daily_max"]), as.numeric(x[, , "max"]))
})

test_that("the three-channel schemes come back ordered", {
  d <- hourly_series(units = c("a", "b"), hours = 24 * 60)
  for (scheme in list(c("min", "mean", "max"),
                      c("mean_daily_min", "mean", "mean_daily_max"),
                      c("cold_day", "mean", "warm_day"))) {
    x <- grain_matrix(d, plot, t, temp, grain = "week", stats = scheme)
    expect_true(all(x[, , 1] <= x[, , 2]), info = scheme[1])
    expect_true(all(x[, , 2] <= x[, , 3]), info = scheme[3])
  }
})

test_that("the grain's own extremes bound the day-level ones", {
  d <- hourly_series(units = "a", hours = 24 * 90)
  x <- grain_matrix(d, plot, t, temp, grain = "month",
                     stats = c("min", "mean_daily_min", "cold_day",
                               "warm_day", "mean_daily_max", "max"))
  expect_true(all(x[, , "min"] <= x[, , "mean_daily_min"]))
  expect_true(all(x[, , "min"] <= x[, , "cold_day"]))
  expect_true(all(x[, , "warm_day"] <= x[, , "max"]))
  expect_true(all(x[, , "mean_daily_max"] <= x[, , "max"]))
})

test_that("the mean of the daily minima is not the coldest day", {
  # A bin of one day at 0 and one at 10 has a coldest day of 0 and an average daily minimum of 5,
  # so the two day-level pairs are not ordered against each other.
  t <- seq(as.POSIXct("2021-09-06", tz = "UTC"), by = "hour", length.out = 48)
  d <- data.frame(plot = "a", t = t, temp = rep(c(0, 10), each = 24))
  x <- grain_matrix(d, plot, t, temp, grain = "week",
                     stats = c("cold_day", "mean_daily_min", "mean_daily_max", "warm_day"))
  expect_equal(as.numeric(x[1, 1, "cold_day"]), 0)
  expect_equal(as.numeric(x[1, 1, "mean_daily_min"]), 5)
  expect_equal(as.numeric(x[1, 1, "mean_daily_max"]), 5)
  expect_equal(as.numeric(x[1, 1, "warm_day"]), 10)
})

test_that("naming several grains returns one representation per grain", {
  d <- hourly_series(units = c("a", "b"), hours = 24 * 60)
  s <- grain_matrix(d, plot, t, temp, grain = c("day", "week", "month"))
  expect_s3_class(s, "timesift_set")
  expect_named(s, c("day", "week", "month"))
  expect_equal(as.numeric(s$week),
               as.numeric(grain_matrix(d, plot, t, temp, grain = "week")))
  expect_s3_class(s["week"], "timesift_set")
  expect_error(grain_matrix(d, plot, t, temp, grain = c("day", "day")), "twice")
  expect_error(grain_matrix(d, plot, t, temp, grain = c("day", "fortnight")), "unknown grain")
})

test_that("a calendar the package does not carry can be passed as a function", {
  d <- hourly_series(units = "a", hours = 24 * 120)
  # Astronomical seasons: the boundaries fall on the equinoxes and solstices, not on the first of
  # a month, so they are not any three calendar months.
  astronomical <- function(when) {
    edges <- as.POSIXct(c("2021-06-23", "2021-09-23", "2021-12-21", "2022-03-20", "2022-06-23"),
                        tz = "UTC")
    edges[findInterval(as.numeric(when), as.numeric(edges))]
  }
  x <- grain_matrix(d, plot, t, temp, grain = astronomical)
  expect_equal(attr(x, "grain"), "custom")
  expect_equal(format(attr(x, "bin_start"), "%Y-%m-%d", tz = "UTC"),
               c("2021-06-23", "2021-09-23", "2021-12-21"))
  expect_equal(sum(attr(x, "bin_n")), 24 * 120)
  expect_error(grain_matrix(d, plot, t, temp, grain = function(when) rep(1, length(when))),
               "POSIXct bin start")
})

test_that("the bin end is the last reading the bin holds", {
  d <- hourly_series(units = "a", hours = 24 * 40)
  x <- grain_matrix(d, plot, t, temp, grain = "week")
  s <- attr(x, "bin_start")
  e <- attr(x, "bin_end")
  expect_true(all(e >= s))
  expect_equal(max(e), max(d$t))
  expect_equal(as.numeric(difftime(e[2], s[2], units = "hours")), 24 * 7 - 1)
})

test_that("the calendar channels are the position of a bin in the year", {
  d <- hourly_series(units = c("a", "b"), hours = 24 * 400)
  x <- grain_matrix(d, plot, t, temp, grain = "month")
  cc <- calendar_channels(x)
  expect_equal(dimnames(cc)[[3]], c("year_sin", "year_cos"))
  expect_equal(cc[1, , ], cc[2, , ])                       # the calendar is not a unit's property
  expect_equal(as.numeric(cc[1, , 1]^2 + cc[1, , 2]^2), rep(1, dim(x)[2]))
  # a month and the same month a year later sit at the same place in the year
  months <- format(attr(x, "bin_start"), "%m", tz = "UTC")
  repeated <- which(months == months[1])
  expect_equal(cc[1, repeated[1], 1], cc[1, repeated[2], 1], tolerance = 0.02)
})

test_that("the day cycle is the position of a reading in the UTC day", {
  d <- hourly_series(units = c("a", "b"), hours = 24 * 3)
  x <- grain_matrix(d, plot, t, temp, grain = "native")
  cc <- calendar_channels(x, cycles = c("year", "day"))
  expect_equal(dimnames(cc)[[3]], c("year_sin", "year_cos", "day_sin", "day_cos"))
  expect_equal(cc[, , 1:2], calendar_channels(x)[, , 1:2])
  hour <- as.numeric(format(attr(x, "bin_start"), "%H", tz = "UTC"))
  expect_equal(as.numeric(cc[1, , "day_sin"]), sin(2 * pi * hour / 24))
  expect_equal(as.numeric(cc[1, , "day_cos"]), cos(2 * pi * hour / 24))
  expect_equal(dimnames(calendar_channels(x, cycles = c("day", "year")))[[3]],
               c("day_sin", "day_cos", "year_sin", "year_cos"))
  daily <- grain_matrix(d, plot, t, temp, grain = "day")
  expect_error(calendar_channels(daily, cycles = "day"), "a day or more apart")
  expect_error(calendar_channels(x, cycles = "month"), "unknown cycle: month")
  expect_error(calendar_channels(x, cycles = c("day", "day")), "each cycle once")
})

test_that("channels are joined in the order they are given", {
  d <- hourly_series(units = c("a", "b"), hours = 24 * 60)
  x <- grain_matrix(d, plot, t, temp, grain = "week", stats = c("cold_day", "mean"))
  b <- bind_channels(x, calendar_channels(x))
  expect_equal(dimnames(b)[[3]], c("cold_day", "mean", "year_sin", "year_cos"))
  expect_equal(as.numeric(b[, , "mean"]), as.numeric(x[, , "mean"]))
  expect_equal(attr(b, "bin_start"), attr(x, "bin_start"))
  expect_null(attr(x, "position"))
  expect_equal(attr(calendar_channels(x), "position"), c("year_sin", "year_cos"))
  expect_equal(attr(b, "position"), c("year_sin", "year_cos"))
  expect_equal(attr(bind_channels(calendar_channels(x), x), "position"),
               c("year_sin", "year_cos"))
  elevation <- matrix(c(2000, 2500), ncol = 1L, dimnames = list(c("a", "b"), "elevation"))
  s <- bind_channels(.append_static(x, elevation), calendar_channels(x))
  expect_equal(attr(s, "static"), "elevation")
  expect_equal(attr(s, "position"), c("year_sin", "year_cos"))
  expect_error(bind_channels(x, x), "same name")
  other <- grain_matrix(d, plot, t, temp, grain = "month")
  expect_error(bind_channels(x, other), "different units or bins")
})

test_that("coverage lays a refused record's gaps out as zeros, unit by unit and bin by bin", {
  d <- hourly_series(hours = 24 * 40)
  # Unit b loses the calendar week beginning Monday 6 September; the record still spans it.
  lost <- d$plot == "b" & d$t >= as.POSIXct("2021-09-06", tz = "UTC") &
    d$t < as.POSIXct("2021-09-13", tz = "UTC")
  cov <- coverage(d[!lost, ], plot, t, grain = "week")
  expect_s3_class(cov, "timesift_coverage")
  expect_equal(dim(cov), c(2L, 6L))
  expect_equal(unname(unclass(cov)["b", ]), c(120L, 0L, 168L, 168L, 168L, 168L))
  expect_equal(unname(unclass(cov)["a", ]), c(120L, 168L, 168L, 168L, 168L, 168L))
  expect_equal(attr(cov, "grain"), "week")
  expect_output(print(cov), "1 empty (unit, bin) cell in 1 unit: b", fixed = TRUE)
  expect_error(grain_matrix(d[!lost, ], plot, t, temp, grain = "week"), "coverage() lists", fixed = TRUE)

  # A week no unit reaches is not a bin the reduction would build, so coverage builds it: a
  # column of zeros where the calendar says the week was.
  skipped <- d$t >= as.POSIXct("2021-09-13", tz = "UTC") & d$t < as.POSIXct("2021-09-20", tz = "UTC")
  cov <- coverage(d[!skipped, ], plot, t, grain = "week")
  expect_equal(dim(cov), c(2L, 6L))
  expect_equal(unname(colSums(unclass(cov)) == 0L), c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(colnames(cov)[3L], "2021-09-13T00:00:00Z")
  expect_output(print(cov), "1 bin no unit reaches: 2021-09-13T00:00:00Z")
  expect_error(grain_matrix(d[!skipped, ], plot, t, temp, grain = "week"), "not contiguous")

  # A full record is full, and agrees with what the reduction counted.
  full <- coverage(d, plot, t, grain = "week")
  expect_output(print(full), "every unit reaches every bin")
  expect_equal(unclass(full), attr(grain_matrix(d, plot, t, temp, grain = "week"), "bin_n"),
               ignore_attr = TRUE)
  expect_equal(dim(coverage(d, plot, t, grain = "native")), c(2L, 24L * 40L))
  expect_error(coverage(d, plot, t, grain = c("day", "week")), "one grain at a time")
})

test_that("a numeric identifier is written by its digits", {
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 3)
  named <- c(100000, 9, 10)
  d <- data.frame(id = rep(named, each = length(t)), t = rep(t, 3),
                  v = rep(0, 3 * length(t)))
  # The same three names Python writes: not 1e+05, and not 100000.0 either.
  expect_identical(dimnames(grain_matrix(d, id, t, v, grain = "day"))[[1L]],
                   c("10", "100000", "9"))
  whole <- transform(d, id = as.integer(d$id))
  expect_identical(dimnames(grain_matrix(whole, id, t, v, grain = "day"))[[1L]],
                   c("10", "100000", "9"))

  expect_error(grain_matrix(transform(d, id = d$id + 0.5), id, t, v, grain = "day"),
               "not a whole number")
  expect_error(grain_matrix(transform(d, id = d$t), id, t, v, grain = "day"),
               "must identify a unit by text")
})

test_that("a zone the database does not know is an error, not a warning and a UTC calendar", {
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 3)
  d <- data.frame(id = "a", t = t, v = seq_along(t), stringsAsFactors = FALSE)
  attr(d$t, "tzone") <- "Europe/Vienn"
  expect_error(grain_matrix(d, id, t, v, grain = "day"), "\"Europe/Vienn\", which the zone database")
  expect_error(coverage(d, id, t, grain = "day"), "which the zone database")
  at <- data.frame(id = "a", at = t[48L], stringsAsFactors = FALSE)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "1 day"), "which the zone database")
  attr(d$t, "tzone") <- "Europe/Vienna"
  expect_silent(grain_matrix(d, id, t, v, grain = "day"))
})

test_that("a supplied calendar that gives no bin start for a reading is refused, and names it", {
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 3)
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
                  v = seq_len(2 * length(t)), stringsAsFactors = FALSE)
  days <- function(when) .POSIXct(floor(as.numeric(when) / 86400) * 86400, tz = "UTC")
  holed <- function(when) {
    out <- days(when)
    out[as.numeric(when) == as.numeric(t[30L])] <- NA
    out
  }
  expect_error(grain_matrix(d, id, t, v, grain = holed),
               "gives no bin start for 2 readings, first: unit a at 2021-09-02T05:00:00Z")
  expect_error(coverage(d, id, t, grain = holed), "gives no bin start for 2 readings")
  expect_identical(dim(grain_matrix(d, id, t, v, grain = days))[2L], 3L)
})

test_that("a duplicated reading is named by its instant in UTC, and the noun agrees", {
  t <- as.POSIXct(c(0, 0, 3600, 7200), origin = "1970-01-01", tz = "Europe/Vienna")
  d <- data.frame(id = "a", t = t, v = 1:4, stringsAsFactors = FALSE)
  expect_error(grain_matrix(d, id, t, v, grain = "native"),
               "1 duplicated (unit, time) pair, first: a at 1970-01-01T00:00:00Z.", fixed = TRUE)
  d <- rbind(d, d[3L, ])
  expect_error(grain_matrix(d, id, t, v, grain = "native"),
               "2 duplicated (unit, time) pairs, first: a at 1970-01-01T00:00:00Z.", fixed = TRUE)
})
