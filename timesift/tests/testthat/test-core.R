test_that("the core reproduces the pure-R oracle on every grain and statistic", {
  set.seed(20260903)
  starts <- c("2019-09-01", "2020-02-17 05:00:00", "2021-06-11 13:00:00")
  schemes <- list(c("min", "mean", "max"),
                  c("mean_daily_min", "mean", "mean_daily_max"),
                  c("cold_day", "mean", "warm_day"))

  for (start in starts) {
    t <- seq(as.POSIXct(start, tz = "UTC"), by = "hour", length.out = 24 * 400)
    d <- data.frame(id = rep(c("p1", "p2", "p3"), each = length(t)),
                    t = rep(t, 3),
                    v = rnorm(3 * length(t), sd = 5))
    for (w in c("native", "halfday", "day", "week", "month", "season", "year")) {
      for (scheme in schemes) {
        if (w %in% c("native", "halfday") &&
              any(scheme %in% c("cold_day", "warm_day", "mean_daily_min", "mean_daily_max"))) {
          next
        }
        x <- grain_matrix(d, id, t, v, grain = w, stats = scheme)
        o <- oracle_grain_matrix(d, "id", "t", "v", grain = w, stats = scheme)
        expect_identical(as.vector(unclass(x)), as.vector(o$values),
                         info = paste(start, w, paste(scheme, collapse = "+")))
        expect_identical(dimnames(x), dimnames(o$values))
        expect_identical(as.vector(attr(x, "bin_n")), as.vector(o$bin_n))
        expect_identical(attr(x, "bin_partial"), o$bin_partial)
        expect_equal(as.numeric(attr(x, "bin_start")), as.numeric(o$bin_start))
        expect_equal(as.numeric(attr(x, "bin_end")), as.numeric(o$bin_end))
      }
    }
  }
})

test_that("the seven statistics keep the two orderings the definitions imply, in the core and the oracle", {
  set.seed(20260908)
  t <- seq(as.POSIXct("2020-02-17 05:00:00", tz = "UTC"), by = "hour", length.out = 24 * 400)
  d <- data.frame(id = rep(c("p1", "p2", "p3"), each = length(t)), t = rep(t, 3),
                  v = rnorm(3 * length(t), sd = 5))
  stats <- c("min", "mean_daily_min", "cold_day", "mean", "warm_day", "mean_daily_max", "max")
  ordered <- function(m, chain) {
    for (i in seq_len(length(chain) - 1L)) {
      expect_true(all(m[, , chain[i]] <= m[, , chain[i + 1L]] + 1e-9),
                  info = paste(attr(m, "grain"), chain[i], "<=", chain[i + 1L]))
    }
  }
  for (w in c("day", "week", "month", "season", "year")) {
    x <- grain_matrix(d, id, t, v, grain = w, stats = stats)
    o <- oracle_grain_matrix(d, "id", "t", "v", grain = w, stats = stats)$values
    attr(o, "grain") <- w
    for (m in list(x, o)) {
      ordered(m, c("min", "mean_daily_min", "mean", "mean_daily_max", "max"))
      ordered(m, c("min", "cold_day", "mean", "warm_day", "max"))
    }
  }
})

test_that("the core reproduces the oracle at anniversaries other than the default", {
  set.seed(11)
  t <- seq(as.POSIXct("2019-01-01", tz = "UTC"), by = "hour", length.out = 24 * 500)
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t)))
  for (ys in c("01-01", "03-01", "09-01", "12-28")) {
    for (w in c("season", "year")) {
      x <- grain_matrix(d, id, t, v, grain = w, stats = c("cold_day", "mean", "warm_day"),
                         year_start = ys)
      o <- oracle_grain_matrix(d, "id", "t", "v", grain = w,
                                stats = c("cold_day", "mean", "warm_day"), year_start = ys)
      expect_identical(as.vector(unclass(x)), as.vector(o$values), info = paste(w, ys))
      expect_identical(dimnames(x)[[2]], dimnames(o$values)[[2]])
    }
  }
})

test_that("the core reproduces the oracle under a supplied calendar", {
  set.seed(12)
  t <- seq(as.POSIXct("2019-09-01", tz = "UTC"), by = "hour", length.out = 24 * 400)
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t)))
  ten_days <- function(when) {
    .POSIXct(floor(as.numeric(when) / (10 * 86400)) * 10 * 86400, tz = "UTC")
  }
  x <- grain_matrix(d, id, t, v, grain = ten_days, stats = c("cold_day", "mean", "warm_day"))
  o <- oracle_grain_matrix(d, "id", "t", "v", grain = ten_days,
                            stats = c("cold_day", "mean", "warm_day"))
  expect_identical(as.vector(unclass(x)), as.vector(o$values))
  expect_identical(attr(x, "bin_partial"), o$bin_partial)
})

test_that("the core's calendar agrees with the oracle's, instant by instant", {
  t <- seq(as.POSIXct("2018-01-01", tz = "UTC"), by = "97 min", length.out = 20000)
  ys <- list(month = 9L, day = 1L)
  for (w in c("native", "halfday", "day", "week", "month", "season", "year")) {
    expect_equal(ts_bin_starts_(as.numeric(t), w, 9L, 1L),
                 as.numeric(oracle_bin_start(t, w, ys)), info = w)
  }
  bins <- unique(oracle_bin_start(t, "month", ys))
  expect_equal(ts_bin_nexts_(as.numeric(bins), "month", 9L, 1L),
               as.numeric(oracle_bin_next(bins, "month", ys, max(as.numeric(t)))))
})

test_that("a gap the whole record shares is an error, not four adjacent bins", {
  set.seed(3)
  t <- seq(as.POSIXct("2021-12-01", tz = "UTC"), by = "hour", length.out = 24 * 150)
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t)))

  gap <- d[format(d$t, "%Y-%m") != "2022-02", ]
  expect_error(grain_matrix(gap, id, t, v, grain = "month", stats = "mean"),
               "month bins are not contiguous")
  expect_error(grain_matrix(gap, id, t, v, grain = "day", stats = "mean"),
               "day bins are not contiguous")

  # The record's own ends are not a gap: a partial bin at either end is reported, not rejected.
  expect_silent(grain_matrix(d, id, t, v, grain = "month", stats = "mean"))

  # At the `native` grain the bin is the reading itself, so the calendar cannot say what a bin
  # between two others would have been and none is asserted.
  sparse <- d[as.integer(format(d$t, "%H")) %% 3L == 0L, ]
  expect_silent(grain_matrix(sparse, id, t, v, grain = "native", stats = "mean"))
})

test_that("a day-level statistic needs bins of a day or coarser, whoever supplied them", {
  set.seed(4)
  t <- seq(as.POSIXct("2021-12-01", tz = "UTC"), by = "hour", length.out = 24 * 60)
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t)))
  six <- function(when) .POSIXct(floor(as.numeric(when) / 21600) * 21600, tz = "UTC")

  expect_error(grain_matrix(d, id, t, v, grain = six, stats = c("cold_day", "warm_day")),
               "need bins of a calendar day or coarser")
  expect_error(grain_matrix(d, id, t, v, grain = six, stats = "mean_daily_min"),
               "mean_daily_min needs bins of a calendar day or coarser")
  expect_silent(grain_matrix(d, id, t, v, grain = six, stats = c("min", "mean", "max")))

  # A calendar that cuts on the day boundary is fine however unusual its bin lengths are.
  ten <- function(when) .POSIXct(floor(as.numeric(when) / (10 * 86400)) * 10 * 86400, tz = "UTC")
  expect_silent(grain_matrix(d, id, t, v, grain = ten, stats = c("cold_day", "warm_day")))
})

test_that("a zone whose local midnight does not exist bins without an NA", {
  set.seed(5)
  t <- seq(as.POSIXct("2018-11-01 12:00:00", tz = "America/Sao_Paulo"), by = "hour",
           length.out = 24 * 8)
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t)))

  for (w in c("native", "halfday", "day", "week", "month")) {
    x <- grain_matrix(d, id, t, v, grain = w, stats = "mean")
    expect_false(anyNA(x), info = w)
    expect_false(anyNA(attr(x, "bin_start")), info = w)
  }

  # 4 November 2018 is 23 hours long in Sao Paulo, and the day it opens is the instant the clock
  # jumped to rather than a midnight that never happened.
  x <- grain_matrix(d, id, t, v, grain = "day", stats = "mean")
  n <- attr(x, "bin_n")["a", ]
  expect_identical(unname(n[format(attr(x, "bin_start"), "%Y-%m-%d") == "2018-11-04"]), 23L)
  expect_identical(format(attr(x, "bin_start"), "%H:%M:%S"),
                   c("00:00:00", "00:00:00", "00:00:00", "01:00:00", "00:00:00",
                     "00:00:00", "00:00:00", "00:00:00", "00:00:00"))

  # A year boundary landing on that date is an argument, not an error.
  expect_silent(grain_matrix(d, id, t, v, grain = "year", year_start = "11-04", stats = "mean"))
})

test_that("a series carried in a zone bins by that zone's calendar", {
  set.seed(6)
  t <- seq(as.POSIXct("2021-12-20", tz = "UTC"), by = "hour", length.out = 24 * 40)
  v <- rnorm(2 * length(t))
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2), v = v)
  utc <- grain_matrix(d, id, t, v, grain = "day", stats = c("min", "mean", "max"))

  attr(t, "tzone") <- "Europe/Vienna"
  d$t <- rep(t, 2)
  vienna <- grain_matrix(d, id, t, v, grain = "day", stats = c("min", "mean", "max"))

  expect_identical(dim(utc)[2L], 40L)
  expect_identical(dim(vienna)[2L], 41L)
  expect_identical(dimnames(vienna)[[2]][1], "2021-12-19T23:00:00Z")

  # The same instants, relabelled into their Vienna clock and binned as if that clock were UTC,
  # give the same numbers: the zone is the whole of the difference.
  relabelled <- data.frame(
    id = d$id,
    t = as.POSIXct(format(d$t, "%Y-%m-%d %H:%M:%S", tz = "Europe/Vienna"), tz = "UTC"),
    v = d$v)
  naive <- grain_matrix(relabelled, id, t, v, grain = "day", stats = c("min", "mean", "max"))
  expect_identical(as.vector(unclass(vienna)), as.vector(unclass(naive)))
  expect_identical(as.vector(attr(vienna, "bin_n")), as.vector(attr(naive, "bin_n")))
})

test_that("readings a fraction of a second apart are the same reading twice", {
  t <- as.POSIXct(c(0, 0.25, 1, 2), origin = "1970-01-01", tz = "UTC")
  d <- data.frame(id = "a", t = t, v = c(1, 2, 3, 4))
  expect_error(grain_matrix(d, id, t, v, grain = "native", stats = "mean"),
               "duplicated \\(unit, time\\) pair")
})

test_that("a reading that is not a finite number is refused, and named", {
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 8)
  d <- data.frame(id = rep(c("p1", "p2"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t)))
  at <- data.frame(id = c("p1", "p2"), at = rep(max(t), 2))

  for (hole in list(NA_real_, NaN, Inf, -Inf)) {
    bad <- d
    bad$v[30L] <- hole
    expect_error(grain_matrix(bad, id, t, v, grain = "day"),
                 "1 reading is not a finite number, first: unit p1 at 2021-09-02T05:00:00")
    expect_error(lookback_matrix(bad, id, t, v, at = at, span = "2 days"),
                 "1 reading is not a finite number, first: unit p1 at 2021-09-02T05:00:00")
  }

  two <- d
  two$v[c(30L, 31L)] <- Inf
  expect_error(grain_matrix(two, id, t, v, grain = "day"), "2 readings are not a finite number")

  # coverage() reads how many readings a unit has in each bin and never their values, so it is
  # not the guard's business.
  bad <- d
  bad$v[30L] <- NA_real_
  expect_identical(sum(coverage(bad, id, t, grain = "day")), nrow(d))
})

test_that("the two readings of an hour a zone repeats are two `native` bins", {
  set.seed(31)
  # 2021-10-31 in Europe/Vienna: at 03:00 CEST the clock goes back to 02:00 CET, so the readings
  # at 00:00Z and 01:00Z both read 02:00 on that clock.
  t <- seq(as.POSIXct("2021-10-30", tz = "UTC"), by = "hour", length.out = 72)
  d <- data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t)))
  utc <- grain_matrix(d, id, t, v, grain = "native", stats = "mean")
  attr(d$t, "tzone") <- "Europe/Vienna"
  vienna <- grain_matrix(d, id, t, v, grain = "native", stats = "mean")

  # The record unreduced is the record, whichever clock it is read on.
  expect_identical(dim(vienna)[2L], 72L)
  expect_true(all(attr(vienna, "bin_n") == 1L))
  expect_identical(digest_array(vienna), digest_array(utc))
  expect_identical(dimnames(vienna)[[2L]], dimnames(utc)[[2L]])
  expect_identical(as.numeric(attr(vienna, "bin_start")), as.numeric(t))
  expect_identical(attr(vienna, "bin_partial"), attr(utc, "bin_partial"))
  expect_identical(dim(coverage(d, id, t, grain = "native"))[2L], 72L)

  # The day that hour falls in holds 25 readings, and its digest is the oracle's.
  day <- grain_matrix(d, id, t, v, grain = "day", stats = c("min", "mean", "max"))
  expect_identical(unname(attr(day, "bin_n")["a", "2021-10-30T22:00:00Z"]), 25L)
  o <- oracle_grain_matrix(d, "id", "t", "v", grain = "day", stats = c("min", "mean", "max"))
  expect_identical(as.vector(unclass(day)), as.vector(o$values))
})

test_that("a day in a daylight-saving zone is a wall-clock day and a fixed offset's is 24 hours", {
  # Hourly readings across both of Europe/Vienna's 2021 transitions: 28 March, when 02:00 CET
  # jumps to 03:00 CEST, and 31 October, when 03:00 CEST falls back to 02:00 CET.
  # Each record starts at local midnight of a Monday and runs two weeks and one hour.
  record <- function(from, zone) {
    t <- seq(as.POSIXct(from, tz = "UTC"), by = "hour", length.out = 24 * 14 + 1)
    attr(t, "tzone") <- zone
    data.frame(id = rep(c("a", "b"), each = length(t)), t = rep(t, 2), v = 1)
  }
  bins <- function(d, grain) grain_matrix(d, id, t, v, grain = grain, stats = "mean",
                                          partial = "keep")
  n_of <- function(x, local) {
    at <- format(attr(x, "bin_start"), "%Y-%m-%d", tz = attr(attr(x, "bin_start"), "tzone"))
    unname(attr(x, "bin_n")["a", match(local, at)])
  }

  march <- record("2021-03-21 23:00", "Europe/Vienna")
  october <- record("2021-10-24 22:00", "Europe/Vienna")
  expect_identical(n_of(bins(march, "day"), c("2021-03-27", "2021-03-28", "2021-03-29")),
                   c(24L, 23L, 24L))
  expect_identical(n_of(bins(october, "day"), c("2021-10-30", "2021-10-31", "2021-11-01")),
                   c(24L, 25L, 24L))
  expect_identical(n_of(bins(march, "week"), c("2021-03-22", "2021-03-29")), c(167L, 168L))
  expect_identical(n_of(bins(october, "week"), c("2021-10-25", "2021-11-01")), c(169L, 168L))

  # The same instants on the fixed offset of standard time: every whole day is 24 readings and
  # every whole week 168, and a summer day starts at 01:00 on the Vienna clock.
  for (d in list(record("2021-03-21 23:00", "Etc/GMT-1"), record("2021-10-24 22:00", "Etc/GMT-1"))) {
    day <- bins(d, "day")
    week <- bins(d, "week")
    expect_true(all(attr(day, "bin_n")[, !attr(day, "bin_partial")] == 24L))
    expect_true(all(attr(week, "bin_n")[, !attr(week, "bin_partial")] == 168L))
  }
  fixed <- attr(bins(record("2021-03-21 23:00", "Etc/GMT-1"), "day"), "bin_start")
  expect_identical(format(fixed[format(fixed, "%Y-%m-%d", tz = "Etc/GMT-1") == "2021-03-29"],
                          "%H:%M", tz = "Europe/Vienna"), "01:00")
})

test_that("the core reproduces the oracle on a series carried in a zone that moves its clock", {
  set.seed(20260908)
  # Across both of Europe/Vienna's transitions in 2021, at a sampling step that puts two readings
  # in the repeated hour and none on some local hours.
  t <- seq(as.POSIXct("2021-03-20 13:00:00", tz = "UTC"), by = "50 min", length.out = 24 * 260)
  attr(t, "tzone") <- "Europe/Vienna"
  d <- data.frame(id = rep(c("p1", "p2"), each = length(t)), t = rep(t, 2),
                  v = rnorm(2 * length(t), sd = 5))
  schemes <- list(c("min", "mean", "max"),
                  c("mean_daily_min", "mean", "mean_daily_max"),
                  c("cold_day", "mean", "warm_day"))
  for (w in c("native", "halfday", "day", "week", "month", "season", "year")) {
    for (scheme in schemes) {
      if (w %in% c("native", "halfday") && !identical(scheme, schemes[[1L]])) next
      x <- grain_matrix(d, id, t, v, grain = w, stats = scheme)
      o <- oracle_grain_matrix(d, "id", "t", "v", grain = w, stats = scheme)
      label <- paste(w, paste(scheme, collapse = "+"))
      expect_identical(as.vector(unclass(x)), as.vector(o$values), info = label)
      expect_identical(as.vector(attr(x, "bin_n")), as.vector(o$bin_n), info = label)
      expect_identical(attr(x, "bin_partial"), o$bin_partial, info = label)
      expect_equal(as.numeric(attr(x, "bin_end")), as.numeric(o$bin_end), info = label)
      # The oracle's bin starts are on the local clock, and the core's are the instants that
      # clock reads them at.
      clock <- if (w == "native") attr(x, "bin_start") else
        oracle_local_clock(attr(x, "bin_start"), "Europe/Vienna")
      expect_equal(as.numeric(clock), as.numeric(o$bin_start), info = label)
    }
  }
})

test_that("a lookback measures the local clock, so a day across a clock change is 25 or 23 hours", {
  set.seed(32)
  t <- seq(as.POSIXct("2021-03-25", tz = "UTC"), by = "hour", length.out = 24 * 230)
  attr(t, "tzone") <- "Europe/Vienna"
  d <- data.frame(id = "p1", t = t, v = rnorm(length(t)), stringsAsFactors = FALSE)
  at <- data.frame(id = c("p1", "p1", "p1"),
                   at = as.POSIXct(c("2021-03-29", "2021-11-01", "2021-06-01"),
                                   tz = "Europe/Vienna"),
                   stringsAsFactors = FALSE)
  x <- lookback_matrix(d, id, t, v, at = at, span = "1 day", stats = c("mean", "cold_day"))
  expect_identical(as.vector(attr(x, "bin_n")), c(23L, 25L, 24L))

  # The whole local day before each anchor, which is what a day-level statistic reads.
  for (i in seq_len(nrow(at))) {
    day <- d$v[d$t >= at$at[i] - 3600 * attr(x, "bin_n")[i] & d$t < at$at[i]]
    expect_equal(x[i, 1L, "mean"], mean(day))
    expect_equal(x[i, 1L, "cold_day"], mean(day))
  }
})

test_that("a negative lag is refused by the core", {
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 20)
  d <- data.frame(id = "p1", t = t, v = seq_along(t), stringsAsFactors = FALSE)
  at <- data.frame(id = "p1", at = as.POSIXct("2021-09-15", tz = "UTC"), stringsAsFactors = FALSE)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "2 days", lag = -3600),
               "lag cannot be negative")
})

test_that("a record too long for the day table is refused by the core", {
  # The day-level stage of a lookback tables every calendar day the record spans, and stops at
  # 2^24 of them: a record with a reading 45,000 years after its first.
  far <- .POSIXct(c(0, (2^24 + 1) * 86400), tz = "UTC")
  d <- data.frame(id = "p1", t = far, v = c(1, 2), stringsAsFactors = FALSE)
  at <- data.frame(id = "p1", at = far[2L] + 86400, stringsAsFactors = FALSE)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "1 day", stats = "cold_day"),
               "too many days")
  expect_silent(lookback_matrix(d, id, t, v, at = at, span = "1 day", stats = "max"))
})

test_that("a unit index outside the units is refused by the core, which no wrapper can send", {
  expect_error(timesift:::ts_reduce_(2L, 1, 0, 0, NULL, "a", "day", 9L, 1L, "mean", 0),
               "unit index outside the units")
  expect_error(timesift:::ts_reduce_lookbacks_(2L, 1, 0, 0, "a", 1L, 86400, "1", 86400, 0, 1L,
                                               "mean"),
               "unit index outside the units")
  expect_error(timesift:::ts_reduce_lookbacks_(1L, 1, 0, 0, "a", 2L, 86400, "1", 86400, 0, 1L,
                                               "mean"),
               "target carries a unit index outside the units")
})
