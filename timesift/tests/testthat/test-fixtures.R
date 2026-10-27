fixture_series <- function(dir, name) {
  file <- switch(name, aligned = "series.csv", offset = "series_offset.csv",
                 zoned = "series_zoned.csv", order = "series_order.csv")
  s <- read.csv(file.path(dir, file), stringsAsFactors = FALSE)
  s$time <- as.POSIXct(s$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  s
}

fixture_binning <- function(dir, name, grain) {
  if (grain != "astronomical") {
    return(grain)
  }
  edges <- read.csv(file.path(dir, "seasons.csv"), stringsAsFactors = FALSE)
  edges <- as.POSIXct(edges$edge[edges$series == name], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  function(when) edges[findInterval(as.numeric(when), as.numeric(edges))]
}

# The readings a coverage case takes out, named in the fixture so that both suites build the same
# record rather than each writing one that happens to have a hole in it.
coverage_input <- function(dir, row) {
  series <- fixture_series(dir, row$series)
  if (!nzchar(row$unit)) {
    return(series)
  }
  from <- as.POSIXct(row$from, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  to <- as.POSIXct(row$to, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  lost <- series$time >= from & series$time < to & (row$unit == "all" | series$id == row$unit)
  series[!lost, , drop = FALSE]
}

# The three calendars the guard fixture names, built from the name so that both suites build the
# same function rather than each writing one that happens to break the same rule.
fixture_calendar <- function(name) {
  switch(name,
    late = function(when) {
      as.POSIXct((floor(as.numeric(when) / 86400) + 1) * 86400, origin = "1970-01-01", tz = "UTC")
    },
    alternate = function(when) {
      t0 <- min(as.numeric(when))
      as.POSIXct(t0 + 3600 * (((as.numeric(when) - t0) / 3600) %% 2), origin = "1970-01-01",
                 tz = "UTC")
    },
    missing = function(when) {
      out <- as.POSIXct(floor(as.numeric(when) / 86400) * 86400, origin = "1970-01-01", tz = "UTC")
      out[as.numeric(when) == min(as.numeric(when)) + 86400] <- NA
      out
    },
    stop("no calendar called ", name))
}

test_that("every representation matches the digest the Python side reads", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  expected <- read.csv(file.path(dir, "digests.csv"), stringsAsFactors = FALSE)
  series <- lapply(stats::setNames(nm = unique(expected$series)), fixture_series, dir = dir)

  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    label <- paste(row$series, row$grain, row$tz, row$year_start, row$partial, row$stat)
    # The instants are the same bytes on disk whichever calendar reads them; the zone is the clock
    # laid over them, and a zone row asserts that both languages read that clock the same way.
    record <- series[[row$series]]
    attr(record$time, "tzone") <- row$tz
    x <- grain_matrix(record, id, time, value,
                       grain = fixture_binning(dir, row$series, row$grain),
                       stats = strsplit(row$stat, "+", fixed = TRUE)[[1L]],
                       year_start = row$year_start, partial = row$partial)
    start <- attr(x, "bin_start")
    # The shape is asserted before the digest, so a binning that puts the record into a different
    # number of bins is reported as that rather than as an unexplained hash mismatch.
    expect_equal(dim(x)[1], row$n_unit, info = label)
    expect_equal(dim(x)[2], row$n_bin, info = label)
    expect_equal(format(start[1], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), row$first_bin, info = label)
    expect_equal(format(start[length(start)], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 row$last_bin, info = label)
    expect_equal(sum(attr(x, "bin_partial")), row$n_partial, info = label)
    # The row order is asserted by name before the digest, so a session whose collation orders the
    # ids differently is reported as that rather than as an unexplained hash mismatch.
    expect_identical(dimnames(x)[[1L]][1L], row$first_unit, info = label)
    expect_identical(dimnames(x)[[1L]][dim(x)[1]], row$last_unit, info = label)
    expect_identical(digest_array(x), row$digest, info = label)
  }
})

test_that("the fixtures cover a record that starts on no bin boundary", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  expected <- read.csv(file.path(dir, "digests.csv"), stringsAsFactors = FALSE)

  # A record beginning at midnight on the year_start anniversary puts every grain in phase with
  # it, which is the one input on which a rule that keeps a partial leading bin and a rule that
  # never makes one agree. The contract is only a contract if it also carries the other case.
  expect_true(all(c("aligned", "offset", "zoned", "order") %in% expected$series))
  offset <- expected[expected$series == "offset", ]
  expect_true(all(c("native", "halfday", "day", "week", "month", "season", "year", "astronomical")
                  %in% offset$grain))
  expect_gt(sum(offset$n_partial), 0)
  expect_true(all(c("keep", "drop") %in% expected$partial))
  expect_gt(length(unique(expected$year_start)), 1L)

  # A contract checked only in UTC verifies the calendar on the one zone where the question does
  # not arise. Both a zone that moves its clock in the middle of the day and one that moves it at
  # midnight are pinned.
  expect_true(all(c("UTC", "Europe/Vienna", "America/Sao_Paulo") %in% expected$tz))
  expect_true(any(expected$tz == "America/Sao_Paulo" & expected$year_start == "11-04"))

  # Every grain whose bin count the offset record splits differently from the aligned one is
  # pinned by a row of its own, so a change to either binning rule moves a digest here.
  aligned <- expected[expected$series == "aligned" & expected$stat == "mean" &
                        expected$partial == "keep" & expected$year_start == "09-01" &
                        expected$tz == "UTC", ]
  expect_setequal(aligned$grain,
                  c("native", "halfday", "day", "week", "month", "season", "year", "astronomical"))
})

test_that("dropping every bin is an error rather than an empty representation", {
  t <- seq(as.POSIXct("2021-10-17 05:00:00", tz = "UTC"), by = "hour", length.out = 24 * 40)
  d <- data.frame(plot = "a", t = t, temp = sin(seq_along(t) / 24), stringsAsFactors = FALSE)
  expect_error(grain_matrix(d, plot, t, temp, grain = "year", partial = "drop"),
               "no whole year")
  expect_error(grain_matrix(d, plot, t, temp, grain = "day", partial = "sometimes"),
               "'arg' should be one of")
})

test_that("the digest is the LF-terminated twelve-place form and nothing else", {
  x <- array(c(1, -0.5), dim = c(2L, 1L, 1L))
  body <- charToRaw("1.000000000000\n-0.500000000000\n")
  f <- tempfile()
  con <- file(f, open = "wb")
  writeBin(body, con)
  close(con)
  expect_identical(digest_array(x), unname(tools::md5sum(f)))
  unlink(f)
})

test_that("a digest is refused over an array that is not finite", {
  expect_error(digest_array(array(c(1, Inf), dim = c(2L, 1L, 1L))), "not finite")
  expect_error(digest_array(array(c(1, -Inf), dim = c(2L, 1L, 1L))), "not finite")
  expect_error(digest_array(array(c(1, NaN), dim = c(2L, 1L, 1L))), "not finite")
  expect_error(digest_array(array(c(1, NA_real_), dim = c(2L, 1L, 1L))), "1 of 2")
})

test_that("coverage() counts the readings the fixtures pin, gaps and all", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  expected <- read.csv(file.path(dir, "coverage.csv"), stringsAsFactors = FALSE,
                       colClasses = c(unit = "character", from = "character", to = "character"))
  expected[is.na(expected)] <- ""
  expect_true(any(expected$n_bin_skipped > 0L))
  expect_true(any(expected$n_empty == 0L))

  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    got <- coverage(coverage_input(dir, row), id, time,
                    grain = fixture_binning(dir, row$series, row$grain))
    empty <- got == 0L
    expect_equal(nrow(got), row$n_unit, info = row$case)
    expect_equal(ncol(got), row$n_bin, info = row$case)
    expect_identical(colnames(got)[1L], row$first_bin, info = row$case)
    expect_identical(colnames(got)[ncol(got)], row$last_bin, info = row$case)
    expect_equal(sum(empty), row$n_empty, info = row$case)
    expect_equal(sum(rowSums(empty) > 0L), row$n_unit_gap, info = row$case)
    expect_equal(sum(colSums(empty) == nrow(got)), row$n_bin_skipped, info = row$case)
    expect_identical(digest_array(array(as.numeric(got), dim = c(dim(got), 1L))),
                     row$digest, info = row$case)
  }
})

test_that("the reduction reads the same record however its rows are ordered", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  record <- fixture_series(dir, "aligned")
  set.seed(11)
  shuffled <- record[sample(nrow(record)), , drop = FALSE]

  # Addition is not associative, so a reduction that accumulated in the order the caller wrote its
  # rows in would move in its last bits under this. The digest is a statement about the
  # representation, so the two are the same bytes and not merely close.
  for (g in c("native", "halfday", "day", "week", "month", "season", "year")) {
    day_level <- !g %in% c("native", "halfday")
    schemes <- list(c("min", "mean", "max"))
    if (day_level) {
      schemes <- c(schemes, list(c("cold_day", "mean", "warm_day"),
                                 c("mean_daily_min", "mean", "mean_daily_max")))
    }
    for (s in schemes) {
      label <- paste(g, paste(s, collapse = "+"))
      expect_identical(
        digest_array(grain_matrix(shuffled, id, time, value, grain = g, stats = s)),
        digest_array(grain_matrix(record, id, time, value, grain = g, stats = s)),
        info = label)
    }
  }
})

test_that("the row order is C collation and not the session's", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  record <- fixture_series(dir, "order")

  # These five ids are the case that made the two languages disagree: an English locale orders
  # them _x a1 A1 P10 P9, C collation orders them A1 P10 P9 _x a1, and NumPy gives the second.
  # The representation must give the second whatever LC_COLLATE the session runs in.
  x <- grain_matrix(record, id, time, value, grain = "day")
  expect_identical(dimnames(x)[[1L]], c("A1", "P10", "P9", "_x", "a1"))

  # The input row order carries no meaning: the ids arrive in a third order again.
  expect_identical(unique(record$id), c("a1", "P9", "_x", "A1", "P10"))
  shuffled <- record[order(record$value), , drop = FALSE]
  expect_identical(digest_array(grain_matrix(shuffled, id, time, value, grain = "day")),
                   digest_array(x))
})

test_that("the scorable mask orders its variables by C collation too", {
  y <- matrix(c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1), nrow = 4,
              dimnames = list(paste0("u", 1:4), c("a1", "P9", "_x")))
  cells <- scorable_cells(y, c(u1 = 1L, u2 = 1L, u3 = 2L, u4 = 2L))
  expect_identical(unique(cells$variable), c("P9", "_x", "a1"))
})

test_that("a supplied calendar that breaks its guarantees is refused, as the fixtures pin", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  guards <- read.csv(file.path(dir, "grain_guards.csv"), stringsAsFactors = FALSE)
  expect_gte(nrow(guards), 3L)

  for (i in seq_len(nrow(guards))) {
    row <- guards[i, ]
    record <- fixture_series(dir, row$series)
    expect_error(grain_matrix(record, id, time, value,
                              grain = fixture_calendar(row$calendar), stats = "mean"),
                 row$message, fixed = TRUE)
  }
})

test_that("every zoned digest has the oracle as its independent witness", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  expected <- read.csv(file.path(dir, "digests.csv"), stringsAsFactors = FALSE)
  zoned <- expected[expected$tz != "UTC", ]
  expect_gt(nrow(zoned), 0L)

  # A digest the core produced under a zone is a regression pin until something that is not the
  # core reproduces it. The oracle reads the same instants as a clock in the same zone and bins
  # that clock with its own calendar.
  series <- lapply(stats::setNames(nm = unique(zoned$series)), fixture_series, dir = dir)
  for (i in seq_len(nrow(zoned))) {
    row <- zoned[i, ]
    label <- paste(row$series, row$grain, row$tz, row$year_start, row$partial, row$stat)
    record <- series[[row$series]]
    attr(record$time, "tzone") <- row$tz
    o <- oracle_grain_matrix(record, "id", "time", "value",
                             grain = fixture_binning(dir, row$series, row$grain),
                             stats = strsplit(row$stat, "+", fixed = TRUE)[[1L]],
                             year_start = row$year_start)
    values <- o$values
    if (row$partial == "drop") values <- values[, !o$bin_partial, , drop = FALSE]
    expect_equal(dim(values)[2L], row$n_bin, info = label)
    expect_identical(digest_array(values), row$digest, info = label)
  }
})

# The channels a learner reads beside the readings. What is pinned is the fraction of the year each
# bin sits at, which is arithmetic on the calendar; the sine and the cosine of it are the
# platform's library, and the contract states a tolerance on them rather than hashing them.
test_that("the calendar channels match the fraction the Python side reads", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  expected <- read.csv(file.path(dir, "channels_digests.csv"), stringsAsFactors = FALSE)
  series <- lapply(stats::setNames(nm = unique(expected$series)), fixture_series, dir = dir)

  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    label <- paste(row$series, row$grain, row$tz, row$year_start, row$partial, row$kind)
    record <- series[[row$series]]
    attr(record$time, "tzone") <- row$tz
    x <- grain_matrix(record, id, time, value,
                       grain = fixture_binning(dir, row$series, row$grain),
                       stats = strsplit(row$stat, "+", fixed = TRUE)[[1L]],
                       year_start = row$year_start, partial = row$partial)
    cc <- calendar_channels(x, cycles = row$cycle)
    got <- if (row$kind == "bound") bind_channels(x, cc) else cc

    start <- attr(got, "bin_start")
    expect_equal(dim(got)[1], row$n_unit, info = label)
    expect_equal(dim(got)[2], row$n_bin, info = label)
    expect_identical(paste(dimnames(got)[[3L]], collapse = "+"), row$channels, info = label)
    expect_equal(format(start[1], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), row$first_bin, info = label)
    expect_equal(format(start[length(start)], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 row$last_bin, info = label)

    frac <- ts_cycle_fraction_(as.numeric(start), as.numeric(attr(got, "bin_end")), row$cycle)
    expect_identical(digest_array(frac), row$digest, info = label)
    expect_equal(frac, oracle_cycle_fraction(row$cycle, start, attr(got, "bin_end")),
                 info = label)

    sin_name <- paste0(row$cycle, "_sin")
    cos_name <- paste0(row$cycle, "_cos")
    expect_lt(max(abs(as.numeric(got[1, , sin_name]) - sin(2 * pi * frac))), row$tolerance)
    expect_lt(max(abs(as.numeric(got[1, , cos_name]) - cos(2 * pi * frac))), row$tolerance)
    # The calendar is where a bin sits, not something a unit has, so every unit reads the same two
    # channels; a bound array carries its readings unchanged beside them.
    for (u in seq_len(dim(got)[1])) {
      expect_identical(got[u, , sin_name], got[1L, , sin_name], info = label)
      expect_identical(got[u, , cos_name], got[1L, , cos_name], info = label)
    }
    if (row$kind == "bound") {
      expect_identical(as.numeric(got[, , dimnames(x)[[3L]]]), as.numeric(x), info = label)
      expect_identical(attr(got, "bin_start"), attr(x, "bin_start"), info = label)
      expect_identical(attr(got, "bin_n"), attr(x, "bin_n"), info = label)
      expect_identical(attr(got, "bin_partial"), attr(x, "bin_partial"), info = label)
    }
  }
})

test_that("what the two channel functions refuse is what the Python side refuses", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  expected <- read.csv(file.path(dir, "channels_guards.csv"), stringsAsFactors = FALSE)
  record <- fixture_series(dir, "aligned")
  x <- grain_matrix(record, id, time, value, grain = "week")
  other <- grain_matrix(record, id, time, value, grain = "month")
  at <- data.frame(id = unique(record$id), at = max(record$time), stringsAsFactors = FALSE)
  back <- lookback_matrix(record, id, time, value, at = at, span = "30 days")

  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    raised <- switch(row$case,
      lookback = function() calendar_channels(back),
      not_a_representation = function() bind_channels(x, 1),
      one_argument = function() bind_channels(x),
      duplicate = function() bind_channels(x, x),
      different_bins = function() bind_channels(x, other),
      day_coarse = function() calendar_channels(x, cycles = "day"),
      unknown_cycle = function() calendar_channels(x, cycles = "month"),
      stop("no channel guard called ", row$case))
    expect_error(raised(), row$message, fixed = TRUE, info = row$case)
  }
  expect_setequal(expected$case, c("lookback", "not_a_representation", "one_argument",
                                   "duplicate", "different_bins", "day_coarse", "unknown_cycle"))
})
