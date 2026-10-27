lookback_record <- function(days = 200L, seed = 20260904L) {
  set.seed(seed)
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24L * days)
  data.frame(id = rep(c("p1", "p2"), each = length(t)), t = rep(t, 2L),
             v = stats::rnorm(2L * length(t), sd = 5), stringsAsFactors = FALSE)
}

lookback_anchors <- function() {
  data.frame(id = c("p1", "p2", "p1", "p2"),
             at = as.POSIXct(c("2022-01-01", "2022-02-10", "2022-03-05", "2022-03-15"),
                               tz = "UTC"),
             stringsAsFactors = FALSE)
}

lookback_fixture_series <- function(dir, name) {
  file <- switch(name, aligned = "series.csv", offset = "series_offset.csv",
                 zoned = "series_zoned.csv", order = "series_order.csv")
  s <- read.csv(file.path(dir, file), stringsAsFactors = FALSE)
  s$time <- as.POSIXct(s$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  s
}

lookback_fixture_at <- function(targets, set) {
  taken <- targets[targets$set == set, ]
  data.frame(id = taken$id,
             at = as.POSIXct(taken$at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
             stringsAsFactors = FALSE)
}

test_that("the core reproduces the pure-R oracle over the lookback grid", {
  d <- lookback_record()
  at <- lookback_anchors()
  schemes <- list("mean", "min", "max", "cold_day", "warm_day", "mean_daily_min",
                  "mean_daily_max", c("min", "mean", "max"), c("cold_day", "mean", "warm_day"),
                  c("mean_daily_min", "mean", "mean_daily_max"))
  grid <- list(list("30 days", "0 days", 1L), list("30 days", "0 days", 3L),
               list("30 days", "7 days", 3L), list("7 days", "0 days", 7L),
               list("60 days", "1 day", 2L), list("2 days", "0 days", 2L))

  for (g in grid) {
    for (s in schemes) {
      label <- paste(g[[1L]], g[[2L]], g[[3L]], paste(s, collapse = "+"))
      x <- lookback_matrix(d, id, t, v, at = at, span = g[[1L]], lag = g[[2L]], bins = g[[3L]],
                         stats = s)
      o <- oracle_lookback_matrix(d, "id", "t", "v", at = at, span = g[[1L]], lag = g[[2L]],
                                bins = g[[3L]], stats = s)
      # R's sum() accumulates in long double where the core accumulates in double, so the two
      # agree on the arithmetic rather than on the last bit. What is asserted byte-exactly is the
      # digest, which both languages take off the core.
      expect_equal(as.vector(unclass(x)), as.vector(o$values), tolerance = 1e-12, info = label)
      expect_identical(dimnames(x), dimnames(o$values), info = label)
      expect_identical(as.vector(attr(x, "bin_n")), as.vector(o$bin_n), info = label)
    }
  }
})

test_that("a lookback reads only the target's own unit and only its own stretch", {
  d <- lookback_record(days = 60L)
  at <- data.frame(id = c("p1", "p2"),
                   at = as.POSIXct(c("2021-10-20", "2021-10-20"), tz = "UTC"),
                   stringsAsFactors = FALSE)
  x <- lookback_matrix(d, id, t, v, at = at, span = "10 days", stats = "mean")

  own <- vapply(c("p1", "p2"), function(u) {
    mean(d$v[d$id == u &
               d$t >= as.POSIXct("2021-10-10", tz = "UTC") &
               d$t < as.POSIXct("2021-10-20", tz = "UTC")])
  }, numeric(1L))
  expect_equal(as.vector(unclass(x)), unname(own))

  # The interval is closed at the left and open at the right, so the anchor's own reading is out
  # and the reading a span earlier is in.
  expect_identical(as.vector(attr(x, "bin_n")), c(240L, 240L))

  # Two targets on one unit a fortnight apart read two different stretches of one series, which is
  # the whole reason the reduction exists.
  pair <- lookback_matrix(d, id, t, v,
                        at = data.frame(id = c("p1", "p1"),
                                        at = as.POSIXct(c("2021-10-06", "2021-10-20"),
                                                          tz = "UTC")),
                        span = "10 days", stats = "mean")
  expect_false(isTRUE(all.equal(pair[1L, 1L, 1L], pair[2L, 1L, 1L])))
})

test_that("the lookback digests are the ones the Python side reads", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")

  expected <- read.csv(file.path(dir, "lookback_digests.csv"), stringsAsFactors = FALSE)
  targets <- read.csv(file.path(dir, "lookback_targets.csv"), stringsAsFactors = FALSE)
  series <- lapply(stats::setNames(nm = unique(targets$series)), lookback_fixture_series, dir = dir)

  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    label <- paste(row$set, row$tz, row$span, row$lag, row$bins, row$stat)
    name <- unique(targets$series[targets$set == row$set])
    record <- series[[name]]
    attr(record$time, "tzone") <- row$tz
    at <- lookback_fixture_at(targets, row$set)

    x <- lookback_matrix(record, id, time, value, at = at, span = row$span, lag = row$lag,
                       bins = row$bins, stats = strsplit(row$stat, "+", fixed = TRUE)[[1L]])
    # The shape and the bin naming are asserted before the digest, so a lookback cut differently is
    # reported as that rather than as an unexplained hash mismatch.
    expect_equal(dim(x)[1], row$n_target, info = label)
    expect_equal(dim(x)[2], row$n_bin, info = label)
    expect_identical(dimnames(x)[[2L]][1L], row$first_bin, info = label)
    expect_identical(dimnames(x)[[2L]][dim(x)[2]], row$last_bin, info = label)
    expect_identical(at$id[1L], row$first_unit, info = label)
    expect_identical(at$id[nrow(at)], row$last_unit, info = label)
    expect_identical(digest_array(x), row$digest, info = label)
  }
})

test_that("the lookback fixtures carry what the contract says they carry", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  expected <- read.csv(file.path(dir, "lookback_digests.csv"), stringsAsFactors = FALSE)
  targets <- read.csv(file.path(dir, "lookback_targets.csv"), stringsAsFactors = FALSE)

  # An anchor that is a local midnight in one zone is not one in another, and an anchor on the
  # hour rules out the four day-level statistics a midnight allows, so the sets are what carry
  # both halves rather than one set per series.
  expect_setequal(unique(targets$set), c("aligned", "offset", "hourly", "zoned"))
  expect_true(all(c("UTC", "America/Sao_Paulo") %in% expected$tz))
  expect_true(any(expected$set == "zoned" & expected$tz == "America/Sao_Paulo" &
                    grepl("cold_day", expected$stat, fixed = TRUE)))

  named <- unlist(strsplit(expected$stat, "+", fixed = TRUE))
  expect_true(all(c("mean", "min", "max", "cold_day", "warm_day",
                    "mean_daily_min", "mean_daily_max") %in% named))
  expect_true(all(c(1L, 3L, 4L, 7L) %in% expected$bins))
  expect_gt(length(unique(expected$lag)), 1L)
  # A span written as a bare count of seconds, and a step that is no whole number of days.
  expect_true(any(grepl("^[0-9]+$", expected$span)))
  expect_true(any(expected$span == "7 days" & expected$bins == 3L))
})

test_that("both guards fire with the message the fixtures pin", {
  dir <- fixture_dir()
  skip_if(is.null(dir), "fixtures are not in the built package")
  guards <- read.csv(file.path(dir, "lookback_guards.csv"), stringsAsFactors = FALSE)
  targets <- read.csv(file.path(dir, "lookback_targets.csv"), stringsAsFactors = FALSE)
  expect_gte(nrow(guards), 3L)

  for (i in seq_len(nrow(guards))) {
    row <- guards[i, ]
    name <- unique(targets$series[targets$set == row$set])
    record <- lookback_fixture_series(dir, name)
    attr(record$time, "tzone") <- row$tz
    expect_error(
      lookback_matrix(record, id, time, value, at = lookback_fixture_at(targets, row$set),
                    span = row$span, lag = row$lag, bins = row$bins, stats = row$stat),
      row$message, fixed = TRUE)
  }
})

test_that("a cell the record cannot fill names the target and the interval", {
  d <- lookback_record(days = 30L)
  at <- data.frame(id = "p1", at = as.POSIXct("2021-09-20", tz = "UTC"),
                   stringsAsFactors = FALSE)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "30 days", bins = 3L, stats = "mean"),
               "1 (target, bin) cell hold no readings, first: target 1 over ", fixed = TRUE)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "30 days", bins = 3L, stats = "mean"),
               "[2021-08-21T00:00:00, 2021-08-31T00:00:00)", fixed = TRUE)

  # The row label the guard uses is the target's own, where `at` carries one.
  rownames(at) <- "plot-a"
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "30 days", bins = 3L, stats = "mean"),
               "first: target plot-a", fixed = TRUE)
})

test_that("a day-level statistic is refused where a calendar day would fall in two bins", {
  d <- lookback_record(days = 60L)
  at <- data.frame(id = "p1", at = as.POSIXct("2021-10-20", tz = "UTC"),
                   stringsAsFactors = FALSE)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "7 days", bins = 3L, stats = "cold_day"),
               "cold_day needs bins of a calendar day or coarser: target 1", fixed = TRUE)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "7 days", bins = 3L,
                             stats = c("cold_day", "warm_day")),
               "cold_day and warm_day need bins of a calendar day or coarser", fixed = TRUE)
  expect_silent(lookback_matrix(d, id, t, v, at = at, span = "7 days", bins = 3L,
                              stats = c("min", "mean", "max")))

  hour <- data.frame(id = "p1", at = as.POSIXct("2021-10-20 05:00:00", tz = "UTC"),
                     stringsAsFactors = FALSE)
  expect_error(lookback_matrix(d, id, t, v, at = hour, span = "7 days", bins = 7L,
                             stats = "warm_day"),
               "warm_day needs bins that open on a day boundary: target 1's lookback opens at ",
               fixed = TRUE)
  expect_silent(lookback_matrix(d, id, t, v, at = hour, span = "7 days", bins = 7L, stats = "mean"))
})

test_that("a duration is a count and a unit, or a count of seconds", {
  expect_identical(.parse_duration("30 days", "span"), 2592000)
  expect_identical(.parse_duration("7 days", "span"), 604800)
  expect_identical(.parse_duration("12 hours", "span"), 43200)
  expect_identical(.parse_duration("1 day", "span"), 86400)
  expect_identical(.parse_duration("1 week", "span"), 604800)
  # A year is 365 days and a month is 30 days: a lookback of a fixed length is a fixed length.
  expect_identical(.parse_duration("1 year", "span"), 31536000)
  expect_identical(.parse_duration("1 month", "span"), 2592000)
  expect_identical(.parse_duration("90", "span"), 90)
  expect_identical(.parse_duration(90, "span"), 90)
  expect_identical(.parse_duration("2 HOURS", "span"), 7200)

  expect_error(.parse_duration("a fortnight", "span"), "must be a count and a unit")
  expect_error(.parse_duration("3 fortnights", "lag"), "unknown duration unit")
  expect_error(.parse_duration(1.5, "span"), "whole number of seconds")
  expect_error(.parse_duration("-1 day", "lag"), "must be a count and a unit")
})

test_that("a bin is named by where it opens relative to the anchor", {
  expect_identical(.bin_offsets(2592000, 0, 3L), c("-30 days", "-20 days", "-10 days"))
  expect_identical(.bin_offsets(86400, 0, 1L), "-1 day")
  expect_identical(.bin_offsets(43200, 43200, 3L), c("-1 day", "-20 hours", "-16 hours"))
  expect_identical(.bin_offsets(604800, 43200, 1L), "-180 hours")
})

test_that("a lookback states what it was built from", {
  d <- lookback_record(days = 60L)
  at <- data.frame(id = c("p1", "p2"),
                   at = as.POSIXct(c("2021-10-20", "2021-10-21"), tz = "UTC"),
                   stringsAsFactors = FALSE)
  x <- lookback_matrix(d, id, t, v, at = at, span = "30 days", lag = "12 hours", bins = 3L,
                     stats = c("min", "mean", "max"))

  expect_s3_class(x, "timesift_matrix")
  expect_identical(attr(x, "grain"), "lookback")
  expect_identical(attr(x, "span"), 2592000)
  expect_identical(attr(x, "lag"), 43200)
  expect_identical(attr(x, "bins"), 3L)
  expect_identical(attr(x, "stats"), c("min", "mean", "max"))
  expect_identical(dim(attr(x, "bin_n")), c(2L, 3L))
  expect_identical(dimnames(x)[[1L]], c("1", "2"))
  expect_output(print(x), "grain: lookback")
})

test_that("a lookback refuses an input it cannot answer for", {
  d <- lookback_record(days = 60L)
  at <- data.frame(id = "p1", at = as.POSIXct("2021-10-20", tz = "UTC"),
                   stringsAsFactors = FALSE)

  expect_error(lookback_matrix(d, id, t, v, at = at, span = "7 days", bins = 11L, stats = "mean"),
               "does not divide into 11 bins")
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "7 days", bins = 0L, stats = "mean"),
               "`bins` must be a positive whole number")
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "7 days", stats = "warmest"),
               "unknown statistic")
  expect_error(lookback_matrix(d, id, t, v, at = data.frame(id = "p9", at = at$at),
                             span = "7 days", stats = "mean"),
               "1 target name a unit the series does not carry, first: p9", fixed = TRUE)
  expect_error(lookback_matrix(d, id, t, v, at = at$id, span = "7 days", stats = "mean"),
               "`at` must be a data frame")
  expect_error(lookback_matrix(d, id, t, v, at = data.frame(id = "p1", at = "2021-10-20"),
                             span = "7 days", stats = "mean"),
               "must be POSIXct")
})

test_that("the anchors are read as a clock in the series' own calendar", {
  set.seed(7)
  t <- seq(as.POSIXct("2021-12-01", tz = "UTC"), by = "hour", length.out = 24 * 40)
  d <- data.frame(id = "p1", t = t, v = stats::rnorm(length(t)), stringsAsFactors = FALSE)
  # An instant that opens a Vienna day, which is 23:00 the evening before in UTC.
  at <- data.frame(id = "p1", at = as.POSIXct("2021-12-20", tz = "Europe/Vienna"),
                   stringsAsFactors = FALSE)

  attr(d$t, "tzone") <- "Europe/Vienna"
  vienna <- lookback_matrix(d, id, t, v, at = at, span = "7 days", bins = 7L, stats = "cold_day")

  # The same instants relabelled into their Vienna clock, with the anchor relabelled too, give the
  # same answer: the zone is the whole of the difference and it is resolved once, at the edge.
  relabelled <- data.frame(
    id = d$id,
    t = as.POSIXct(format(d$t, "%Y-%m-%d %H:%M:%S", tz = "Europe/Vienna"), tz = "UTC"),
    v = d$v, stringsAsFactors = FALSE)
  shifted <- data.frame(
    id = "p1",
    at = as.POSIXct(format(at$at, "%Y-%m-%d %H:%M:%S", tz = "Europe/Vienna"), tz = "UTC"),
    stringsAsFactors = FALSE)
  naive <- lookback_matrix(relabelled, id, t, v, at = shifted, span = "7 days", bins = 7L,
                         stats = "cold_day")
  expect_identical(as.vector(unclass(vienna)), as.vector(unclass(naive)))

  # A lookback is placed relative to its anchor, so an offset the same at both ends of it cancels and
  # the zone shows only where it decides something: where a calendar day begins. The anchor that
  # opens a Vienna day opens no UTC one, and the day-level four are refused there.
  attr(d$t, "tzone") <- "UTC"
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "7 days", bins = 7L, stats = "cold_day"),
               "open on a day boundary")
  expect_identical(as.vector(unclass(lookback_matrix(d, id, t, v, at = at, span = "7 days",
                                                   stats = "mean"))),
                   as.vector(unclass(lookback_matrix(relabelled, id, t, v, at = shifted,
                                                   span = "7 days", stats = "mean"))))
})

test_that("the target table is read by name", {
  d <- lookback_record(days = 60L)
  at <- data.frame(id = "p1", at = as.POSIXct("2021-10-20", tz = "UTC"),
                   stringsAsFactors = FALSE)
  x <- lookback_matrix(d, id, t, v, at = at, span = "10 days", stats = "mean")
  # A bin is a position relative to an anchor rather than a span of the calendar, so the three
  # attributes the calendar owns are absent here as they are in Python.
  expect_null(attr(x, "bin_start"))
  expect_null(attr(x, "bin_end"))
  expect_null(attr(x, "bin_partial"))

  named <- data.frame(unit = at$id, when = at$at, stringsAsFactors = FALSE)
  expect_error(lookback_matrix(d, id, t, v, at = named, span = "10 days", stats = "mean"),
               "an `id` column naming the unit and an `at` column", fixed = TRUE)
})

test_that("a target row is named by its row name, whatever kind of row name the frame carries", {
  d <- lookback_record(days = 60L)
  at <- data.frame(id = c("p1", "p2", "p1"),
                   at = as.POSIXct(c("2021-10-20", "2021-10-21", "2021-10-22"), tz = "UTC"),
                   stringsAsFactors = FALSE)
  x <- lookback_matrix(d, id, t, v, at = at, span = "7 days")
  expect_identical(dimnames(x)[[1L]], c("1", "2", "3"))
  # A frame subset keeps its rows' original names, and so does the array built from it.
  expect_identical(dimnames(lookback_matrix(d, id, t, v, at = at[c(3L, 1L), ],
                                            span = "7 days"))[[1L]],
                   c("3", "1"))
  expect_identical(dimnames(lookback_matrix(d, id, t, v, at = at[2L, , drop = FALSE],
                                            span = "7 days"))[[1L]],
                   "2")
})

test_that("a double `bins` that is a whole number is that whole number", {
  d <- lookback_record(days = 60L)
  at <- data.frame(id = "p1", at = as.POSIXct("2021-10-20", tz = "UTC"), stringsAsFactors = FALSE)
  expect_identical(dim(lookback_matrix(d, id, t, v, at = at, span = "6 days", bins = 3))[2L], 3L)
  expect_error(lookback_matrix(d, id, t, v, at = at, span = "6 days", bins = 2.5),
               "positive whole number")
})
