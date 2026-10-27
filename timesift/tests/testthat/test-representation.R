rep_spec <- function(id = "plot", time = "t", value = "temp", target_time = NULL,
                     static = character()) {
  list(id = id, time = time, value = value, target_time = target_time, static = static,
       partial = "keep")
}

rep_targets <- function(units, shuffle = FALSE) {
  out <- data.frame(plot = units, elevation = seq_along(units) * 100,
                    site = rep(c("a", "b"), length.out = length(units)),
                    stringsAsFactors = FALSE)
  if (shuffle) out[rev(seq_len(nrow(out))), , drop = FALSE] else out
}

test_that("each constructor carries the fields the layer above reads", {
  r <- native()
  expect_s3_class(r, "timesift_representation")
  expect_equal(r$label, "native")
  expect_equal(r$kind, "grain")
  expect_equal(r$grain, "native")
  expect_true(r$sequence)

  g <- grain("week", stats = c("cold_day", "mean", "warm_day"))
  expect_equal(g$label, "week")
  expect_equal(g$stats, c("cold_day", "mean", "warm_day"))
  expect_true(g$sequence)

  m <- multigrain(c("month", "season"))
  expect_equal(m$kind, "multigrain")
  expect_equal(m$grains, c("month", "season"))
  expect_false(m$sequence)

  w <- lookback("30 days", bins = 3L)
  expect_equal(w$kind, "lookback")
  expect_equal(w$span, 30 * 86400)
  expect_equal(w$lag, 0)
  expect_equal(w$bins, 3L)
  expect_true(w$sequence)
  expect_false(lookback("30 days")$sequence)
})

test_that("a representation refuses a setting it cannot carry", {
  expect_error(grain(c("day", "week")), "one grain")
  expect_error(grain("fortnight"), "unknown grain")
  expect_error(grain("native", stats = "cold_day"), "shorter than a day")
  expect_error(grain("week", stats = "median"), "unknown statistic")
  expect_error(grain("week", year_start = "13-01"), "month 01-12")
  expect_error(multigrain(c("halfday", "week"), stats = "warm_day"), "shorter than a day")
  expect_error(lookback("0 days"), "positive length")
  expect_error(lookback("30 days", bins = 0L), "positive whole number")
})

test_that("a lookback is labelled by what tells it from its neighbours", {
  expect_equal(lookback("30 days")$label, "30 days")
  expect_equal(lookback("30 days", bins = 3L)$label, "30 days x3")
  expect_equal(lookback("30 days", lag = "7 days")$label, "30 days lag 7 days")
  expect_equal(lookback("30 days", lag = "7 days", bins = 2L)$label, "30 days x2 lag 7 days")
})

test_that("a sift is keyed by label, whichever way it was written", {
  s <- grains("day", "week")
  expect_s3_class(s, "timesift_sift")
  expect_equal(names(s), c("day", "week"))
  expect_equal(names(grains(c("month", "year"))), c("month", "year"))
  expect_equal(names(lookbacks("30 days", "90 days")), c("30 days", "90 days"))
  expect_equal(names(as_sift(c("day", "week"))), c("day", "week"))
  expect_equal(names(as_sift(grain("week"))), "week")
  expect_equal(names(as_sift(list(coarse = grain("month")))), "coarse")
  expect_true(isTRUE(attr(grains("auto"), "auto")))
})

test_that("a set of grains hands every member its statistics and its year start", {
  named <- grains("week", "year", stats = c("cold_day", "mean"), year_start = "01-01")
  expect_equal(vapply(named, function(r) r$year_start, character(1L)),
               c(week = "01-01", year = "01-01"))
  expect_equal(vapply(named, function(r) paste(r$stats, collapse = "+"), character(1L)),
               c(week = "cold_day+mean", year = "cold_day+mean"))
  expect_identical(attr(grains("auto", year_start = "01-01"), "year_start"), "01-01")
  expect_error(grains("week", year_start = "13-01"), "month 01-12")
})

test_that("a sift refuses what it cannot key", {
  expect_error(grains(), "at least one grain")
  expect_error(grains("auto", "week"), "whole set")
  expect_error(lookbacks(), "at least one span")
  expect_error(as_sift(list(grain("week"), grain("week", stats = "min"))),
               "same name")
  expect_error(as_sift(list(grain("week"), 1)), "not a representation")
  expect_error(as_sift(list()), "non-empty list")
})

test_that("a grain block covers the targets in sorted identifier order", {
  sim <- sim_series(n_unit = 6L, days = 60L)
  targets <- rep_targets(sim$units, shuffle = TRUE)
  x <- build_representation(grain("week"), sim$readings, targets, rep_spec())
  expect_s3_class(x, "timesift_matrix")
  expect_equal(dimnames(x)[[1L]], sort(sim$units))
  expect_equal(dim(x)[3L], 1L)
  expect_equal(dimnames(x)[[3L]], "mean")
})

test_that("a target the series does not carry is named rather than dropped", {
  sim <- sim_series(n_unit = 4L, days = 30L)
  targets <- rep_targets(c(sim$units, "p999"))
  expect_error(build_representation(grain("week"), sim$readings, targets, rep_spec()),
               "p999")
})

test_that("multigrain binds its grains into one block of features", {
  sim <- sim_series(n_unit = 5L, days = 90L)
  targets <- rep_targets(sim$units)
  x <- build_representation(multigrain(c("month", "season")), sim$readings, targets, rep_spec())
  month <- build_representation(grain("month"), sim$readings, targets, rep_spec())
  season <- build_representation(grain("season"), sim$readings, targets, rep_spec())

  expect_equal(dim(x)[1L], 5L)
  expect_equal(dim(x)[3L], 1L)
  expect_equal(dim(x)[2L], dim(month)[2L] + dim(season)[2L])
  expect_true(all(grepl("^month:|^season:", dimnames(x)[[2L]])))
  expect_equal(unname(x[, 1L, 1L]), unname(month[, 1L, 1L]))
})

test_that("a lookback block keeps the targets' own order and one row each", {
  sim <- sim_series(n_unit = 3L, days = 120L)
  at <- as.POSIXct(c("2021-11-01", "2021-12-01", "2021-11-15", "2021-12-15"), tz = "UTC")
  targets <- data.frame(plot = c(sim$units[1L], sim$units[1L], sim$units[2L], sim$units[3L]),
                        when = at, stringsAsFactors = FALSE)
  spec <- rep_spec(target_time = "when")
  x <- build_representation(lookback("30 days", bins = 2L), sim$readings, targets, spec)
  expect_equal(dim(x)[1L], 4L)
  expect_equal(dim(x)[2L], 2L)
  expect_equal(attr(x, "span"), 30 * 86400)
  expect_equal(dimnames(x)[[1L]], as.character(1:4))
})

test_that("a lookback without an anchor names the representation and target_time", {
  sim <- sim_series(n_unit = 2L, days = 60L)
  targets <- rep_targets(sim$units)
  expect_error(build_representation(lookback("30 days"), sim$readings, targets, rep_spec()),
               "`target_time` has to name the column", fixed = TRUE)
})

test_that("static columns enter as channels that do not move across the bins", {
  sim <- sim_series(n_unit = 5L, days = 60L)
  targets <- rep_targets(sim$units)
  spec <- rep_spec(static = "elevation")
  x <- build_representation(grain("week"), sim$readings, targets, spec)
  expect_equal(dimnames(x)[[3L]], c("mean", "elevation"))
  expect_equal(unname(x[, 1L, "elevation"]), unname(x[, dim(x)[2L], "elevation"]))
  expect_equal(unname(x[, 1L, "elevation"]), targets$elevation[order(targets$plot)])
})

test_that("a static column is one predictor of the flattened design, not one per bin", {
  sim <- sim_series(n_unit = 5L, days = 60L)
  targets <- rep_targets(sim$units)
  spec <- rep_spec(static = "elevation")
  for (rep in list(grain("week"), multigrain(c("week", "month")), native())) {
    x <- build_representation(rep, sim$readings, targets, spec)
    m <- .flatten(x)
    expect_equal(sum(colnames(m) == "elevation"), 1L)
    expect_equal(ncol(m), dim(x)[2L] * (dim(x)[3L] - 1L) + 1L)
    expect_equal(unname(m[, "elevation"]), targets$elevation[order(targets$plot)])
    # Every reading keeps its own column, and the static one is the last of them.
    expect_equal(colnames(m)[-ncol(m)], colnames(.flatten(build_representation(
      rep, sim$readings, targets, rep_spec()))))
  }
})

test_that("a lookback keeps its span once static channels are beside it", {
  sim <- sim_series(n_unit = 3L, days = 120L)
  targets <- data.frame(plot = sim$units, elevation = c(1000, 2000, 3000),
                        when = as.POSIXct(rep("2021-12-01", 3L), tz = "UTC"),
                        stringsAsFactors = FALSE)
  spec <- rep_spec(target_time = "when", static = "elevation")
  x <- build_representation(lookback("30 days", bins = 2L), sim$readings, targets, spec)
  expect_equal(attr(x, "span"), 30 * 86400)
  expect_equal(attr(x, "bins"), 2L)
  expect_equal(dimnames(x)[[3L]], c("mean", "elevation"))
})

test_that("a static column that is not a number is refused by name", {
  sim <- sim_series(n_unit = 4L, days = 30L)
  targets <- rep_targets(sim$units)
  expect_error(build_representation(grain("week"), sim$readings, targets,
                                    rep_spec(static = "site")),
               "site")
})

test_that("several value columns reach the array as named channels", {
  sim <- sim_series(n_unit = 4L, days = 40L)
  readings <- sim$readings
  readings$snow <- as.numeric(readings$temp < 0)
  targets <- rep_targets(sim$units)
  x <- build_representation(grain("week"), readings, targets,
                            rep_spec(value = c("temp", "snow")))
  expect_equal(dimnames(x)[[3L]], c("temp_mean", "snow_mean"))
  expect_equal(attr(x, "stats"), c("temp_mean", "snow_mean"))
})

test_that("the automatic set is every grain the record gives two bins, in order", {
  sim <- sim_series(n_unit = 4L, days = 60L)
  targets <- rep_targets(sim$units)
  built <- .auto_grains("mean", "09-01", sim$readings, .target_frame(targets, rep_spec()),
                        rep_spec())
  expect_equal(names(built), c("native", "halfday", "day", "week", "month"))
  expect_true(all(vapply(built, function(m) dim(m)[2L] >= 2L, logical(1L))))
})

test_that("the automatic set leaves out a grain the statistics are not defined at", {
  sim <- sim_series(n_unit = 4L, days = 60L)
  targets <- rep_targets(sim$units)
  built <- .auto_grains(c("cold_day", "mean", "warm_day"), "09-01", sim$readings,
                        .target_frame(targets, rep_spec()), rep_spec())
  expect_false(any(c("native", "halfday") %in% names(built)))
  expect_equal(names(built), c("day", "week", "month"))
})

test_that("a targets-only block is the static columns and nothing else", {
  targets <- rep_targets(sprintf("p%02d", 1:6))
  spec <- rep_spec(id = "plot", time = NULL, value = character(), static = "elevation")
  x <- build_representation(.static_representation(), NULL, targets, spec)
  expect_equal(dim(x), c(6L, 1L, 1L))
  expect_equal(dimnames(x)[[2L]], "elevation")
})

test_that("printing a representation and a sift says what they are", {
  expect_output(print.timesift_representation(grain("week")), "grain   : week")
  expect_output(print.timesift_representation(lookback("30 days", bins = 2L)), "30 days in 2 bins")
  expect_output(print.timesift_representation(multigrain(c("day", "week"))), "day, week")
  expect_output(print.timesift_representation(multigrain()), "chosen from the record")
  expect_output(print.timesift_sift(grains("day", "week")), "2 representations")
  expect_output(print.timesift_sift(grains("auto")), "every grain")
})
