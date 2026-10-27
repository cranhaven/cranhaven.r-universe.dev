# A record in which the units differ from each other in one stretch of the calendar and nowhere
# else, so the profile has a known answer to be checked against.
planted_series <- function(planted_month = "2021-11", n_unit = 60L, seed = 61L) {
  set.seed(seed)
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24L * 210L)
  in_month <- format(t, "%Y-%m", tz = "UTC") == planted_month
  units <- sprintf("p%03d", seq_len(n_unit))
  warmth <- stats::rnorm(n_unit)
  shape <- 5 * sin(seq_along(t) / (24 * 40))
  value <- as.numeric(vapply(warmth, function(w) {
    shape + w * in_month * 6 + stats::rnorm(length(t), sd = 0.3)
  }, numeric(length(t))))
  list(units = units, warmth = warmth, planted = planted_month,
       readings = data.frame(plot = rep(units, each = length(t)), t = rep(t, times = n_unit),
                             temp = value, stringsAsFactors = FALSE))
}

test_that("occlusion needs the fits the ladder was told to keep", {
  sim <- sim_series(n_unit = 30L, days = 60L)
  y <- sim_response(sim, n_var = 2L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month")
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(), folds = fold_map(y, v = 3L),
                                        verbose = FALSE))
  expect_error(occlusion(lad, x, y, "month|elasticnet"), "kept no fits")
})

test_that("the bin a signal was planted in is the bin the profile weights", {
  sim <- planted_series()
  y <- matrix(stats::rbinom(length(sim$warmth) * 2L, 1L,
                            stats::plogis(3 * c(sim$warmth, -sim$warmth))),
              ncol = 2L, dimnames = list(sim$units, c("sp1", "sp2")))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month")
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(),
                                        folds = fold_map(y, v = 4L, seed = 6L),
                                        keep_fits = TRUE, verbose = FALSE))
  oc <- occlusion(lad, x, y, "month|elasticnet", permutations = 5L, seed = 4L)

  weight <- stats::aggregate(list(weight = oc$weight), oc["part"], mean)
  heaviest <- weight$part[which.max(weight$weight)]
  expect_equal(substr(heaviest, 1L, 7L), sim$planted)
  expect_gt(max(weight$weight), 2 * stats::median(weight$weight))
})

test_that("every substitute runs and reports the same parts", {
  sim <- planted_series(n_unit = 40L, seed = 62L)
  y <- matrix(stats::rbinom(length(sim$warmth) * 2L, 1L,
                            stats::plogis(3 * c(sim$warmth, -sim$warmth))),
              ncol = 2L, dimnames = list(sim$units, c("sp1", "sp2")))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month",
                     stats = c("cold_day", "mean", "warm_day"))
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(),
                                        folds = fold_map(y, v = 3L, seed = 6L),
                                        keep_fits = TRUE, verbose = FALSE))
  parts <- lapply(c("permute", "fold_mean", "unit_mean"), function(s)
    occlusion(lad, x, y, "month|elasticnet", substitute = s, permutations = 3L))
  expect_true(all(vapply(parts, function(p) identical(p$part, parts[[1L]]$part), logical(1L))))
  expect_true(all(vapply(parts, function(p) all(is.finite(p$weight)), logical(1L))))
})

test_that("the profile is read by the metric the fit was scored under", {
  sim <- planted_series(n_unit = 40L, seed = 64L)
  y <- matrix(stats::rbinom(length(sim$warmth) * 2L, 1L,
                            stats::plogis(3 * c(sim$warmth, -sim$warmth))),
              ncol = 2L, dimnames = list(sim$units, c("sp1", "sp2")))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month")
  folds <- fold_map(y, v = 3L, seed = 6L)
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(), folds = folds, metric = "tss",
                                        keep_fits = TRUE, verbose = FALSE))

  # The ladder was scored by TSS, so a weight is a fall in TSS unless another metric is named.
  under_tss <- occlusion(lad, x, y, "month|elasticnet", permutations = 3L, seed = 4L)
  expect_identical(attr(under_tss, "metric"), "tss")
  named <- occlusion(lad, x, y, "month|elasticnet", metric = "tss", permutations = 3L, seed = 4L)
  expect_equal(under_tss$weight, named$weight)

  under_auc <- occlusion(lad, x, y, "month|elasticnet", metric = "roc_auc",
                         permutations = 3L, seed = 4L)
  expect_identical(attr(under_auc, "metric"), "roc_auc")
  expect_false(isTRUE(all.equal(under_tss$weight, under_auc$weight)))

  # A run reaches the same profile through its own door, under its own metric.
  run <- suppressWarnings(timesift(
    data.frame(plot = sim$units, y, stringsAsFactors = FALSE), sim$readings,
    y = c("sp1", "sp2"), id = plot, time = t, x = temp, models = elasticnet(),
    sift = grains("month"), resampling = folds, metric = "tss", ensemble = FALSE,
    keep_fits = TRUE, verbose = FALSE))
  through_run <- occlusion(run, "elasticnet / month", permutations = 3L, seed = 4L)
  expect_identical(attr(through_run, "metric"), "tss")
  expect_equal(through_run$weight, under_tss$weight)
})

test_that("holding a channel back asks what the statistic carries", {
  sim <- planted_series(n_unit = 40L, seed = 63L)
  y <- matrix(stats::rbinom(length(sim$warmth) * 2L, 1L,
                            stats::plogis(3 * c(sim$warmth, -sim$warmth))),
              ncol = 2L, dimnames = list(sim$units, c("sp1", "sp2")))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month",
                     stats = c("cold_day", "mean", "warm_day"))
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(),
                                        folds = fold_map(y, v = 3L, seed = 6L),
                                        keep_fits = TRUE, verbose = FALSE))
  oc <- occlusion(lad, x, y, "month|elasticnet", over = "channel", permutations = 3L)
  expect_setequal(unique(oc$part), c("cold_day", "mean", "warm_day"))
})

test_that("a channel the model does not read carries no weight, and the one it reads does", {
  sim <- planted_series(n_unit = 40L, seed = 66L)
  y <- matrix(stats::rbinom(length(sim$warmth) * 2L, 1L,
                            stats::plogis(3 * c(sim$warmth, -sim$warmth))),
              ncol = 2L, dimnames = list(sim$units, c("sp1", "sp2")))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month",
                     stats = c("cold_day", "mean", "warm_day"))
  # A model that reads the warmest day and nothing else, so what every channel should cost is
  # known exactly.
  warm_only <- learner(
    name = "warm_only", multi = "joint",
    fit = function(x, y, ...) list(),
    predict = function(model, x) {
      s <- rowMeans(matrix(x[, , "warm_day"], nrow = dim(x)[1L]))
      cbind(s, -s)
    })
  lad <- grain_ladder(x, y, list(warm_only = warm_only), folds = fold_map(y, v = 4L, seed = 6L),
                      keep_fits = TRUE, verbose = FALSE)
  for (s in c("permute", "fold_mean", "unit_mean")) {
    oc <- occlusion(lad, x, y, "month|warm_only", over = "channel", substitute = s,
                    permutations = 5L, seed = 4L)
    weight <- vapply(split(oc$weight, oc$part), mean, numeric(1L))
    expect_identical(unname(weight[c("cold_day", "mean")]), c(0, 0), info = s)
    # The model reads the warmest day averaged over the record, which is what a unit's own mean
    # keeps, so under that substitute holding it back costs nothing either.
    if (s == "unit_mean") {
      expect_equal(weight[["warm_day"]], 0, info = s)
    } else {
      expect_gt(weight[["warm_day"]], 0.05)
    }
  }
})

test_that("a held-back bin moves whole and leaves the calendar where it is", {
  sim <- planted_series(n_unit = 20L, seed = 65L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month",
                     stats = c("cold_day", "mean", "warm_day"))
  x <- bind_channels(x, calendar_channels(x))
  held <- timesift:::.unit_varying(x)
  expect_identical(dimnames(x)[[3L]][held], c("cold_day", "mean", "warm_day"))
  test <- 1:10
  train <- 11:20
  set.seed(1)
  for (s in c("permute", "fold_mean", "unit_mean")) {
    out <- timesift:::.occlude(x, test, train, 3L, "bin", s, held)
    expect_identical(out[, , c("year_sin", "year_cos")], x[test, , c("year_sin", "year_cos")],
                     info = s)
    expect_identical(out[, -3L, ], x[test, -3L, ], info = s)
  }
  # Every unit is shown one unit's whole bin, never channels drawn from different units.
  out <- timesift:::.occlude(x, test, train, 3L, "bin", "permute", held)
  shown <- out[, 3L, held]
  source <- x[test, 3L, held]
  from <- vapply(seq_len(nrow(shown)), function(u) {
    hit <- which(apply(source, 1L, function(v) identical(unname(v), unname(shown[u, ]))))
    if (length(hit) == 1L) hit else NA_integer_
  }, integer(1L))
  expect_false(anyNA(from))
  expect_setequal(from, seq_along(test))
})
