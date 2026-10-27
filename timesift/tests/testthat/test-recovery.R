# The claim the package exists to test is that the grain a record is read at changes what a model
# can predict from it. These check that on data where the answer is planted: a signal put at one
# timescale is found at that timescale, and the ladder locates it.

# Units differ from each other only in a slow component. Hourly noise buries it at the fine end and
# averaging recovers it, so skill should rise as the grain widens up to the scale of the signal.
slow_signal <- function(n_unit = 60L, days = 168L, noise = 20, seed = 71L) {
  set.seed(seed)
  t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24L * days)
  units <- sprintf("p%03d", seq_len(n_unit))
  warmth <- stats::rnorm(n_unit)
  value <- as.numeric(vapply(warmth, function(w) {
    w * 1.5 + stats::rnorm(length(t), sd = noise)
  }, numeric(length(t))))
  list(units = units, warmth = warmth,
       readings = data.frame(plot = rep(units, each = length(t)), t = rep(t, times = n_unit),
                             temp = value, stringsAsFactors = FALSE))
}

test_that("a signal buried in hourly noise is found once the record is averaged", {
  sim <- slow_signal()
  sign <- rep(c(1, -1), length.out = 8L)
  y <- matrix(stats::rbinom(length(sim$warmth) * 8L, 1L,
                            stats::plogis(3 * as.numeric(outer(sim$warmth, sign)))),
              ncol = 8L, dimnames = list(sim$units, paste0("sp", 1:8)))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("day", "week", "month"))
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(),
                                        folds = fold_map(y, v = 5L, seed = 7L), verbose = FALSE))
  s <- summary(lad)
  expect_gt(s$score[s$grain == "month"], s$score[s$grain == "day"])
  expect_false(s$best[s$grain == "day"])

  gain <- paired_contrast(lad, "month|elasticnet", "day|elasticnet")
  expect_gt(gain$diff, 0)
  expect_gt(gain$lower, 0)
})

test_that("a response with no cause in the record scores at chance", {
  sim <- slow_signal(seed = 72L)
  set.seed(73)
  y <- matrix(stats::rbinom(length(sim$units) * 3L, 1L, 0.35), ncol = 3L,
              dimnames = list(sim$units, paste0("sp", 1:3)))
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("week", "month"))
  lad <- suppressWarnings(grain_ladder(x, y, elasticnet(),
                                        folds = fold_map(y, v = 5L, seed = 7L), metric = "tss",
                                        verbose = FALSE))
  # TSS read at the cut that maximises it is biased upward on cells this small, so chance is not
  # zero here; the measured inflation is what "chance" means on this design.
  floor <- tss_inflation(y, fold_map(y, v = 5L, seed = 7L), skill = 0,
                         replicates = 60L, seed = 12L)$reported
  expect_lt(max(summary(lad)$score), floor + 0.12)
})

test_that("keeping the extremes of a grain recovers what averaging removed", {
  # Units differ in how cold one day of each week is, and the other six days carry exactly the
  # compensating warmth, so every week has the same mean whatever the unit. The grain mean is then
  # blind to the difference by construction and the grain's coldest day is not.
  set.seed(74)
  t <- seq(as.POSIXct("2021-09-06", tz = "UTC"), by = "hour", length.out = 24L * 140L)
  is_cold_day <- format(t, "%u", tz = "UTC") == "3"
  units <- sprintf("p%03d", 1:80)
  depth <- stats::rnorm(80)
  value <- as.numeric(vapply(depth, function(dd) {
    5 * sin(seq_along(t) / (24 * 40)) + ifelse(is_cold_day, -6 * dd, dd) +
      stats::rnorm(length(t), sd = 0.3)
  }, numeric(length(t))))
  d <- data.frame(plot = rep(units, each = length(t)), t = rep(t, times = 80L), temp = value)
  sign <- rep(c(1, -1), length.out = 6L)
  y <- matrix(stats::rbinom(80L * 6L, 1L, stats::plogis(3 * as.numeric(outer(depth, sign)))),
              ncol = 6L, dimnames = list(units, paste0("sp", 1:6)))

  f <- fold_map(y, v = 4L, seed = 8L)
  set <- timesift_set(list(
    mean = grain_matrix(d, plot, t, temp, grain = "week", stats = "mean"),
    extreme_day = grain_matrix(d, plot, t, temp, grain = "week",
                                stats = c("cold_day", "mean", "warm_day"))))
  lad <- suppressWarnings(grain_ladder(set, y, elasticnet(), folds = f, verbose = FALSE))
  gain <- paired_contrast(lad, "extreme_day|elasticnet", "mean|elasticnet")
  expect_gt(gain$diff, 0)
  expect_gt(gain$lower, 0)
})
