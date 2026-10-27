test_that("a simulation is reproducible from its seed and its draw", {
  a <- simulate_records(n = 30L, mechanism = "event", variables = 3L, days = 40L, seed = 7L)
  b <- simulate_records(n = 30L, mechanism = "event", variables = 3L, days = 40L, seed = 7L)
  expect_identical(a$readings, b$readings)
  expect_identical(a$y, b$y)
  expect_identical(a$driver, b$driver)
})

test_that("two draws share a design and draw different units", {
  a <- simulate_records(n = 30L, mechanism = "lag", variables = 3L, days = 90L, seed = 7L, draw = 1L)
  b <- simulate_records(n = 40L, mechanism = "lag", variables = 3L, days = 90L, seed = 7L, draw = 2L)
  expect_identical(a$weights, b$weights)
  expect_identical(a$link, b$link)
  expect_identical(a$grain, b$grain)
  expect_equal(length(intersect(rownames(a$y), rownames(b$y))), 0L)
  expect_equal(nrow(b$y), 40L)
})

test_that("the seed is left as it was found", {
  set.seed(99L)
  before <- .Random.seed
  simulate_records(n = 20L, mechanism = "season", variables = 2L, days = 120L)
  expect_identical(.Random.seed, before)
})

test_that("prevalence and the driver's skill are set rather than emergent", {
  sim <- simulate_records(n = 4000L, mechanism = "event", variables = 4L, days = 60L,
                          prevalence = 0.15, auc = 0.8, seed = 3L)
  expect_equal(mean(sim$y), 0.15, tolerance = 0.05)
  expect_equal(mean(sim$driver), 0, tolerance = 0.05)
  expect_equal(stats::sd(sim$driver), 1, tolerance = 0.05)
  got <- vapply(colnames(sim$y), function(v) roc_auc(sim$y[, v], sim$driver[, v]), numeric(1L))
  expect_equal(unname(got), rep(0.8, 4L), tolerance = 0.03)
})

test_that("the readings are a gapless grid every grain can be built on", {
  sim <- simulate_records(n = 12L, mechanism = "none", variables = 2L, days = 365L)
  expect_equal(nrow(sim$readings), 12L * 365L * 8L)
  for (w in c("halfday", "day", "week", "month", "season", "year")) {
    expect_s3_class(grain_matrix(sim$readings, unit, time, reading, grain = w),
                    "timesift_matrix")
  }
})

test_that("the mechanisms report the grain they are generated at", {
  expect_true(is.na(simulate_records(n = 10L, mechanism = "none", days = 40L)$grain))
  expect_identical(simulate_records(n = 10L, mechanism = "event", days = 40L)$grain, "day")
  expect_identical(simulate_records(n = 10L, mechanism = "lag", days = 120L)$grain, "week")
  expect_identical(simulate_records(n = 10L, mechanism = "season", days = 200L)$grain, "season")
})

# The oracle of the generator: apply the planted weights, projected onto a grain's bins, to that
# grain's representation and take out what every bin shares, which removes the unit's constant
# offset. Nothing is fitted, so what this measures is how much of the driver a grain can carry at
# best rather than how much a learner found.
oracle_recovery <- function(sim, grain) {
  m <- grain_matrix(sim$readings, "unit", "time", "reading", grain = grain, stats = "mean",
                     year_start = sim$design$year_start)
  bin <- findInterval(as.numeric(sort(unique(sim$readings$time))),
                      as.numeric(attr(m, "bin_start")))
  count <- tabulate(bin, nbins = dim(m)[2L])
  flat <- matrix(as.numeric(m[, , 1L]), nrow = dim(m)[1L])
  vapply(seq_len(ncol(sim$driver)), function(j) {
    k <- as.numeric(rowsum(sim$weights[, j], bin, reorder = TRUE))
    recon <- as.numeric(flat %*% (k - count / sum(count)))
    if (stats::sd(recon) < .Machine$double.eps) 0 else abs(stats::cor(recon, sim$driver[, j]))
  }, numeric(1L))
}

test_that("the planted signal is recoverable at its own grain and lost at coarser ones", {
  cases <- list(
    list(mechanism = "event", days = 200L, grain = "day", coarse = c("month", "year"),
         nesting = "halfday"),
    list(mechanism = "lag", days = 200L, grain = "week", coarse = c("season", "year"),
         nesting = "day"),
    list(mechanism = "season", days = 365L, grain = "season", coarse = "year",
         nesting = "month")
  )
  for (case in cases) {
    sim <- simulate_records(n = 400L, mechanism = case$mechanism, variables = 4L,
                            days = case$days, sensor_sd = 0.05, seed = 11L)
    expect_identical(sim$grain, case$grain)
    at_grain <- oracle_recovery(sim, case$grain)
    expect_gt(min(at_grain), 0.8)
    # A grain whose bins nest inside the generating one holds the same information, so it must not
    # lose any: that is what makes the choice between them a question about variance, not about
    # what survived the reduction.
    expect_gt(min(oracle_recovery(sim, case$nesting)), min(at_grain) - 0.02)
    for (w in case$coarse) {
      expect_lt(max(oracle_recovery(sim, w)), min(at_grain) - 0.2)
    }
  }
})

test_that("no grain recovers anything when there is no temporal signal", {
  sim <- simulate_records(n = 400L, mechanism = "none", variables = 4L, days = 200L, seed = 11L)
  for (w in c("halfday", "day", "week", "month")) {
    expect_lt(max(oracle_recovery(sim, w)), 0.2)
  }
})

test_that("the offset carries the response only when it is asked to", {
  free <- simulate_records(n = 600L, mechanism = "event", variables = 3L, days = 120L,
                           offset_effect = 0, seed = 5L)
  tied <- simulate_records(n = 600L, mechanism = "event", variables = 3L, days = 120L,
                           offset_effect = 1, seed = 5L)
  at_year <- function(sim) {
    m <- grain_matrix(sim$readings, "unit", "time", "reading", grain = "year", stats = "mean")
    vapply(seq_len(ncol(sim$driver)),
           function(j) abs(stats::cor(m[, 1L, 1L], sim$driver[, j])), numeric(1L))
  }
  expect_lt(max(at_year(free)), 0.25)
  expect_gt(min(at_year(tied)), 0.6)
})

test_that("a learner finds the planted grain end to end", {
  sim <- simulate_records(n = 300L, mechanism = "event", variables = 4L, days = 120L,
                          auc = 0.9, sensor_sd = 0.05, seed = 13L)
  x <- grain_matrix(sim$readings, unit, time, reading, grain = c("day", "year"), stats = "mean")
  lad <- grain_ladder(x, sim$y, elasticnet(), folds = fold_map(sim$y, v = 3L),
                       metric = "roc_auc", verbose = FALSE)
  g <- summary(lad)
  expect_gt(g$score[g$grain == "day"], 0.6)
  expect_gt(g$score[g$grain == "day"], g$score[g$grain == "year"])
})

test_that("the generator refuses a design it cannot place", {
  expect_error(simulate_records(n = 10L, mechanism = "cold"), "should be one of")
  expect_error(simulate_records(n = 10L, mechanism = "season", days = 20L), "no run of")
  expect_error(simulate_records(n = 10L, prevalence = 0), "strictly between 0 and 1")
  expect_error(simulate_records(n = 10L, auc = 0.4), "strictly between 0.5 and 1")
  expect_error(simulate_records(n = 10L, step_hours = 5), "must divide 24")
  expect_error(simulate_records(n = 1L), "at least 2")
})
