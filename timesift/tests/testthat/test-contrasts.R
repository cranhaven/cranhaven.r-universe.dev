skip_if_no_mixed_model <- function() {
  for (p in c("lme4", "lmerTest", "emmeans")) skip_if_not_installed(p)
}

contrast_fixture <- function() {
  sim <- sim_series(n_unit = 60L, days = 90L, sd = 6, seed = 51L)
  y <- sim_response(sim, n_var = 6L, seed = 52L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("day", "week", "month"))
  suppressWarnings(grain_ladder(x, y, elasticnet(), folds = fold_map(y, v = 4L, seed = 9L),
                                 verbose = FALSE))
}

test_that("every grain is compared against the reference, and the reference is not", {
  skip_if_no_mixed_model()
  lad <- contrast_fixture()
  out <- grain_contrasts(lad)
  best <- summary(lad)$grain[summary(lad)$best]
  expect_equal(nrow(out), 2L)
  expect_false(best %in% out$grain)
  expect_setequal(out$grain, setdiff(unique(lad$grain), best))
  expect_true(all(out$reference == best))
  expect_true(all(out$lower <= out$diff & out$diff <= out$upper))
  expect_true(all(out$diff <= 0))
})

test_that("a reference of one's own is honoured", {
  skip_if_no_mixed_model()
  lad <- contrast_fixture()
  out <- grain_contrasts(lad, reference = "day")
  expect_true(all(out$reference == "day"))
  expect_setequal(out$grain, c("week", "month"))
})

test_that("a contrast says which learner it needs and which grains it has", {
  skip_if_no_mixed_model()
  lad <- contrast_fixture()
  expect_error(grain_contrasts(lad, reference = "fortnight"), "not a grain")
  expect_error(grain_contrasts(lad["day" == lad$grain, ]), "at least two grains")
})

test_that("a ladder with no scored cell for a learner names the missing reference", {
  skip_if_not_installed("lmerTest")
  lad <- data.frame(grain = rep(c("week", "month"), each = 4L), learner = "l",
                    variable = rep(c("a", "a", "b", "b"), 2L), fold = rep(1:2, 4L),
                    score = NA_real_, scorable = FALSE, stringsAsFactors = FALSE)
  lad <- structure(lad, class = c("timesift_ladder", "data.frame"), metric = "tss")
  expect_error(grain_contrasts(lad, learner = "l"), "no scored cell")
})

# A ladder whose per-cell scores are a variable's level, a fold's level, a known effect of each
# grain and noise, which is the model grain_contrasts() fits, so its answer is known.
planted_ladder <- function(effect, n_var = 30L, n_fold = 5L, sd = 0.03, seed = 1L) {
  set.seed(seed)
  grains <- names(effect)
  cells <- expand.grid(variable = sprintf("v%02d", seq_len(n_var)), fold = seq_len(n_fold),
                       grain = grains, stringsAsFactors = FALSE)
  level <- stats::setNames(stats::rnorm(n_var, 0.7, 0.08), sprintf("v%02d", seq_len(n_var)))
  shift <- stats::rnorm(n_fold, 0, 0.02)
  cells$score <- level[cells$variable] + shift[cells$fold] + effect[cells$grain] +
    stats::rnorm(nrow(cells), 0, sd)
  cells$learner <- "l"
  cells$scorable <- TRUE
  structure(cells[c("grain", "learner", "variable", "fold", "score", "scorable")],
            class = c("timesift_ladder", "data.frame"), metric = "roc_auc")
}

test_that("the contrast recovers a planted effect of each grain, at its stated coverage", {
  skip_if_no_mixed_model()
  effect <- c(week = 0, day = -0.02, month = -0.05)
  covered <- 0L
  for (seed in 1:20) {
    out <- grain_contrasts(planted_ladder(effect, seed = seed), reference = "week")
    truth <- effect[out$grain]
    expect_true(all(abs(out$diff - truth) < 0.02), info = seed)
    covered <- covered + sum(out$lower <= truth & truth <= out$upper)
  }
  # Two contrasts in each of twenty ladders, each interval at 95 percent jointly.
  expect_gte(covered, 36L)
})

test_that("a ladder past emmeans' default limit of 3000 observations is read the same way", {
  skip_if_no_mixed_model()
  effect <- c(week = 0, day = -0.02, month = -0.05, season = -0.03)
  lad <- planted_ladder(effect, n_var = 101L, n_fold = 10L, seed = 5L)
  expect_gt(nrow(lad), 3000L)
  out <- grain_contrasts(lad, reference = "week")
  expect_named(out, c("learner", "grain", "reference", "diff", "lower", "upper", "p_value"))
  expect_true(all(is.finite(c(out$lower, out$upper))))
  expect_lt(max(abs(out$diff - effect[out$grain])), 0.01)
})

test_that("with no effect of the grain the contrast finds none", {
  skip_if_no_mixed_model()
  rejected <- 0L
  for (seed in 1:20) {
    out <- grain_contrasts(planted_ladder(c(week = 0, day = 0, month = 0), seed = seed),
                           reference = "week")
    rejected <- rejected + any(out$p_value < 0.05)
  }
  expect_lte(rejected, 3L)
})

test_that("a grain is named as the ladder names it, whatever characters it holds", {
  skip_if_no_mixed_model()
  effect <- c("week.extremeday" = 0, "month-mean" = -0.03, "a (b)" = -0.06, "day" = -0.01)
  out <- grain_contrasts(planted_ladder(effect, seed = 3L), reference = "week.extremeday")
  expect_setequal(out$grain, c("month-mean", "a (b)", "day"))
  got <- out$diff[match(c("month-mean", "a (b)", "day"), out$grain)]
  expect_lt(max(abs(got - c(-0.03, -0.06, -0.01))), 0.02)
})
