test_that("TSS is the maximum over every cut, written out the slow way", {
  set.seed(3)
  for (i in 1:20) {
    n <- sample(8:60, 1L)
    y <- stats::rbinom(n, 1L, 0.35)
    if (length(unique(y)) < 2L) next
    p <- round(stats::runif(n), 2)  # rounding forces ties, which is where a cut rule shows
    expect_equal(tss(y, p), brute_tss(y, p), info = paste("draw", i))
  }
})

test_that("units sharing a prediction are decided together", {
  y <- c(1, 1, 0, 0)
  expect_equal(tss(y, c(0.5, 0.5, 0.5, 0.5)), 0)
  expect_equal(tss(y, c(0.9, 0.5, 0.5, 0.1)), 0.5)
})

test_that("the score does not depend on the order the units arrived in", {
  set.seed(4)
  y <- stats::rbinom(50, 1L, 0.3)
  p <- round(stats::runif(50), 2)
  o <- sample(50)
  expect_equal(tss(y, p), tss(y[o], p[o]))
  expect_equal(roc_auc(y, p), roc_auc(y[o], p[o]))
})

test_that("a one-class cell has no skill to measure", {
  expect_true(is.na(tss(c(0, 0, 0), c(0.1, 0.2, 0.3))))
  expect_true(is.na(tss(c(1, 1, 1), c(0.1, 0.2, 0.3))))
  expect_true(is.na(roc_auc(c(0, 0, 0), c(0.1, 0.2, 0.3))))
  expect_true(is.na(kappa_score(c(0, 0, 0), c(0.1, 0.2, 0.3))))
})

test_that("a perfect and a reversed ranking score the way they earned", {
  y <- c(0, 0, 1, 1)
  expect_equal(tss(y, c(0.1, 0.2, 0.8, 0.9)), 1)
  expect_equal(roc_auc(y, c(0.1, 0.2, 0.8, 0.9)), 1)
  expect_equal(roc_auc(y, c(0.9, 0.8, 0.2, 0.1)), 0)
  expect_equal(tss(y, c(0.9, 0.8, 0.2, 0.1)), 0)
})

test_that("the ROC area is the rank sum of the presences", {
  set.seed(5)
  y <- stats::rbinom(40, 1L, 0.4)
  p <- stats::runif(40)
  pairs <- outer(p[y == 1L], p[y == 0L], ">") + 0.5 * outer(p[y == 1L], p[y == 0L], "==")
  expect_equal(roc_auc(y, p), mean(pairs))
})

test_that("kappa recovers what its two-by-two table says", {
  y <- c(rep(1L, 30), rep(0L, 70))
  p <- c(rep(0.9, 25), rep(0.1, 5), rep(0.9, 10), rep(0.1, 60))
  # 25 both, 60 neither, 10 predicted-only, 5 observed-only
  po <- 0.85
  pe <- (35 * 30 + 65 * 70) / 100^2
  expect_equal(kappa_score(y, p, "prevalence"), (po - pe) / (1 - pe))
})

test_that("the prevalence cut predicts as many presences as were observed", {
  set.seed(6)
  y <- stats::rbinom(200, 1L, 0.25)
  p <- stats::runif(200)
  thr <- decision_threshold(y, p, "prevalence")
  expect_equal(sum(p >= thr), sum(y), tolerance = 1)
})

test_that("agreement counts the decisions two models make differently", {
  y <- c(1, 1, 0, 0)
  same <- model_agreement(y, c(0.9, 0.8, 0.2, 0.1), c(0.7, 0.6, 0.3, 0.2), "prevalence")
  expect_equal(same$n_disagree, 0)
  expect_equal(same$kappa, 1)
  apart <- model_agreement(y, c(0.9, 0.8, 0.2, 0.1), c(0.1, 0.2, 0.8, 0.9), "prevalence")
  expect_equal(apart$n_disagree, 4)
  expect_equal(apart$a_right, 4)
  expect_equal(apart$b_right, 0)
})

test_that("the registered metrics are the ones the package ships", {
  expect_true(all(c("tss", "roc_auc", "kappa") %in% metrics()))
  expect_equal(.metrics_reg$get("tss")(c(0, 1), c(0.1, 0.9)), 1)
  expect_error(.metrics_reg$get("nope"), "unknown metric")
})

# ---- the registry a metric reaches a run through ----------------------------------------------

test_that("a registered metric is listed and reachable by name", {
  expect_false("hit_rate" %in% metrics())
  local_metric("hit_rate", function(y, p) mean((p >= 0.5) == (y == 1)))
  expect_true("hit_rate" %in% metrics())
  # C collation, so the list is the same order on every machine and beside Python's.
  expect_equal(metrics(), sort(metrics(), method = "radix"))
  expect_equal(.metrics_reg$get("hit_rate")(c(0, 1), c(0.2, 0.9)), 1)
})

test_that("a metric is a function of (y, p) and a name is not re-registered by accident", {
  expect_error(register_metric("nonsense", "tss"), "function of (y, p)", fixed = TRUE)
  local_metric("hit_rate", function(y, p) 1)
  expect_error(register_metric("hit_rate", function(y, p) 0), "already registered")
  expect_silent(register_metric("hit_rate", function(y, p) 0, overwrite = TRUE))
})

test_that("a ladder scores by a registered metric named at the call", {
  local_metric("prevalence_gap", function(y, p) mean(p) - mean(y))
  sim <- sim_series(n_unit = 30L, days = 40L, seed = 41L)
  y <- sim_response(sim, n_var = 2L, seed = 42L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  ladder <- learner("half", fit = function(x, y, ...) list(n = ncol(y)),
                    predict = function(model, x) matrix(0.5, nrow = dim(x)[1L], ncol = model$n))
  lad <- grain_ladder(x, y, ladder, folds = fold_map(y, v = 3L, seed = 4L),
                       metric = "prevalence_gap", verbose = FALSE)
  expect_equal(attr(lad, "metric"), "prevalence_gap")
  expect_true(all(lad$score[lad$scorable] < 0.5))
  expect_error(grain_ladder(x, y, ladder, folds = fold_map(y, v = 3L), metric = "nope",
                             verbose = FALSE),
               "unknown metric")
})

# ---- scoring predictions that came from somewhere else -----------------------------------------

test_that("score_predictions() scores one row per variable and fold on the mask's cells", {
  set.seed(3)
  y <- matrix(stats::rbinom(120, 1, 0.4), nrow = 30,
              dimnames = list(sprintf("p%02d", 1:30), paste0("sp", 1:4)))
  folds <- fold_map(y, v = 3L, seed = 2L)
  p <- matrix(stats::runif(120), nrow = 30, dimnames = dimnames(y))
  rows <- score_predictions(y, p, folds)
  expect_named(rows, c("variable", "fold", "score", "scorable"))
  expect_equal(nrow(rows), 4L * 3L)
  expect_setequal(rows$variable, colnames(y))
  expect_true(all(is.na(rows$score[!rows$scorable])))
  expect_true(all(!is.na(rows$score[rows$scorable])))
})

test_that("it reads the same cells and the same numbers a ladder arm does", {
  set.seed(4)
  sim <- sim_series(n_unit = 36L, days = 40L, seed = 43L)
  y <- sim_response(sim, n_var = 3L, seed = 44L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  folds <- fold_map(y, v = 3L, seed = 6L)
  arm <- learner("ranker", fit = function(x, y, ...) list(rate = colMeans(y)),
                 predict = function(model, x) {
                   outer(rank(apply(x[, , 1, drop = FALSE], 1L, mean)) / dim(x)[1L], model$rate,
                         function(a, b) a)
                 })
  lad <- grain_ladder(x, y, arm, folds = folds, verbose = FALSE)
  again <- score_predictions(y, attr(lad, "predictions")[["week|ranker"]], folds)
  key <- order(again$variable, again$fold, method = "radix")
  ladder_key <- order(lad$variable, lad$fold, method = "radix")
  expect_equal(again$score[key], lad$score[ladder_key])
  expect_equal(again$scorable[key], lad$scorable[ladder_key])
})

test_that("it takes a mask and a metric of its own", {
  set.seed(5)
  y <- matrix(stats::rbinom(90, 1, 0.4), nrow = 30,
              dimnames = list(sprintf("p%02d", 1:30), paste0("sp", 1:3)))
  folds <- fold_map(y, v = 3L, seed = 2L)
  p <- matrix(stats::runif(90), nrow = 30, dimnames = dimnames(y))
  cells <- scorable_cells(y, folds)
  cells$scorable <- FALSE
  blocked <- score_predictions(y, p, folds, cells = cells)
  expect_true(all(!blocked$scorable))
  expect_true(all(is.na(blocked$score)))
  by_auc <- score_predictions(y, p, folds, metric = "roc_auc")
  by_tss <- score_predictions(y, p, folds, metric = "tss")
  expect_false(isTRUE(all.equal(by_auc$score, by_tss$score)))
  expect_error(score_predictions(y, p, folds, metric = "nope"), "unknown metric")
})

test_that("a response that is not 0/1 is refused rather than truncated to it", {
  p <- c(0.1, 0.2, 0.8, 0.9)
  for (fn in list(tss, roc_auc, kappa_score)) {
    expect_error(fn(c(0.4, 0.6, 1, 1), p), "presence-absence")
  }
  expect_error(model_agreement(c(0.4, 0.6, 1, 1), p, rev(p)), "presence-absence")
  expect_equal(tss(c(FALSE, FALSE, TRUE, TRUE), p), 1)
})
