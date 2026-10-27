# A response, a fold map and a set of out-of-fold predictions of known quality. The combiner is
# handed nothing else anywhere in this file, which is the property the boundary exists for.
stack_fixture <- function(n = 240L, n_var = 5L, seed = 91L) {
  set.seed(seed)
  y <- matrix(stats::rbinom(n * n_var, 1L, 0.35), nrow = n,
              dimnames = list(sprintf("t%03d", seq_len(n)), paste0("v", seq_len(n_var))))
  folds <- fold_map(y, v = 4L, seed = 2L)
  blur <- function(strength, sd) {
    p <- 0.5 + strength * (y - 0.5) + matrix(stats::rnorm(n * n_var, sd = sd), nrow = n)
    matrix(pmin(pmax(p, 0.02), 0.98), nrow = n, dimnames = dimnames(y))
  }
  list(y = y, folds = folds, cells = scorable_cells(y, folds),
       oof = list(good = blur(0.7, 0.10), fair = blur(0.7, 0.11), noise = blur(0, 0.25)))
}

# The scores frame a run carries, computed from the same predictions the combiner reads.
stack_scores <- function(f, metric = "tss") {
  fold <- .as_folds(f$folds, rownames(f$y))
  rows <- lapply(names(f$oof), function(nm) {
    out <- .score_arm(nm, nm, f$y, f$oof[[nm]], fold, sort(unique(fold)), f$cells,
                      .metrics_reg$get(metric))
    data.frame(candidate = nm, variable = out$variable, fold = out$fold, score = out$score,
               scorable = out$scorable, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

test_that("the combiner is handed predictions, the response, the mask and the folds only", {
  expect_equal(names(formals(ensemble_fit)), c("oof", "y", "cells", "folds", "spec", "scores"))
  expect_equal(names(formals(ensemble_combine)), c("stack", "preds"))
})

test_that("a specification says what it is and refuses what it is not", {
  spec <- ensemble()
  expect_s3_class(spec, "timesift_ensemble")
  expect_equal(spec$method, "stack")
  expect_equal(spec$scope, "all")
  expect_null(spec$metric)
  expect_error(ensemble("vote"), "should be one of")
  expect_error(ensemble(metric = "nope"), "unknown metric")
  expect_error(ensemble(response = "nope"), "unknown response")
  expect_output(print(ensemble("weighted", scope = "learners")), "weighted")
})

test_that("the fitted weights are non-negative, sum to one and favour the useful member", {
  f <- stack_fixture()
  st <- ensemble_fit(f$oof, f$y, f$cells, f$folds)
  expect_s3_class(st, "timesift_stack")
  expect_equal(st$method, "stack")
  expect_named(st$weights, c("good", "fair", "noise"))
  expect_true(all(st$weights >= 0))
  expect_equal(sum(st$weights), 1)
  expect_lt(st$weights[["noise"]], 0.05)
  expect_gt(st$weights[["good"]] + st$weights[["fair"]], 0.95)
})

test_that("stacking lowers the loss it minimises against every equal-weight alternative", {
  f <- stack_fixture()
  st <- ensemble_fit(f$oof, f$y, f$cells, f$folds)
  loss <- .stack_loss("presence_absence")
  mask <- .scorable_matrix(f$y, f$cells, f$folds)
  block <- .stacking_block(f$oof, f$y, mask)
  equal <- loss$value(as.numeric(block$predictions %*% rep(1 / 3, 3L)), block$response)
  singles <- vapply(seq_len(3L), function(k) {
    w <- numeric(3L)
    w[k] <- 1
    loss$value(as.numeric(block$predictions %*% w), block$response)
  }, numeric(1L))
  expect_lt(st$value, equal)
  expect_lte(st$value, min(singles) + 1e-8)
})

test_that("the solver lands where an exhaustive search over two members lands", {
  f <- stack_fixture(seed = 93L)
  loss <- .stack_loss("presence_absence")
  mask <- .scorable_matrix(f$y, f$cells, f$folds)
  block <- .stacking_block(f$oof[c("good", "fair")], f$y, mask)
  found <- .simplex_weights(block$predictions, block$response, loss)

  grid <- seq(0, 1, by = 0.0005)
  brute <- vapply(grid, function(w) {
    loss$value(as.numeric(block$predictions %*% c(w, 1 - w)), block$response)
  }, numeric(1L))
  expect_lte(found$value, min(brute) + 1e-8)
  expect_lt(abs(found$weights[1L] - grid[which.min(brute)]), 0.02)
  # Two members carrying the same signal under independent noise are both worth keeping.
  expect_gt(min(found$weights), 0.2)
})

test_that("the solver is deterministic and stops on its own", {
  f <- stack_fixture(seed = 94L)
  a <- ensemble_fit(f$oof, f$y, f$cells, f$folds)
  b <- ensemble_fit(f$oof, f$y, f$cells, f$folds)
  expect_identical(a$weights, b$weights)
  expect_gt(a$iterations, 0L)
  expect_lt(a$iterations, 500L)
})

test_that("one member of no use at all is driven towards no weight", {
  f <- stack_fixture(seed = 95L)
  st <- ensemble_fit(f$oof[c("good", "noise")], f$y, f$cells, f$folds)
  expect_gt(st$weights[["good"]], 0.9)
  expect_gte(st$weights[["noise"]], 0)
})

test_that("mean, median and weighted combine without fitting anything", {
  f <- stack_fixture()
  scores <- stack_scores(f)
  plain <- ensemble_fit(f$oof, f$y, f$cells, f$folds, ensemble("mean"))
  expect_equal(unname(plain$weights), rep(1 / 3, 3L))
  expect_true(is.na(plain$value))

  weighted <- ensemble_fit(f$oof, f$y, f$cells, f$folds, ensemble("weighted"), scores)
  expect_equal(sum(weighted$weights), 1)
  expect_gt(weighted$weights[["good"]], weighted$weights[["noise"]])
  expect_error(ensemble_fit(f$oof, f$y, f$cells, f$folds, ensemble("weighted")),
               "Neither was given")

  middle <- ensemble_fit(f$oof, f$y, f$cells, f$folds, ensemble("median"))
  combined <- ensemble_combine(middle, f$oof)
  expect_equal(combined[3L, 2L],
               stats::median(vapply(f$oof, function(p) p[3L, 2L], numeric(1L))))
  expect_equal(dimnames(combined), dimnames(f$y))
})

test_that("a scope narrows the candidates to one side of the grid", {
  f <- stack_fixture()
  named <- stats::setNames(f$oof, c("cnn / week", "cnn / month", "elasticnet / week"))
  scores <- stack_scores(list(y = f$y, folds = f$folds, cells = f$cells, oof = named))

  all_of_them <- ensemble_fit(named, f$y, f$cells, f$folds, ensemble("mean"), scores)
  expect_named(all_of_them$weights, names(named))

  # The best candidate reads the weekly representation, so "learners" keeps what reads that one.
  learners_only <- ensemble_fit(named, f$y, f$cells, f$folds,
                                ensemble("mean", scope = "learners"), scores)
  expect_named(learners_only$weights, c("cnn / week", "elasticnet / week"))

  # and "representations" keeps the one learner across the representations it ran on.
  reps_only <- ensemble_fit(named, f$y, f$cells, f$folds,
                            ensemble("mean", scope = "representations"), scores)
  expect_named(reps_only$weights, c("cnn / week", "cnn / month"))

  expect_error(ensemble_fit(f$oof, f$y, f$cells, f$folds,
                            ensemble("mean", scope = "learners"), stack_scores(f)),
               "learner / representation")
})

test_that("the combiner refuses predictions it was not fitted on", {
  f <- stack_fixture()
  st <- ensemble_fit(f$oof, f$y, f$cells, f$folds)
  expect_error(ensemble_combine(st, f$oof[c("good", "fair")]), "which `preds` does not carry")
  expect_error(ensemble_combine(st, lapply(f$oof, function(p) p[1:10, , drop = FALSE])), NA)
  ragged <- f$oof
  ragged$noise <- ragged$noise[1:10, , drop = FALSE]
  expect_error(ensemble_combine(st, ragged), "same shape")
  expect_error(ensemble_combine(list(weights = c(a = 1)), f$oof), "ensemble_fit")
})

test_that("a candidate predicting other targets than the response carries is refused", {
  f <- stack_fixture()
  wrong <- f$oof
  rownames(wrong$noise) <- paste0("other", seq_len(nrow(f$y)))
  expect_error(ensemble_fit(wrong, f$y, f$cells, f$folds), "other targets or responses")
  bare <- f$oof
  dimnames(bare$noise) <- NULL
  expect_error(ensemble_fit(bare, f$y, f$cells, f$folds), "no target or response names")
})

test_that("the weights come back off a run and off the stack itself", {
  f <- stack_fixture()
  st <- ensemble_fit(f$oof, f$y, f$cells, f$folds)
  expect_identical(ensemble_weights(st), st$weights)
  expect_null(ensemble_weights(structure(list(stack = NULL), class = "timesift")))
  expect_error(ensemble_weights(1), "expected a timesift")
  expect_output(print(st), "timesift stack")
})


# ---- the report, over a run built to the shape the fitted object declares --------------------

# A run assembled from a ladder: real out-of-fold predictions over one fold map, the per-fold fits
# the ladder kept, and one model per candidate refit on every target.
run_fixture <- function(keep_fits = TRUE, stack = TRUE) {
  sim <- sim_series(n_unit = 56L, days = 90L, seed = 96L)
  y <- sim_response(sim, n_var = 4L, seed = 97L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = c("week", "month"))
  folds <- fold_map(y, v = 3L, seed = 4L)
  learners <- list(constant = report_learner(), tilted = report_learner(0.4))
  lad <- grain_ladder(x, y, learners, folds = folds, keep_fits = keep_fits, verbose = FALSE)

  arms <- unique(paste(lad$grain, lad$learner, sep = "|"))
  parts <- strsplit(arms, "|", fixed = TRUE)
  representation <- vapply(parts, `[`, character(1L), 1L)
  learner <- vapply(parts, `[`, character(1L), 2L)
  candidate <- paste(learner, representation, sep = " / ")

  scores <- data.frame(candidate = paste(lad$learner, lad$grain, sep = " / "),
                       variable = lad$variable, fold = lad$fold, score = lad$score,
                       scorable = lad$scorable, stringsAsFactors = FALSE)
  oof <- stats::setNames(attr(lad, "predictions")[arms], candidate)
  models <- stats::setNames(
    lapply(seq_along(arms), function(i) fit_learner(learners[[learner[i]]],
                                                    x[[representation[i]]], y)),
    candidate)
  # One entry per candidate, each a list of that candidate's fits keyed by fold.
  fits <- if (keep_fits) {
    kept <- attr(lad, "fits")
    keys <- strsplit(names(kept), "|", fixed = TRUE)
    owner <- vapply(keys, function(p) paste(p[2L], p[1L], sep = " / "), character(1L))
    fold <- vapply(keys, `[`, character(1L), 3L)
    stats::setNames(lapply(candidate, function(cd) {
      stats::setNames(kept[owner == cd], fold[owner == cd])
    }), candidate)
  } else {
    NULL
  }
  cells <- attr(lad, "cells")
  out <- list(
    candidates = data.frame(
      candidate = candidate, representation = representation, learner = learner,
      grain = representation,
      bins = vapply(representation, function(w) dim(x[[w]])[2L], numeric(1L)),
      channels = vapply(representation, function(w) dim(x[[w]])[3L], numeric(1L)),
      stringsAsFactors = FALSE),
    scores = scores, oof = oof, representations = x,
    stack = if (stack) ensemble_fit(oof, y, cells, folds, ensemble(), scores) else NULL,
    models = models, fits = fits, folds = folds, cells = cells, y = y,
    metric = attr(lad, "metric"), scorer = attr(lad, "scorer"),
    response = attr(lad, "response"),
    spec = NULL, call = NULL)
  out$weights <- if (stack) out$stack$weights else NULL
  structure(out, class = "timesift", ladder = lad, x = x)
}

report_learner <- function(offset = 0) {
  learner(
    "constant", reads = "tabular", multi = "separate",
    fit = function(x, y, ...) list(rate = colMeans(y), offset = offset),
    predict = function(model, x) {
      m <- apply(x[, , 1, drop = FALSE], 1L, mean) + apply(x[, , 1, drop = FALSE], 1L, stats::sd)
      outer(rank(m) / length(m), model$rate, function(a, b) stats::plogis(a - 0.5 + model$offset))
    }
  )
}

# A run through the entry point itself, on the report learners, so the procedure rows are the ones
# the nested evaluation writes.
nested_run <- function(ensemble = TRUE, inner = 3L) {
  sim <- sim_series(n_unit = 60L, days = 90L, seed = 96L)
  y <- sim_response(sim, n_var = 4L, seed = 97L)
  targets <- cbind(data.frame(plot = rownames(y), stringsAsFactors = FALSE), as.data.frame(y))
  timesift(targets, sim$readings, y = starts_with("sp"), id = plot, time = t,
           models = list(constant = report_learner(), tilted = report_learner(0.4)),
           sift = grains("week", "month"), ensemble = ensemble,
           resampling = fold_map(y, v = 3L, seed = 4L), inner = inner, control = NULL,
           verbose = FALSE)
}

test_that("the report is one row per candidate, one per procedure arm, and the weights", {
  fit <- nested_run()
  s <- summary(fit)
  expect_s3_class(s, "timesift_summary")
  expect_named(s, c("candidate", "mean", "se", "won", "responses", "scored"))
  expect_equal(nrow(s), nrow(fit$candidates) + 2L)
  expect_equal(s$candidate[s$scored == "nested"], c("selected", "ensemble"))
  expect_true(all(is.na(s$won[s$scored == "nested"])))
  cand <- s[s$scored == "outer folds", ]
  expect_equal(cand$responses, rep("separate", nrow(fit$candidates)))
  # every response is won by exactly one candidate
  expect_equal(sum(cand$won), ncol(fit$y))
  expect_true(all(diff(cand$mean) >= 0))
  expect_identical(attr(s, "weights"), ensemble_weights(fit))
  expect_identical(attr(s, "choice"), fit$choice)
})

test_that("the procedure rows are the held-out predictions scored on the run's own cells", {
  fit <- nested_run()
  s <- summary(fit)
  f <- .as_folds(fit$folds, rownames(fit$y))
  for (arm in c("selected", "ensemble")) {
    rows <- .score_arm(arm, arm, fit$y, fit$predictions[[arm]], f, sort(unique(f)), fit$cells,
                       fit$scorer)
    expect_equal(s$mean[s$candidate == arm], mean(.cell_means(rows)$score))
    expect_equal(sum(rows$scorable), sum(fit$cells$scorable))
  }
})

test_that("the ensemble is not scored with weights fitted to the responses it is scored on", {
  fit <- nested_run()
  in_sample <- ensemble_combine(fit$stack, fit$oof)
  expect_false(isTRUE(all.equal(in_sample, fit$predictions$ensemble)))
  expect_equal(nrow(fit$fold_weights), length(unique(unclass(fit$folds))))
  expect_equal(rowSums(fit$fold_weights[names(fit$stack$weights)]),
               rep(1, nrow(fit$fold_weights)), tolerance = 1e-6)
})

test_that("a run with no combiner reports its candidates and the selected arm alone", {
  fit <- nested_run(ensemble = FALSE)
  s <- summary(fit)
  expect_equal(nrow(s), nrow(fit$candidates) + 1L)
  expect_false("ensemble" %in% s$candidate)
  expect_null(attr(s, "weights"))
  expect_null(ensemble_weights(fit))
  expect_null(fit$fold_weights)
})

test_that("a run without an inner split compares the candidates and estimates nothing", {
  fit <- nested_run(inner = NULL)
  s <- summary(fit)
  expect_null(fit$estimate)
  expect_null(fit$selected)
  expect_equal(nrow(s), nrow(fit$candidates))
  expect_false(is.null(fit$stack))
  expect_true(is.na(.ensemble_estimate(fit)))
})

test_that("a candidate nothing could be fitted for is listed rather than dropped", {
  fit <- run_fixture(stack = FALSE)
  fit$candidates <- rbind(fit$candidates,
                          data.frame(candidate = "cnn / month", representation = "month",
                                     learner = "cnn", grain = "month", bins = 4, channels = 1,
                                     stringsAsFactors = FALSE))
  s <- summary(fit)
  expect_true("cnn / month" %in% s$candidate)
  expect_true(is.na(s$mean[s$candidate == "cnn / month"]))
  expect_equal(s$won[s$candidate == "cnn / month"], 0L)
  expect_output(print(s), "not applicable")
})

test_that("the report says what the run read and what it found", {
  fit <- nested_run()
  out <- utils::capture.output(print(summary(fit)))
  expect_match(out[1L], "^timesift  60 targets, 4 responses, 3-fold random CV, roc_auc$")
  expect_true(any(grepl("^candidates, scored on the outer folds", out)))
  expect_true(any(grepl("^procedure, chosen and weighted inside each outer training fold", out)))
  expect_true(any(grepl("^weights on every target", out)))
  expect_true(any(grepl("^ensemble", out)))
  expect_true(any(grepl("^selected .* of 3 folds$", out)))
  expect_true(any(grepl("constant / week", out, fixed = TRUE)))
})

test_that("a grouped fold map is reported as one", {
  fit <- run_fixture(stack = FALSE)
  attr(fit$folds, "grouped") <- TRUE
  expect_match(utils::capture.output(print(summary(fit)))[1L], "3-fold grouped CV")
})

test_that("the run plot draws one line per learner across the representations", {
  fit <- run_fixture()
  path <- tempfile(fileext = ".png")
  grDevices::png(path)
  drawn <- plot(fit)
  grDevices::dev.off()
  expect_true(file.exists(path))
  expect_named(drawn, c("learner", "representation", "score", "se"))
  expect_equal(nrow(drawn), 2L * 2L)
  expect_setequal(drawn$representation, c("week", "month"))
  unlink(path)
})

test_that("occlusion on a run is the ladder's occlusion on the same candidate", {
  fit <- run_fixture()
  lad <- attr(fit, "ladder")
  through_run <- occlusion(fit, "constant / month", permutations = 3L, seed = 5L)
  through_ladder <- occlusion(lad, attr(fit, "x"), fit$y, "month|constant",
                                  permutations = 3L, seed = 5L)
  expect_equal(as.data.frame(through_run), as.data.frame(through_ladder))
  expect_s3_class(through_run, "timesift_occlusion")

  by_channel <- occlusion(fit, "constant / month", over = "channel", permutations = 2L)
  expect_equal(unique(by_channel$part), "mean")
})

test_that("occlusion refuses a run that kept no fits and a candidate that was never fitted", {
  fit <- run_fixture(keep_fits = FALSE)
  expect_error(occlusion(fit, "constant / month"), "kept no per-fold fits")
  kept <- run_fixture()
  expect_error(occlusion(kept, "nope / month"), "no candidate called")
  expect_error(occlusion(structure(list(), class = "list"), "a"), "expected a timesift")
})

test_that("the combiner refuses to drop a scorable cell rather than fitting on fewer", {
  f <- stack_fixture()
  mask <- .scorable_matrix(f$y, f$cells, f$folds)
  hit <- which(mask, arr.ind = TRUE)[1L, ]
  f$oof$noise[hit[1L], hit[2L]] <- NaN
  expect_error(ensemble_fit(f$oof, f$y, f$cells, f$folds), "did not settle")
  expect_error(ensemble_fit(f$oof, f$y, f$cells, f$folds), "noise (1)", fixed = TRUE)
})

test_that("a combiner handed no scorable cell says so rather than fitting on nothing", {
  y <- matrix(0, nrow = 6L, ncol = 1L, dimnames = list(paste0("u", 1:6), "sp1"))
  f <- stats::setNames(rep(1:2, 3L), rownames(y))
  cells <- scorable_cells(y, f)
  oof <- list(a = matrix(0.5, 6L, 1L, dimnames = dimnames(y)),
              b = matrix(0.4, 6L, 1L, dimnames = dimnames(y)))
  expect_error(ensemble_fit(oof, y, cells, f), "no cell is scorable")
})
