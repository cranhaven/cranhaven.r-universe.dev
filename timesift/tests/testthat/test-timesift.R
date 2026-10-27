# A learner with no dependency and a deterministic fit, so the fitting layer is what is under
# test. It declares the two fields the layer reads and records how many responses it was handed,
# which is how a joint fit is told from a separate one from the outside.
toy <- function(name = "toy", reads = "tabular", multi = "joint", data = NULL) {
  out <- learner(
    name,
    fit = function(x, y, ...) {
      mu <- apply(x, 1L, mean)
      s <- stats::sd(mu)
      if (!is.finite(s) || s == 0) s <- 1
      z <- (mu - mean(mu)) / s
      beta <- vapply(seq_len(ncol(y)), function(j) {
        if (length(unique(y[, j])) < 2L) 0 else unname(stats::cov(z, y[, j]))
      }, numeric(1L))
      list(centre = mean(mu), scale = s, beta = beta, prevalence = colMeans(y),
           responses = ncol(y), cells = dim(x)[2L] * dim(x)[3L])
    },
    predict = function(model, x) {
      z <- (apply(x, 1L, mean) - model$centre) / model$scale
      stats::plogis(outer(z, 3 * model$beta) +
                      rep(stats::qlogis(pmin(pmax(model$prevalence, 0.05), 0.95)),
                          each = length(z)))
    }
  )
  out$reads <- reads
  out$multi <- multi
  out$data <- data
  out
}

toy_case <- function(n_unit = 24L, days = 90L, n_var = 2L) {
  sim <- sim_series(n_unit = n_unit, days = days)
  y <- sim_response(sim, n_var = n_var)
  targets <- data.frame(plot = sim$units, elevation = seq_len(n_unit) * 10,
                        stringsAsFactors = FALSE)
  targets <- cbind(targets, as.data.frame(y))
  list(targets = targets, series = sim$readings, units = sim$units)
}

run_toy <- function(case, ...) {
  timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
           models = list(toy()), sift = grains("week", "month"), ensemble = FALSE,
           resampling = cv(v = 3L), control = NULL, verbose = FALSE, ...)
}

test_that("a run given c() and a run given list() agree", {
  case <- toy_case()
  by_c <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                   models = c(toy(), toy("toy2")), sift = c(grain("week"), grain("month")),
                   ensemble = FALSE, resampling = cv(v = 3L), verbose = FALSE)
  by_list <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                      models = list(toy(), toy("toy2")),
                      sift = list(grain("week"), grain("month")),
                      ensemble = FALSE, resampling = cv(v = 3L), verbose = FALSE)
  expect_equal(names(by_c$oof), names(by_list$oof))
  expect_equal(by_c$scores, by_list$scores)
  expect_equal(by_c$oof, by_list$oof)
})

test_that("a fit carries every element the layers above it read", {
  fit <- run_toy(toy_case())
  expect_s3_class(fit, "timesift")
  expect_named(fit, c("estimate", "selected", "inner", "fold_weights", "predictions",
                      "candidates", "scores", "oof", "choice", "models", "stack", "weights",
                      "representations", "fits", "folds", "cells", "y", "metric", "scorer",
                      "response", "spec", "call"))
  expect_equal(sort(names(fit$oof)), c("toy / month", "toy / week"))
  expect_equal(names(fit$representations), c("week", "month"))
  expect_s3_class(fit$representations, "timesift_set")
  expect_equal(names(fit$models), names(fit$oof))
  expect_s3_class(fit$folds, "timesift_folds")
  expect_s3_class(fit$cells, "timesift_cells")
  expect_equal(fit$metric, "roc_auc")
  expect_identical(fit$scorer, roc_auc)
  expect_null(fit$stack)
})

test_that("a metric given as a function scores the run and everything that rescores it", {
  case <- toy_case()
  mine <- function(y, p) tss(y, p)
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = list(toy(), toy("toy2")), sift = grains("week", "month"),
                  resampling = cv(v = 3L), metric = mine, verbose = FALSE)
  by_name <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                      models = list(toy(), toy("toy2")), sift = grains("week", "month"),
                      resampling = cv(v = 3L), metric = "tss", verbose = FALSE)

  # The function is what scores, so every number is the registered metric's; only the name differs.
  expect_equal(fit$scores, by_name$scores)
  expect_identical(fit$metric, "<function>")
  expect_false(is.null(fit$stack))

  # The ensemble row is scored with the function the fit carries, not by looking a name up again.
  expect_equal(summary(fit)$mean, summary(by_name)$mean)
  expect_output(print(fit), "<function>", fixed = TRUE)

  # The selection reports the estimate under every registered metric, so its own has to be one.
  y <- as.matrix(case$targets[, c("sp1", "sp2")])
  rownames(y) <- case$targets$plot
  expect_error(select_grain(grain_matrix(case$series, plot, t, temp, grain = "week"),
                            y, list(toy()), metric = mine),
               "has to be registered")
  expect_error(timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                        models = list(toy()), sift = grains("week"), resampling = cv(v = 3L),
                        metric = 3L, verbose = FALSE),
               "name of a registered metric or a function")
})

test_that("the candidate table names the representation, the learner and the array", {
  fit <- run_toy(toy_case())
  expect_equal(names(fit$candidates),
               c("candidate", "representation", "learner", "grain", "bins", "channels",
                 "status", "note"))
  expect_true(all(fit$candidates$status == "fitted"))
  expect_equal(fit$candidates$bins,
               vapply(fit$representations, function(m) dim(m)[2L], integer(1L)),
               ignore_attr = TRUE)
})

test_that("every candidate predicts every target out of fold, on the same folds", {
  case <- toy_case()
  fit <- run_toy(case)
  for (p in fit$oof) {
    expect_equal(dim(p), dim(fit$y))
    expect_equal(rownames(p), sort(case$units))
    expect_false(anyNA(p))
  }
  expect_equal(sort(unique(unclass(fit$folds))), 1:3)
  expect_equal(names(fit$folds), sort(case$units))
})

test_that("the scores are one row per candidate, variable and fold", {
  fit <- run_toy(toy_case())
  expect_equal(names(fit$scores),
               c("candidate", "representation", "learner", "variable", "fold", "score",
                 "scorable"))
  expect_equal(nrow(fit$scores), 2L * ncol(fit$y) * 3L)
  expect_true(all(is.na(fit$scores$score) | fit$scores$scorable))
})

test_that("a learner is handed the whole response matrix however it covers the responses", {
  case <- toy_case()
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = list(one = toy(multi = "separate"), all = toy(multi = "joint")),
                  sift = grains("week"), ensemble = FALSE, resampling = cv(v = 3L),
                  control = NULL, verbose = FALSE, keep_fits = TRUE)
  separate <- fit$fits[["one / week"]][["1"]]
  joint <- fit$fits[["all / week"]][["1"]]
  expect_equal(separate$model$responses, ncol(fit$y))
  expect_equal(joint$model$responses, ncol(fit$y))
  expect_equal(separate$variables, colnames(fit$y))
  expect_equal(dim(fit$oof[["one / week"]]), dim(fit$oof[["all / week"]]))
  expect_equal(colnames(fit$oof[["one / week"]]), colnames(fit$y))
})

test_that("the predictor block is built once for a fit rather than once per response", {
  case <- toy_case()
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L
  counting <- learner(
    "counting", multi = "separate",
    fit = function(x, y, ...) {
      seen$n <- seen$n + 1L
      list(rate = colMeans(y))
    },
    predict = function(model, x) outer(rep(1, dim(x)[1L]), model$rate))
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = counting, sift = grains("week"), ensemble = FALSE,
                  resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  # Three folds and the refit on all targets, whatever the number of responses. Fitting per
  # response inside the layer instead would build the flattened block once for every one of them,
  # which on the published grid is 101 copies of a 47 MB matrix per fold and per arm.
  expect_equal(seen$n, 4L)
  expect_true(ncol(fit$y) > 1L)
})

test_that("a pair no learner can read is skipped, named, and reported as not applicable", {
  case <- toy_case()
  expect_message(
    fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                    models = list(seq = toy(reads = "sequence")),
                    sift = as_sift(list(grain("week"), multigrain(c("month", "season")))),
                    ensemble = FALSE, resampling = cv(v = 3L), control = NULL, verbose = TRUE),
    "multigrain")
  skipped <- fit$candidates[fit$candidates$status == "not applicable", ]
  expect_equal(nrow(skipped), 1L)
  expect_equal(skipped$representation, "multigrain")
  expect_match(skipped$note, "reads a sequence")
  expect_equal(names(fit$oof), "seq / week")
})

test_that("a representation named through data = is an error rather than a skip", {
  case <- toy_case()
  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(toy(data = native())), sift = grains("week"), ensemble = FALSE,
             resampling = cv(v = 3L), control = NULL, verbose = FALSE),
    "one column per reading")
  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(toy(reads = "sequence", data = multigrain(c("month", "season")))),
             sift = grains("week"), ensemble = FALSE, resampling = cv(v = 3L), control = NULL,
             verbose = FALSE),
    "gives one row of features")
})

test_that("a learner pinned to its own representation runs on that one alone", {
  case <- toy_case()
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = list(pinned = toy(data = grain("month")), free = toy()),
                  sift = grains("week"), ensemble = FALSE, resampling = cv(v = 3L),
                  control = NULL, verbose = FALSE)
  expect_equal(sort(names(fit$oof)), c("free / week", "pinned / month"))
  expect_equal(sort(names(fit$representations)), c("month", "week"))
})

test_that("a learner that does not declare what it reads is refused by name", {
  case <- toy_case()
  bare <- toy("bare")
  bare$reads <- NULL
  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(bare), sift = grains("week"), ensemble = FALSE, control = NULL,
             verbose = FALSE),
    "does not declare `reads`")
})

test_that("repeated identifiers without an anchor are refused, naming them", {
  case <- toy_case(n_unit = 6L, days = 40L)
  doubled <- rbind(case$targets, case$targets[1:2, ])
  expect_error(
    timesift(doubled, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(toy()), sift = grains("week"), ensemble = FALSE, control = NULL,
             verbose = FALSE),
    "p001")
})

test_that("an anchored fit refuses a calendar representation and a default set of spans", {
  case <- toy_case(n_unit = 6L, days = 120L)
  case$targets$when <- as.POSIXct(rep("2021-12-01", 6L), tz = "UTC")
  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             target_time = when, models = list(toy()), ensemble = FALSE, control = NULL,
             verbose = FALSE),
    "no default set of spans")
  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             target_time = when, models = list(toy()), sift = grains("week"), ensemble = FALSE,
             control = NULL, verbose = FALSE),
    "follows the calendar")
})

test_that("a lookback without `target_time` is refused, in the sift and pinned alike", {
  case <- toy_case(n_unit = 6L, days = 120L)
  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(toy()), sift = lookbacks("30 days"), ensemble = FALSE,
             control = NULL, verbose = FALSE),
    "`target_time` has to name the column", fixed = TRUE)
  # The same check sees a representation a learner pinned itself to, which no sift carries.
  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(toy(data = lookback("30 days"))), sift = grains("week"),
             ensemble = FALSE, control = NULL, verbose = FALSE),
    "`target_time` has to name the column", fixed = TRUE)
})

test_that("an anchored fit runs across lookback spans", {
  case <- toy_case(n_unit = 8L, days = 150L)
  case$targets$when <- as.POSIXct(rep("2021-12-01", 8L), tz = "UTC")
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  target_time = when, models = list(toy()),
                  sift = lookbacks("30 days", "60 days"), ensemble = FALSE,
                  resampling = cv(v = 3L), inner = NULL, control = NULL, verbose = FALSE)
  expect_equal(sort(names(fit$oof)), c("toy / 30 days", "toy / 60 days"))
  expect_equal(rownames(fit$y), rownames(case$targets))
})

test_that("static is never implicit and never doubles as something else", {
  case <- toy_case(n_unit = 12L, days = 60L)
  plain <- run_toy(case)
  expect_equal(dim(plain$representations$week)[3L], 1L)

  carried <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                      static = elevation, models = list(toy()), sift = grains("week"),
                      ensemble = FALSE, resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  expect_equal(dimnames(carried$representations$week)[[3L]], c("mean", "elevation"))

  expect_error(
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             static = plot, models = list(toy()), sift = grains("week"), ensemble = FALSE,
             control = NULL, verbose = FALSE),
    "already carries")
})

test_that("a targets-only fit is the static block and nothing else", {
  case <- toy_case(n_unit = 20L, days = 30L)
  fit <- timesift(case$targets, y = starts_with("sp"), id = plot, static = elevation,
                  models = list(toy()), sift = grains("week"), ensemble = FALSE,
                  resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  expect_equal(names(fit$representations), "static")
  expect_equal(dim(fit$representations$static), c(20L, 1L, 1L))
  expect_error(
    timesift(case$targets, y = starts_with("sp"), id = plot, models = list(toy()),
             ensemble = FALSE, control = NULL, verbose = FALSE),
    "nothing to fit on")
})

test_that("the response and the readings are named by selection, not by position", {
  case <- toy_case(n_unit = 10L, days = 40L)
  case$series$snow <- as.numeric(case$series$temp < 0)
  fit <- timesift(case$targets, case$series, y = all_of(c("sp1", "sp2")), x = c(temp, snow),
                  id = plot, time = t, models = list(toy()), sift = grains("week"),
                  ensemble = FALSE, resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  expect_equal(colnames(fit$y), c("sp1", "sp2"))
  expect_equal(dimnames(fit$representations$week)[[3L]], c("temp_mean", "snow_mean"))

  bare <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                   models = list(toy()), sift = grains("week"), ensemble = FALSE,
                   resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  expect_equal(dimnames(bare$representations$week)[[3L]], c("temp_mean", "snow_mean"))
  expect_error(
    timesift(case$targets, case$series, y = starts_with("nothing"), id = plot, time = t,
             models = list(toy()), sift = grains("week"), ensemble = FALSE, control = NULL,
             verbose = FALSE),
    "must name the response")
})

test_that("the resampling arrives as a spec, a fold vector or a fold map", {
  case <- toy_case(n_unit = 15L, days = 40L)
  by_vector <- stats::setNames(rep(1:3, length.out = 15L), sort(case$units))
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = list(toy()), sift = grains("week"), ensemble = FALSE,
                  resampling = by_vector, control = NULL, verbose = FALSE)
  expect_equal(as.integer(fit$folds), unname(by_vector))

  grouped <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                      models = list(toy()), sift = grains("week"), ensemble = FALSE,
                      resampling = grouped_cv(rep(1:5, each = 3L), v = 5L), control = NULL,
                      verbose = FALSE)
  expect_equal(length(unique(tapply(unclass(grouped$folds), rep(1:5, each = 3L), length))), 1L)
  expect_true(all(tapply(unclass(grouped$folds), rep(1:5, each = 3L),
                         function(v) length(unique(v))) == 1L))
})

test_that("a split without names is read against the targets as they were given", {
  case <- toy_case(n_unit = 15L, days = 40L)
  reversed <- case$targets[rev(seq_len(nrow(case$targets))), , drop = FALSE]
  given <- rep(1:3, length.out = 15L)
  fit <- timesift(reversed, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = list(toy()), sift = grains("week"), ensemble = FALSE,
                  resampling = given, control = NULL, verbose = FALSE)
  expect_equal(as.integer(fit$folds[reversed$plot]), given)
})

test_that("the combiner minimises the loss of the head the run was fitted under", {
  local_response("gauss_test", list(
    prepare = function(y) .as_response(y), activation = "identity", loss = "squared_error",
    metric = "roc_auc", cells = function(y, folds) scorable_cells(y, folds)))
  case <- toy_case(n_unit = 24L, days = 60L)
  run <- function(...) {
    timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(a = toy(), b = toy(multi = "separate")), sift = grains("week"),
             resampling = cv(v = 3L), control = NULL, verbose = FALSE, ...)
  }

  fit <- run(response = "gauss_test")
  expect_equal(fit$stack$response, "gauss_test")
  expect_equal(fit$stack$loss, "squared_error")
  expect_equal(run()$stack$loss, "binary_cross_entropy")
  # Naming the run's own head is the same thing said twice, and is not a contradiction.
  expect_equal(run(response = "gauss_test",
                   ensemble = ensemble(response = "gauss_test"))$stack$loss, "squared_error")
  expect_error(run(response = "gauss_test", ensemble = ensemble(response = "presence_absence")),
               "names another")
})

test_that("an ensemble is fitted on the out-of-fold predictions and predicts through the refits", {
  case <- toy_case(n_unit = 30L, days = 90L)
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = list(a = toy(), b = toy(multi = "separate")),
                  sift = grains("week"), ensemble = TRUE, resampling = cv(v = 3L),
                  control = NULL, verbose = FALSE)
  expect_s3_class(fit$stack, "timesift_stack")
  expect_equal(sort(names(fit$weights)), sort(names(fit$oof)))
  expect_equal(sum(fit$weights), 1, tolerance = 1e-6)

  p <- predict.timesift(fit, case$targets, case$series)
  expect_equal(dim(p), dim(fit$y))
  one <- predict.timesift(fit, case$targets, case$series, candidate = "a / week")
  expect_equal(dim(one), dim(fit$y))
  expect_equal(one, fit$oof[["a / week"]], tolerance = 0.5)
  expect_error(predict.timesift(fit, case$targets, case$series, candidate = "nobody"),
               "unknown candidate")
})

test_that("a single candidate leaves the ensemble unfitted rather than degenerate", {
  case <- toy_case(n_unit = 12L, days = 40L)
  expect_message(
    fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                    models = list(toy()), sift = grains("week"), ensemble = TRUE,
                    resampling = cv(v = 3L), control = NULL, verbose = TRUE),
    "at least two candidates")
  expect_null(fit$stack)
  expect_null(fit$weights)
})

test_that("a fit prints what it compared", {
  fit <- run_toy(toy_case())
  expect_output(print.timesift(fit), "2 responses")
  expect_output(print.timesift(fit), "toy / week")
})

# ---- predicting targets the fit never saw -------------------------------------------------------

test_that("a fit predicts units it was never fitted on", {
  case <- toy_case(n_unit = 30L, days = 60L)
  fitting <- case$units[1:20]
  fit <- timesift(case$targets[case$targets$plot %in% fitting, , drop = FALSE],
                  case$series[case$series$plot %in% fitting, , drop = FALSE],
                  y = starts_with("sp"), id = plot, time = t, models = list(a = toy(), b = toy("b")),
                  sift = grains("week"), resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  fresh <- case$units[21:30]
  new_targets <- case$targets[case$targets$plot %in% fresh, , drop = FALSE]
  new_series <- case$series[case$series$plot %in% fresh, , drop = FALSE]

  p <- predict(fit, new_targets, new_series)
  expect_equal(dim(p), c(10L, ncol(fit$y)))
  expect_equal(rownames(p), sort(fresh))
  expect_equal(colnames(p), colnames(fit$y))
  expect_true(all(is.finite(p)))
  one <- predict(fit, new_targets, new_series, candidate = "a / week")
  expect_equal(dim(one), dim(p))
})

test_that("predicting needs no response column, because a new target has none", {
  case <- toy_case(n_unit = 24L, days = 60L)
  fit <- run_toy(case)
  bare <- case$targets[, setdiff(names(case$targets), colnames(fit$y)), drop = FALSE]
  expect_false(any(colnames(fit$y) %in% names(bare)))
  expect_equal(predict(fit, bare, case$series, candidate = "toy / week"),
               predict(fit, case$targets, case$series, candidate = "toy / week"))
})

test_that("a new target frame has to carry the static predictors the fit was made with", {
  case <- toy_case(n_unit = 20L, days = 40L)
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  static = elevation, models = list(toy()), sift = grains("week"),
                  ensemble = FALSE, resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  expect_equal(dim(predict(fit, case$targets, case$series, candidate = "toy / week")),
               c(20L, ncol(fit$y)))
  without <- case$targets[, setdiff(names(case$targets), "elevation"), drop = FALSE]
  expect_error(predict(fit, without, case$series, candidate = "toy / week"),
               "does not carry the static predictor elevation", fixed = TRUE)
})

test_that("an anchored fit predicts new targets at their own instants", {
  case <- toy_case(n_unit = 10L, days = 150L)
  case$targets$when <- as.POSIXct(rep("2021-12-01", 10L), tz = "UTC")
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  target_time = when, models = list(toy()), sift = lookbacks("30 days"),
                  ensemble = FALSE, resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  later <- case$targets
  later$when <- as.POSIXct(rep("2022-01-05", 10L), tz = "UTC")
  moved <- predict(fit, later, case$series, candidate = "toy / 30 days")
  same <- predict(fit, case$targets, case$series, candidate = "toy / 30 days")
  expect_equal(dim(moved), c(10L, ncol(fit$y)))
  # A different anchor is a different stretch of record, so the two are not the same prediction.
  expect_false(isTRUE(all.equal(as.numeric(moved), as.numeric(same))))
  expect_error(predict(fit, case$targets[, setdiff(names(case$targets), "when")], case$series,
                       candidate = "toy / 30 days"),
               "`target_time` has to name the column", fixed = TRUE)
})

test_that("a targets-only fit predicts from the static block alone", {
  case <- toy_case(n_unit = 20L, days = 30L)
  fit <- timesift(case$targets, y = starts_with("sp"), id = plot, static = elevation,
                  models = list(a = toy(), b = toy("b")), resampling = cv(v = 3L),
                  control = NULL, verbose = FALSE)
  p <- predict(fit, case$targets)
  expect_equal(dim(p), c(20L, ncol(fit$y)))
  expect_equal(rownames(p), sort(case$units))
  expect_true(all(is.finite(p)))
})

test_that("new targets are held to one row per identifier, as the fit's own were", {
  case <- toy_case(n_unit = 15L, days = 40L)
  fit <- run_toy(case)
  twice <- case$targets[c(1L, 1L, 2L), ]
  expect_error(stats::predict(fit, twice, case$series, candidate = names(fit$models)[1L]),
               "more than one row")
})

test_that("a design under which no cell is scorable stops before anything is fitted", {
  case <- toy_case(n_unit = 15L, days = 40L)
  case$targets$sp1 <- 0
  case$targets$sp2 <- 0
  expect_error(run_toy(case), "no \\(response, fold\\) cell is scorable")
})

test_that("a grouped run hands every fit the grouping of the units it is fitted on", {
  case <- toy_case(n_unit = 8L, days = 150L)
  case$targets <- case$targets[rep(1:8, each = 2L), ]
  rownames(case$targets) <- NULL
  case$targets$when <- as.POSIXct(rep(c("2021-11-01", "2021-12-01"), 8L), tz = "UTC")
  seen <- list()
  recorder <- learner(
    "recorder", reads = "tabular", multi = "joint",
    fit = function(x, y, group = NULL, ...) {
      seen[[length(seen) + 1L]] <<- list(units = dimnames(x)[[1L]], group = group)
      colMeans(y)
    },
    predict = function(model, x) matrix(model, nrow = dim(x)[1L], ncol = length(model),
                                        byrow = TRUE))
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  target_time = when, models = list(recorder), sift = lookbacks("30 days"),
                  ensemble = FALSE, resampling = grouped_cv("plot", v = 4L), control = NULL,
                  verbose = FALSE)
  # Four fold fits and the refit on every target, each handed one group value per unit it was
  # fitted on, and the group is the plot the row belongs to.
  expect_length(seen, 5L)
  for (s in seen) {
    expect_length(s$group, length(s$units))
    expect_equal(s$group, as.character(case$targets$plot[as.integer(s$units)]))
  }
  # A plain run hands NULL, so a fit that draws no split of its own is not told about one.
  seen <- list()
  plain <- toy_case(n_unit = 12L, days = 40L)
  timesift(plain$targets, plain$series, y = starts_with("sp"), id = plot, time = t,
           models = list(recorder), sift = grains("week"), ensemble = FALSE,
           resampling = cv(v = 3L), control = NULL, verbose = FALSE)
  expect_length(seen, 4L)
  expect_true(all(vapply(seen, function(s) is.null(s$group), logical(1L))))
})

# ---- the nested evaluation ----------------------------------------------------------------------

test_that("the selected arm is the selection select_grain() makes on the same split", {
  case <- toy_case(n_unit = 48L, days = 90L, n_var = 3L)
  folds <- stats::setNames(rep(1:3, length.out = 48L), sort(case$units))
  fit <- timesift(case$targets, case$series, y = starts_with("sp"), id = plot, time = t,
                  models = list(toy = toy()), sift = grains("week", "month"), ensemble = FALSE,
                  resampling = folds, inner = 3L, seed = 5L, control = NULL, verbose = FALSE)
  x <- grain_matrix(case$series, plot, t, temp, grain = c("week", "month"))
  sel <- select_grain(x, fit$y, list(toy = toy()), folds = fit$folds, inner = 3L, seed = 5L,
                      metric = "roc_auc", verbose = FALSE)
  expect_equal(fit$selected$representation, sel$selected$grain)
  expect_equal(fit$selected$inner_score, sel$selected$inner_score)
  expect_equal(fit$predictions$selected, attr(sel, "predictions")[["selected|selected"]])
  est <- fit$estimate[fit$estimate$arm == "selected", ]
  expect_equal(est$score, sel$estimate$score[match(est$metric, sel$estimate$metric)])
})

test_that("an outer test fold's responses reach neither its choice nor its weights", {
  case <- toy_case(n_unit = 48L, days = 90L, n_var = 3L)
  folds <- stats::setNames(rep(1:3, length.out = 48L), sort(case$units))
  run <- function(targets) {
    timesift(targets, case$series, y = starts_with("sp"), id = plot, time = t,
             models = list(a = toy(), b = toy("b", multi = "separate")),
             sift = grains("week", "month"), resampling = folds, inner = 3L, control = NULL,
             verbose = FALSE)
  }
  base <- run(case$targets)
  flipped <- case$targets
  held <- flipped$plot %in% names(folds)[folds == 1L]
  for (v in c("sp1", "sp2", "sp3")) flipped[[v]][held] <- 1 - flipped[[v]][held]
  moved <- run(flipped)
  expect_equal(moved$selected[moved$selected$fold == 1L, ],
               base$selected[base$selected$fold == 1L, ])
  expect_equal(moved$fold_weights[moved$fold_weights$fold == 1L, ],
               base$fold_weights[base$fold_weights$fold == 1L, ])
  rows <- names(folds)[folds == 1L]
  expect_equal(moved$predictions$ensemble[rows, ], base$predictions$ensemble[rows, ])
  expect_equal(moved$predictions$selected[rows, ], base$predictions$selected[rows, ])
})

test_that("choosing among candidates that carry no signal is not reported as skill", {
  # Eight candidates predicting noise: the best of them on the outer folds is selected on the
  # folds it is scored on and sits above one half, while the procedure that chooses inside the
  # training folds is scored on folds its choice never saw and centres on one half.
  noise <- function(name, offset) {
    learner(name, reads = "tabular", multi = "joint",
            fit = function(x, y, ...) list(n = ncol(y)),
            predict = function(model, x) {
              code <- lapply(dimnames(x)[[1L]], utf8ToInt)
              key <- vapply(code, function(u) sum(u * seq_along(u)), numeric(1L))
              (sin(outer(key, seq_len(model$n)) * (offset + 1.7)) + 1) / 2
            })
  }
  skip_on_cran()
  models <- stats::setNames(lapply(1:8, function(i) noise(paste0("n", i), i)), paste0("n", 1:8))
  best <- nested <- numeric()
  for (r in 1:20) {
    sim <- sim_series(n_unit = 90L, days = 30L, seed = 300L + r)
    set.seed(400L + r)
    y <- matrix(stats::rbinom(90L * 6L, 1L, 0.4), nrow = 90L,
                dimnames = list(sim$units, paste0("sp", 1:6)))
    targets <- cbind(data.frame(plot = sim$units, stringsAsFactors = FALSE), as.data.frame(y))
    fit <- timesift(targets, sim$readings, y = starts_with("sp"), id = plot, time = t,
                    models = models, sift = grains("week"), ensemble = FALSE,
                    resampling = fold_map(y, v = 5L, seed = r), inner = 4L, control = NULL,
                    verbose = FALSE)
    s <- summary(fit)
    best <- c(best, max(s$mean[s$scored == "outer folds"]))
    nested <- c(nested, s$mean[s$candidate == "selected"])
  }
  # Twenty replicates put the standard error of the nested mean near 0.009 and the selection gain
  # of the best candidate near 0.04.
  se <- stats::sd(nested) / sqrt(length(nested))
  expect_lt(abs(mean(nested) - 0.5), 2.5 * se)
  expect_gt(mean(best - nested), 0.02)
})
