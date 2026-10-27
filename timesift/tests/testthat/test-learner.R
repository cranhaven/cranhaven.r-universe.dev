test_that("a learner of one's own needs nothing but a fit and a predict", {
  mine <- learner(
    "mine",
    fit = function(x, y, ...) colMeans(y),
    predict = function(model, x) matrix(rep(model, each = dim(x)[1L]), nrow = dim(x)[1L])
  )
  sim <- sim_series(n_unit = 20L, days = 30L)
  y <- sim_response(sim, n_var = 2L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  fit <- fit_learner(mine, x, y)
  p <- stats::predict(fit, x)
  expect_equal(dim(p), c(20L, 2L))
  expect_equal(colnames(p), colnames(y))
  expect_equal(rownames(p), rownames(y))
})

test_that("a registered learner can be asked for by name", {
  expect_true(all(c("elasticnet", "stepwise", "forest", "mlp", "cnn", "rescnn") %in% learners()))
  expect_s3_class(.as_learner("elasticnet"), "timesift_learner")
  expect_error(.as_learner("nope"), "unknown learner")
  register_learner("test_only", function() learner("test_only",
                                                  fit = function(x, y, ...) NULL,
                                                  predict = function(model, x) NULL),
                   overwrite = TRUE)
  expect_true("test_only" %in% learners())
  expect_error(register_learner("test_only", function() NULL), "already registered")
})

test_that("a learner that cannot run says so instead of doing something else", {
  needy <- learner("needy", fit = function(x, y, ...) NULL,
                   predict = function(model, x) NULL,
                   needs = "a.package.that.does.not.exist")
  sim <- sim_series(n_unit = 10L, days = 10L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  expect_error(fit_learner(needy, x, sim_response(sim)),
               "needs a.package.that.does.not.exist")
})

test_that("the response is matched to the representation by unit, not by position", {
  sim <- sim_series(n_unit = 20L, days = 30L)
  y <- sim_response(sim, n_var = 2L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  seen <- NULL
  spy <- learner("spy",
                 fit = function(x, y, ...) {
                   seen <<- y
                   colMeans(y)
                 },
                 predict = function(model, x)
                   matrix(rep(model, each = dim(x)[1L]), nrow = dim(x)[1L]))
  invisible(fit_learner(spy, x, y[nrow(y):1, , drop = FALSE]))
  expect_equal(rownames(seen), dimnames(x)[[1]])
  expect_equal(seen, y)
  expect_error(fit_learner(spy, x, y[-1, , drop = FALSE]), "not the response")
})

test_that("flattening names every predictor by its bin and its channel", {
  sim <- sim_series(n_unit = 5L, days = 20L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week", stats = c("mean", "max"))
  m <- .flatten(x)
  expect_equal(dim(m), c(5L, dim(x)[2] * 2L))
  expect_equal(m[, 1], x[, 1, "mean"])
  expect_true(all(grepl("^mean@|^max@", colnames(m))))
})

test_that("a scaler is computed on what it is given and applied to something else", {
  m <- matrix(c(1, 2, 3, 10, 20, 30), ncol = 2)
  s <- .scaler(m)
  expect_equal(s$centre, c(2, 20))
  expect_equal(.apply_scaler(s, m)[, 1], c(-1, 0, 1))
  constant <- .scaler(matrix(1, nrow = 4, ncol = 2))
  expect_equal(constant$scale, c(1, 1))
})

test_that("the penalised learner fits, predicts and refuses a different representation", {
  sim <- sim_series(n_unit = 60L, days = 60L, seed = 31L)
  y <- sim_response(sim, n_var = 2L, seed = 32L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  fit <- fit_learner(elasticnet(), x, y)
  p <- stats::predict(fit, x)
  expect_true(all(p >= 0 & p <= 1))
  expect_gt(tss(y[, 1], p[, 1]), 0.4)
  other <- grain_matrix(sim$readings, plot, t, temp, grain = "month")
  expect_error(stats::predict(fit, other), "different channels or bins")
})

test_that("the inner folds spread a rare outcome, and one too rare to choose a penalty on is named", {
  # Five presences over five inner folds: a plain deal puts two in one fold about four draws in
  # five, the stratified one puts one in each.
  yj <- c(rep(1, 5L), rep(0, 45L))
  inner <- .inner_folds(yj, 5L, 11L)
  expect_equal(as.integer(tapply(yj, inner, sum)), rep(1L, 5L))
  expect_true(.inner_fittable(yj, inner))
  expect_false(.inner_fittable(c(1, 1, rep(0, 48L)), .inner_folds(c(1, 1, rep(0, 48L)), 5L, 11L)))

  sim <- sim_series(n_unit = 50L, days = 40L, seed = 35L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  units <- dimnames(x)[[1L]]
  y <- cbind(common = rep(c(1, 0), 25L), rare = c(1, 1, rep(0, 48L)))
  rownames(y) <- units
  # The common response carries no signal, so the far end of its path is nearly flat. What is
  # asserted is that the rare response no longer stops the fit.
  fit <- suppressWarnings(fit_learner(elasticnet(), x, y))
  expect_identical(fit$model$unfitted, "rare")
  p <- stats::predict(fit, x)
  expect_equal(unname(p[, "rare"]), rep(2 / 50, 50L))
})

test_that("forward selection stops at its budget and is non-monotone in a predictor", {
  sim <- sim_series(n_unit = 60L, days = 60L, seed = 33L)
  y <- sim_response(sim, n_var = 1L, seed = 34L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month")
  fit <- fit_learner(stepwise(max_terms = 2L), x, y)
  chosen <- fit$model$models[[1L]]$columns
  expect_lte(length(chosen), 2L)
  p <- stats::predict(fit, x)
  expect_true(all(p >= 0 & p <= 1))
})

test_that("a column holding one value is not offered to the forward search", {
  local_response("constant_continuous_test", list(
    prepare = function(y) .as_response(y), activation = "identity",
    loss = "squared_error", metric = "roc_auc",
    cells = function(y, folds) scorable_cells(y > stats::median(y), folds)))

  sim <- sim_series(n_unit = 40L, days = 60L, seed = 37L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month",
                    stats = c("min", "mean", "max"))
  # A channel every unit reads the same value on: what a sensor that never moved records, or a
  # predictor that is constant over the units this fold holds.
  flat <- x
  flat[, , "min"] <- 7

  binary <- sim_response(sim, n_var = 1L, seed = 38L)
  level <- matrix(10 + 3 * scale(rowMeans(x[, , "mean"]))[, 1L], ncol = 1L,
                  dimnames = list(dimnames(x)[[1L]], "height"))
  for (arm in list(list(y = binary, response = "presence_absence"),
                   list(y = level, response = "constant_continuous_test"))) {
    fit <- fit_learner(stepwise(max_terms = 2L), flat, arm$y, response = arm$response)
    chosen <- fit$model$models[[1L]]$columns
    expect_gt(length(chosen), 0L)
    expect_false(any(grepl("min", fit$model$columns[chosen], fixed = TRUE)), info = arm$response)
    expect_equal(dim(stats::predict(fit, flat)), c(40L, 1L), info = arm$response)
  }

  expect_error(.poly_basis(rep(7, 20), 2L), "no polynomial basis")
})

test_that("the penalised learner reports what its fitter refuses instead of a constant", {
  sim <- sim_series(n_unit = 40L, days = 60L, seed = 39L)
  y <- sim_response(sim, n_var = 1L, seed = 40L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "month")
  # One inner fold cannot be cross-validated over, and the fit says so. Swallowed, it would have
  # come back as a constant predictor and been scored and stacked as a candidate.
  expect_error(fit_learner(elasticnet(n_inner = 1L), x, y))
})

test_that("a forest fits, predicts and refuses a different representation", {
  skip_if_not_installed("ranger")
  sim <- sim_series(n_unit = 60L, days = 60L, seed = 35L)
  y <- sim_response(sim, n_var = 2L, seed = 36L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  fit <- fit_learner(forest(trees = 200L), x, y)
  p <- stats::predict(fit, x)
  expect_equal(dim(p), c(60L, 2L))
  expect_true(all(p >= 0 & p <= 1))
  expect_gt(tss(y[, 1], p[, 1]), 0.4)
  other <- grain_matrix(sim$readings, plot, t, temp, grain = "month")
  expect_error(stats::predict(fit, other), "different channels or bins")
})

test_that("a forest is the same forest twice and reads the same columns as the linear arms", {
  skip_if_not_installed("ranger")
  sim <- sim_series(n_unit = 40L, days = 40L, seed = 37L)
  y <- sim_response(sim, n_var = 1L, seed = 38L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week", stats = c("mean", "max"))
  a <- stats::predict(fit_learner(forest(trees = 100L, seed = 7L), x, y), x)
  b <- stats::predict(fit_learner(forest(trees = 100L, seed = 7L), x, y), x)
  expect_equal(a, b)
  expect_equal(fit_learner(forest(trees = 50L), x, y)$model$columns, colnames(.flatten(x)))
})

test_that("a learner declares what it reads, how it covers responses and what it is pinned to", {
  expect_equal(vapply(list(elasticnet(), stepwise(), forest()), function(l) l$reads, character(1L)),
               rep("tabular", 3L))
  expect_equal(vapply(list(elasticnet(), stepwise(), forest()), function(l) l$multi, character(1L)),
               rep("separate", 3L))
  expect_equal(vapply(list(mlp(), cnn(), rescnn()), function(l) l$multi, character(1L)),
               rep("joint", 3L))
  expect_equal(vapply(list(mlp(), cnn(), rescnn()), function(l) l$reads, character(1L)),
               c("tabular", "sequence", "sequence"))
  expect_null(elasticnet()$data)

  weekly <- structure(list(label = "week", kind = "grain", grain = "week", stats = "mean",
                           sequence = TRUE),
                      class = "timesift_representation")
  expect_identical(cnn(data = weekly)$data, weekly)
  expect_identical(elasticnet(data = weekly)$data, weekly)
  expect_error(cnn(data = "week"), "is a representation")
  expect_output(print(cnn(data = weekly)), "sequence")
  expect_output(print(cnn(data = weekly)), "week")
  expect_output(print(elasticnet()), "every representation of the run")
})

test_that("the old learner names are gone", {
  for (nm in c("elasticnet_learner", "stepwise_learner", "mlp_learner", "cnn_learner",
               "rescnn_learner", "ensemble_learner")) {
    expect_false(nm %in% getNamespaceExports("timesift"), info = nm)
  }
  expect_true(all(c("elasticnet", "stepwise", "forest", "mlp", "cnn", "rescnn") %in% learners()))
  expect_false("ensemble" %in% learners())
})

test_that("a training setting is defaulted in the control and nowhere else", {
  cfg <- train_control()
  expect_s3_class(cfg, "timesift_control")
  expect_equal(cfg$epochs, 60L)
  expect_equal(cfg$batch_size, 64L)
  expect_equal(cfg$learning_rate, 1e-3)
  expect_equal(cfg$weight_decay, 1e-4)
  expect_equal(cfg$early_stopping, 10L)
  expect_equal(cfg$val_frac, 0)
  expect_equal(cfg$device, "auto")
  expect_equal(cfg$seed, 1L)
  expect_error(cfg$lr, "no setting called lr")
  expect_error(train_control(epochs = 0L), "positive")
  expect_error(train_control(val_frac = 1), "share of the run")
  expect_output(print(cfg), "epochs")

  # Only the settings a control names move; the rest come from the control below it.
  run <- train_control(epochs = 100L, batch_size = 16L)
  own <- train_control(epochs = 200L)
  merged <- .resolve_control(run, own)
  expect_equal(merged$epochs, 200L)
  expect_equal(merged$batch_size, 16L)
  expect_equal(merged$val_frac, train_control()$val_frac)
})

test_that("an architecture constructor carries architecture and its training settings separately", {
  l <- cnn(channels = c(8L, 16L), epochs = 5L)
  expect_equal(l$params$channels, c(8L, 16L))
  expect_null(l$params$epochs)
  expect_equal(l$control$epochs, 5L)
  expect_equal(attr(l$control, "given"), "epochs")
  expect_null(cnn()$control)
  expect_error(cnn(epocs = 5L), "no setting called epocs")
  expect_error(mlp(hiden = 4L), "no setting called hiden")
})

test_that("a setting given at fit time overrides the one a linear learner carries", {
  sim <- sim_series(n_unit = 40L, days = 40L, seed = 43L)
  y <- sim_response(sim, n_var = 1L, seed = 44L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  overridden <- suppressWarnings(fit_learner(elasticnet(), x, y, squares = FALSE))
  built <- suppressWarnings(fit_learner(elasticnet(squares = FALSE), x, y))
  expect_false(overridden$model$squares)
  expect_equal(suppressWarnings(stats::predict(overridden, x)),
               suppressWarnings(stats::predict(built, x)))
})

test_that("predicting a single unit returns one row and not one column", {
  sim <- sim_series(n_unit = 24L, days = 40L)
  y <- sim_response(sim, n_var = 3L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  one <- x[1L, , , drop = FALSE]
  attributes(one) <- utils::modifyList(attributes(x)[c("grain", "stats", "year_start",
                                                       "bin_start", "bin_end", "bin_partial")],
                                       list(dim = dim(one), dimnames = dimnames(one),
                                            class = c("timesift_matrix", "array")))
  for (l in list(elasticnet(), stepwise())) {
    fit <- suppressWarnings(fit_learner(l, x, y))
    p <- stats::predict(fit, one)
    expect_equal(dim(p), c(1L, 3L))
    expect_identical(rownames(p), dimnames(x)[[1L]][1L])
  }
})

test_that("the per-response learners fit the family the response head's loss names", {
  skip_if_not_installed("ranger")
  local_response("continuous_test", list(
    prepare = function(y) .as_response(y), activation = "identity",
    loss = "squared_error", metric = "roc_auc",
    cells = function(y, folds) scorable_cells(y > stats::median(y), folds)))
  sim <- sim_series(n_unit = 40L, days = 60L, seed = 91L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  level <- 10 + 3 * scale(rowMeans(x[, , 1L]))[, 1L]
  y <- matrix(level, ncol = 1L, dimnames = list(dimnames(x)[[1L]], "height"))
  for (l in list(elasticnet(squares = FALSE), forest(trees = 100L), stepwise(max_terms = 1L))) {
    fit <- fit_learner(l, x, y, response = "continuous_test")
    p <- stats::predict(fit, x)
    expect_true(all(p > 1), info = l$name)
    expect_gt(stats::cor(p[, 1L], y[, 1L]), 0.8, label = l$name)
  }
  expect_equal(fit_learner(forest(trees = 20L), x, y, response = "continuous_test")$model$family,
               "gaussian")
  expect_equal(fit_learner(stepwise(max_terms = 1L), x, y, response = "continuous_test")$model$family,
               "gaussian")

  local_response("poisson_test", list(
    prepare = function(y) .as_response(y), activation = "exp", loss = "poisson",
    metric = "roc_auc", cells = function(y, folds) scorable_cells(y, folds)))
  expect_error(fit_learner(stepwise(), x, y, response = "poisson_test"), "no family for the poisson")
})

test_that("a learner's fit is handed the head only where it declares one", {
  seen <- NULL
  plain <- learner("plain", fit = function(x, y, ...) { seen <<- names(list(...)); 1 },
                   predict = function(model, x) matrix(0.5, nrow = dim(x)[1L], ncol = 1L))
  sim <- sim_series(n_unit = 10L, days = 20L, seed = 92L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  y <- sim_response(sim, n_var = 1L, seed = 93L)
  fit_learner(plain, x, y)
  expect_false("head" %in% seen)
  aware <- learner("aware", fit = function(x, y, head, ...) head$loss,
                   predict = function(model, x) matrix(0.5, nrow = dim(x)[1L], ncol = 1L))
  expect_equal(fit_learner(aware, x, y)$model, "binary_cross_entropy")
})

# ---- what a fit carries of the learner that made it -------------------------------------------

toy_registered <- function(shift = 0) {
  learner("toy_ref",
          fit = function(x, y, shift, ...) list(rate = colMeans(y), shift = shift),
          predict = function(model, x) {
            outer(rep(1, dim(x)[1L]), model$rate + model$shift)
          },
          params = list(shift = shift))
}

fit_case <- function() {
  sim <- sim_series(n_unit = 20L, days = 40L, seed = 61L)
  list(x = grain_matrix(sim$readings, plot, t, temp, grain = "week"),
       y = sim_response(sim, n_var = 2L, seed = 62L))
}

test_that("a fit made by a registered learner refers to it rather than carrying its code", {
  local_learner("toy_ref", toy_registered)
  case <- fit_case()
  fit <- fit_learner(toy_registered(shift = 0.1), case$x, case$y)
  expect_s3_class(fit$learner, "timesift_learner_ref")
  expect_false(any(c("fit", "predict") %in% names(fit$learner)))
  expect_identical(fit$learner$name, "toy_ref")
  expect_equal(fit$learner$params$shift, 0.1)
  expect_output(print(fit$learner), "toy_ref")
})

test_that("a fit predicts through the registry rather than through the code it was made with", {
  local_learner("toy_ref", toy_registered)
  case <- fit_case()
  fit <- fit_learner(toy_registered(), case$x, case$y)
  before <- stats::predict(fit, case$x)
  # The registration is replaced by one whose predict answers differently. A fit carrying a copy of
  # the code that made it would go on predicting the old way; one that refers to the registry
  # reads whatever is registered now, which is what a package upgrade looks like from inside a
  # saved fit.
  local_learner("toy_ref", function() {
    learner("toy_ref",
            fit = function(x, y, shift, ...) list(rate = colMeans(y), shift = shift),
            predict = function(model, x) outer(rep(1, dim(x)[1L]), model$rate) + 1,
            params = list(shift = 0))
  })
  expect_equal(stats::predict(fit, case$x), before + 1)
})

test_that("a learner defined outside any registry travels whole", {
  case <- fit_case()
  # Not registered, and the name of a registered one is not enough to make it one: the two
  # functions have to be the ones the registry writes.
  local_learner("toy_ref", toy_registered)
  mine <- learner("toy_ref",
                  fit = function(x, y, ...) list(rate = colMeans(y)),
                  predict = function(model, x) outer(rep(1, dim(x)[1L]), model$rate) - 1)
  fit <- fit_learner(mine, case$x, case$y)
  expect_s3_class(fit$learner, "timesift_learner")
  expect_true(is.function(fit$learner$predict))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(fit, path)
  expect_equal(stats::predict(readRDS(path), case$x), stats::predict(fit, case$x))
})

test_that("a fit whose learner is no longer registered says so by name", {
  local_learner("toy_ref", toy_registered)
  case <- fit_case()
  fit <- fit_learner(toy_registered(), case$x, case$y)
  .learners_reg$remove("toy_ref")
  expect_error(stats::predict(fit, case$x), "toy_ref")
  expect_error(stats::predict(fit, case$x), "not registered")
})

test_that("a fit read back through its reference keeps the settings held at NULL", {
  skip_if_not_installed("ranger")
  case <- fit_case()
  fit <- fit_learner(forest(trees = 20L), case$x, case$y)
  rebuilt <- .as_learner(fit$learner)
  expect_true("mtry" %in% names(rebuilt$params))
  expect_null(rebuilt$params$mtry)
  again <- fit_learner(rebuilt, case$x, case$y)
  expect_equal(stats::predict(again, case$x), stats::predict(fit, case$x))
})

test_that("the penalised learner refuses a design of one column in its own words", {
  sim <- sim_series(n_unit = 20L, days = 40L, seed = 61L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "year")
  y <- sim_response(sim, n_var = 2L, seed = 62L)
  expect_error(fit_learner(elasticnet(squares = FALSE), x, y), "at least two columns")
})

test_that("a calendar-grain fit refuses a record from another period, naming the bin", {
  # Same number of weekly bins, six months later: read by position, week 1 of March would be
  # week 1 of September. The check is one, on the fit, so every learner is held to it.
  sim <- sim_series(n_unit = 12L, days = 42L, seed = 41L)
  later <- sim_series(n_unit = 12L, days = 42L, seed = 41L, from = "2022-03-01")
  y <- sim_response(sim, n_var = 2L, seed = 42L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  shifted <- grain_matrix(later$readings, plot, t, temp, grain = "week")
  expect_equal(dim(shifted), dim(x))
  toy <- learner("toy", fit = function(x, y, ...) colMeans(y),
                 predict = function(model, x) matrix(model, nrow = dim(x)[1L], ncol = length(model),
                                                     byrow = TRUE))
  fit <- fit_learner(toy, x, y)
  expect_error(stats::predict(fit, shifted), "bin 1 is 2022-02-28T00:00:00Z here")
  # A lookback's bins are relative to each target, so another period predicts.
  at <- data.frame(id = sim$units, at = as.POSIXct("2021-10-01", tz = "UTC"),
                   row.names = sim$units)
  at_later <- data.frame(id = later$units, at = as.POSIXct("2022-04-01", tz = "UTC"),
                         row.names = later$units)
  w <- lookback_matrix(sim$readings, plot, t, temp, at = at, span = "14 days", bins = 2L)
  w_later <- lookback_matrix(later$readings, plot, t, temp, at = at_later, span = "14 days",
                             bins = 2L)
  expect_equal(dim(stats::predict(fit_learner(toy, w, y), w_later)), c(12L, 2L))
})

test_that("the head owns what a rare response weighs", {
  y <- cbind(a = c(1, 0, 0, 0, 0, 0), b = c(1, 1, 0, 0, 0, 0))
  w <- positive_weights(y)
  # Five absences to one presence in the first response, four to two in the second: the presence
  # weighs the ratio, at least one, and the absence weighs one.
  expect_equal(unname(w[, "a"]), c(5, 1, 1, 1, 1, 1))
  expect_equal(unname(w[, "b"]), c(2, 2, 1, 1, 1, 1))
  expect_equal(unname(positive_weights(y, cap = 3)[1, "a"]), 3)
  expect_error(positive_weights(y, cap = 0.5), "at least 1")
  # The ratio is read off the fitting rows alone and the weight applies to every row: with the
  # first four rows fitting, the first response has three absences to one presence and the second
  # two to two, and the presences held back weigh the same as the ones fitted on.
  fitting <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  y_held <- cbind(a = c(1, 0, 0, 0, 1, 0), b = c(1, 1, 0, 0, 0, 1))
  w_held <- positive_weights(y_held, fitting = fitting)
  expect_equal(unname(w_held[, "a"]), c(3, 1, 1, 1, 3, 1))
  expect_equal(unname(w_held[, "b"]), c(1, 1, 1, 1, 1, 1))
  expect_error(positive_weights(y_held, fitting = c(TRUE, FALSE)), "one entry per row")
  head <- .responses_reg$get("presence_absence")
  expect_equal(.head_weights(head, y), w)
  expect_equal(.head_weights(head, y_held, fitting = fitting), w_held)
  # A head without `weights` fits unweighted, and a cap of one's own is a registration.
  plain <- head[setdiff(names(head), "weights")]
  expect_equal(unname(.head_weights(plain, y)), matrix(1, 6L, 2L))
  local_response("capped_test", utils::modifyList(
    head, list(weights = function(y, fitting) positive_weights(y, cap = 2, fitting = fitting))))
  expect_equal(unname(.head_weights(.responses_reg$get("capped_test"), y)[1, "a"]), 2)
  expect_error(.head_weights(list(weights = function(y, fitting) rep(1, 3)), y),
               "response's shape")
})

test_that("every shipped learner fits a rare response under the head's weight", {
  skip_if_not_installed("ranger")
  head <- .responses_reg$get("presence_absence")
  local_response("unweighted_test", head[setdiff(names(head), "weights")])
  sim <- sim_series(n_unit = 60L, days = 28L, seed = 71L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  # A rare response drawn on the warmth, so every learner has a signal to read and none of them
  # a column that separates it, which the forward search refuses. Enough presences that every
  # inner fold of the penalised arm holds some of each outcome.
  set.seed(72)
  present <- stats::rbinom(60L, 1L, stats::plogis(3 * sim$warmth - 1.5)) == 1
  stopifnot(sum(present) >= 15L, sum(present) <= 30L)
  rare <- matrix(as.numeric(present), ncol = 1L, dimnames = list(sim$units, "rare"))
  for (make in list(function() elasticnet(n_inner = 3L), function() forest(trees = 30L),
                    function() stepwise(max_terms = 1L))) {
    weighted <- stats::predict(fit_learner(make(), x, rare), x)
    plain <- stats::predict(fit_learner(make(), x, rare, response = "unweighted_test"), x)
    expect_false(isTRUE(all.equal(weighted, plain)), info = make()$name)
    # Presences weigh more than twice an absence, so the weighted fit predicts them higher.
    expect_gt(mean(weighted[present, 1L]), mean(plain[present, 1L]))
  }
})
