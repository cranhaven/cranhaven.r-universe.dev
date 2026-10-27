skip_if_no_torch <- function() {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed(), "the torch runtime is not installed")
}

torch_fixture <- function(n_unit = 40L, days = 90L) {
  sim <- sim_series(n_unit = n_unit, days = days, seed = 81L)
  list(sim = sim,
       y = sim_response(sim, n_var = 2L, seed = 82L),
       x = grain_matrix(sim$readings, plot, t, temp, grain = "week",
                         stats = c("cold_day", "mean", "warm_day")))
}

test_that("every encoder fits and returns one probability per unit and variable", {
  skip_if_no_torch()
  f <- torch_fixture()
  for (l in list(mlp(epochs = 3L), cnn(epochs = 3L),
                 rescnn(epochs = 3L, channels = c(16L, 32L)))) {
    fit <- fit_learner(l, f$x, f$y)
    p <- stats::predict(fit, f$x)
    expect_equal(dim(p), c(40L, 2L), info = l$name)
    expect_true(all(p > 0 & p < 1), info = l$name)
    expect_equal(rownames(p), rownames(f$y), info = l$name)
  }
})

test_that("the same seed gives the same fit", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 24L, days = 60L)
  a <- stats::predict(fit_learner(cnn(epochs = 3L, seed = 4L), f$x, f$y), f$x)
  b <- stats::predict(fit_learner(cnn(epochs = 3L, seed = 4L), f$x, f$y), f$x)
  expect_equal(a, b)
})

test_that("the stack still runs where the record is one bin per year", {
  skip_if_no_torch()
  sim <- sim_series(n_unit = 24L, days = 400L, seed = 83L)
  y <- sim_response(sim, n_var = 2L, seed = 84L)
  for (w in c("season", "year")) {
    x <- grain_matrix(sim$readings, plot, t, temp, grain = w)
    expect_lte(dim(x)[2L], 5L)
    for (l in list(cnn(epochs = 2L), rescnn(epochs = 2L, channels = c(16L, 32L)))) {
      p <- stats::predict(fit_learner(l, x, y), x)
      expect_equal(dim(p), c(24L, 2L), info = paste(l$name, w))
    }
  }
})

test_that("standardisation is per channel and computed on the units the learner was handed", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 24L, days = 60L)
  half <- seq_len(12L)
  fit <- fit_learner(cnn(epochs = 2L), .subset_units(f$x, half),
                     f$y[half, , drop = FALSE])
  expect_equal(unname(fit$model$scaler$centre),
               vapply(1:3, function(k) mean(f$x[half, , k]), numeric(1L)))
  expect_equal(unname(fit$model$scaler$scale),
               vapply(1:3, function(k) stats::sd(f$x[half, , k]), numeric(1L)))
  expect_false(isTRUE(all.equal(fit$model$scaler$centre[[1L]], mean(f$x[, , 1L]))))
})

test_that("the calendar channels reach an encoder at their own amplitude", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 24L, days = 60L)
  x <- bind_channels(f$x, calendar_channels(f$x))
  fit <- fit_learner(mlp(epochs = 2L), .subset_units(x, seq_len(12L)), f$y[seq_len(12L), ])
  readings <- seq_len(dim(f$x)[3L])
  expect_equal(unname(fit$model$scaler$centre[readings]),
               vapply(readings, function(k) mean(f$x[1:12, , k]), numeric(1L)))
  expect_equal(unname(fit$model$scaler$centre[-readings]), c(0, 0))
  expect_equal(unname(fit$model$scaler$scale[-readings]), c(1, 1))
  expect_true(all(is.finite(stats::predict(fit, x))))
})

test_that("a static predictor appended as a channel is put on the readings' footing", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 24L, days = 60L)
  elevation <- matrix(2000 + 500 * seq_len(24L), ncol = 1L,
                      dimnames = list(dimnames(f$x)[[1L]], "elevation"))
  x <- .append_static(f$x, elevation)
  fit <- fit_learner(cnn(epochs = 2L), x, f$y)
  # Standardising every channel by one global scale would crush the readings to a constant
  # beside metres of elevation; per channel, the readings keep their spread.
  scaled <- .scale_channels(.to_nchw(x), fit$model$scaler)
  expect_gt(stats::sd(scaled[, 1L, ]), 0.5)
  expect_equal(unname(fit$model$scaler$centre[[4L]]), mean(elevation))
  expect_true(all(is.finite(stats::predict(fit, x))))
})

test_that("a snapshot of the weights is a copy and not the storage the optimiser updates", {
  skip_if_no_torch()
  torch <- .torch()
  net <- torch$nn_linear(3L, 1L)
  before <- .snapshot(net)
  opt <- torch$optim_adamw(net$parameters, lr = 0.1)
  for (i in 1:3) {
    opt$zero_grad()
    net(torch$torch_randn(8L, 3L))$pow(2)$mean()$backward()
    opt$step()
  }
  expect_false(isTRUE(all.equal(as.numeric(before$weight), as.numeric(net$weight))))
  net$load_state_dict(before)
  expect_equal(as.numeric(net$weight), as.numeric(before$weight))
})

test_that("the default holds no validation set back and keeps the last epoch", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 30L, days = 60L)
  # A patience of one would stop and restore an earlier epoch wherever a validation loss is read,
  # so a default fit equal to it is one that read none and kept the epoch the budget ended on.
  default <- stats::predict(fit_learner(mlp(epochs = 6L, learning_rate = 0.5, seed = 2L), f$x, f$y),
                            f$x)
  impatient <- stats::predict(fit_learner(mlp(epochs = 6L, learning_rate = 0.5, seed = 2L,
                                              early_stopping = 1L), f$x, f$y), f$x)
  expect_equal(default, impatient)
})

test_that("early stopping restores the best epoch rather than the last one", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 30L, days = 60L)
  # One epoch is fitted, and then the same first epoch followed by more under a learning rate this
  # large, stopping after one epoch without improvement. The weights the second fit restores are
  # either epoch one's, which is the first fit, or a later epoch's that did better on the inner
  # validation loss; the last epoch's, which is what a snapshot aliasing the live weights hands
  # back, would be worse.
  one <- fit_learner(mlp(epochs = 1L, learning_rate = 0.5, seed = 2L, val_frac = 0.3), f$x, f$y)
  stopped <- fit_learner(mlp(epochs = 12L, early_stopping = 1L, learning_rate = 0.5, seed = 2L,
                             val_frac = 0.3), f$x, f$y)
  set.seed(2L)
  val <- .validation_split(f$y, 0.3)
  fit_idx <- setdiff(seq_len(nrow(f$y)), val)
  pos <- colSums(f$y[fit_idx, ])
  w <- pmin(pmax(ifelse(pos > 0, (length(fit_idx) - pos) / pmax(pos, 1), 1), 1), 50)
  loss <- function(fit) {
    p <- pmin(pmax(stats::predict(fit, f$x)[val, , drop = FALSE], 1e-6), 1 - 1e-6)
    yv <- f$y[val, , drop = FALSE]
    -mean(sweep(yv * log(p), 2L, w, "*") + (1 - yv) * log(1 - p))
  }
  expect_lte(loss(stopped), loss(one) * (1 + 1e-5))
})

test_that("a fitted encoder survives saveRDS() and predicts the same after readRDS()", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 24L, days = 60L)
  fit <- fit_learner(rescnn(epochs = 2L, channels = c(8L, 16L)), f$x, f$y)
  p <- stats::predict(fit, f$x)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(fit, path)
  back <- readRDS(path)
  expect_equal(stats::predict(back, f$x), p)
  expect_true(all(vapply(back$model$state, function(e) is.numeric(e$values), logical(1L))))
})

test_that("the encoders train under the loss the response head names", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 40L, days = 60L)
  local_response("continuous_test", list(
    prepare = function(y) .as_response(y), activation = "identity",
    loss = "squared_error", metric = "roc_auc",
    cells = function(y, folds) scorable_cells(y > stats::median(y), folds)))
  level <- 3 * scale(rowMeans(f$x[, , "mean"]))[, 1L]
  y <- matrix(level, ncol = 1L, dimnames = list(dimnames(f$x)[[1L]], "height"))
  fit <- fit_learner(mlp(epochs = 80L, learning_rate = 0.01, seed = 3L, val_frac = 0), f$x, y,
                     response = "continuous_test")
  p <- stats::predict(fit, f$x)
  expect_equal(fit$model$activation, "identity")
  expect_true(any(abs(p) > 1))
  expect_gt(stats::cor(p[, 1L], y[, 1L]), 0.8)
})

test_that("a loss the encoders do not know is refused by name", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 20L, days = 40L)
  local_response("poisson_test", list(
    prepare = function(y) .as_response(y), activation = "exp", loss = "poisson",
    metric = "roc_auc", cells = function(y, folds) scorable_cells(y, folds)))
  expect_error(fit_learner(cnn(epochs = 1L), f$x, f$y, response = "poisson_test"),
               "do not train under the poisson loss")
})

test_that("the averaged weights' batch-norm statistics are the plain mean over the pass", {
  skip_if_no_torch()
  torch <- .torch()
  net <- torch$nn_sequential(torch$nn_batch_norm1d(2L))
  xt <- torch$torch_randn(20L, 2L, 5L) * 4 + 7
  # Leave stale statistics behind, as the last epoch of training does.
  net$train()
  net(xt[1:10, , ] * 0 - 100)
  .refresh_batchnorm(torch, net, xt, seq_len(20L), 5L, "cpu")
  expect_equal(as.numeric(net[[1L]]$running_mean),
               as.numeric(xt$mean(dim = c(1L, 3L))), tolerance = 1e-5)
  expect_equal(net[[1L]]$momentum, 0.1)
})

test_that("an encoder refuses a representation it was not fitted on", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 24L, days = 60L)
  fit <- fit_learner(cnn(epochs = 2L), f$x, f$y)
  other <- grain_matrix(f$sim$readings, plot, t, temp, grain = "month")
  expect_error(stats::predict(fit, other), "different channels or bins")
})

test_that("a fully connected encoder recovers a planted signal", {
  skip_if_no_torch()
  sim <- sim_series(n_unit = 90L, days = 90L, sd = 0.3, level = 3, seed = 85L)
  y <- sim_response(sim, n_var = 2L, strength = 4, seed = 86L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  fit <- fit_learner(mlp(epochs = 40L, seed = 3L), x, y)
  p <- stats::predict(fit, x)
  expect_gt(roc_auc(y[, 1], p[, 1]), 0.8)
})

test_that("weight averaging runs the whole averaging grain and returns a usable fit", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 30L, days = 60L)
  fit <- fit_learner(cnn(epochs = 6L, swa = TRUE, swa_start = 0.5), f$x, f$y)
  p <- stats::predict(fit, f$x)
  expect_equal(dim(p), c(30L, 2L))
  expect_true(all(is.finite(p) & p > 0 & p < 1))
})

test_that("a setting given at fit time overrides the one the learner carries", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 20L, days = 40L)
  wide <- fit_learner(cnn(epochs = 2L), f$x, f$y, channels = c(8L, 16L))
  narrow <- fit_learner(cnn(epochs = 2L, channels = c(8L, 16L)), f$x, f$y)
  expect_equal(stats::predict(wide, f$x), stats::predict(narrow, f$x))
  expect_false(isTRUE(all.equal(stats::predict(wide, f$x),
                                stats::predict(fit_learner(cnn(epochs = 2L), f$x, f$y),
                                               f$x))))
})

test_that("a setting the learner does not have is refused rather than ignored", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 20L, days = 40L)
  expect_error(fit_learner(cnn(epochs = 2L), f$x, f$y, chanels = c(8L, 16L)),
               "no setting called chanels")
})

test_that("the run's control trains the encoder and the encoder's own control overrides it", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 20L, days = 40L)
  # No inner validation, so the six-epoch fit is the sixth epoch and not whichever epoch of the
  # six an inner split happened to prefer, which on twenty units can be the first.
  run <- train_control(epochs = 6L, batch_size = 8L, val_frac = 0)
  # The learner names one setting, so that one moves and batch_size still comes from the run.
  own <- fit_learner(cnn(epochs = 1L), f$x, f$y, control = run)
  short <- fit_learner(cnn(), f$x, f$y,
                       control = train_control(epochs = 1L, batch_size = 8L, val_frac = 0))
  long <- fit_learner(cnn(), f$x, f$y, control = run)
  expect_equal(stats::predict(own, f$x), stats::predict(short, f$x))
  expect_false(isTRUE(all.equal(stats::predict(own, f$x), stats::predict(long, f$x))))
})

test_that("a learner with no training settings of its own is never handed a control", {
  sim <- sim_series(n_unit = 30L, days = 40L, seed = 89L)
  y <- sim_response(sim, n_var = 1L, seed = 90L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  expect_false("control" %in% names(formals(elasticnet()$fit)))
  expect_error(suppressWarnings(fit_learner(elasticnet(), x, y,
                                            control = train_control(epochs = 3L))), NA)
})

test_that("the device is resolved once, from the control", {
  skip_if_no_torch()
  expect_true(.torch_device("auto") %in% c("cuda", "mps", "cpu"))
  expect_equal(.torch_device("cpu"), "cpu")
  expect_equal(train_control()$device, "auto")
})

test_that("the inner validation set is drawn by a plain random permutation", {
  set.seed(1)
  y <- cbind(c(rep(1, 6), rep(0, 54)), 0)
  draws <- lapply(1:200, function(i) sort(.validation_split(y, 1 / 6)))
  expect_true(all(lengths(draws) == 10L))
  expect_gt(length(unique(draws)), 1L)
  # Six presences among sixty units and ten validation units: a stratified draw guaranteed one
  # presence in the set, a plain draw of ten misses them all about a third of the time and
  # catches at least one the rest.
  presence_counts <- vapply(draws, function(val) sum(y[val, 1L]), numeric(1L))
  expect_true(any(presence_counts == 0))
  expect_true(any(presence_counts >= 1))
  expect_length(.validation_split(y, 0), 0L)
  expect_length(.validation_split(y[1:2, , drop = FALSE], 0.5), 0L)
})

test_that("the control ranges are the ones the Python side checks", {
  expect_error(train_control(pos_weight_cap = 50), "unused argument")
  expect_error(train_control(swa_start = 1), "under 1")
  expect_equal(train_control(swa_start = 0)$swa_start, 0)
})

test_that("no training batch holds one row, which batch normalisation cannot standardise", {
  for (n in c(17L, 20L, 33L, 65L)) {
    sizes <- lengths(.batches(seq_len(n), 8L, shuffle = FALSE))
    expect_true(all(sizes > 1L & sizes <= 8L))
  }
  expect_equal(sort(unlist(.batches(seq_len(17L), 8L, shuffle = FALSE), use.names = FALSE)), 1:17)
  # Three rows under a batch size of two would cut into two and one; they stay one batch.
  expect_equal(lengths(.batches(1:3, 2L, shuffle = FALSE), use.names = FALSE), 3L)
  expect_equal(lengths(.batches(1:65, 64L, shuffle = FALSE), use.names = FALSE), c(33L, 32L))
  skip_if_no_torch()
  # Twenty units and a fifteenth held back leaves seventeen to fit on, which fixed-length batches
  # of eight would cut into eight, eight and one.
  f <- torch_fixture(n_unit = 20L, days = 40L)
  fit <- fit_learner(cnn(epochs = 1L), f$x, f$y, control = train_control(batch_size = 8L))
  expect_true(all(is.finite(stats::predict(fit, f$x))))
})

test_that("a fitted encoder names its module builder rather than carrying it", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 20L, days = 40L)
  fit <- fit_learner(cnn(epochs = 2L, channels = c(8L, 16L)), f$x, f$y)
  expect_identical(fit$model$module, "cnn")
  expect_false(any(vapply(fit$model, is.function, logical(1L))))
  # A saved fit rebuilds the network from this version's builder, so a fit that names a builder the
  # package no longer carries says so rather than loading its weights into the wrong architecture.
  broken <- fit
  broken$model$module <- "gru"
  expect_error(stats::predict(broken, f$x), "gru")
})

test_that("a fit leaves torch's random stream where it found it, as it leaves R's", {
  skip_if_no_torch()
  f <- torch_fixture(n_unit = 24L, days = 60L)
  torch::torch_manual_seed(11L)
  before <- as.numeric(torch::torch_rand(3L))
  torch::torch_manual_seed(11L)
  fit_learner(cnn(epochs = 2L, seed = 4L), f$x, f$y)
  after <- as.numeric(torch::torch_rand(3L))
  expect_equal(after, before)
})

test_that("a patience of Inf trains the whole budget", {
  ctrl <- train_control(early_stopping = Inf)
  expect_true(ctrl$early_stopping >= .Machine$integer.max)
  expect_error(train_control(early_stopping = -Inf), "at least one")
})

test_that("the inner validation set keeps a grouping whole, as the outer folds do", {
  set.seed(3)
  y <- cbind(rbinom(40, 1, 0.3), rbinom(40, 1, 0.5))
  group <- rep(sprintf("g%02d", 1:20), each = 2L)
  for (i in 1:10) {
    val <- .validation_split(y, 0.25, group)
    # Five of twenty groups, so ten rows, and never one row of a group without the other.
    expect_length(val, 10L)
    expect_true(all(table(group[val]) == 2L))
  }
})

test_that("the encoders fit under the head's weight", {
  skip_if_no_torch()
  head <- .responses_reg$get("presence_absence")
  local_response("unweighted_test", head[setdiff(names(head), "weights")])
  f <- torch_fixture(n_unit = 30L, days = 28L)
  rare <- matrix(c(rep(1, 3), rep(0, 27)), ncol = 1L, dimnames = list(rownames(f$y), "rare"))
  weighted <- stats::predict(fit_learner(mlp(epochs = 8L, seed = 3L, val_frac = 0), f$x, rare),
                             f$x)
  plain <- stats::predict(fit_learner(mlp(epochs = 8L, seed = 3L, val_frac = 0), f$x, rare,
                                      response = "unweighted_test"), f$x)
  expect_false(isTRUE(all.equal(weighted, plain)))
  expect_gt(mean(weighted[1:3, 1L]), mean(plain[1:3, 1L]))
})
