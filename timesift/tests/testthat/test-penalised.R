# The penalised core, against the reference the fixtures carry.
#
# These are not digests. Two implementations of a coordinate descent settle at the same point to
# the tolerance they are run at, never to the last bit, so what is asserted is the distance from
# the reference. The reference is glmnet's, which is what the arm the networks are measured
# against has always been, and the Python suite reads the same three files.

penalised_dir <- function() {
  dir <- system.file("spec", "fixtures", package = "timesift")
  if (!nzchar(dir)) skip("the fixtures ship with the package and are not installed here")
  dir
}

penalised_input <- function(dir) {
  d <- utils::read.csv(file.path(dir, "penalised_input.csv"), stringsAsFactors = FALSE,
                       check.names = FALSE)
  held <- c("unit", "y_gaussian", "y_binomial", "w", "fold")
  x <- as.matrix(d[, setdiff(names(d), held), drop = FALSE])
  rownames(x) <- d$unit
  list(x = x, gaussian = d$y_gaussian, binomial = d$y_binomial, w = d$w, fold = d$fold)
}

# The objective both descents minimise, on the scale the reference states it: the mean deviance
# halved for a Gaussian family and the mean negative log likelihood for a binomial one, plus the
# penalty.
penalised_objective <- function(x, y, w, family, alpha, lambda, a0, beta) {
  wn <- w / sum(w)
  eta <- a0 + as.numeric(x %*% beta)
  fit <- if (family == "gaussian") sum(wn * (y - eta)^2) / 2 else
    -sum(wn * (y * eta - log1p(exp(eta))))
  fit + lambda * (alpha * sum(abs(beta)) + (1 - alpha) / 2 * sum(beta^2))
}

penalised_case <- function(input, row) {
  list(y = if (row$family == "binomial") input$binomial else input$gaussian,
       w = if (row$weighted) input$w else rep(1, nrow(input$x)))
}

test_that("the design the penalised fixtures carry is a representation", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  # Every column names the bin and the channel it came from, or is the square of one that does.
  expect_true(all(grepl("^mean@[0-9TZ:-]+(\\^2)?$", colnames(input$x))))
  expect_gt(ncol(input$x), 8L)
  expect_identical(length(unique(input$fold)), 5L)
})

test_that("the penalised path reproduces the reference glmnet gives", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  cases <- utils::read.csv(file.path(dir, "penalised_cases.csv"), stringsAsFactors = FALSE)
  reference <- utils::read.csv(file.path(dir, "penalised_path.csv"), stringsAsFactors = FALSE)
  lengths <- utils::read.csv(file.path(dir, "penalised_cv.csv"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    row <- cases[i, ]
    case <- penalised_case(input, row)
    fit <- .penalised_path(input$x, case$y, case$w, row$family, row$alpha, thresh = row$thresh,
                           max_pass = row$max_pass)
    expect_identical(length(fit$lambda), lengths$n_point[lengths$case == row$case],
                     info = row$case)
    want <- reference[reference$case == row$case, ]
    at <- want$point
    expect_equal(fit$lambda[at], want$lambda, tolerance = 1e-10, info = row$case)
    scale <- max(1, max(abs(fit$beta)))
    expect_equal(fit$a0[at], want$a0, tolerance = row$tolerance * scale, info = row$case)
    expect_equal(unname(fit$beta[, at]),
                 unname(t(as.matrix(want[, paste0("b", seq_len(ncol(input$x)))]))),
                 tolerance = row$tolerance * scale, info = row$case)
    # The objective is what both descents minimise and what the fixture pins tightly. Sitting
    # below the reference is the fit being closer to the optimum than the reference is, which is
    # the direction the arm is allowed to move in; sitting above it is the arm being weakened.
    ours <- vapply(at, function(k) penalised_objective(input$x, case$y, case$w, row$family,
                                                       row$alpha, fit$lambda[k], fit$a0[k],
                                                       fit$beta[, k]), numeric(1L))
    expect_lt(max(ours - want$objective), row$objective_tolerance)
  }
})

test_that("the cross-validated penalty is the one glmnet chooses", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  cases <- utils::read.csv(file.path(dir, "penalised_cases.csv"), stringsAsFactors = FALSE)
  reference <- utils::read.csv(file.path(dir, "penalised_cv.csv"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    row <- cases[i, ]
    case <- penalised_case(input, row)
    want <- reference[reference$case == row$case, ]
    fit <- .penalised_cv(input$x, case$y, case$w, row$family, row$alpha, input$fold, 5L,
                         thresh = row$thresh, max_pass = row$max_pass)
    expect_equal(fit$lambda_min, want$lambda_min, tolerance = 1e-10, info = row$case)
    expect_equal(fit$lambda_1se, want$lambda_1se, tolerance = 1e-10, info = row$case)
    expect_equal(min(fit$cv_mean), want$cv_min, tolerance = row$tolerance, info = row$case)
    expect_equal(fit$cv_sd[which.min(fit$cv_mean)], want$cv_sd_min, tolerance = row$tolerance,
                 info = row$case)
  }
})

test_that("the path's first point leaves every coefficient at zero above a ridge", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  # The largest penalty of the path is the smallest that holds every coefficient down, so an
  # elastic net starts from the null model. A ridge has no threshold to cross and starts from the
  # solution at that penalty, which is the one point of the path glmnet reports differently: it
  # fits that one at a penalty of 9.9e35 and labels it with the smallest that would have done.
  fit <- .penalised_path(input$x, input$binomial, rep(1, nrow(input$x)), "binomial", 0.5)
  expect_true(all(fit$beta[, 1L] == 0))
  expect_identical(fit$df[1L], 0L)
  ridge <- .penalised_path(input$x, input$binomial, rep(1, nrow(input$x)), "binomial", 0)
  expect_true(any(ridge$beta[, 1L] != 0))
  expect_lt(max(abs(ridge$beta[, 1L])), 1e-2)
})

test_that("a fit is read at a named penalty, at one of its own, and refuses anything else", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  fit <- .penalised_cv(input$x, input$binomial, rep(1, nrow(input$x)), "binomial", 0.5,
                       input$fold, 5L)
  at_min <- .penalised_predict(fit, input$x, "lambda.min")
  expect_equal(at_min, .penalised_predict(fit, input$x, fit$lambda_min))
  expect_false(isTRUE(all.equal(at_min, .penalised_predict(fit, input$x, "lambda.1se"))))
  expect_true(all(at_min > 0 & at_min < 1))
  # A penalty between two points of the path is read between their coefficients, so it sits
  # between the two fits rather than at either.
  between <- (fit$lambda[3L] + fit$lambda[4L]) / 2
  mid <- .penalised_coef(fit, between)
  expect_true(all(mid >= pmin(.penalised_coef(fit, fit$lambda[3L]),
                              .penalised_coef(fit, fit$lambda[4L])) - 1e-12))
  expect_named(mid, c("(Intercept)", colnames(input$x)))
  expect_error(.penalised_predict(fit, input$x, "lambda.best"), "lambda.min")
  # A path carries no cross-validation, so it has no named penalty to be read at.
  path <- .penalised_path(input$x, input$binomial, rep(1, nrow(input$x)), "binomial", 0.5)
  expect_error(.penalised_predict(path, input$x, "lambda.min"), "cross-validation")
})

test_that("the penalised core says what it cannot fit rather than fitting something else", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  w <- rep(1, nrow(input$x))
  expect_error(.penalised_path(input$x, rep(1, nrow(input$x)), w, "binomial", 0.5),
               "one outcome")
  expect_error(.penalised_path(input$x, input$gaussian, w, "binomial", 0.5), "zero and one")
  expect_error(.penalised_path(input$x, rep(2, nrow(input$x)), w, "gaussian", 0.5), "one value")
  expect_error(.penalised_path(input$x, input$binomial, w, "poisson", 0.5), "gaussian")
  expect_error(.penalised_path(input$x, input$binomial, w, "binomial", 2), "between zero and one")
  expect_error(.penalised_path(input$x, input$binomial, -w, "binomial", 0.5), "negative")
  expect_error(.penalised_cv(input$x, input$binomial, w, "binomial", 0.5, rep(0L, nrow(input$x)),
                             5L), "every unit or none")
})

test_that("a column holding one value is carried through the penalised fit at zero", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  x <- cbind(input$x, flat = 3)
  fit <- .penalised_path(x, input$binomial, rep(1, nrow(x)), "binomial", 0.5)
  expect_true(all(fit$beta["flat", ] == 0))
  # The column changes nothing else about the fit, which is what says it was left out rather than
  # penalised to zero by a path the rest of the columns then had to share.
  without <- .penalised_path(input$x, input$binomial, rep(1, nrow(input$x)), "binomial", 0.5)
  expect_equal(fit$lambda, without$lambda)
  expect_equal(fit$beta[colnames(input$x), ], without$beta)
})

test_that("a cross-validation on threads returns what one on a single thread returns", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  w <- rep(1, nrow(input$x))
  # The whole-unit path and each fold's path are one independent fit each, reading the design and
  # sharing nothing, so running them at once is a scheduling decision and not a numerical one.
  # Two threads rather than more: a package's own tests do not take a machine's cores.
  serial <- .penalised_cv(input$x, input$binomial, w, "binomial", 0.5, input$fold, 5L)
  threaded <- .penalised_cv(input$x, input$binomial, w, "binomial", 0.5, input$fold, 5L,
                            threads = 2L)
  expect_identical(threaded, serial)
  expect_identical(.penalised_predict(threaded, input$x), .penalised_predict(serial, input$x))
})

test_that("a penalty the fit does not settle at ends the path and keeps the points before it", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  w <- rep(1, nrow(input$x))
  full <- .penalised_path(input$x, input$binomial, w, "binomial", 0.5)
  expect_identical(full$stalled, 0L)
  # A pass budget that runs out halfway down the path is a penalty the fit did not settle at. The
  # budget changes no arithmetic before it runs out, so the points returned are the full path's
  # own first points, to the bit, and the path says which penalty it stopped at.
  cut <- .penalised_path(input$x, input$binomial, w, "binomial", 0.5,
                         max_pass = full$passes %/% 2L)
  k <- length(cut$lambda)
  expect_gt(k, 1L)
  expect_lt(k, length(full$lambda))
  expect_identical(cut$stalled, k + 1L)
  expect_identical(cut$lambda, full$lambda[seq_len(k)])
  expect_identical(cut$a0, full$a0[seq_len(k)])
  expect_identical(cut$beta, full$beta[, seq_len(k), drop = FALSE])
  gaussian <- .penalised_path(input$x, input$gaussian, w, "gaussian", 0.5, max_pass = 20)
  expect_identical(gaussian$stalled, length(gaussian$lambda) + 1L)
  # At the first penalty there is nothing before it to return.
  expect_error(.penalised_path(input$x, input$binomial, w, "binomial", 0.5, max_pass = 0),
               "first penalty")
})

test_that("a cross-validation reads a fold whose path ended early and says which did", {
  dir <- penalised_dir()
  input <- penalised_input(dir)
  w <- rep(1, nrow(input$x))
  full <- .penalised_path(input$x, input$binomial, w, "binomial", 0.5)
  fit <- .penalised_cv(input$x, input$binomial, w, "binomial", 0.5, input$fold, 5L,
                       max_pass = full$passes %/% 2L)
  expect_length(fit$fold_stalled, 5L)
  expect_true(all(fit$fold_stalled > 0L))
  expect_true(all(is.finite(fit$cv_mean)))
  expect_true(fit$lambda_min %in% fit$lambda)
  expect_true(.penalised_stopped(fit))
  settled <- .penalised_cv(input$x, input$binomial, w, "binomial", 0.5, input$fold, 5L)
  expect_identical(settled$fold_stalled, rep(0L, 5L))
  expect_false(.penalised_stopped(settled))
})

test_that("the captured call glmnet does not settle on chooses the penalty cv.glmnet chooses", {
  dir <- penalised_dir()
  d <- utils::read.csv(file.path(dir, "penalised_stall_input.csv"), check.names = FALSE)
  want <- utils::read.csv(file.path(dir, "penalised_stall_cv.csv"))
  x <- as.matrix(d[, setdiff(names(d), c("unit", "y", "w", "fold"))])
  # One inner cross-validation of the selection benchmark (#78), on which glmnet stops two folds'
  # paths early. It stopped the whole run here once; it has to complete, and to choose what
  # cv.glmnet chooses over the same folds.
  fit <- .penalised_cv(x, d$y, d$w, want$family, want$alpha, d$fold, want$n_fold,
                       thresh = want$thresh)
  expect_gt(want$glmnet_stops, 0L)
  expect_equal(fit$lambda_min, want$lambda_min, tolerance = 1e-10)
  expect_equal(fit$lambda_1se, want$lambda_1se, tolerance = 1e-10)
})

test_that("the penalised learner fits over the core and carries no fitter of its own", {
  sim <- sim_series(n_unit = 60L, days = 56L, seed = 91L)
  y <- sim_response(sim, n_var = 2L, seed = 92L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  fit <- fit_learner(elasticnet(), x, y)
  expect_identical(fit$learner$needs, character(0))
  expect_true(all(vapply(fit$model$models, function(f) is.list(f) && !is.null(f$lambda_min),
                         logical(1L))))
  expect_identical(fit$model$stopped, character(0))
  p <- stats::predict(fit, x)
  expect_equal(dim(p), c(60L, 2L))
  expect_true(all(p > 0 & p < 1))
  # A fit is arrays, so it round trips and predicts the same afterwards.
  file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fit, file)
  expect_equal(stats::predict(readRDS(file), x), p)
})

test_that("the penalised learner reads the penalty its `s` names", {
  sim <- sim_series(n_unit = 60L, days = 56L, seed = 93L)
  y <- sim_response(sim, n_var = 1L, seed = 94L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  at_min <- stats::predict(fit_learner(elasticnet(), x, y), x)
  at_1se <- stats::predict(fit_learner(elasticnet(s = "lambda.1se"), x, y), x)
  expect_false(isTRUE(all.equal(at_min, at_1se)))
  # The larger penalty shrinks harder, so its predictions sit closer to the prevalence.
  expect_lt(stats::sd(at_1se), stats::sd(at_min))
})
