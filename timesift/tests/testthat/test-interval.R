# The interval for the procedure's risk: the pieces of the estimator, and its coverage on a design
# whose truth is measured rather than assumed.

test_that("the jackknife variance of a mean of per-unit losses is the variance over the count", {
  set.seed(4)
  e <- rnorm(60)
  got <- .jackknife_var(length(e), function(i) mean(e[-i]))
  expect_equal(got, stats::var(e) / length(e))
})

test_that("the interval rescales, bounds and bias-corrects as the paper states", {
  terms <- list(
    terms = data.frame(`repeat` = 1L, fold = 1:5, s_in = c(0.70, 0.72, 0.68, 0.71, 0.69),
                       s_out = c(0.74, 0.70, 0.72, 0.75, 0.69), b = rep(0.0004, 5L),
                       e_hat = c(0.77, 0.66, 0.75, 0.79, 0.65),
                       check.names = FALSE),
    estimate = 0.72, se_naive = 0.02, se_naive_center = 0.025, folds = 5L, n_variable = 10L)
  got <- .ncv_interval(terms)
  t <- terms$terms
  mse_ncv <- mean((t$s_in - t$s_out)^2) - mean(t$b)
  mse_center <- mean((t$e_hat - t$s_out)^2) - mean(t$b)
  expect_equal(got$mse_ncv, mse_ncv)
  expect_equal(got$mse_center, mse_center)
  # Rescaled by (K - 1) / K, then held between a naive standard error and sqrt(K) times it: the
  # width the interval carries from the corrected estimate's error and its own naive standard
  # error, the paper's from the plain estimate's.
  bounded <- function(mse, floor) min(max(sqrt(max(4 / 5 * mse, 0)), floor), sqrt(5) * floor)
  expect_gt(mse_center, 0)
  expect_equal(got$se, bounded(mse_center, 0.025))
  expect_equal(got$se_bates, bounded(mse_ncv, 0.02))
  expect_false(isTRUE(all.equal(got$se, got$se_bates)))
  expect_equal(got$err_ncv, mean(t$s_in))
  expect_equal(got$bias, (1 + 3 / 5) * (mean(t$s_in) - 0.72))
  expect_equal(got$center, mean(t$s_in) - got$bias)
  expect_equal(c(got$lower, got$upper),
               got$center + c(-1, 1) * stats::qnorm(0.975) * got$se)

  # A mean squared error estimated below the naive standard error is raised to it, and one above
  # sqrt(K) times it is cut back, so neither bound can be crossed.
  tiny <- terms
  tiny$terms$s_out <- tiny$terms$s_in
  tiny$terms$e_hat <- tiny$terms$s_in
  expect_equal(.ncv_interval(tiny)$se, 0.025)
  expect_equal(.ncv_interval(tiny)$se_bates, 0.02)
  wide <- terms
  wide$terms$s_out <- wide$terms$s_in + 1
  expect_equal(.ncv_interval(wide)$se, sqrt(5) * 0.025)
  expect_equal(.ncv_interval(wide)$se_bates, sqrt(5) * 0.02)
})

test_that("the corrected centre's naive standard error is the jackknife of the combination", {
  # Two arms of fixed predictions, no refitting: the centre is 1.6 times the outer level less 0.6
  # times the mean inner level, so its leave-one-out values are that combination of theirs.
  set.seed(5)
  n <- 40L
  y <- matrix(rbinom(n * 2L, 1, 0.4), n, 2L, dimnames = list(sprintf("u%02d", 1:n), c("a", "b")))
  m <- stats::setNames(rep(1:5, length.out = n), rownames(y))
  p <- matrix(runif(n * 2L), n, 2L, dimnames = dimnames(y)) + 0.5 * y
  runs <- list(.ncv_collect(function(train, test, tag) p[test, , drop = FALSE], y, list(m), p))
  cells_fun <- function(y, m) scorable_cells(y, stats::setNames(m, rownames(y)))
  terms <- .ncv_terms(y, runs, cells_fun, roc_auc)
  levels <- 1:5
  theta_est <- .level_jackknife(y, list(p), m, .cell_values(y, list(p), m, levels, cells_fun(y, m),
                                                              roc_auc), roc_auc)
  theta_in <- numeric(n)
  for (k in levels) {
    train <- which(m != k)
    inner <- .ncv_level(y[train, ], list(p[train, ]), m[train], setdiff(levels, k), cells_fun,
                        roc_auc)
    left <- rep(inner$level, n)
    left[train] <- .level_jackknife(y[train, ], list(p[train, ]), m[train], inner$values, roc_auc)
    theta_in <- theta_in + left / 5
  }
  expect_equal(terms$se_naive, sqrt(.jackknife_of(theta_est)))
  expect_equal(terms$se_naive_center, sqrt(.jackknife_of(1.6 * theta_est - 0.6 * theta_in)))
})

test_that("the bias-corrected estimate one level down is the paper's formula at one fold fewer", {
  # Four folds in the training set: the innermost cross-validation has three, and the correction
  # scales the gap between the two estimates by 1 + (4 - 2) / 4.
  expect_equal(.ncv_bias(4L, 0.70, 0.72), (1 + 2 / 4) * (0.70 - 0.72))
  expect_equal(.ncv_seed(1L, c(2L, 3L, 4L, 0L)), 1L + 10007L * 2L + 101L * 3L + 4L)
  expect_equal(.ncv_seed(1L, c(2L, 3L, 4L, 5L)), 1L + 10007L * 2L + 101L * 3L + 4L + 3001L * 5L)
})

test_that("the triple fits fill every inner-inner prediction and nothing else", {
  set.seed(3)
  y <- matrix(rbinom(80, 1, 0.4), 40, 2, dimnames = list(sprintf("u%02d", 1:40), c("a", "b")))
  m <- stats::setNames(rep(1:5, length.out = 40), rownames(y))
  seen <- list()
  runs <- .ncv_collect(function(train, test, tag) {
    seen[[length(seen) + 1L]] <<- tag
    matrix(tag[2L] * 100 + tag[3L] * 10 + tag[4L], length(test), 2L,
           dimnames = list(rownames(y)[test], colnames(y)))
  }, y, list(m), matrix(0, 40, 2, dimnames = dimnames(y)))
  tags <- do.call(rbind, seen)
  expect_equal(nrow(tags), choose(5, 2) + choose(5, 3))
  deep <- runs[[1L]]$deep
  expect_equal(names(deep), as.character(1:5))
  for (k in 1:5) {
    expect_equal(names(deep[[k]]), as.character(setdiff(1:5, k)))
    for (j in setdiff(1:5, k)) {
      block <- deep[[as.character(k)]][[as.character(j)]]
      expect_equal(rownames(block), names(m)[m != k & m != j])
      expect_false(anyNA(block))
      # Every prediction of fold l came from the fit that left out exactly k, j and l, whose tag
      # names the three in order.
      for (l in setdiff(1:5, c(k, j))) {
        code <- unname(block[names(m)[m == l], 1L])
        expect_true(all(code == sum(sort(c(k, j, l)) * c(100, 10, 1))))
      }
    }
  }
})

# A cheap design whose truth is known by measurement: each variable's driver is standard normal,
# the response is Bernoulli on it, and the representation the procedure chooses between is that
# driver read through a little noise or through a lot. Twenty thousand further units drawn from
# the same design are what the procedure fitted on the whole sample is scored on, so the interval
# has a number to cover rather than an assumption.
ncv_design <- function(n, seed, variables = 3L) {
  set.seed(seed)
  d <- matrix(stats::rnorm(n * variables), n, variables)
  y <- matrix(stats::rbinom(n * variables, 1L, stats::plogis(-1 + 1.2 * d)), n, variables)
  dimnames(y) <- list(sprintf("s%d_%05d", seed, seq_len(n)), sprintf("v%d", seq_len(variables)))
  good <- d + matrix(stats::rnorm(n * variables, sd = 0.7), n, variables)
  noisy <- d + matrix(stats::rnorm(n * variables, sd = 2), n, variables)
  dimnames(good) <- dimnames(noisy) <- dimnames(y)
  list(y = y, x = timesift_set(list(good = feature_matrix(good, "good"),
                                    noisy = feature_matrix(noisy, "noisy"))))
}

ncv_learner <- function() {
  learner("centroid",
          fit = function(x, y, ...) {
            m <- matrix(x[, , 1], nrow = dim(x)[1L])
            list(w = vapply(seq_len(ncol(y)), function(j) {
              colMeans(m[y[, j] == 1, , drop = FALSE]) - colMeans(m[y[, j] == 0, , drop = FALSE])
            }, numeric(ncol(m))))
          },
          predict = function(model, x) matrix(x[, , 1], nrow = dim(x)[1L]) %*% model$w)
}

test_that("the nested cross-validation interval covers the procedure's risk at the nominal rate", {
  skip_on_cran()
  replicates <- 150L
  deployment <- ncv_design(20000L, 999999L)
  rows <- lapply(seq_len(replicates), function(r) {
    s <- ncv_design(150L, r)
    sel <- select_grain(s$x, s$y, ncv_learner(), folds = fold_map(s$y, v = 5L, seed = r),
                        inner = 3L, metric = "roc_auc", interval = "nested_cv", repeats = 1L,
                        seed = r, verbose = FALSE)
    # The truth: the procedure fitted on every unit of the sample, scored on units it never saw.
    p <- stats::predict(sel$final$fit, deployment$x[[sel$final$grain]])
    truth <- mean(vapply(colnames(deployment$y),
                         function(v) roc_auc(deployment$y[, v], p[, v]), numeric(1L)))
    est <- sel$estimate[sel$estimate$metric == "roc_auc", ]
    data.frame(truth = truth, est[c("interval", "center", "se", "lower", "upper")])
  })
  d <- do.call(rbind, rows)
  d$cover <- d$lower <= d$truth & d$truth <= d$upper
  nested <- d[d$interval == "nested_cv", ]
  across <- d[d$interval == "variables", ]

  # Coverage inside its own Monte Carlo margin of the nominal rate.
  margin <- stats::qnorm(0.975) * sqrt(0.95 * 0.05 / replicates)
  expect_lt(abs(mean(nested$cover) - 0.95), margin)
  # The across-variable interval is the narrower one, which is what it is not an interval for the
  # procedure's risk: its width is the spread between variables.
  expect_gt(mean(nested$se), mean(across$se))
  expect_lt(abs(mean(nested$center - nested$truth)), 0.02)
})
