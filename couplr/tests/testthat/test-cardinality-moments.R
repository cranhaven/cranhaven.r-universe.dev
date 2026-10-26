# Linear moment constraints for balance-constrained cardinality matching.

# The fields that decide what a normalized row means. `origin` and `label` are
# reporting, so two routes that state the same constraint agree on these and
# may differ on those.
moment_core <- function(row) {
  row[c("var", "stat", "direction", "limit", "bound", "denominator", "trivial",
        "trivial_reason")]
}

moment_pools <- function(n = 30, m = 40, seed = 1) {
  set.seed(seed)
  list(
    left = data.frame(id = seq_len(n), x = stats::rnorm(n),
                      y = stats::rnorm(n, 2, 3)),
    right = data.frame(id = seq_len(m) + n, x = stats::rnorm(m, 0.4),
                       y = stats::rnorm(m, 1.5, 2))
  )
}

pooled_sd <- function(a, b) sqrt((stats::var(a) + stats::var(b)) / 2)


test_that("moment coefficients reproduce the constraint they stand for", {
  pools <- moment_pools(seed = 11)
  left <- pools$left
  right <- pools$right
  delta <- 0.15

  specs <- .moment_specs(moments = c(x = delta), max_std_diff = NULL,
                         vars = NULL, left = left, right = right)
  expect_length(specs, 2L)

  s_full <- pooled_sd(left$x, right$x)

  for (rep_id in 1:5) {
    set.seed(100 + rep_id)
    k <- sample(2:12, 1)
    i <- sample(nrow(left), k)
    j <- sample(nrow(right), k)

    for (spec in specs) {
      coefs <- .moment_coefficients(spec, left, right)
      d <- spec$direction
      # sum_ij a_ij = k * ( d*(mean_L - mean_R) - delta * s_v )
      expected <- k * (d * (mean(left$x[i]) - mean(right$x[j])) - delta * s_full)
      expect_equal(.moment_violation(coefs, i, j), expected, tolerance = 1e-10)
    }
  }
})


test_that("a row's value is the number of pairs times the bound's slack", {
  pools <- moment_pools(seed = 12)
  left <- pools$left
  right <- pools$right
  delta <- 0.05

  specs <- .moment_specs(moments = c(x = delta), max_std_diff = NULL,
                         vars = NULL, left = left, right = right)
  coefs <- lapply(specs, .moment_coefficients, left = left, right = right)

  i <- seq_len(20)
  j <- seq_len(20)
  k <- 20
  s_full <- pooled_sd(left$x, right$x)
  raw <- mean(left$x[i]) - mean(right$x[j])

  expect_equal(.moment_violation(coefs[[1L]], i, j) / k,
               raw - delta * s_full, tolerance = 1e-12)
  expect_equal(.moment_violation(coefs[[2L]], i, j) / k,
               -raw - delta * s_full, tolerance = 1e-12)

  # Both rows non-positive is exactly the two-sided bound holding.
  worst <- max(vapply(coefs, function(cf) .moment_violation(cf, i, j),
                      numeric(1)))
  expect_equal(worst <= 0, abs(raw) <= delta * s_full)
})


test_that("the constraint and standardized_difference() share a numerator", {
  pools <- moment_pools(seed = 13)
  left <- pools$left
  right <- pools$right
  delta <- 0.2

  specs <- .moment_specs(moments = list(list(var = "x", stat = "std_diff",
                                             max = delta)),
                         max_std_diff = NULL, vars = NULL,
                         left = left, right = right)
  up <- .moment_coefficients(specs[[1L]], left, right)

  set.seed(77)
  k <- 15L
  i <- sample(nrow(left), k)
  j <- sample(nrow(right), k)

  s_full <- pooled_sd(left$x, right$x)
  s_matched <- pooled_sd(left$x[i], right$x[j])
  mean_diff <- mean(left$x[i]) - mean(right$x[j])

  # The diagnostic is the same numerator over the matched-sample denominator.
  expect_equal(standardized_difference(left$x[i], right$x[j]) * s_matched,
               mean_diff, tolerance = 1e-10)

  # The constraint is that same numerator over the full-pool denominator, which
  # is what makes it linear in the pair indicators. The two denominators are
  # different numbers and are meant to stay different.
  expect_equal(.moment_violation(up, i, j) / k + delta * s_full,
               mean_diff, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(s_matched, s_full)))
  expect_equal(specs[[1L]]$denominator, s_full, tolerance = 1e-12)
})


test_that("the full-pool denominator is the package's own moment definition", {
  pools <- moment_pools(seed = 14)
  left <- pools$left
  right <- pools$right

  mL <- .weighted_moments(left$y, rep(1, nrow(left)))
  mR <- .weighted_moments(right$y, rep(1, nrow(right)))
  expect_equal(mL$var, stats::var(left$y), tolerance = 1e-12)

  specs <- .moment_specs(moments = c(y = 0.1), max_std_diff = NULL,
                         vars = NULL, left = left, right = right)
  expect_equal(specs[[1L]]$denominator, sqrt((mL$var + mR$var) / 2),
               tolerance = 1e-12)
})


test_that("coefficients and repricing stay O(n + m) on a large instance", {
  n <- 2000L
  m <- 5000L
  set.seed(21)
  left <- data.frame(x = stats::rnorm(n), y = stats::rnorm(n))
  right <- data.frame(x = stats::rnorm(m), y = stats::rnorm(m))

  n_arcs <- 400L
  arcs <- data.frame(i = sample(n, n_arcs, replace = TRUE),
                     j = sample(m, n_arcs, replace = TRUE),
                     cost = stats::runif(n_arcs))

  elapsed <- system.time({
    specs <- .moment_specs(moments = NULL, max_std_diff = 0.1,
                           vars = c("x", "y"), left = left, right = right)
    coefs <- lapply(specs, .moment_coefficients, left = left, right = right)
    priced <- .moment_reprice(arcs, coefs, rep(0.5, length(coefs)))
  })[["elapsed"]]

  expect_length(specs, 4L)
  expect_length(priced, n_arcs)
  expect_lt(elapsed, 5)

  for (cf in coefs) {
    expect_length(cf$u, n)
    expect_length(cf$w, m)
    expect_length(cf$b, 1L)
    # Three vectors, not a table: an n by m matrix of doubles would be 80 MB.
    expect_lt(as.numeric(utils::object.size(cf)), 8 * 4 * (n + m))
  }
  expect_lt(as.numeric(utils::object.size(coefs)),
            8 * 4 * length(coefs) * (n + m))
})


test_that("every accepted input form normalizes to the same rows", {
  pools <- moment_pools(seed = 31)
  left <- pools$left
  right <- pools$right

  shorthand <- .moment_specs(c(x = 0.1), NULL, NULL, left, right)
  explicit <- .moment_specs(list(list(var = "x", stat = "std_diff", max = 0.1)),
                            NULL, NULL, left, right)
  bare <- .moment_specs(list(var = "x", stat = "std_diff", max = 0.1),
                        NULL, NULL, left, right)
  defaulted <- .moment_specs(list(list(var = "x", max = 0.1)),
                             NULL, NULL, left, right)
  expanded <- .moment_specs(NULL, 0.1, "x", left, right)

  expect_equal(lapply(explicit, moment_core), lapply(shorthand, moment_core))
  expect_equal(lapply(bare, moment_core), lapply(shorthand, moment_core))
  expect_equal(lapply(defaulted, moment_core), lapply(shorthand, moment_core))
  expect_equal(lapply(expanded, moment_core), lapply(shorthand, moment_core))

  # Two one-sided rows, opposite directions, the same bound after scaling.
  expect_equal(vapply(shorthand, function(r) r$direction, numeric(1)),
               c(1, -1))
  expect_equal(vapply(shorthand, function(r) r$bound, numeric(1)),
               rep(0.1 * pooled_sd(left$x, right$x), 2), tolerance = 1e-12)
  expect_equal(vapply(shorthand, function(r) r$stat, character(1)),
               rep("std_diff", 2))

  # The origin is reporting, and it does record which route stated the row.
  expect_equal(vapply(shorthand, function(r) r$origin, character(1)),
               rep("moments", 2))
  expect_equal(vapply(expanded, function(r) r$origin, character(1)),
               rep("max_std_diff", 2))
})


test_that("mean_diff bounds are one-sided and carry no denominator", {
  pools <- moment_pools(seed = 32)
  left <- pools$left
  right <- pools$right

  one <- .moment_specs(list(list(var = "x", stat = "mean_diff", max = 2)),
                       NULL, NULL, left, right)
  expect_length(one, 1L)
  expect_equal(one[[1L]]$direction, 1)
  expect_equal(one[[1L]]$bound, 2)
  expect_equal(one[[1L]]$denominator, 1)

  both <- .moment_specs(list(list(var = "x", stat = "mean_diff",
                                  max = 2, min = -2)),
                        NULL, NULL, left, right)
  expect_length(both, 2L)
  expect_equal(vapply(both, function(r) r$direction, numeric(1)), c(1, -1))
  expect_equal(vapply(both, function(r) r$bound, numeric(1)), c(2, 2))

  low <- .moment_specs(list(list(var = "x", stat = "mean_diff", min = -1)),
                       NULL, NULL, left, right)
  expect_length(low, 1L)
  expect_equal(low[[1L]]$direction, -1)
  expect_equal(low[[1L]]$bound, 1)

  coefs <- .moment_coefficients(both[[1L]], left, right)
  i <- c(1L, 5L, 9L)
  j <- c(2L, 4L, 8L)
  expect_equal(.moment_violation(coefs, i, j),
               sum(left$x[i]) - sum(right$x[j]) - 3 * 2, tolerance = 1e-12)
})


test_that("transforms apply on one variable and on several", {
  pools <- moment_pools(seed = 33)
  left <- pools$left
  right <- pools$right

  square <- .moment_specs(list(list(var = "x", transform = function(v) v^2,
                                    stat = "mean_diff", max = 1)),
                          NULL, NULL, left, right)
  cf <- .moment_coefficients(square[[1L]], left, right)
  expect_equal(cf$u, left$x^2, tolerance = 1e-12)
  expect_equal(cf$w, right$x^2, tolerance = 1e-12)
  expect_equal(cf$b, 1)

  inter <- .moment_specs(list(list(var = c("x", "y"),
                                   transform = function(d) d$x * d$y,
                                   stat = "mean_diff", max = 0.5)),
                         NULL, NULL, left, right)
  cf_inter <- .moment_coefficients(inter[[1L]], left, right)
  expect_equal(cf_inter$u, left$x * left$y, tolerance = 1e-12)
  expect_equal(cf_inter$w, right$x * right$y, tolerance = 1e-12)

  # Several variables with no transform reduce by the row product.
  product <- .moment_specs(list(list(var = c("x", "y"), stat = "mean_diff",
                                     max = 0.5)),
                           NULL, NULL, left, right)
  cf_product <- .moment_coefficients(product[[1L]], left, right)
  expect_equal(cf_product$u, cf_inter$u, tolerance = 1e-12)
  expect_equal(cf_product$w, cf_inter$w, tolerance = 1e-12)

  # A transformed row bounds the transformed covariate, on the same identity.
  set.seed(34)
  i <- sample(nrow(left), 8)
  j <- sample(nrow(right), 8)
  expect_equal(.moment_violation(cf, i, j),
               8 * (mean(left$x[i]^2) - mean(right$x[j]^2) - 1),
               tolerance = 1e-10)
})


test_that("a std_diff transform takes its denominator from the transform", {
  pools <- moment_pools(seed = 35)
  left <- pools$left
  right <- pools$right

  spec <- .moment_specs(list(list(var = "x", transform = function(v) v^2,
                                  stat = "std_diff", max = 0.1)),
                        NULL, NULL, left, right)[[1L]]
  expect_equal(spec$denominator, pooled_sd(left$x^2, right$x^2),
               tolerance = 1e-12)
  expect_equal(spec$bound, 0.1 * pooled_sd(left$x^2, right$x^2),
               tolerance = 1e-12)
})


test_that("labels name the row they stand for", {
  pools <- moment_pools(seed = 36)
  labs <- vapply(.moment_specs(c(x = 0.1), NULL, NULL, pools$left, pools$right),
                 function(r) r$label, character(1))
  expect_true(any(grepl("std_diff(x)", labs, fixed = TRUE)))
  expect_true(any(grepl("<=", labs, fixed = TRUE)))
  expect_true(any(grepl(">=", labs, fixed = TRUE)))

  inter <- .moment_specs(list(list(var = c("x", "y"),
                                   transform = function(d) d$x * d$y,
                                   stat = "mean_diff", max = 0.5)),
                         NULL, NULL, pools$left, pools$right)
  expect_true(grepl("f(x, y)", inter[[1L]]$label, fixed = TRUE))
})


test_that("no constraint is stated twice by two routes", {
  pools <- moment_pools(seed = 41)
  left <- pools$left
  right <- pools$right

  expect_error(
    .moment_specs(c(x = 0.1), 0.2, c("x", "y"), left, right),
    "both bound the standardized difference of 'x'", fixed = TRUE
  )
  expect_error(
    .moment_specs(list(list(var = "x", stat = "std_diff", max = 0.1)),
                  0.2, c("x", "y"), left, right),
    "both bound the standardized difference of 'x'", fixed = TRUE
  )

  # A mean_diff on the same variable is a different constraint, so both stand.
  mixed <- .moment_specs(list(list(var = "x", stat = "mean_diff", max = 1)),
                         0.2, "x", left, right)
  expect_length(mixed, 3L)
  expect_equal(sort(vapply(mixed, function(r) r$stat, character(1))),
               c("mean_diff", "std_diff", "std_diff"))

  # A one-sided std_diff leaves the other direction free, and the default would
  # supply it, so the clash is still reported rather than half-applied.
  expect_error(
    .moment_specs(list(list(var = "x", stat = "std_diff", min = -0.1)),
                  0.2, "x", left, right),
    "both bound the standardized difference of 'x'", fixed = TRUE
  )
})


test_that("bad variables are refused", {
  pools <- moment_pools(seed = 42)
  left <- pools$left
  right <- pools$right

  expect_error(.moment_specs(c(z = 0.1), NULL, NULL, left, right),
               "is not present in both pools", fixed = TRUE)

  left_only <- left
  left_only$w <- 1
  expect_error(.moment_specs(c(w = 0.1), NULL, NULL, left_only, right),
               "is not present in both pools", fixed = TRUE)

  lg <- left; rg <- right
  lg$g <- letters[seq_len(nrow(lg))]
  rg$g <- letters[seq_len(nrow(rg)) %% 26 + 1]
  expect_error(.moment_specs(c(g = 0.1), NULL, NULL, lg, rg),
               "must be numeric in both pools", fixed = TRUE)

  ln <- left; ln$x[3] <- NA
  expect_error(.moment_specs(c(x = 0.1), NULL, NULL, ln, right),
               "carries NA in the left pool", fixed = TRUE)

  rn <- right; rn$x[4] <- NA
  expect_error(.moment_specs(c(x = 0.1), NULL, NULL, left, rn),
               "carries NA in the right pool", fixed = TRUE)
})


test_that("bad bounds are refused", {
  pools <- moment_pools(seed = 43)
  left <- pools$left
  right <- pools$right

  expect_error(.moment_specs(c(x = Inf), NULL, NULL, left, right),
               "must be a single finite number", fixed = TRUE)
  expect_error(.moment_specs(c(x = NA_real_), NULL, NULL, left, right),
               "must be a single finite number", fixed = TRUE)
  expect_error(.moment_specs(c(x = -0.1), NULL, NULL, left, right),
               "must not be negative", fixed = TRUE)
  expect_error(
    .moment_specs(list(list(var = "x", stat = "mean_diff", max = 1, min = 2)),
                  NULL, NULL, left, right),
    "must not exceed", fixed = TRUE
  )
  expect_error(
    .moment_specs(list(list(var = "x", stat = "mean_diff")),
                  NULL, NULL, left, right),
    "needs `max` or `min`", fixed = TRUE
  )
  expect_error(.moment_specs(NULL, -1, "x", left, right),
               "`max_std_diff` must not be negative", fixed = TRUE)
  expect_error(.moment_specs(NULL, 0.1, NULL, left, right),
               "so `vars` must name them", fixed = TRUE)
})


test_that("bad specs and bad transforms are refused", {
  pools <- moment_pools(seed = 44)
  left <- pools$left
  right <- pools$right

  expect_error(.moment_specs(list(list(stat = "std_diff", max = 0.1)),
                             NULL, NULL, left, right),
               "needs a `var`", fixed = TRUE)
  expect_error(.moment_specs(list(list(var = "x", stat = "median_diff",
                                       max = 0.1)),
                             NULL, NULL, left, right),
               "`stat` must be one of", fixed = TRUE)
  expect_error(.moment_specs(list(list(var = "x", max = 0.1, maks = 1)),
                             NULL, NULL, left, right),
               "Unknown field(s)", fixed = TRUE)
  expect_error(.moment_specs(list(list(var = "x", transform = "square",
                                       max = 0.1)),
                             NULL, NULL, left, right),
               "must be a function", fixed = TRUE)
  expect_error(.moment_specs(list(list(var = "x",
                                       transform = function(v) v[1:2],
                                       stat = "mean_diff", max = 1)),
                             NULL, NULL, left, right),
               "must return one value per unit", fixed = TRUE)
  expect_error(.moment_specs(list(list(var = "x",
                                       transform = function(v) as.character(v),
                                       stat = "mean_diff", max = 1)),
                             NULL, NULL, left, right),
               "must return a numeric vector", fixed = TRUE)
  expect_error(.moment_specs(list(list(var = "x",
                                       transform = function(v) 1 / (v - v),
                                       stat = "mean_diff", max = 1)),
                             NULL, NULL, left, right),
               "is not finite everywhere", fixed = TRUE)
  expect_error(.moment_specs(c(0.1), NULL, NULL, left, right),
               "must name every bound", fixed = TRUE)
  expect_error(.moment_specs("x", NULL, NULL, left, right),
               "must be NULL, a named numeric vector, or a list", fixed = TRUE)
})


test_that("a constant covariate is a trivially satisfied row, not a dropped one", {
  set.seed(51)
  left <- data.frame(x = stats::rnorm(10), k = rep(3, 10))
  right <- data.frame(x = stats::rnorm(12), k = rep(3, 12))

  specs <- .moment_specs(NULL, 0.1, c("x", "k"), left, right)
  expect_length(specs, 4L)

  const <- Filter(function(r) identical(r$var, "k"), specs)
  expect_length(const, 2L)
  for (r in const) {
    expect_true(r$trivial)
    expect_equal(r$trivial_reason, "constant")
    expect_equal(r$denominator, 0)
    expect_equal(r$bound, 0)
    expect_true(grepl("trivially satisfied", r$label, fixed = TRUE))

    cf <- .moment_coefficients(r, left, right)
    expect_equal(cf$u, numeric(10))
    expect_equal(cf$w, numeric(12))
    expect_equal(.moment_violation(cf, c(1L, 2L, 3L), c(4L, 5L, 6L)), 0)
  }
  # The diagnostic reports the same thing on a constant covariate.
  expect_equal(standardized_difference(rep(3, 10), rep(3, 12)), 0)
})


test_that("a covariate constant at different levels is refused, not inherited", {
  left <- data.frame(k = rep(3, 8))
  right <- data.frame(k = rep(5, 9))

  # standardized_difference() returns 0 here. That is the diagnostic's own
  # convention for an undefined ratio, on a sample that is already fixed. The
  # constraint layer deliberately does not inherit it: the spread is 0 while
  # every matched subset carries the same mean difference of 2, so no subset
  # achieves any standardized-difference bound, and a satisfied row would let a
  # solve certify an answer the caller did not ask for. The divergence is
  # intentional and is not drift to be reconciled later.
  expect_equal(standardized_difference(left$k, right$k), 0)

  expect_error(.moment_specs(c(k = 0.1), NULL, NULL, left, right),
               "std_diff(k) cannot be satisfied", fixed = TRUE)
  expect_error(.moment_specs(c(k = 0.1), NULL, NULL, left, right),
               "constant at 3 in the left pool and at 5 in the right pool",
               fixed = TRUE)
  expect_error(.moment_specs(c(k = 0.1), NULL, NULL, left, right),
               "no matched subset achieves any standardized-difference bound",
               fixed = TRUE)
  expect_error(.moment_specs(c(k = 0.1), NULL, NULL, left, right),
               "stat = \"mean_diff\"", fixed = TRUE)

  # It is refused by whichever route states it, one-sided included.
  expect_error(.moment_specs(NULL, 0.1, "k", left, right),
               "cannot be satisfied", fixed = TRUE)
  expect_error(.moment_specs(list(list(var = "k", stat = "std_diff",
                                       min = -0.1)),
                             NULL, NULL, left, right),
               "cannot be satisfied", fixed = TRUE)

  # mean_diff is the constraint that can be stated there, and it is a real row.
  ok <- .moment_specs(list(list(var = "k", stat = "mean_diff", max = 2)),
                      NULL, NULL, left, right)
  expect_length(ok, 1L)
  expect_false(ok[[1L]]$trivial)
  expect_equal(ok[[1L]]$denominator, 1)
  cf <- .moment_coefficients(ok[[1L]], left, right)
  # Four pairs, each contributing 3 - 5 = -2, against a bound of 2 per pair.
  expect_equal(.moment_violation(cf, 1:4, 1:4), 4 * (-2 - 2))
})


test_that("an undefined pooled spread is satisfied by default, not refused", {
  # A one-unit pool has an undefined variance rather than a zero one, so the
  # bound delta * s_v names no number. Nothing is proven unachievable, and a
  # one-unit pool is legitimate input, so the row is kept and says why.
  left <- data.frame(x = 4)
  right <- data.frame(x = c(1, 2, 3))

  expect_true(is.na(.weighted_moments(left$x, 1)$var))

  specs <- .moment_specs(c(x = 0.1), NULL, NULL, left, right)
  expect_length(specs, 2L)
  for (r in specs) {
    expect_true(r$trivial)
    expect_equal(r$trivial_reason, "undefined_spread")
    expect_equal(r$denominator, 0)
    expect_equal(r$bound, 0)
    expect_true(grepl("undefined pooled spread", r$label, fixed = TRUE))

    cf <- .moment_coefficients(r, left, right)
    expect_equal(cf$u, numeric(1))
    expect_equal(cf$w, numeric(3))
    expect_equal(.moment_violation(cf, 1L, 2L), 0)
  }

  # A mean_diff bound on the same pools is unaffected: its denominator is 1 and
  # never had to be estimated.
  md <- .moment_specs(list(list(var = "x", stat = "mean_diff", max = 1)),
                      NULL, NULL, left, right)[[1L]]
  expect_false(md$trivial)
  expect_equal(.moment_violation(.moment_coefficients(md, left, right), 1L, 1L),
               4 - 1 - 1)

  # An empty pool reads the same way rather than erroring.
  empty <- .moment_specs(c(x = 0.1), NULL, NULL,
                         left[0, , drop = FALSE], right)
  expect_true(all(vapply(empty, function(r) r$trivial, logical(1))))
  expect_equal(empty[[1L]]$trivial_reason, "undefined_spread")
})


test_that("degenerate pair sets and empty constraint sets behave", {
  pools <- moment_pools(seed = 52)
  left <- pools$left
  right <- pools$right

  expect_equal(.moment_specs(NULL, NULL, NULL, left, right), list())
  expect_equal(.moment_specs(NULL, Inf, c("x", "y"), left, right), list())
  expect_equal(.moment_specs(list(), NULL, NULL, left, right), list())
  expect_equal(.moment_specs(numeric(0), NULL, NULL, left, right), list())
  expect_equal(.moment_specs(NULL, NULL, c("x", "y"), left, right), list())

  spec <- .moment_specs(c(x = 0.1), NULL, NULL, left, right)[[1L]]
  cf <- .moment_coefficients(spec, left, right)

  expect_equal(.moment_violation(cf, integer(0), integer(0)), 0)
  expect_equal(.moment_violation(cf, 2L, 7L),
               cf$u[2] - cf$w[7] - cf$b, tolerance = 1e-12)
  expect_error(.moment_violation(cf, 1:3, 1:2),
               "same number of pairs", fixed = TRUE)
  expect_error(.moment_violation(cf, 0L, 1L),
               "must index the left and right pools", fixed = TRUE)
  expect_error(.moment_violation(cf, nrow(left) + 1L, 1L),
               "must index the left and right pools", fixed = TRUE)
  expect_error(.moment_violation(list(a = 1), 1L, 1L),
               "must be a coefficient object", fixed = TRUE)
})


test_that("repricing is the identity at lambda = 0 and additive in lambda", {
  pools <- moment_pools(n = 40, m = 60, seed = 61)
  left <- pools$left
  right <- pools$right

  specs <- .moment_specs(NULL, 0.1, c("x", "y"), left, right)
  coefs <- lapply(specs, .moment_coefficients, left = left, right = right)
  expect_length(coefs, 4L)

  set.seed(62)
  n_arcs <- 150L
  arcs <- data.frame(i = sample(nrow(left), n_arcs, replace = TRUE),
                     j = sample(nrow(right), n_arcs, replace = TRUE),
                     cost = stats::runif(n_arcs, 0, 10))

  expect_identical(.moment_reprice(arcs, coefs, rep(0, 4)), arcs$cost)
  expect_identical(.moment_reprice(arcs, list(), numeric(0)), arcs$cost)
  expect_identical(.moment_reprice(arcs, NULL, numeric(0)), arcs$cost)

  l1 <- c(0.3, 0, 1.2, 0.7)
  l2 <- c(1.0, 0.5, 0, 2.0)
  p1 <- .moment_reprice(arcs, coefs, l1)
  p2 <- .moment_reprice(arcs, coefs, l2)
  p12 <- .moment_reprice(arcs, coefs, l1 + l2)
  expect_equal(p12 - arcs$cost, (p1 - arcs$cost) + (p2 - arcs$cost),
               tolerance = 1e-10)

  # Against the definition, arc by arc.
  manual <- arcs$cost
  for (r in seq_along(coefs)) {
    manual <- manual + l1[r] * (coefs[[r]]$u[arcs$i] - coefs[[r]]$w[arcs$j] -
                                  coefs[[r]]$b)
  }
  expect_equal(p1, manual, tolerance = 1e-12)

  # A single coefficient object stands in for a list of one.
  expect_equal(.moment_reprice(arcs, coefs[[1L]], 0.9),
               .moment_reprice(arcs, coefs[1L], 0.9), tolerance = 1e-12)
})


test_that("repricing refuses a mismatched call", {
  pools <- moment_pools(n = 12, m = 15, seed = 63)
  left <- pools$left
  right <- pools$right
  specs <- .moment_specs(c(x = 0.1), NULL, NULL, left, right)
  coefs <- lapply(specs, .moment_coefficients, left = left, right = right)

  arcs <- data.frame(i = c(1L, 2L, 3L), j = c(4L, 5L, 6L), cost = c(1, 2, 3))

  expect_error(.moment_reprice(arcs, coefs, 1), "finite multipliers",
               fixed = TRUE)
  expect_error(.moment_reprice(arcs, coefs, c(1, NA)), "finite multipliers",
               fixed = TRUE)
  expect_error(.moment_reprice(arcs, coefs, c(1, Inf)), "finite multipliers",
               fixed = TRUE)
  expect_error(.moment_reprice(arcs[, c("i", "j")], coefs, c(1, 1)),
               "missing the column(s) cost", fixed = TRUE)
  expect_error(.moment_reprice(arcs[, c("i", "cost")], coefs, c(1, 1)),
               "missing the column(s) j", fixed = TRUE)
  expect_error(.moment_coefficients(list(var = "x"), left, right),
               "must be a normalized moment spec", fixed = TRUE)
})
