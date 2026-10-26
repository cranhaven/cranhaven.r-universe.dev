# Parameter-recovery / correctness tests for the statistical (causal-inference)
# layer: propensity matching, subclassification weights, weighted balance, and
# Rosenbaum sensitivity pairing. These validate the methods against known truth,
# not just object structure.

test_that("sensitivity_analysis pairs outcomes by the true matched pairing", {
  # Correct pairing (row-wise) vs the pairing you get from sorting each side's
  # ids independently give DIFFERENT Wilcoxon T+, so t_observed pins down which
  # pairing was used.
  left <- data.frame(id = c("L1", "L2", "L3", "L4"),
                     y = c(41, 32, 23, 14), stringsAsFactors = FALSE)
  right <- data.frame(id = c("R1", "R2", "R3", "R4"),
                      y = c(10, 20, 30, 40), stringsAsFactors = FALSE)
  # True pairs: L1-R4, L2-R3, L3-R2, L4-R1  ->  d = 1, 2, 3, 4 (all positive)
  res <- structure(
    list(pairs = tibble::tibble(left_id = c("L1", "L2", "L3", "L4"),
                                right_id = c("R4", "R3", "R2", "R1"))),
    class = "matching_result"
  )

  sa <- sensitivity_analysis(res, left, right, outcome_var = "y")

  # Correct pairing: every d > 0, so T+ = sum of ranks 1..4 = 10.
  # The sorted-merge bug would pair L*-R* by sorted id, giving d = 31,12,-7,-26
  # and T+ = 6.
  expect_equal(sa$t_observed, 10)
  expect_equal(sa$n_pairs, 4)
})

test_that("prefitted propensity model is applied to the supplied data rows", {
  set.seed(1)
  n <- 300
  d <- data.frame(id = 1:n, x = rnorm(n))
  d$treat <- rbinom(n, 1, stats::plogis(-0.4 + 1.4 * d$x))

  # Same model, but one is fit on a row-permuted copy of the data. Fitting on a
  # permutation moves the coefficients by around 1e-15, so predict(newdata = d)
  # agrees to that tolerance rather than exactly, and the two matchings solve
  # cost matrices that differ in their last bits. Using the model's own fitted
  # values (in training-row order) instead of predict(newdata = ) would
  # misalign the shuffled fit and change the scores outright.
  m       <- stats::glm(treat ~ x, data = d, family = stats::binomial())
  d_shuf  <- d[sample(n), ]
  m_shuf  <- stats::glm(treat ~ x, data = d_shuf, family = stats::binomial())

  r1 <- suppressWarnings(ps_match(data = d, treatment = "treat", ps_model = m))
  r2 <- suppressWarnings(ps_match(data = d, treatment = "treat", ps_model = m_shuf))

  # The caliper leaves this problem with several co-optimal assignments, so a
  # perturbation that small can move which one the solver returns without
  # changing its quality. Assert the parts that a correctly applied model
  # fixes: who gets matched, and at what total cost. A model applied in
  # training-row order fails the cost assertion by a wide margin.
  expect_setequal(r1$pairs$left_id, r2$pairs$left_id)
  expect_setequal(r1$pairs$right_id, r2$pairs$right_id)
  expect_equal(sum(r1$pairs$distance), sum(r2$pairs$distance))
})

test_that("propensity matching reduces covariate imbalance", {
  set.seed(2)
  n <- 400
  d <- data.frame(id = 1:n, x = rnorm(n))
  d$treat <- rbinom(n, 1, stats::plogis(-0.3 + 1.6 * d$x))

  pre <- couplr:::standardized_difference(d$x[d$treat == 1], d$x[d$treat == 0])

  r <- suppressWarnings(ps_match(formula = treat ~ x, data = d, treatment = "treat"))
  tr_x <- d$x[match(r$pairs$left_id, d$id)]
  ct_x <- d$x[match(r$pairs$right_id, d$id)]
  post <- couplr:::standardized_difference(tr_x, ct_x)

  expect_lt(abs(post), abs(pre))
})

test_that("ATE subclass weights sum to the stratum fraction within each arm", {
  set.seed(5)
  n <- 400
  dat <- data.frame(id = 1:n, x = rnorm(n))
  dat$treat <- rbinom(n, 1, stats::plogis(0.8 * dat$x))

  sc <- subclass_match(formula = treat ~ x, data = dat, treatment = "treat",
                       estimand = "ATE", n_subclasses = 5)
  mt <- sc$matched
  N <- n

  # For ATE, within each overlapping subclass the weights of each arm sum to
  # n_k / N (the stratum fraction). The old n_k^2 / N formula fails this.
  for (k in unique(mt$subclass)) {
    ink <- mt[mt$subclass == k, ]
    left_w  <- sum(ink$weight[ink$side == "left"])
    right_w <- sum(ink$weight[ink$side == "right"])
    if (left_w > 0 && right_w > 0) {  # overlapping stratum
      frac <- nrow(ink) / N
      expect_equal(left_w, frac, tolerance = 1e-6)
      expect_equal(right_w, frac, tolerance = 1e-6)
    }
  }
  # Arm totals sum to 1.
  expect_equal(sum(mt$weight[mt$side == "left"]), 1, tolerance = 1e-6)
})

test_that("balance_diagnostics uses subclass weights (weighted means)", {
  set.seed(7)
  n <- 300
  dat <- data.frame(id = 1:n, x = rnorm(n))
  dat$treat <- rbinom(n, 1, stats::plogis(0.9 * dat$x))

  sc <- subclass_match(formula = treat ~ x, data = dat, treatment = "treat",
                       estimand = "ATE", n_subclasses = 5)
  bal <- balance_diagnostics(sc, data = dat, vars = "x")
  vs <- bal$var_stats[bal$var_stats$variable == "x", ]

  # Expected weighted mean of x on the left (treated) arm, computed by hand.
  mt <- sc$matched
  left_w <- mt[mt$side == "left" & mt$weight > 0, ]
  wx <- stats::setNames(left_w$weight, left_w$id)
  ids <- as.character(dat$id) %in% left_w$id
  xl <- dat$x[ids]
  w  <- unname(wx[as.character(dat$id[ids])])
  expected_weighted_mean <- sum(w * xl) / sum(w)

  expect_equal(vs$mean_left, expected_weighted_mean, tolerance = 1e-6)
  # The unweighted mean generally differs, so this would fail if weights were ignored.
})

# A homogeneous additive treatment effect makes ATT = ATE = tau. Matching on the
# (correctly specified) propensity removes the x-driven confounding, so the mean
# matched-pair outcome difference recovers tau. left_id = treated, right_id =
# control, as elsewhere in couplr.
.sim_ps_effect <- function(seed, tau = 1.0, n = 800) {
  set.seed(seed)
  x <- rnorm(n)
  treat <- rbinom(n, 1, stats::plogis(-0.2 + 0.9 * x))
  y <- tau * treat + 0.6 * x + rnorm(n)
  d <- data.frame(id = seq_len(n), x = x, treat = treat, y = y)
  r <- suppressWarnings(ps_match(formula = treat ~ x, data = d,
                                 treatment = "treat"))
  yt <- d$y[match(r$pairs$left_id, d$id)]
  yc <- d$y[match(r$pairs$right_id, d$id)]
  diff <- yt - yc
  ci <- stats::t.test(diff)$conf.int
  naive <- mean(d$y[d$treat == 1]) - mean(d$y[d$treat == 0])
  list(est = mean(diff), ci = ci, naive = naive)
}

test_that("propensity matching recovers a known treatment effect across seeds", {
  skip_on_cran()
  tau <- 1.0
  res <- lapply(31:60, .sim_ps_effect)
  ests   <- vapply(res, function(z) z$est,   numeric(1))
  naives <- vapply(res, function(z) z$naive, numeric(1))

  # Averaged over seeds the matched estimate is close to tau...
  expect_lt(abs(mean(ests) - tau), 0.1)
  # ...and materially less biased than the naive unadjusted difference (which is
  # inflated by the positive x-confounding).
  expect_lt(abs(mean(ests) - tau), abs(mean(naives) - tau))
})

test_that("matched-pair confidence intervals cover the true effect at ~nominal rate", {
  skip_on_cran()
  tau <- 1.0
  res <- lapply(201:230, .sim_ps_effect)
  covered <- vapply(res, function(z) tau >= z$ci[1] && tau <= z$ci[2], logical(1))

  # Nominal 95% paired-difference CI should cover the truth on >= 85% of the
  # >= 20 simulated seeds.
  expect_gte(length(covered), 20)
  expect_gte(mean(covered), 0.85)
})
