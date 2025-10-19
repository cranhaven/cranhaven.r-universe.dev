# AR(1) simulator for within-subject residuals
ar1_sim <- function(n, rho, sd = 1) {
  z <- rnorm(n)
  e <- numeric(n)
  e[1] <- z[1] * sd
  if (n > 1) for (t in 2:n) e[t] <- rho * e[t - 1] + sqrt(1 - rho^2) * z[t] * sd
  e
}

# Two-method data with known difference model:
# d_{s,t} = (y2 - y1) = mu + b_s + e_{s,t},
# with b_s ~ N(0, sig_s^2), e ~ N(0, sig_e^2) or AR(1)
# ==> truth: bias = mu; sd_loa = sqrt(sig_s^2 + sig_e^2)
sim_two_method_known <- function(S = 60L, Tm = 25L,
                                 mu = 1.25, sig_s = 1.7, sig_e = 2.3,
                                 rho = NULL, seed = 1L) {
  set.seed(seed)
  subj <- rep(seq_len(S), each = Tm)
  time <- rep(seq_len(Tm), times = S)
  b_s  <- rnorm(S, 0, sig_s)
  b    <- b_s[subj]
  e    <- if (is.null(rho)) rnorm(S * Tm, 0, sig_e) else
    unlist(lapply(split(seq_along(time), subj), function(ix) ar1_sim(length(ix), rho, sig_e)))

  # Baseline cancels in differences; include to mimic realistic responses
  baseline <- rnorm(S * Tm, 10, 3)
  y1 <- baseline
  y2 <- baseline + mu + b + e

  data.frame(
    y = c(y1, y2),
    subject = rep(subj, 2),
    method  = factor(rep(c("M1", "M2"), each = S * Tm), levels = c("M1", "M2")),
    time    = rep(time, 2),
    check.names = FALSE
  )
}

# Multi-method data for matrix invariants
# M2 ~= M1 + small noise; M3 = -M1; M4 = unrelated
sim_multi_method <- function(S = 30L, Tm = 15L, seed = 11L) {
  set.seed(seed)
  subj <- rep(seq_len(S), each = Tm)
  time <- rep(seq_len(Tm), times = S)

  mu_s <- rnorm(S, mean = 0, sd = 8)
  true <- mu_s[subj]
  e0   <- rnorm(length(true), 0, 2)

  y1 <- true + e0
  y2 <- y1 + rnorm(length(true), 0, 0.01)
  y3 <- -y1
  y4 <- rnorm(length(true), 3, 6)

  out <- rbind(
    data.frame(y = y1, subject = subj, method = "M1", time = time),
    data.frame(y = y2, subject = subj, method = "M2", time = time),
    data.frame(y = y3, subject = subj, method = "M3", time = time),
    data.frame(y = y4, subject = subj, method = "M4", time = time),
    make.row.names = FALSE
  )
  out$method <- factor(out$method, levels = c("M1", "M2", "M3", "M4"))
  out
}

test_that("two-method contrast matches factor order (method2 - method1)", {
  dat <- sim_two_method_known(S = 50, Tm = 20, mu = 1.0, sig_s = 1.2, sig_e = 1.5, seed = 1)
  dat$method <- factor(dat$method, levels = c("M1","M2"))
  fit <- bland_altman_repeated(data = dat, response="y", subject="subject",
                               method="method", time="time", use_ar1=FALSE)
  expect_gt(as.numeric(fit$mean.diffs), 0)  # should be ~ +1.0
})

test_that("Two-method BA recovers bias and sd_loa under i.i.d. residuals", {
  dat <- sim_two_method_known(S = 80, Tm = 50, mu = 1.25, sig_s = 1.7,
                              sig_e = 2.3, rho = NULL, seed = 100)
  truth_sd <- sqrt(1.7^2 + 2.3^2)

  fit <- bland_altman_repeated(
    data = dat,
    response = "y", subject = "subject", method = "method", time = "time",
    include_slope = FALSE, use_ar1 = FALSE, two = 1.96, conf_level = 0.95
  )

  expect_s3_class(fit, "ba_repeated")

  # Bias and sd_loa close to truth
  expect_equal(as.numeric(fit$mean.diffs), 1.25, tolerance = 0.08)
  expect_equal(as.numeric(fit$critical.diff) / as.numeric(fit$two), truth_sd, tolerance = 0.08)

  # LoA consistent with sd and 'two'
  sd_hat <- as.numeric(fit$critical.diff) / as.numeric(fit$two)
  expect_equal(as.numeric(fit$upper.limit) - as.numeric(fit$mean.diffs), 1.96 * sd_hat, tolerance = 1e-6)
  expect_equal(as.numeric(fit$mean.diffs) - as.numeric(fit$lower.limit), 1.96 * sd_hat, tolerance = 1e-6)

  # Summary object sanity
  sm <- summary(fit)
  expect_s3_class(sm, "summary.ba_repeated")
  expect_true(all(c("bias","sd_loa","loa_low","loa_up","width","n") %in% names(sm)))
})

test_that("Two-method BA with AR(1) honours supplied rho and recovers truth", {
  skip_on_cran()

  mu    <- 0.8
  sig_s <- 1.2
  sig_e <- 1.5
  rho   <- 0.45
  dat <- sim_two_method_known(S = 70, Tm = 40, mu = mu, sig_s = sig_s, sig_e = sig_e, rho = rho, seed = 200)
  truth_sd <- sqrt(sig_s^2 + sig_e^2)  # definition of sd_loa in docs

  fit <- bland_altman_repeated(
    data = dat,
    response = "y", subject = "subject", method = "method", time = "time",
    include_slope = FALSE, use_ar1 = TRUE, ar1_rho = rho, two = 2.0, conf_level = 0.9
  )

  expect_true(isTRUE(fit$use_ar1))
  expect_false(is.na(fit$ar1_rho))
  expect_equal(as.numeric(fit$ar1_rho), rho, tolerance = 1e-12)
  expect_false(isTRUE(fit$ar1_estimated))  # supplied, so not estimated

  expect_equal(as.numeric(fit$mean.diffs), mu,   tolerance = 0.08)
  expect_equal(as.numeric(fit$critical.diff) / 2.0, truth_sd, tolerance = 0.09)
})

test_that("Edge case: constant difference gives zero sd_loa and degenerate LoA", {
  # Small jitter to avoid numerical singularities in optimisers; still near-zero SD
  set.seed(1)
  S <- 25; Tm <- 8; mu <- 5
  subj <- rep(seq_len(S), each = Tm)
  time <- rep(seq_len(Tm), times = S)
  base <- rnorm(S * Tm, 12, 2)
  y1 <- base
  y2 <- base + mu + rnorm(S * Tm, 0, 1e-6)

  dat <- data.frame(y = c(y1,y2), subject = rep(subj, 2), method = rep(c("A","B"), each = length(y1)),
                    time = rep(time, 2))
  dat$method <- factor(dat$method, levels = c("A","B"))

  fit <- bland_altman_repeated(
    data = dat, response = "y", subject = "subject", method = "method", time = "time",
    include_slope = FALSE, use_ar1 = FALSE
  )

  expect_equal(as.numeric(fit$mean.diffs), mu, tolerance = 1e-3)
  expect_lt(as.numeric(fit$critical.diff), 1e-2)  # ~ zero
  expect_equal(as.numeric(fit$lower.limit), as.numeric(fit$upper.limit), tolerance = 1e-2)
})

test_that("Pairwise matrix algebraic invariants hold", {
  dat <- sim_multi_method(S = 24, Tm = 12, seed = 99)

  fit <- bland_altman_repeated(
    data = dat,
    response = "y", subject = "subject", method = "method", time = "time",
    include_slope = FALSE, use_ar1 = FALSE, two = 1.96, conf_level = 0.95
  )

  expect_s3_class(fit, "ba_repeated_matrix")
  m <- length(fit$methods)

  # Symmetry / antisymmetry conditions
  expect_true(all(is.na(diag(fit$bias))))
  expect_true(all(abs(fit$bias + t(fit$bias)) < 1e-8, na.rm = TRUE))

  expect_true(all(abs(fit$sd_loa - t(fit$sd_loa)) < 1e-12, na.rm = TRUE))
  expect_true(all(abs(fit$width  - t(fit$width )) < 1e-12, na.rm = TRUE))

  # LoA sign conventions (row - column)
  for (i in 1:(m-1)) for (j in (i+1):m) {
    expect_equal(fit$loa_lower[j,i], -fit$loa_upper[i,j], tolerance = 1e-10)
    expect_equal(fit$loa_upper[j,i], -fit$loa_lower[i,j], tolerance = 1e-10)
  }

  # Width equals 2*two*sd_loa
  expect_equal(fit$width, 2 * fit$two * fit$sd_loa, tolerance = 1e-10)
})

test_that("Pairwise result equals two-method fit for the same pair", {
  dat <- sim_multi_method(S = 30, Tm = 15, seed = 7)
  # Compare M1 vs M2
  dat12 <- subset(dat, method %in% c("M1","M2"))

  fit2 <- bland_altman_repeated(
    data = dat12, response = "y", subject = "subject", method = "method", time = "time",
    include_slope = FALSE, use_ar1 = FALSE, two = 1.96, conf_level = 0.95
  )

  fitN <- bland_altman_repeated(
    data = dat, response = "y", subject = "subject", method = "method", time = "time",
    include_slope = FALSE, use_ar1 = FALSE, two = 1.96, conf_level = 0.95
  )

  i <- match("M1", fitN$methods); j <- match("M2", fitN$methods)
  expect_equal(as.numeric(fit2$mean.diffs), fitN$bias[i,j], tolerance = 1e-8)
  expect_equal(as.numeric(fit2$critical.diff) / as.numeric(fit2$two), fitN$sd_loa[i,j], tolerance = 1e-8)
  expect_equal(as.numeric(fit2$lower.limit), fitN$loa_lower[i,j], tolerance = 1e-8)
  expect_equal(as.numeric(fit2$upper.limit), fitN$loa_upper[i,j], tolerance = 1e-8)
})

test_that("n matrix equals number of subject-time complete pairs per contrast", {
  # Create imbalance: drop some method-time rows
  dat <- sim_multi_method(S = 12, Tm = 10, seed = 123)
  # Remove random 15% of M3 rows to induce missingness
  set.seed(123)
  drop_idx <- which(dat$method == "M3")
  rm <- sample(drop_idx, size = floor(0.15 * length(drop_idx)))
  dat2 <- dat[-rm, ]

  fit <- bland_altman_repeated(
    data = dat2, response = "y", subject = "subject", method = "method", time = "time",
    include_slope = FALSE, use_ar1 = FALSE
  )

  # Manual complete-case counts for M1 vs M3
  cc <- merge(
    subset(dat2, method == "M1")[, c("subject","time")],
    subset(dat2, method == "M3")[, c("subject","time")],
    by = c("subject","time"),
    all = FALSE
  )
  i <- match("M1", fit$methods); j <- match("M3", fit$methods)
  expect_equal(as.integer(fit$n[i,j]), nrow(cc))
})

test_that("Column mapping works both with data= and with vectors", {
  dat <- sim_multi_method(S = 10, Tm = 6, seed = 42)
  # With data=
  fit1 <- bland_altman_repeated(
    data = dat, response = "y", subject = "subject", method = "method", time = "time"
  )
  expect_true(inherits(fit1, "ba_repeated_matrix"))

  # With vectors (no data=)
  fit2 <- bland_altman_repeated(
    response = dat$y, subject = dat$subject, method = dat$method, time = dat$time
  )
  expect_true(inherits(fit2, "ba_repeated_matrix"))

  # Stored data_long/mapping present for plotting
  expect_true(!is.null(fit1$data_long) && !is.null(fit1$mapping))
  expect_true(!is.null(fit2$data_long) && !is.null(fit2$mapping))
})

test_that("summary/print produce expected classes and do not error", {
  dat <- sim_multi_method(S = 10, Tm = 6, seed = 2)

  fit_mat <- bland_altman_repeated(
    data = dat, response = "y", subject = "subject", method = "method", time = "time"
  )
  sm_mat <- summary(fit_mat)
  expect_s3_class(sm_mat, "summary.ba_repeated_matrix")
  expect_true(all(c("method1","method2","bias","sd_loa","loa_low","loa_up","width","n") %in% names(sm_mat)))

  # Two-method
  dat12 <- subset(dat, method %in% c("M1","M2"))
  fit2  <- bland_altman_repeated(
    data = dat12, response = "y", subject = "subject", method = droplevels(dat12$method), time = "time"
  )
  sm2 <- summary(fit2)
  expect_s3_class(sm2, "summary.ba_repeated")
  expect_true(all(c("bias","sd_loa","loa_low","loa_up","width","n") %in% names(sm2)))
})

test_that("plot methods return a ggplot object and do not error", {
  skip_if_not_installed("ggplot2")
  dat <- sim_multi_method(S = 12, Tm = 8, seed = 5)

  # Matrix plot
  fit_mat <- bland_altman_repeated(
    data = dat, response = "y", subject = "subject", method = "method", time = "time",
    include_slope = TRUE
  )
  p_mat <- plot(fit_mat, smoother = "lm", facet_scales = "free_y")
  expect_s3_class(p_mat, "ggplot")

  # Two-method plot
  dat12 <- subset(dat, method %in% c("M1","M2"))
  fit2  <- bland_altman_repeated(
    data = dat12, response = "y", subject = "subject", method = droplevels(dat12$method), time = "time",
    include_slope = TRUE
  )
  p2 <- plot(fit2, smoother = "loess", symmetrize_y = TRUE)
  expect_s3_class(p2, "ggplot")
})

test_that("Input argument validation works", {
  dat <- sim_multi_method(S = 6, Tm = 4, seed = 1)

  # bad 'two'
  expect_error(
    bland_altman_repeated(data = dat, response = "y", subject = "subject", method = "method", time = "time",
                          two = -1),
    "`two` must be a positive scalar.", fixed = TRUE
  )

  # bad conf_level
  expect_error(
    bland_altman_repeated(data = dat, response = "y", subject = "subject", method = "method", time = "time",
                          conf_level = 1.1),
    "`conf_level` must be in (0,1).", fixed = TRUE
  )

  # AR1 rho bounds when use_ar1 = TRUE
  expect_error(
    bland_altman_repeated(data = dat, response = "y", subject = "subject", method = "method", time = "time",
                          use_ar1 = TRUE, ar1_rho = 1.2),
    "`ar1_rho` must be in (-0.999, 0.999).", fixed = TRUE
  )

  # Need at least 2 methods
  dat1 <- subset(dat, method == "M1")
  expect_error(
    bland_altman_repeated(data = dat1, response = "y", subject = "subject", method = "method", time = "time"),
    "Need at least 2 distinct methods in `method`.", fixed = TRUE
  )

  # Wrong column name
  expect_error(
    bland_altman_repeated(data = dat, response = "y", subject = "subject", method = "NOPE", time = "time"),
    "Column 'NOPE' not found in `data`.", fixed = TRUE
  )
})
