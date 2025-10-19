test_that("ccc_pairwise_u_stat: basic structure, symmetry, CI container", {
  set.seed(123)
  n_subj <- 600L
  id     <- factor(rep(seq_len(n_subj), each = 2L))
  method <- factor(rep(c("A","B"), times = n_subj))

  # model: y_A = u + e ; y_B = b + u + e
  sigA <- 1.0   # subject variance
  sigE <- 0.5   # error variance
  biasB <- 0.2  # fixed method bias for B
  u <- rnorm(n_subj, 0, sqrt(sigA))[as.integer(id)]
  e <- rnorm(n_subj * 2L, 0, sqrt(sigE))
  y <- (method == "B") * biasB + u + e
  df <- data.frame(y, id, method)

  # Theoretical CCC for 2 methods, T = 1, no AxM/AxT:
  # N = sA; D = sA + S_B + sE
  ccc_theory <- sigA / (sigA + biasB^2 + sigE)

  # estimates only
  c1 <- ccc_pairwise_u_stat(df, response = "y", method = "method")
  expect_s3_class(c1, "ccc")
  expect_true(is.matrix(c1) && all(rownames(c1) == c("A","B")))
  expect_equal(as.numeric(diag(c1)), c(1,1))
  expect_equal(c1["A","B"], c1["B","A"])
  expect_true(c1["A","B"] > 0 && c1["A","B"] < 1)

  # within Monte Carlo tolerance
  expect_lt(abs(c1["A","B"] - ccc_theory), 0.05)

  # with CI container
  c2 <- ccc_pairwise_u_stat(df, response = "y", method = "method", ci = TRUE, conf_level = 0.95)
  expect_s3_class(c2, "ccc_ci")
  expect_named(c2, c("est","lwr.ci","upr.ci"))
  expect_equal(dim(c2$est), c(2,2))
  expect_true(is.na(diag(c2$lwr.ci))[1] && is.na(diag(c2$upr.ci))[1])
  est <- c2$est["A","B"]; lwr <- c2$lwr.ci["A","B"]; upr <- c2$upr.ci["A","B"]
  expect_true(lwr <= est && est <= upr)
})

test_that("ccc_lmm_reml (pairwise, no time): matches simple theory and returns VCs", {
  set.seed(123)
  n_subj <- 500L
  id     <- factor(rep(seq_len(n_subj), each = 2L))
  method <- factor(rep(c("A","B"), times = n_subj))

  sigA <- 1.0
  sigE <- 0.5
  biasB <- 0.2
  u <- rnorm(n_subj, 0, sqrt(sigA))[as.integer(id)]
  e <- rnorm(n_subj * 2L, 0, sqrt(sigE))
  y <- (method == "B") * biasB + u + e
  df <- data.frame(y, id, method)

  cfit <- ccc_lmm_reml(df, response = "y", rind = "id",
                       method = "method", ci = TRUE)
  expect_s3_class(cfit, "ccc_lmm_reml")
  expect_named(cfit, c("est","lwr.ci","upr.ci"))
  expect_equal(rownames(cfit$est), c("A","B"))
  expect_equal(colnames(cfit$est), c("A","B"))
  expect_equal(as.numeric(diag(cfit$est)), c(1,1))

  # theory for T=1 (no AxM/AxT): sA / (sA + b^2 + sE)
  ccc_theory <- sigA / (sigA + biasB^2 + sigE)
  est <- cfit$est["A","B"]
  expect_lt(abs(est - ccc_theory), 0.05)

  # variance-component attributes exist and are numeric matrices
  for (nm in c("sigma2_subject","sigma2_subject_method","sigma2_subject_time",
               "sigma2_error","SB","se_ccc")) {
    v <- attr(cfit, nm)
    expect_true(is.matrix(v))
    expect_equal(dim(v), c(2,2))
  }

  # summary data frame columns present
  sm <- summary(cfit, show_ci = "yes", digits = 4)
  expect_s3_class(sm, "summary.ccc_lmm_reml")
  expect_true(all(c("method1","method2","estimate","lwr","upr",
                    "sigma2_subject","sigma2_subject_method","sigma2_subject_time",
                    "sigma2_error","SB","se_ccc") %in% names(sm)))
})

# helper to center time within subject
center_by_id <- function(x, id) ave(x, id, FUN = function(v) v - mean(v))

test_that("Dmat_type affects CCC as expected when biases flip over time", {
  set.seed(42)
  n_subj <- 200L
  n_time <- 6L
  id     <- factor(rep(seq_len(n_subj), each = 2L * n_time))
  time   <- factor(rep(rep(seq_len(n_time), times = 2L), times = n_subj))
  method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))

  sigA  <- 0.8
  sigAT <- 0.3
  sigE  <- 0.4

  # time-varying bias for B: +b, -b, +b, -b, ...
  b0 <- 0.35
  tnum <- as.integer(time)
  bias_t <- ifelse(tnum %% 2L == 1L,  b0, -b0)
  bias  <- ifelse(method == "B", bias_t, 0)

  u  <- rnorm(n_subj, 0, sqrt(sigA))[as.integer(id)]
  gIT <- rnorm(n_subj * n_time, 0, sqrt(sigAT))
  g  <- gIT[(as.integer(id) - 1L) * n_time + as.integer(time)]
  y  <- bias + u + g + rnorm(length(id), 0, sqrt(sigE))
  df <- data.frame(y, id, method, time)

  fit_avg <- ccc_lmm_reml(df, "y", "id", method = "method", time = "time",
                          Dmat_type = "time-avg")
  fit_typ <- ccc_lmm_reml(df, "y", "id", method = "method", time = "time",
                          Dmat_type = "typical-visit")

  # With alternating biases, squared-average is ~0, average of squares > 0,
  # so CCC(time-avg) should be >= CCC(typical-visit)
  c_avg <- fit_avg["A","B"]; c_typ <- fit_typ["A","B"]
  expect_gte(c_avg, c_typ)
})

test_that("Supplying a wrong-sized Dmat errors clearly", {
  set.seed(1)
  n_subj <- 10L; n_time <- 3L
  id     <- factor(rep(seq_len(n_subj), each = 2L * n_time))
  time   <- factor(rep(rep(seq_len(n_time), times = 2L), times = n_subj))
  method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))
  y <- rnorm(length(id))
  df <- data.frame(y, id, method, time)

  badD <- diag(5L)   # wrong dimension
  expect_error(
    ccc_lmm_reml(df, "y", "id", method = "method", time = "time", Dmat = badD),
    "Dmat has incompatible dimension", fixed = FALSE
  )
})

test_that("summary adds sigma2_extra* columns when slope is enabled", {
  set.seed(2)
  n_subj <- 120L; n_time <- 4L
  id     <- factor(rep(seq_len(n_subj), each = 2L * n_time))
  time   <- factor(rep(rep(seq_len(n_time), times = 2L), times = n_subj))
  method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))

  # Build a genuine slope effect so sigma2_extra > 0
  tnum <- as.integer(time)
  t_c  <- center_by_id(tnum, id)
  slope_subj <- rnorm(n_subj, 0, 0.15)[as.integer(id)]

  y <- 0.3 * (method == "B") +
    slope_subj * t_c +
    rnorm(length(id), 0, 0.6)

  df <- data.frame(y, id, method, time, t_c = t_c)

  fit_slope <- ccc_lmm_reml(df, "y", "id", method = "method", time = "time",
                            slope = "subject", slope_var = "t_c", ci = TRUE)

  # Summary should expose sigma2_extra columns
  sm <- summary(fit_slope, show_ci = "yes")
  extra_cols <- grep("^sigma2_extra", names(sm), value = TRUE)
  expect_true(length(extra_cols) >= 1)
  expect_true(any(is.finite(sm[[extra_cols[1]]])))

  # In the no-slope case, those columns should not be present
  fit_noslope <- ccc_lmm_reml(df, "y", "id", method = "method", time = "time", ci = TRUE)
  sm0 <- summary(fit_noslope, show_ci = "yes")
  expect_length(grep("^sigma2_extra", names(sm0), value = TRUE), 0)
})

test_that("AR(1) path: fixed rho is carried in attributes", {
  set.seed(10)
  n_subj <- 50L; n_time <- 6L
  id     <- factor(rep(seq_len(n_subj), each = 2L * n_time))
  time   <- factor(rep(rep(seq_len(n_time), times = 2L), times = n_subj))
  method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))

  rho_true <- 0.6
  # simulate AR(1) per (subject,method) run
  y <- numeric(length(id))
  for (s in seq_len(n_subj)) {
    for (m in c("A","B")) {
      idx <- which(id == levels(id)[s] & method == m)
      y[idx] <- 0.2 * (m == "B") +
        as.numeric(stats::arima.sim(list(ar = rho_true), n = n_time, sd = 0.7))
    }
  }
  df <- data.frame(y, id, method, time)

  fit_ar <- ccc_lmm_reml(df, "y", "id", method = "method", time = "time",
                         ar = "ar1", ar_rho = rho_true)
  rr <- attr(fit_ar, "ar_rho")
  expect_true(is.matrix(rr))
  expect_equal(rr["A","B"], rho_true, tolerance = 1e-12)

  # summary returns AR1 diagnostics columns (may have NAs but should exist)
  sm <- summary(fit_ar)
  expect_true(all(c("ar1_rho_mom","ar1_pairs","ar1_pval","ar1_recommend") %in% names(sm)))
})


ccc_lin <- function(x, y, na.rm = TRUE) {
  stopifnot(length(x) == length(y))
  if (na.rm) {
    keep <- is.finite(x) & is.finite(y)
    x <- x[keep]; y <- y[keep]
  }
  if (length(x) < 2L) return(NA_real_)
  mx <- mean(x); my <- mean(y)
  vx <- stats::var(x); vy <- stats::var(y)
  sxy <- stats::cov(x, y)
  den <- vx + vy + (mx - my)^2
  if (!is.finite(den) || den <= 0) return(NA_real_)
  2 * sxy / den
}

test_that("ccc_pairwise_u_stat reduces to Lin's CCC when T = 1", {
  set.seed(123)
  n <- 1500L
  u  <- rnorm(n, 0, 1.0)
  eA <- rnorm(n, 0, 0.7)
  eB <- rnorm(n, 0, 0.7)
  bias <- 0.3
  xA <- u + eA
  xB <- bias + u + eB

  # Lin on paired data
  c_lin <- ccc_lin(xA, xB)

  df <- data.frame(
    id     = factor(rep(seq_len(n), each = 2L)),
    method = factor(rep(c("A","B"), times = n)),
    time   = factor(rep(1L, 2L * n))
  )

  # INTERLEAVE A,B per subject (A1,B1,A2,B2,...)
  df$y <- c(rbind(xA, xB))

  c_us <- ccc_pairwise_u_stat(df, response = "y", method = "method")
  c_us_AB <- unname(c_us["A","B"])

  # should match Lin very closely
  expect_equal(c_us_AB, c_lin, tolerance = 5e-3)
})

test_that("ccc_lmm_reml (no time) approximates Lin's CCC when T = 1", {
  set.seed(124)
  n <- 1500L
  u  <- rnorm(n, 0, 1.0)
  eA <- rnorm(n, 0, 0.7)
  eB <- rnorm(n, 0, 0.7)
  bias <- 0.25
  xA <- u + eA
  xB <- bias + u + eB
  c_lin <- ccc_lin(xA, xB)

  df <- data.frame(
    id     = factor(rep(seq_len(n), each = 2L)),
    method = factor(rep(c("A","B"), times = n)),
    y      = c(rbind(xA, xB))
  )

  fit <- ccc_lmm_reml(df, response = "y", rind = "id", method = "method")
  c_reml <- unname(fit["A","B"])

  expect_equal(c_reml, c_lin, tolerance = 1e-2)
})
