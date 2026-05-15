#' @description
#' Internal generator for small mixed-type datasets (ordinal + continuous)
#' used in examples and unit tests. Not exported.
#' @keywords internal
#' @noRd
.make_data <- function(
    n        = 120,
    rho      = 0.30,
    load_o   = c(.30, .40, .50),
    load_c   = c(.30, .40, .50),
    n_cat    = 2,
    thr      = 0,
    groups   = FALSE,
    g_diff   = c(F1 = 0.0, F2 = 0.0),
    seed     = 2025
) {
  stopifnot(length(load_o) == 3L, length(load_c) == 3L)
  if (length(thr) != (n_cat - 1L)) stop("thr must have length n_cat - 1")

  set.seed(seed)

  # latens ~ N(0,1) with rho correlation
  Sig <- matrix(c(1, rho, rho, 1), 2, 2)
  L   <- chol(Sig)
  Z   <- matrix(stats::rnorm(n * 2L), n, 2L)
  Eta <- Z %*% L
  colnames(Eta) <- c("F1", "F2")

  # Optional 2-group design
  if (isTRUE(groups)) {
    g <- rep(1:2, length.out = n)
    Eta[g == 2, "F1"] <- Eta[g == 2, "F1"] + g_diff["F1"]
    Eta[g == 2, "F2"] <- Eta[g == 2, "F2"] + g_diff["F2"]
  } else {
    g <- NULL
  }

  # Ordinal indcators (F1): y* -> cut -> integer 1..n_cat
  make_ord <- function(lambda, eta, thr) {
    eps_sd <- sqrt(max(1 - lambda^2, 1e-6))
    ystar  <- lambda * eta + stats::rnorm(length(eta), sd = eps_sd)
    as.integer(cut(ystar, breaks = c(-Inf, thr, Inf), labels = FALSE))
  }
  o1 <- make_ord(load_o[1], Eta[, "F1"], thr)
  o2 <- make_ord(load_o[2], Eta[, "F1"], thr)
  o3 <- make_ord(load_o[3], Eta[, "F1"], thr)

  # Continous indicators (F2)
  make_cont <- function(lambda, eta) {
    eps_sd <- sqrt(max(1 - lambda^2, 1e-6))
    as.numeric(lambda * eta + stats::rnorm(length(eta), sd = eps_sd))
  }
  c1 <- make_cont(load_c[1], Eta[, "F2"])
  c2 <- make_cont(load_c[2], Eta[, "F2"])
  c3 <- make_cont(load_c[3], Eta[, "F2"])

  df <- data.frame(o1, o2, o3, c1, c2, c3)
  if (!is.null(g)) df$group <- factor(g)
  df
}

# Common model strings ---------------------------------------------------------
.model_mixed <- '
  F1 =~ o1 + o2 + o3    # ordinal block
  F2 =~ c1 + c2 + c3    # continuous block
'

.model_cont <- '
  F2 =~ c1 + c2 + c3
'

.model_ord <- '
  F1 =~ o1 + o2 + o3
'

# Fit builders ----------------------------------------------------------------
.fit_mixed <- function(n = 1000, estimator = "WLSMV", parameterization = "delta",
                       n_cat = 2, thr = 0) {
  d  <- .make_data(n = n, n_cat = n_cat, thr = thr)   # mixed: o* ordinal, c* continuous
  ord <- c("o1","o2","o3")
  lavaan::cfa(
    .model_mixed,
    data             = d,
    ordered          = ord,
    estimator        = estimator,
    parameterization = parameterization
  )
}

.fit_mixed_mg <- function(n = 2000, estimator = "WLSMV", parameterization = "delta") {
  d  <- .make_data(n = n, n_cat = 2, groups = TRUE, g_diff = c(F1 = 0.2, F2 = -0.1))
  ord <- c("o1","o2","o3")
  lavaan::cfa(
    .model_mixed,
    data             = d,
    group            = "group",
    ordered          = ord,
    estimator        = estimator,
    parameterization = parameterization,
    meanstructure    = TRUE
  )
}

.fit_cont <- function(n = 120) {
  d <- .make_data(n = n, n_cat = 2)
  d <- d[, c("c1","c2","c3")]  # keep only continuous block
  lavaan::cfa(
    .model_cont,
    data          = d,
    meanstructure = TRUE
  )
}

.fit_ord <- function(n = 120, estimator = "WLSMV", parameterization = "delta") {
  d <- .make_data(n = n, n_cat = 2)
  lavaan::cfa(
    .model_ord,
    data             = d[, c("o1","o2","o3")],
    ordered          = c("o1","o2","o3"),
    estimator        = estimator,
    parameterization = parameterization,
    meanstructure    = TRUE
  )
}
