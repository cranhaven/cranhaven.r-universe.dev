# R/utils.R -- Internal utilities, logging, theming, numeric helpers

# ============================================================
# Package-level imports
# ============================================================

#' @importFrom stats coef lm sd setNames quantile median var rnorm
#' @importFrom utils head
#' @importFrom rlang .data
NULL

# ============================================================
# Internal logging (not exported)
# ============================================================

.et_log <- function(..., level = "INFO") {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste0("[", ts, "] [ErrorTracer/", level, "] ", paste0(..., collapse = ""))
  message(msg)
}

.et_info  <- function(...) .et_log(..., level = "INFO")
.et_warn  <- function(...) .et_log(..., level = "WARN")
.et_error <- function(...) .et_log(..., level = "ERROR")

# ============================================================
# Numeric helpers
# ============================================================

#' Standardize a numeric vector (mean-centre, unit-variance)
#'
#' @param x Numeric vector.
#' @return Standardized numeric vector. Returns zeros if variance is zero.
#' @export
standardize <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s < 1e-12) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

#' Reverse standardization
#'
#' @param z Standardized values.
#' @param mu Original mean.
#' @param s  Original standard deviation.
#' @return Values on the original scale.
#' @export
unstandardize <- function(z, mu, s) {
  z * s + mu
}

# ============================================================
# ggplot2 theme
# ============================================================

#' Minimal ggplot2 theme for ErrorTracer plots
#'
#' @param base_size Base font size (default 12).
#' @return A \code{ggplot2} theme object.
#' @export
et_theme <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}

# ============================================================
# Internal helpers for et_predict / decomposition
# ============================================================

# Normalise env_noise to a named list of per-observation SD vectors.
# Each element of the returned list has length n_obs, so every row of
# newdata can have its own noise SD (enabling time-varying uncertainty).
#
# Accepted forms for env_noise:
#   NULL                  -- zero noise for all predictors
#   scalar numeric        -- fraction of each predictor's SD in newdata
#   named scalar list/vec -- fixed SD per predictor (constant across obs)
#   named vector list     -- per-row SDs; each entry must be length 1 or n_obs
.resolve_env_noise <- function(env_noise, pred_names, newdata) {
  n_obs <- nrow(newdata)
  zeros <- stats::setNames(lapply(pred_names, function(p) rep(0, n_obs)),
                           pred_names)

  if (is.null(env_noise)) return(zeros)

  if (is.numeric(env_noise) && length(env_noise) == 1) {
    # Scalar fraction: scale by each predictor's empirical SD
    result <- lapply(pred_names, function(p) {
      sd_p <- if (p %in% colnames(newdata)) {
        s <- stats::sd(newdata[[p]], na.rm = TRUE)
        if (is.na(s) || s < 1e-12) 1 else s
      } else 1
      rep(env_noise * sd_p, n_obs)
    })
    return(stats::setNames(result, pred_names))
  }

  # Named list or named numeric vector (possibly with per-row vectors)
  env_noise_list <- if (is.list(env_noise)) env_noise else as.list(env_noise)

  unknown <- setdiff(names(env_noise_list), pred_names)
  if (length(unknown) > 0) {
    .et_warn("env_noise contains predictor(s) not in model: ",
             paste(unknown, collapse = ", "), " -- ignored")
  }

  result <- zeros
  shared <- intersect(names(env_noise_list), pred_names)

  for (p in shared) {
    v <- as.numeric(env_noise_list[[p]])
    if (length(v) == 1L) {
      result[[p]] <- rep(v, n_obs)
    } else if (length(v) == n_obs) {
      result[[p]] <- v
    } else {
      stop(
        "env_noise[[\"", p, "\"]] has length ", length(v),
        " but newdata has ", n_obs, " row(s). ",
        "Supply a scalar (constant noise) or a vector of length n_obs ",
        "(time-varying noise)."
      )
    }
  }

  result
}

# Resolve env_cov into a p x p correlation matrix over pred_names.
# Accepted forms:
#   NULL          -- identity (independent noise; preserves legacy behavior)
#   "empirical"   -- correlation of pred_names columns in training_data
#   "newdata"     -- correlation of pred_names columns in newdata
#   numeric matrix -- user-supplied correlation or covariance; if the diagonal
#                     is not ~1, it is rescaled to a correlation matrix.
#                     rownames/colnames must contain all pred_names.
#
# The returned matrix is symmetric PD (eigenvalue-clipped if necessary) and has
# dimnames = pred_names in the order of pred_names.
.resolve_env_cor <- function(env_cov, pred_names, training_data, newdata) {
  p   <- length(pred_names)
  I_p <- diag(p)
  dimnames(I_p) <- list(pred_names, pred_names)

  if (is.null(env_cov) || isFALSE(env_cov)) return(I_p)

  if (is.character(env_cov) && length(env_cov) == 1L) {
    src <- switch(
      env_cov,
      empirical = training_data,
      newdata   = newdata,
      stop("env_cov must be NULL, 'empirical', 'newdata', or a matrix.")
    )
    keep <- intersect(pred_names, colnames(src))
    if (length(keep) < 2L) {
      .et_warn("env_cov = '", env_cov, "' needs >= 2 predictors present in ",
               "the source data; falling back to independent noise.")
      return(I_p)
    }
    X <- as.matrix(src[, keep, drop = FALSE])
    R <- suppressWarnings(stats::cor(X, use = "pairwise.complete.obs"))
    R[is.na(R)] <- 0
    full <- I_p
    full[keep, keep] <- R
    return(.nearest_pd(full))
  }

  if (is.matrix(env_cov) || is.data.frame(env_cov)) {
    M <- as.matrix(env_cov)
    if (!all(dim(M) == c(p, p))) {
      stop("env_cov matrix must be ", p, " x ", p,
           " (one entry per predictor).")
    }
    if (is.null(dimnames(M))) dimnames(M) <- list(pred_names, pred_names)
    if (!all(pred_names %in% rownames(M)) ||
        !all(pred_names %in% colnames(M))) {
      stop("env_cov matrix dimnames must include all predictors: ",
           paste(pred_names, collapse = ", "))
    }
    M <- M[pred_names, pred_names, drop = FALSE]
    if (max(abs(diag(M) - 1)) > 1e-6) {
      d <- sqrt(pmax(diag(M), 1e-12))
      M <- M / tcrossprod(d)
    }
    return(.nearest_pd(M))
  }

  stop("Unrecognized env_cov argument (class: ",
       paste(class(env_cov), collapse = "/"), ").")
}

# Resolve env_dist into a named character vector (pred_names -> distribution).
# Accepted forms:
#   NULL          -- "gaussian" for every predictor (additive Gaussian noise;
#                    preserves ErrorTracer's legacy behaviour)
#   single string -- apply that distribution to every predictor
#   named list / named character vector -- per-predictor distribution; any
#                    predictor not named defaults to "gaussian"
#
# Supported distributions: "gaussian", "lognormal", "gamma", "beta".
.resolve_env_dist <- function(env_dist, pred_names) {
  valid   <- c("gaussian", "lognormal", "gamma", "beta")
  default <- stats::setNames(rep("gaussian", length(pred_names)), pred_names)

  if (is.null(env_dist)) return(default)

  if (is.character(env_dist) && length(env_dist) == 1L && is.null(names(env_dist))) {
    if (!env_dist %in% valid) {
      stop("env_dist = '", env_dist, "' not in ",
           paste(valid, collapse = ", "))
    }
    return(stats::setNames(rep(env_dist, length(pred_names)), pred_names))
  }

  if (!is.null(names(env_dist))) {
    unknown <- setdiff(names(env_dist), pred_names)
    if (length(unknown) > 0) {
      .et_warn("env_dist contains predictor(s) not in model: ",
               paste(unknown, collapse = ", "), " -- ignored")
    }
    result <- default
    for (p in intersect(names(env_dist), pred_names)) {
      d <- as.character(env_dist[[p]])
      if (!d %in% valid) {
        stop("env_dist[['", p, "']] = '", d, "' not in ",
             paste(valid, collapse = ", "))
      }
      result[[p]] <- d
    }
    return(result)
  }

  stop("env_dist must be NULL, a single distribution name, ",
       "or a named list / named character vector.")
}

# Draw a distribution-aware perturbation of predictor x using latent
# standard-normal draws Z_std (already correlation-scaled). The returned
# vector is the perturbed predictor on its original scale, calibrated so
# that (approximately) E[perturbed] = x and Var[perturbed] = sigma^2, using
# the following parameterisations:
#
#   gaussian  : perturbed = x + Z_std * sigma                      (additive)
#   lognormal : log(perturbed) ~ N(log(x) - s^2/2, s^2),
#               s^2 = log(1 + (sigma/x)^2)                     (multiplicative)
#   gamma     : perturbed ~ Gamma(shape = (x/sigma)^2,
#                                 rate  =  x /sigma^2)
#   beta      : perturbed ~ Beta(alpha, beta) with
#               alpha + beta = x*(1-x)/sigma^2 - 1
#
# Correlation across predictors is induced by the latent Z_std (Gaussian
# copula); the marginal distribution is supplied here. Rows that cannot be
# parameterised for the requested distribution (e.g. gamma/lognormal with
# x <= 0; beta with sigma^2 >= x*(1-x)) are returned unperturbed and a
# warning is emitted at most once per call.
.perturb_predictor <- function(x, sigma, Z_std, dist) {
  if (length(sigma) == 1L) sigma <- rep(sigma, length(x))
  if (all(is.na(sigma) | sigma == 0)) return(x)

  if (dist == "gaussian") {
    return(x + Z_std * sigma)
  }

  u <- pmin(pmax(stats::pnorm(Z_std), 1e-10), 1 - 1e-10)
  out <- x

  if (dist == "lognormal") {
    safe <- !is.na(sigma) & sigma > 0 & !is.na(x) & x > 0
    if (any(safe)) {
      s2 <- log1p((sigma[safe] / x[safe])^2)
      mu <- log(x[safe]) - s2 / 2
      out[safe] <- stats::qlnorm(u[safe], meanlog = mu, sdlog = sqrt(s2))
    }
    if (any(!safe & sigma > 0 & !is.na(sigma))) {
      .et_warn("lognormal perturbation skipped for ", sum(!safe),
               " row(s) with non-positive x.")
    }
    return(out)
  }

  if (dist == "gamma") {
    safe <- !is.na(sigma) & sigma > 0 & !is.na(x) & x > 0
    if (any(safe)) {
      shape <- (x[safe] / sigma[safe])^2
      rate  <-  x[safe] / sigma[safe]^2
      out[safe] <- stats::qgamma(u[safe], shape = shape, rate = rate)
    }
    if (any(!safe & sigma > 0 & !is.na(sigma))) {
      .et_warn("gamma perturbation skipped for ", sum(!safe),
               " row(s) with non-positive x.")
    }
    return(out)
  }

  if (dist == "beta") {
    safe <- !is.na(sigma) & sigma > 0 & !is.na(x) & x > 0 & x < 1 &
            sigma^2 < x * (1 - x) - 1e-12
    if (any(safe)) {
      mu <- x[safe]
      nu <- mu * (1 - mu) / sigma[safe]^2 - 1
      out[safe] <- stats::qbeta(u[safe], shape1 = mu * nu,
                                         shape2 = (1 - mu) * nu)
    }
    n_skip <- sum(!safe & sigma > 0 & !is.na(sigma))
    if (n_skip > 0) {
      .et_warn("beta perturbation skipped for ", n_skip,
               " row(s) outside (0,1) or with sigma^2 >= x*(1-x).")
    }
    return(out)
  }

  stop("Unknown distribution: ", dist)
}

# Project a (possibly non-PD) symmetric matrix onto the PD cone by clipping
# eigenvalues. Used to defend against numerical noise and pairwise-complete
# correlations that are not strictly PSD.
.nearest_pd <- function(M, eps = 1e-8) {
  M  <- (M + t(M)) / 2
  ev <- eigen(M, symmetric = TRUE)
  lam <- pmax(ev$values, eps)
  out <- ev$vectors %*% diag(lam, length(lam)) %*% t(ev$vectors)
  dimnames(out) <- dimnames(M)
  out
}

# Extract the names of fixed-effect predictors (excludes Intercept) from a
# brmsfit object.
.brms_pred_names <- function(fit) {
  fe <- rownames(brms::fixef(fit))
  setdiff(fe, "Intercept")
}

# Extract posterior draws matrix from a brmsfit (rows = draws, cols = params).
# Optionally thin to at most max_draws rows.
.brms_draws_matrix <- function(fit, max_draws = NULL) {
  mat <- as.matrix(fit)
  if (!is.null(max_draws) && nrow(mat) > max_draws) {
    mat <- mat[seq_len(max_draws), , drop = FALSE]
  }
  mat
}

# Detect whether a brms formula (or fitted brmsfit's formula) carries an
# autocorrelation term -- ar(), ma(), arma(), cosy(), unstr(), sar(), car().
# Accepts a formula, brmsformula, mvbrmsformula, or brmsfit. Returns TRUE
# if any response component carries an autocor structure.
#
# Primary path: ask brms::brmsterms() and look at $dpars$mu$ac (covers both
# inline ar()/ma() syntax and the legacy autocor= slot, since brmsterms()
# normalises both onto $ac). For mvbrmsformula objects, scan each response.
# Fallback: regex-scan the deparsed formula for an inline autocor call.
.formula_has_autocor <- function(formula) {
  if (is.null(formula)) return(FALSE)
  if (inherits(formula, "brmsfit")) formula <- formula$formula

  has_ac <- function(bt) {
    if (is.null(bt)) return(FALSE)
    if (!is.null(bt$dpars$mu$ac)) return(TRUE)
    if (!is.null(bt$adforms$ac)) return(TRUE)
    FALSE
  }

  ok <- tryCatch({
    bt <- brms::brmsterms(formula)
    if (inherits(bt, "mvbrmsterms")) {
      any(vapply(bt$terms, has_ac, logical(1)))
    } else {
      has_ac(bt)
    }
  }, error = function(e) NA)
  if (isTRUE(ok) || isFALSE(ok)) return(ok)

  txt <- paste(deparse(formula), collapse = " ")
  grepl("\\b(ar|ma|arma|cosy|unstr|sar|car)\\s*\\(", txt)
}
