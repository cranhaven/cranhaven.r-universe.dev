# ---------------------------------------------------------------------------
# R/variance-taylor.R
# ---------------------------------------------------------------------------
# Taylor series variance estimation for survey_taylor designs.
#
# ATTRIBUTION — VENDORED CODE:
# The functions .svy_onestrat(), .svy_onestage(), .svy_multistage(), and
# .svy_recvar() are adapted from the survey package by Thomas Lumley
# (https://cran.r-project.org/package=survey), licensed under GPL-2 | GPL-3.
# Modifications: integrated with S7 classes; RCPP dispatch removed; error
# messages converted to cli format.
# ---------------------------------------------------------------------------

# ===========================================================================
# Section 1: Vendored linearization helpers
# (adapted from survey package, Thomas Lumley, GPL-2/GPL-3)
# ===========================================================================

# Variance estimator within one stratum.
# Adapted from survey:::onestrat.
.svy_onestrat <- function(
  x,
  cluster,
  nPSU,
  fpc,
  lonely.psu = "remove",
  stratum = NULL,
  stage = 1L
) {
  stratum_center <- attr(x, "recentering")
  if (is.null(stratum_center)) {
    stratum_center <- 0
  } # nocov — always set by .svy_onestage

  if (is.null(fpc)) {
    f <- rep(1, NROW(x))
  } else {
    f <- ifelse(fpc == Inf, 1, (fpc - nPSU) / fpc)
  }

  scale <- if (nPSU > 1L) f * nPSU / (nPSU - 1L) else f

  if (all(f < 1e-7)) {
    return(matrix(0, NCOL(x), NCOL(x)))
  }

  scale <- scale[!duplicated(cluster)]
  x <- rowsum(x, cluster)
  nsubset <- nrow(x)

  # nocov start — sampsize computed post-na.rm filter, so nsubset == nPSU always
  if (nsubset < nPSU) {
    x <- rbind(x, matrix(0, ncol = ncol(x), nrow = nPSU - nsubset))
    scale <- rep(scale[[1L]], NROW(x))
  }
  # nocov end

  if (lonely.psu != "adjust" || nsubset > 1L) {
    stratum_center <- colMeans(x)
  }
  x <- sweep(x = x, MARGIN = 2L, STATS = stratum_center, FUN = "-")

  # nocov start — by construction nsubset == nPSU, so nsubset == 1 implies
  # nPSU == 1
  if (nsubset == 1L && nPSU > 1L) {
    if (lonely.psu == "average") scale <- NA_real_
  }
  # nocov end

  if (nPSU > 1L) {
    return(crossprod(x * sqrt(scale)))
  }

  switch(
    lonely.psu,
    certainty = crossprod(x * sqrt(scale)),
    remove = crossprod(x * sqrt(scale)),
    adjust = crossprod(x * sqrt(scale)),
    average = NA_real_ * crossprod(x),
    fail = cli::cli_abort(
      c("x" = "Stratum {.val {stratum}} has only one PSU at stage {stage}."),
      class = "surveycore_error_lonely_psu"
    ),
    cli::cli_abort(
      c("x" = "Unknown {.arg lonely.psu} value: {.val {lonely.psu}}."),
      class = "surveycore_error_lonely_psu_unknown_option"
    )
  )
}

# One stage of the multistage variance calculation (iterates over strata).
# Adapted from survey:::onestage.
.svy_onestage <- function(
  x,
  strata,
  clusters,
  nPSU,
  fpc,
  lonely.psu = "remove",
  stage = 0L
) {
  if (NROW(x) == 0L) {
    return(matrix(0, NCOL(x), NCOL(x)))
  } # nocov — empty subsets excluded before call

  if (!is.null(lonely.psu) && lonely.psu == "adjust") {
    n_psus_all <- sum(
      tapply(X = nPSU, INDEX = as.numeric(strata), FUN = function(v) v[[1L]])
    )
    recentering <- colSums(x) / n_psus_all
  } else {
    recentering <- 0
  }

  stratvars <- tapply(seq_len(NROW(x)), list(factor(strata)), function(index) {
    x_sub <- `attr<-`(x[index, , drop = FALSE], "recentering", recentering)
    fpc_sub <- if (is.null(fpc)) NULL else fpc[index]
    .svy_onestrat(
      x_sub,
      clusters[index],
      nPSU[index][[1L]],
      fpc_sub,
      lonely.psu = lonely.psu,
      stratum = strata[index][[1L]],
      stage = stage
    )
  })

  p <- NCOL(x)
  # Note: factor(strata) in tapply() above already computes unique levels;
  # nlevels(factor(strata)) would avoid the re-scan, but changing vendored code
  # for this micro-optimisation is not worth the maintenance risk.
  nstrat <- length(unique(strata))
  nokstrat <- sum(vapply(stratvars, function(m) !any(is.na(m)), logical(1L)))

  apply(
    array(unlist(stratvars), c(p, p, length(stratvars))),
    1:2,
    sum,
    na.rm = TRUE
  ) *
    nstrat /
    nokstrat
}

# Recursive multistage variance estimator (pure R; no RCPP path).
# Adapted from survey:::multistage.
# nPSUs: per-row PSU count matrix (sampsize); popmat: per-row pop-size matrix.
.svy_multistage <- function(
  x,
  clusters,
  stratas,
  nPSUs,
  popmat,
  lonely.psu = "remove",
  one.stage = FALSE,
  stage = 1L
) {
  n <- NROW(x)
  fpc <- if (is.null(popmat)) NULL else popmat[, 1L]

  v <- .svy_onestage(
    x,
    stratas[, 1L],
    clusters[, 1L],
    nPSUs[, 1L],
    fpc,
    lonely.psu = lonely.psu,
    stage = stage
  )

  if (!isTRUE(one.stage) && !is.null(popmat) && NCOL(clusters) > 1L) {
    v.sub <- by(seq_len(n), list(as.numeric(clusters[, 1L])), function(index) {
      .svy_multistage(
        x[index, , drop = FALSE],
        clusters[index, -1L, drop = FALSE],
        stratas[index, -1L, drop = FALSE],
        nPSUs[index, -1L, drop = FALSE],
        popmat[index, -1L, drop = FALSE],
        lonely.psu = lonely.psu,
        one.stage = one.stage - 1L,
        stage = stage + 1L
      ) *
        nPSUs[index[[1L]], 1L] /
        popmat[index[[1L]], 1L]
    })
    for (i in seq_along(v.sub)) {
      v <- v + v.sub[[i]]
    }
  }

  dimnames(v) <- list(colnames(x), colnames(x))
  v
}

# Master variance function (pure R; no post-strata).
# Adapted from survey:::svyrecvar.
# fpcs: list(sampsize = matrix, popsize = matrix or NULL)
.svy_recvar <- function(
  x,
  clusters,
  stratas,
  fpcs,
  lonely.psu = "remove",
  one.stage = FALSE
) {
  x <- as.matrix(x)
  .svy_multistage(
    x,
    clusters,
    stratas,
    nPSUs = fpcs[["sampsize"]],
    popmat = fpcs[["popsize"]],
    lonely.psu = lonely.psu,
    one.stage = one.stage,
    stage = 1L
  )
}


# ===========================================================================
# Section 2: Design input extractor for survey_taylor
# ===========================================================================

# Build all inputs needed for .svy_recvar() from a survey_taylor object.
# Returns a list ready for calling the variance functions.
.taylor_build_inputs <- function(design, y_col, na.rm = TRUE) {
  data <- design@data
  vars <- design@variables

  y <- data[[y_col]]

  # Build cluster/strata/FPC matrices from full dataset BEFORE na.rm
  # filtering. sampsize is a design property (number of PSUs per
  # stratum), not an outcome property — it must reflect the full
  # dataset. This matches survey package semantics.
  mats <- .build_cluster_matrices(data, vars)

  # --- Handle na.rm ---
  keep <- if (na.rm) !is.na(y) else seq_along(y)
  y <- y[keep]
  w <- data[[vars$weights]][keep]
  mats$clusters_mat <- mats$clusters_mat[keep, , drop = FALSE]
  mats$strata_mat <- mats$strata_mat[keep, , drop = FALSE]
  mats$fpcs$sampsize <- mats$fpcs$sampsize[keep, , drop = FALSE]
  if (!is.null(mats$fpcs$popsize)) {
    mats$fpcs$popsize <- mats$fpcs$popsize[keep, , drop = FALSE]
  }

  list(
    y = y,
    w = w,
    clusters = mats$clusters_mat,
    stratas = mats$strata_mat,
    fpcs = mats$fpcs
  )
}


# ===========================================================================
# Section 3: Core Taylor series estimation functions (internal)
# ===========================================================================

# Compute weighted mean and its variance for a single numeric variable.
# Returns list(mean, var, se, df).
.taylor_mean <- function(design, y_col, na.rm = TRUE) {
  inp <- .taylor_build_inputs(design, y_col, na.rm = na.rm)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  x <- matrix(inp$y, ncol = 1L, dimnames = list(NULL, y_col))
  w <- inp$w
  psum <- sum(w)
  average <- drop(crossprod(x, w) / psum) # weighted mean

  x_centered <- sweep(x, 2L, average)
  linearized <- .maybe_apply_calibration(x_centered * w / psum, design)
  cal_df_reduction <- .get_calibration_df_reduction(design)

  v <- .svy_recvar(
    linearized,
    inp$clusters,
    inp$stratas,
    inp$fpcs,
    lonely.psu = lonely.psu
  )

  df_design <- max(1L, .degf_taylor(design@data, design@variables))
  df_final <- df_design - cal_df_reduction
  if (df_final <= 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "Calibration reduces design df ({df_design}) to {df_final}."
        ),
        "i" = "CIs and p-values may be invalid.",
        "v" = paste0(
          "Reduce the number of calibration columns or use a larger design."
        )
      ),
      class = "surveycore_warning_zero_df_after_calibration"
    )
    df_final <- max(1L, df_final)
  }

  list(
    mean = average[[1L]],
    var = v[[1L, 1L]],
    se = sqrt(v[[1L, 1L]]),
    df = df_final
  )
}

# Compute weighted total and its variance for a single numeric variable.
# Returns list(total, var, se, df).
.taylor_total <- function(design, y_col, na.rm = TRUE) {
  inp <- .taylor_build_inputs(design, y_col, na.rm = na.rm)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  x <- matrix(inp$y, ncol = 1L, dimnames = list(NULL, y_col))
  w <- inp$w
  total <- drop(crossprod(x, w)) # weighted total

  linearized <- .maybe_apply_calibration(x * w, design)
  cal_df_reduction <- .get_calibration_df_reduction(design)

  v <- .svy_recvar(
    linearized,
    inp$clusters,
    inp$stratas,
    inp$fpcs,
    lonely.psu = lonely.psu
  )

  df_design <- max(1L, .degf_taylor(design@data, design@variables))
  df_final <- df_design - cal_df_reduction
  if (df_final <= 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "Calibration reduces design df ({df_design}) to {df_final}."
        ),
        "i" = "CIs and p-values may be invalid.",
        "v" = paste0(
          "Reduce the number of calibration columns or use a larger design."
        )
      ),
      class = "surveycore_warning_zero_df_after_calibration"
    )
    df_final <- max(1L, df_final)
  }

  list(
    total = total[[1L]],
    var = v[[1L, 1L]],
    se = sqrt(v[[1L, 1L]]),
    df = df_final
  )
}


# ===========================================================================
# Section 4: Variance-covariance pair estimation for survey_taylor
# (used by get_corr())
# ===========================================================================

# Compute the design-based variance-covariance of two variables and the
# variance-covariance (meta-vcov) of the three estimands
# (Var(X), Cov(X,Y), Var(Y)) via Taylor linearization.
#
# Adapted from the linearization approach used in survey::svyvar().
# For a pair (X, Y), the influence function for Var(X) is:
#   infl_i = w_i * d_i * ((x_i - xbar)^2 - Var(X)) / W_d
# where d_i = pair_mask_i (1 if in domain AND non-NA for both X and Y, else 0)
# and W_d = sum(w_i * d_i). Out-of-domain rows contribute 0 to the influence,
# preserving correct cluster/strata variance estimation.
#
# @param design  A survey_taylor object.
# @param x_col  Character. Name of the first numeric variable.
# @param y_col  Character. Name of the second numeric variable.
# @param domain Numeric 0/1 vector (full length). Domain membership mask
#               (from .apply_domain()); 1 = in-domain row.
# @param na.rm  Logical. If TRUE, exclude rows where x or y is NA.
# @return Named list:
#   $a          Var(X) estimate
#   $b          Cov(X, Y) estimate
#   $c          Var(Y) estimate
#   $sigma      3x3 meta-vcov of (Var(X), Cov(X,Y), Var(Y))
#   $n          Pairwise unweighted count (in-domain, non-NA for both)
#   $n_weighted Pairwise weighted sum
.vcov_pair_taylor <- function(design, x_col, y_col, domain, na.rm = TRUE) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]
  x_all <- data[[x_col]]
  y_all <- data[[y_col]]

  # Pair mask: domain AND non-NA for both variables
  if (na.rm) {
    pair_mask <- domain * as.numeric(!is.na(x_all) & !is.na(y_all))
  } else {
    pair_mask <- domain
  }

  n_d <- as.integer(sum(pair_mask))
  W_d <- sum(w * pair_mask)

  if (n_d < 2L || W_d <= 0) {
    sigma <- matrix(NA_real_, 3L, 3L)
    return(list(
      a = NA_real_,
      b = NA_real_,
      c = NA_real_,
      sigma = sigma,
      n = n_d,
      n_weighted = W_d
    ))
  }

  # Weighted means over the pair domain
  x_safe <- ifelse(pair_mask > 0, x_all, 0)
  y_safe <- ifelse(pair_mask > 0, y_all, 0)
  xbar <- sum(w * pair_mask * x_safe) / W_d
  ybar <- sum(w * pair_mask * y_safe) / W_d

  # Centered variables (0 for out-of-domain rows)
  cx <- pair_mask * (x_safe - xbar)
  cy <- pair_mask * (y_safe - ybar)

  # Variance-covariance estimates
  a <- sum(w * cx^2) / W_d
  b <- sum(w * cx * cy) / W_d
  c_val <- sum(w * cy^2) / W_d

  # Linearized influence functions (full-length vectors)
  # infl_j_i = w_i * pair_mask_i * (score_i^2 - estimand) / W_d
  infl_a <- w * pair_mask * (cx^2 - a) / W_d
  infl_b <- w * pair_mask * (cx * cy - b) / W_d
  infl_c <- w * pair_mask * (cy^2 - c_val) / W_d

  # Build cluster / strata / FPC structure from full dataset
  mats <- .build_cluster_matrices(data, vars)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  infl_mat <- cbind(infl_a, infl_b, infl_c)
  colnames(infl_mat) <- c("a", "b", "c")
  sigma <- .svy_recvar(
    infl_mat,
    mats$clusters_mat,
    mats$strata_mat,
    mats$fpcs,
    lonely.psu = lonely.psu
  )

  list(a = a, b = b, c = c_val, sigma = sigma, n = n_d, n_weighted = W_d)
}
