#' Calibrate weights to multiple pass-rate targets
#'
#' Adjust initial positive weights so that an overall binary outcome rate and
#' subgroup outcome rates approach or exactly match specified targets. The
#' optimization is performed on demographic-cell-by-outcome aggregates.
#'
#' @param data A data frame containing one row per sampled unit.
#' @param outcome Name of a binary 0/1 outcome column.
#' @param weight Name of the initial positive weight column.
#' @param group_vars Character vector naming grouping variables.
#' @param targets A data frame with columns `variable`, `level`, and
#'   `target_rate`; optional column `priority` controls soft-mode importance.
#' @param lower,upper Scalar lower and upper bounds on the multiplier applied
#'   to each initial cell weight.
#' @param mode Either `"soft"` or `"exact"`.
#' @param distance Distance function of the calibration family. `"chi2"` (the
#'   default) is the linear/quadratic distance solved as a bounded QP via OSQP
#'   and reproduces earlier behaviour. `"raking"` is the entropy distance
#'   `g log g - g + 1`, whose solution `g = exp(eta)` is strictly positive by
#'   construction; it is solved by a dual Newton iteration and currently
#'   supports `mode = "exact"` only. Raking is unbounded above, so `lower` and
#'   `upper` are not enforced for it but multipliers outside that range are
#'   reported in the diagnostics. `"logit"` is the bounded logit distance whose
#'   multipliers stay strictly inside `(lower, upper)` by construction (requires
#'   `lower < 1 < upper`); it is also dual-Newton solved and `mode = "exact"`
#'   only. Use `"logit"` when capping extreme weights is a hard requirement.
#' @param lambda Positive soft-constraint penalty. Larger values emphasize
#'   target matching more strongly.
#' @param new_weight Name of the calibrated weight column added to `data`.
#' @param verbose Logical; passed to OSQP.
#'
#' @return An object of class `pass_rate_calibration`.
#' @examples
#' d <- example_rate_data(300)
#' targets <- make_rate_targets(groups = list(sex = c(M = 0.72, F = 0.68)))
#' fit <- calibrate_pass_rates(d, "qualified", "initial_weight",
#'                             group_vars = "sex", targets = targets)
#' fit$target_check
#' @export
calibrate_pass_rates <- function(
    data,
    outcome,
    weight,
    group_vars,
    targets,
    lower = 0.25,
    upper = 4,
    mode = c("soft", "exact"),
    distance = c("chi2", "raking", "logit"),
    lambda = 1e4,
    new_weight = "weight_calibrated",
    verbose = FALSE
) {
  mode <- match.arg(mode)
  distance <- match.arg(distance)

  if (!requireNamespace("osqp", quietly = TRUE)) {
    stop("Package 'osqp' is required. Run install.packages('osqp').", call. = FALSE)
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required. Run install.packages('Matrix').", call. = FALSE)
  }

  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  if (!is.character(outcome) || length(outcome) != 1L) {
    stop("outcome must be one column name.", call. = FALSE)
  }
  if (!is.character(weight) || length(weight) != 1L) {
    stop("weight must be one column name.", call. = FALSE)
  }
  if (!is.character(group_vars) || length(group_vars) < 1L) {
    stop("group_vars must contain at least one column name.", call. = FALSE)
  }
  if (anyDuplicated(group_vars)) {
    stop("group_vars must not contain duplicates.", call. = FALSE)
  }

  required_cols <- unique(c(outcome, weight, group_vars))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  target_cols <- c("variable", "level", "target_rate")
  missing_target_cols <- setdiff(target_cols, names(targets))
  if (length(missing_target_cols) > 0L) {
    stop("targets is missing columns: ",
         paste(missing_target_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.numeric(lower) || length(lower) != 1L ||
      !is.numeric(upper) || length(upper) != 1L ||
      !is.finite(lower) || !is.finite(upper) ||
      lower < 0 || upper <= lower || lower > 1 || upper < 1) {
    stop("Use scalar bounds satisfying 0 <= lower <= 1 <= upper and lower < upper.",
         call. = FALSE)
  }
  if (!is.numeric(lambda) || length(lambda) != 1L ||
      !is.finite(lambda) || lambda <= 0) {
    stop("lambda must be one finite positive number.", call. = FALSE)
  }

  d <- as.numeric(data[[weight]])
  y_raw <- data[[outcome]]
  if (is.logical(y_raw)) y_raw <- as.integer(y_raw)
  y <- suppressWarnings(as.numeric(as.character(y_raw)))

  if (any(!is.finite(d)) || any(d <= 0)) {
    stop("Initial weights must all be finite and strictly positive.", call. = FALSE)
  }
  if (anyNA(y) || any(!y %in% c(0, 1))) {
    stop("The outcome column must contain only 0 and 1.", call. = FALSE)
  }
  if (anyNA(data[group_vars])) {
    stop("Grouping variables contain missing values. Recode missing values as a category first.",
         call. = FALSE)
  }

  targets <- as.data.frame(targets, stringsAsFactors = FALSE)
  targets$variable <- as.character(targets$variable)
  targets$level <- as.character(targets$level)
  targets$target_rate <- as.numeric(targets$target_rate)

  # Optional target statistic: "proportion" (the outcome rate, default and
  # backward compatible), "mean" or "total" of a numeric value_var.
  if (!"statistic" %in% names(targets)) targets$statistic <- "proportion"
  targets$statistic <- as.character(targets$statistic)
  if (!"value_var" %in% names(targets)) targets$value_var <- NA_character_
  targets$value_var <- as.character(targets$value_var)
  valid_stats <- c("proportion", "mean", "total")
  if (any(!targets$statistic %in% valid_stats)) {
    stop("statistic must be one of: ", paste(valid_stats, collapse = ", "),
         call. = FALSE)
  }
  is_value_stat <- targets$statistic %in% c("mean", "total")
  if (any(is_value_stat & (is.na(targets$value_var) | targets$value_var == ""))) {
    stop("Targets with statistic 'mean' or 'total' require a value_var.",
         call. = FALSE)
  }

  # `value` selects which category a proportion target measures. A proportion
  # target measures the share of value_var == value; it defaults to the legacy
  # pass rate (value_var = outcome, value = 1) so old target tables are
  # unchanged. value_var/value are not used by mean/total.
  if (!"value" %in% names(targets)) targets$value <- NA_character_
  targets$value <- as.character(targets$value)
  is_prop <- targets$statistic == "proportion"
  no_vv <- is_prop & (is.na(targets$value_var) | targets$value_var == "")
  targets$value_var[no_vv] <- outcome
  targets$value[is_prop & is.na(targets$value)] <- "1"

  if (nrow(targets) < 1L) stop("targets must contain at least one row.", call. = FALSE)
  if (any(!is.finite(targets$target_rate))) {
    stop("All target_rate values must be finite.", call. = FALSE)
  }
  is_prop <- targets$statistic == "proportion"
  if (any(is_prop & (targets$target_rate < 0 | targets$target_rate > 1))) {
    stop("proportion target_rate values must be between 0 and 1.", call. = FALSE)
  }

  if (!"priority" %in% names(targets)) targets$priority <- 1
  targets$priority <- as.numeric(targets$priority)
  if (any(!is.finite(targets$priority)) || any(targets$priority <= 0)) {
    stop("priority must contain finite positive numbers.", call. = FALSE)
  }

  target_key <- paste(targets$variable, targets$level, targets$statistic, targets$value_var, targets$value, sep = "\u001F")
  if (anyDuplicated(target_key)) {
    stop("targets contains duplicate target rows.", call. = FALSE)
  }

  # Categorical variables (other than the outcome and grouping variables) named
  # by proportion targets must split the aggregation cells so each cell is pure
  # in the measured indicator; otherwise a single cell multiplier could not
  # change the within-group share. No extra splits when only the legacy outcome
  # proportion is used, so old behaviour is preserved byte for byte.
  extra_split_vars <- setdiff(unique(targets$value_var[is_prop]),
                              c(group_vars, outcome))
  missing_split <- setdiff(extra_split_vars, names(data))
  if (length(missing_split)) {
    stop("Proportion value_var(s) not found in data: ",
         paste(missing_split, collapse = ", "), call. = FALSE)
  }

  # Aggregate to demographic cell x outcome (x any extra proportion splits).
  cell_vars <- unique(c(group_vars, extra_split_vars))
  key_data <- data[cell_vars]
  key_data[] <- lapply(key_data, as.character)
  key_parts <- c(key_data, list(.outcome = as.character(y)))
  aggregate_key <- do.call(paste, c(key_parts, list(sep = "\u001F")))

  D_matrix <- rowsum(d, group = aggregate_key, reorder = FALSE)
  D <- as.numeric(D_matrix[, 1L])
  aggregate_names <- rownames(D_matrix)
  first_index <- match(aggregate_names, aggregate_key)

  cells <- data[first_index, cell_vars, drop = FALSE]
  cells[] <- lapply(cells, as.character)
  cells$.outcome <- y[first_index]
  cells$.initial_total <- D
  cells$.cell_id <- seq_len(nrow(cells))

  m <- nrow(cells)
  grand_total <- sum(D)

  # Per-cell weighted sums of each numeric value variable used by mean/total
  # targets: cell_value_sum[[vv]][c] = sum_{i in cell c} d_i * vv_i, aligned to
  # the cell order. The cell mean is then cell_value_sum / D.
  stat_vars <- unique(targets$value_var[is_value_stat])
  stat_vars <- stat_vars[!is.na(stat_vars)]
  cell_value_sum <- list()
  for (vv in stat_vars) {
    if (!vv %in% names(data)) {
      stop("value_var '", vv, "' is not a column in data.", call. = FALSE)
    }
    vals <- suppressWarnings(as.numeric(data[[vv]]))
    if (anyNA(vals) || any(!is.finite(vals))) {
      stop("value_var '", vv, "' must be numeric and free of missing values.",
           call. = FALSE)
    }
    s <- rowsum(d * vals, group = aggregate_key, reorder = FALSE)
    cell_value_sum[[vv]] <- as.numeric(s[, 1L])
  }

  # Preserve the current population margins as hard constraints.
  margin_rows <- list(overall = rep(1, m))
  margin_rhs_named <- c(overall = grand_total)

  for (v in group_vars) {
    observed_levels <- unique(cells[[v]])
    if (length(observed_levels) > 1L) {
      included_levels <- observed_levels[-length(observed_levels)]
      for (lev in included_levels) {
        row_name <- paste(v, lev, sep = "=")
        z <- as.numeric(cells[[v]] == lev)
        margin_rows[[row_name]] <- z
        margin_rhs_named[row_name] <- sum(D * z)
      }
    }
  }

  M <- Matrix::Matrix(do.call(rbind, margin_rows), sparse = TRUE)
  margin_rhs <- as.numeric(margin_rhs_named)

  # Construct linearized target rows. Each row is A_row %*% x compared to
  # rate_rhs: proportion/mean rows are homogeneous (rhs 0), total rows have
  # rhs = target total.
  rate_rows <- vector("list", nrow(targets))
  rate_rhs <- numeric(nrow(targets))
  target_sizes <- numeric(nrow(targets))
  initial_rates <- numeric(nrow(targets))
  target_names <- character(nrow(targets))

  # Build the cell mask for a target. Supports the overall target, single
  # grouping variables, and colon-joined interaction (cross-classification)
  # keys such as variable = "sex:residence", level = "M:Urban" (intersection of
  # the component masks). Interaction targets add only a rate row, not a margin.
  build_mask <- function(v, lev) {
    if (v %in% c(".overall", "overall", "TOTAL")) return(rep(TRUE, m))
    if (grepl(":", v, fixed = TRUE)) {
      vars <- strsplit(v, ":", fixed = TRUE)[[1]]
      levs <- strsplit(lev, ":", fixed = TRUE)[[1]]
      if (length(vars) != length(levs)) {
        stop("Interaction target '", v, "' = '", lev,
             "' has a mismatched number of component variables and levels.",
             call. = FALSE)
      }
      bad <- setdiff(vars, group_vars)
      if (length(bad)) {
        stop("Interaction target variable(s) not in group_vars: ",
             paste(bad, collapse = ", "), call. = FALSE)
      }
      mask <- rep(TRUE, m)
      for (i in seq_along(vars)) mask <- mask & (cells[[vars[i]]] == levs[i])
      return(mask)
    }
    if (!v %in% group_vars) {
      stop("Target variable '", v,
           "' is not in group_vars. Use '.overall' for the total target.",
           call. = FALSE)
    }
    cells[[v]] == lev
  }

  # Per-cell 0/1 indicator that a proportion target's measured variable equals
  # its value. The outcome uses the numeric .outcome column; any other variable
  # is one of the (pure) cell-defining columns.
  prop_indicator <- function(vv, val) {
    if (vv == outcome) {
      as.numeric(cells$.outcome == as.numeric(val))
    } else {
      as.numeric(cells[[vv]] == val)
    }
  }

  for (j in seq_len(nrow(targets))) {
    v <- targets$variable[j]
    lev <- targets$level[j]
    r <- targets$target_rate[j]

    if (v %in% c(".overall", "overall", "TOTAL")) {
      v <- ".overall"
      lev <- ".all"
      targets$variable[j] <- v
      targets$level[j] <- lev
    }
    mask <- build_mask(v, lev)

    if (!any(mask)) {
      stop("No observed sample cell for target: ", v, " = ", lev, call. = FALSE)
    }

    target_sizes[j] <- sum(D[mask])
    stat <- targets$statistic[j]
    if (stat == "proportion") {
      # sum x_c * I(group) * (ind_c - target) = 0  ->  group share = target,
      # where ind_c is the per-cell indicator of value_var == value.
      ind <- prop_indicator(targets$value_var[j], targets$value[j])
      initial_rates[j] <- sum(D[mask] * ind[mask]) / target_sizes[j]
      rate_rows[[j]] <- as.numeric(mask) * (ind - r)
      rate_rhs[j] <- 0
    } else {
      wbar <- cell_value_sum[[targets$value_var[j]]] / D
      if (stat == "mean") {
        # sum x_c * I(group) * (wbar_c - target) = 0  ->  group mean = target
        initial_rates[j] <- sum(D[mask] * wbar[mask]) / target_sizes[j]
        rate_rows[[j]] <- as.numeric(mask) * (wbar - r)
        rate_rhs[j] <- 0
      } else {
        # sum x_c * I(group) * wbar_c = target  ->  group total = target
        initial_rates[j] <- sum(D[mask] * wbar[mask])
        rate_rows[[j]] <- as.numeric(mask) * wbar
        rate_rhs[j] <- r
      }
    }
    target_names[j] <- paste(v, lev, sep = "=")
  }

  R <- Matrix::Matrix(do.call(rbind, rate_rows), sparse = TRUE)
  rownames(R) <- make.unique(target_names)

  # Soft-penalty scale per target: proportions use absolute rate error (scale 1);
  # mean/total are normalized by their target magnitude so their penalty is a
  # relative error, comparable to rate penalties when statistics are mixed.
  pen_scale <- ifelse(targets$statistic == "proportion", 1,
                      pmax(abs(targets$target_rate), 1e-8))

  if (distance == "chi2") {
    # Minimize sum_c (x_c - D_c)^2 / D_c.
    # OSQP form: 0.5*x'P*x + q'x.
    P <- Matrix::Diagonal(m, x = 2 / D)
    q <- rep(-2, m)

    if (mode == "soft") {
      penalty <- lambda * grand_total * targets$priority /
        (target_sizes^2 * pen_scale^2)
      W <- Matrix::Diagonal(nrow(R), x = penalty)
      P <- P + 2 * crossprod(R, W %*% R)
      # Linear term for penalized targets with a non-zero right-hand side
      # (totals): penalize (Rx - rate_rhs)^2, not just (Rx)^2.
      if (any(rate_rhs != 0)) {
        q <- q - 2 * as.numeric(crossprod(R, W %*% rate_rhs))
      }
    }

    I_m <- Matrix::Diagonal(m)

    if (mode == "exact") {
      A <- rbind(M, R, I_m)
      l <- c(margin_rhs, rate_rhs, lower * D)
      u <- c(margin_rhs, rate_rhs, upper * D)
    } else {
      A <- rbind(M, I_m)
      l <- c(margin_rhs, lower * D)
      u <- c(margin_rhs, upper * D)
    }

    P <- methods::as(
      methods::as(Matrix::forceSymmetric(P, uplo = "U"), "generalMatrix"),
      "CsparseMatrix"
    )
    A <- methods::as(methods::as(A, "generalMatrix"), "CsparseMatrix")

    settings <- osqp::osqpSettings(
      verbose = verbose,
      max_iter = 100000L,
      eps_abs = 1e-8,
      eps_rel = 1e-8,
      polishing = TRUE,
      scaled_termination = TRUE
    )

    solution <- osqp::solve_osqp(P = P, q = q, A = A, l = l, u = u,
                                 pars = settings)
    status <- as.character(solution$info$status)

    if (!grepl("solved", status, ignore.case = TRUE)) {
      stop(
        "Optimization did not solve successfully. OSQP status: ", status,
        if (mode == "exact") {
          paste0(". The targets may be mutually inconsistent or the bounds ",
                 "may be too narrow. Try mode='soft', or widen lower/upper.")
        } else {
          ". Try widening lower/upper or reducing numerical strictness."
        },
        call. = FALSE
      )
    }

    x <- as.numeric(solution$x)
  } else {
    # Deville-Sarndal distance family, exact calibration via dual Newton.
    # Constraints A_eq x = t_eq: population margins plus linearized rate rows.
    if (distance == "raking") {
      # Entropy distance: g(eta) = exp(eta), strictly positive, unbounded above.
      gfun <- function(eta) exp(eta)
      gpfun <- function(eta) exp(eta)
    } else {
      # Logit distance: g maps eta into the open interval (lower, upper).
      if (lower >= 1 || upper <= 1) {
        stop("distance='logit' requires lower < 1 < upper.", call. = FALSE)
      }
      Lb <- lower; Ub <- upper
      Acoef <- (Ub - Lb) / ((1 - Lb) * (Ub - 1))
      gfun <- function(eta) {
        z <- exp(Acoef * eta)
        (Lb * (Ub - 1) + Ub * (1 - Lb) * z) / ((Ub - 1) + (1 - Lb) * z)
      }
      gpfun <- function(eta) {
        z <- exp(Acoef * eta)
        den <- (Ub - 1) + (1 - Lb) * z
        (Ub - Lb)^2 * z / den^2
      }
    }
    A_eq <- rbind(M, R)
    t_eq <- c(margin_rhs, rate_rhs)
    # Margins stay hard (reg = 0); in soft mode the target rows get a ridge
    # Omega^{-1} = size^2 / (2 lambda grand_total priority), matching the chi2
    # soft penalty strength, so larger lambda approaches the exact solution.
    if (mode == "soft") {
      penalty <- lambda * grand_total * targets$priority /
        (target_sizes^2 * pen_scale^2)
      reg <- c(rep(0, nrow(M)), 1 / (2 * penalty))
    } else {
      reg <- rep(0, nrow(M) + nrow(R))
    }
    solution <- .calibrate_dual(D, A_eq, t_eq, gfun, gpfun, reg = reg,
                                verbose = verbose)
    status <- if (isTRUE(solution$converged)) "solved" else "not converged"
    if (!isTRUE(solution$converged)) {
      stop(
        "The ", distance, " dual iteration did not converge (max residual ",
        signif(solution$max_resid, 3), " after ", solution$iterations,
        " iterations). ",
        if (mode == "exact") {
          paste0("The exact targets may be infeasible or unreachable",
                 if (distance == "logit")
                   " within (lower, upper); try widening the bounds or "
                 else ", for example an all-pass or all-fail group; try ",
                 "mode='soft'.")
        } else {
          "Try a smaller lambda or distance='chi2'."
        },
        call. = FALSE
      )
    }
    x <- solution$x
  }

  g <- x / D

  cells$.adjusted_total <- x
  cells$.multiplier <- g
  cells$.at_lower_bound <- g <= lower + 1e-6
  cells$.at_upper_bound <- g >= upper - 1e-6

  multiplier_map <- stats::setNames(g, aggregate_names)
  output_data <- data
  output_data[[new_weight]] <- d * unname(multiplier_map[aggregate_key])

  achieved_rates <- numeric(nrow(targets))
  for (j in seq_len(nrow(targets))) {
    mask <- build_mask(targets$variable[j], targets$level[j])
    stat <- targets$statistic[j]
    if (stat == "proportion") {
      ind <- prop_indicator(targets$value_var[j], targets$value[j])
      achieved_rates[j] <- sum(x[mask] * ind[mask]) / sum(x[mask])
    } else {
      wbar <- cell_value_sum[[targets$value_var[j]]] / D
      achieved_rates[j] <- if (stat == "mean") {
        sum(x[mask] * wbar[mask]) / sum(x[mask])
      } else {
        sum(x[mask] * wbar[mask])  # total
      }
    }
  }

  target_check <- data.frame(
    variable = targets$variable,
    level = targets$level,
    statistic = targets$statistic,
    value_var = targets$value_var,
    value = targets$value,
    target_rate = targets$target_rate,
    initial_rate = initial_rates,
    achieved_rate = achieved_rates,
    error = achieved_rates - targets$target_rate,
    abs_error = abs(achieved_rates - targets$target_rate),
    priority = targets$priority,
    stringsAsFactors = FALSE
  )

  margin_check <- data.frame(
    variable = ".overall",
    level = ".all",
    initial_total = grand_total,
    adjusted_total = sum(x),
    relative_change = sum(x) / grand_total - 1,
    stringsAsFactors = FALSE
  )

  for (v in group_vars) {
    for (lev in unique(cells[[v]])) {
      mask <- cells[[v]] == lev
      level_initial_total <- sum(D[mask])
      level_adjusted_total <- sum(x[mask])
      margin_check <- rbind(
        margin_check,
        data.frame(
          variable = v,
          level = lev,
          initial_total = level_initial_total,
          adjusted_total = level_adjusted_total,
          relative_change = level_adjusted_total / level_initial_total - 1,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  old_w <- d
  new_w <- output_data[[new_weight]]
  ess <- function(w) sum(w)^2 / sum(w^2)
  deff <- function(w) {
    if (length(w) < 2L || mean(w) == 0) return(NA_real_)
    1 + (stats::sd(w) / mean(w))^2
  }

  diagnostics <- data.frame(
    metric = c(
      "sample_size",
      "observed_optimization_cells",
      "initial_ESS",
      "calibrated_ESS",
      "initial_weight_DEFF",
      "calibrated_weight_DEFF",
      "minimum_multiplier",
      "median_multiplier",
      "maximum_multiplier",
      "cells_at_lower_bound",
      "cells_at_upper_bound",
      "maximum_absolute_target_error"
    ),
    value = c(
      nrow(data),
      m,
      ess(old_w),
      ess(new_w),
      deff(old_w),
      deff(new_w),
      min(g),
      stats::median(g),
      max(g),
      sum(cells$.at_lower_bound),
      sum(cells$.at_upper_bound),
      max(target_check$abs_error)
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      data = output_data,
      cell_weights = cells,
      target_check = target_check,
      margin_check = margin_check,
      diagnostics = diagnostics,
      solver_status = status,
      solver = solution,
      settings = list(
        outcome = outcome,
        weight = weight,
        group_vars = group_vars,
        lower = lower,
        upper = upper,
        mode = mode,
        distance = distance,
        lambda = lambda,
        new_weight = new_weight
      ),
      call = match.call()
    ),
    class = "pass_rate_calibration"
  )
}

# Dual Newton solver for calibration in the Deville-Sarndal distance family.
# Solves for the dual variable lambda with x_c = D_c * g(eta_c), eta = A^T
# lambda, where g is the distance's weight-ratio function and gprime its
# derivative. `reg` is a per-constraint ridge: 0 for hard (exactly enforced)
# rows and Omega^{-1} > 0 for soft (penalized) rows. The stationarity system is
#   F(lambda) = A x - t + reg * lambda = 0,   J = A diag(D g'(eta)) A^T + diag(reg)
# so reg = 0 recovers exact calibration and larger penalties (smaller reg)
# approach it. A backtracking line search on the infinity-norm residual gives
# robustness. Returns the primal solution x and convergence information.
.calibrate_dual <- function(D, A, t, gfun, gpfun, reg = NULL, max_iter = 200L,
                            tol = 1e-10, verbose = FALSE) {
  A <- methods::as(A, "CsparseMatrix")
  k <- nrow(A)
  m <- ncol(A)
  if (is.null(reg)) reg <- rep(0, k)
  lambda <- rep(0, k)

  resid_of <- function(lambda) {
    eta <- as.numeric(Matrix::crossprod(A, lambda))  # A^T lambda, length m
    x <- D * gfun(eta)
    list(eta = eta, x = x, F = as.numeric(A %*% x) - t + reg * lambda)
  }

  cur <- resid_of(lambda)
  resid <- max(abs(cur$F))

  for (it in seq_len(max_iter)) {
    if (is.finite(resid) && resid < tol) {
      return(list(x = cur$x, iterations = it - 1L, converged = TRUE,
                  max_resid = resid))
    }
    Dg <- D * gpfun(cur$eta)
    J <- as.matrix(A %*% Matrix::Diagonal(m, x = Dg) %*% Matrix::t(A)) +
      diag(reg, k)
    step <- tryCatch(solve(J, cur$F),
                     error = function(e) solve(J + diag(1e-10, k), cur$F))

    # Backtracking line search: accept the step only if it reduces the residual.
    alpha <- 1
    accepted <- FALSE
    repeat {
      try_res <- resid_of(lambda - alpha * step)
      new_resid <- max(abs(try_res$F))
      if (isTRUE(new_resid < resid)) {
        accepted <- TRUE
        break
      }
      alpha <- alpha / 2
      if (alpha < 1e-8) break
    }
    if (!accepted) break  # cannot make progress: likely infeasible

    lambda <- lambda - alpha * step
    cur <- try_res
    resid <- new_resid
    if (isTRUE(verbose)) {
      message(sprintf("dual iter %d: max residual %.3e", it, resid))
    }
  }

  list(x = cur$x, iterations = max_iter,
       converged = is.finite(resid) && resid < tol, max_resid = resid)
}
