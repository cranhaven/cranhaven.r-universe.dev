#' Fit a CoxPH Model Assuming Exchangeable Linkage Errors
#' @import survival
#' @import nleqslv
#'
#' @description
#' Fit Cox proportional hazards regression adjusted for mismatched data based
#' on the approach developed in Vo et al., 2024 assuming exchangeable linkage
#' errors. Block-wise mismatch rates are assumed to be known.
#'
#' @param x A matrix or data.frame of covariates (design matrix).
#' @param y A numeric vector of observed time-to-event outcomes.
#' @param cens A numeric vector indicating censoring status (1 = censored, 0 = event).
#'   Note: This is the reverse of the standard `Surv` object convention where 1 usually
#'   indicates an event.
#' @param m.rate block-wise mismatch rates (should be a vector with length equal
#' to the number of blocks) - by default assume a single block.
#' @param blocks block indicators.
#' @param audit.size a vector of block sizes in the audit sample (selected by
#' simple random sampling) if used to estimate the m.rate (optional). If a
#' single value is provided, assume the same value for all blocks and put out a warning.
#' @param control an optional list variable to of control arguments including
#' "init.beta" for the initial outcome model coefficient estimates) - by
#' default is the naive estimator.
#' @param ... the option to directly pass "control" arguments
#'
#' @returns a list of results from the function called depending on the "family"
#' specified.
#' \item{coefficients}{the outcome model coefficient estimates}
#' \item{var}{the variance-covariance matrix}
#' \item{linear.predictors}{the linear predictors}
#' \item{means}{Column means of the covariate matrix `x`.}
#' \item{n}{Number of observations.}
#' \item{nevent}{Number of events.}
#'
#' @examples
#' set.seed(101)
#' n <- 250
#'
#' # 1. Simulate true data
#' age <- rnorm(n, 60, 8)
#' treatment <- rbinom(n, 1, 0.5)
#'
#' # Simulate true hazard
#' # Older age and lack of treatment increase hazard
#' true_hazard <- exp(0.05 * (age - 60) - 0.5 * treatment)
#' true_time <- rexp(n, rate = true_hazard)
#' cens_time <- rexp(n, rate = 0.1)
#'
#' obs_time <- pmin(true_time, cens_time)
#' status <- as.numeric(true_time <= cens_time) # 1 = event, 0 = censored
#'
#' # 2. Induce 15% exchangeable linkage errors
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' linked_age <- age
#' linked_treatment <- treatment
#' false_link_idx <- sample(1:n, size = length(mis_idx), replace = TRUE)
#' linked_age[mis_idx] <- age[false_link_idx]
#' linked_treatment[mis_idx] <- treatment[false_link_idx]
#'
#' # 3. Prepare matrices for the internal routine
#' cens_ele <- 1 - status
#' X_mat <- cbind(age = linked_age, treatment = linked_treatment)
#'
#' # 4. Fit the model directly
#' # cens = 1 for censored, 0 for event
#' fit_ele <- coxphELE(x = X_mat, y = obs_time, cens = cens_ele, m.rate = 0.15)
#' print(fit_ele$coefficients)
#'
#' @references Vo, T. H., Garès, V., Zhang, L. C., Happe, A., Oger, E.,
#' Paquelet, S., & Chauvet, G. (2024). Cox regression with linked data.
#' Statistics in Medicine, 43(2), 296-314.\cr
#'
#' @export
coxphELE <- function(x, y, cens,
                      m.rate, blocks, audit.size = NULL,
                      control = list(init.beta = NULL), ...) {

  # Data Validation & Pre-processing
  if (!is.matrix(x)) x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)

  # Handle blocks: Map to 1..V for safe indexing
  if (missing(blocks) || is.null(blocks)) {
    blocks <- rep(1, n)
    uni_blocks <- 1
    block_idx <- rep(1, n)
  } else {
    uni_blocks <- unique(blocks)
    block_idx <- match(blocks, uni_blocks)
  }
  V <- length(uni_blocks)
  n_v <- as.vector(table(block_idx))

  # Handle m.rate (alpha = 1 - mismatch rate)
  if (length(m.rate) == 1 && V > 1) m.rate <- rep(m.rate, V)
  if (length(m.rate) != V) stop("Length of m.rate must match number of unique blocks.")
  alpha_vec <- 1 - m.rate
  alpha_obs <- alpha_vec[block_idx]

  # Handle initial beta
  if (is.null(control$init.beta) || identical(control$init.beta, "default")) {
    naive_df <- data.frame(time = y, status = cens, x)
    naive_fit <- survival::coxph(survival::Surv(time, status) ~ ., data = naive_df)
    init.beta <- naive_fit$coefficients
  } else {
    init.beta <- control$init.beta
  }

  # Sorting for Efficient Risk Set Calculation
  ord <- order(y, decreasing = TRUE)
  x_sorted <- x[ord, , drop = FALSE]
  y_sorted <- y[ord]
  delta_sorted <- cens[ord]
  block_idx_sorted <- block_idx[ord]
  alpha_sorted <- alpha_obs[ord]

  x_sums <- rowsum(x, block_idx)
  x_means <- x_sums / n_v
  x_bar_obs <- x_means[block_idx_sorted, , drop = FALSE]

  term_inv_a <- 1 / alpha_sorted
  term_diff  <- (1 / alpha_sorted) - 1
  x_star_sorted <- x_sorted * term_inv_a - x_bar_obs * term_diff

  # Define Estimating Equation Function
  est_eq <- function(beta, ...) {
    lp <- as.vector(x_sorted %*% beta)
    exp_lp <- exp(lp)

    g_sums <- rowsum(exp_lp, block_idx_sorted)
    h_sums <- rowsum(x_sorted * exp_lp, block_idx_sorted)

    g_means <- g_sums / n_v
    h_means <- h_sums / n_v

    g_bar_obs <- g_means[block_idx_sorted]
    h_bar_obs <- h_means[block_idx_sorted, , drop = FALSE]

    g_star <- (exp_lp * term_inv_a) - (g_bar_obs * term_diff)
    h_star <- (x_sorted * exp_lp * term_inv_a) - (h_bar_obs * term_diff)

    S0 <- cumsum(g_star)
    S1 <- apply(h_star, 2, cumsum)
    E <- S1 / S0

    resid <- (x_star_sorted - E) * delta_sorted
    return(colMeans(resid))
  }

  # Identify solver-specific arguments from ...
  solver_args <- list(...)
  # Remove 'data' if it accidentally leaked in from plcoxph
  solver_args$data <- NULL

  # Solve Estimating Equation
  sol <- do.call(nleqslv::nleqslv,
                 c(list(x = init.beta, fn = est_eq, jacobian = TRUE), solver_args))
  coef_est <- sol$x
  J <- sol$jac
  if (sol$termcd > 2) warning("nleqslv algorithm may not have fully converged.")

  # Variance Estimation
  beta <- coef_est
  lp <- as.vector(x_sorted %*% beta)
  exp_lp <- exp(lp)

  g_sums <- rowsum(exp_lp, block_idx_sorted)
  h_sums <- rowsum(x_sorted * exp_lp, block_idx_sorted)
  g_means <- g_sums / n_v
  h_means <- h_sums / n_v

  g_bar_obs <- g_means[block_idx_sorted]
  h_bar_obs <- h_means[block_idx_sorted, , drop = FALSE]

  g_star <- (exp_lp * term_inv_a) - (g_bar_obs * term_diff)
  h_star <- (x_sorted * exp_lp * term_inv_a) - (h_bar_obs * term_diff)

  S0 <- cumsum(g_star)
  S1 <- apply(h_star, 2, cumsum)
  E <- S1 / S0

  H_i <- (x_star_sorted - E) * delta_sorted
  H_bar <- colMeans(H_i)
  H_centered <- sweep(H_i, 2, H_bar, "-")
  S_H2 <- crossprod(H_centered) / (n - 1)
  V2 <- S_H2 / n

  V1 <- matrix(0, p, p)
  if (!is.null(audit.size)) {
    if (length(audit.size) == 1 && V > 1) audit.size <- rep(audit.size, V)

    dev_g <- exp_lp - g_bar_obs
    dev_h <- (x_sorted * exp_lp) - h_bar_obs

    cum_dev_g <- cumsum(dev_g)
    cum_dev_h <- apply(dev_h, 2, cumsum)

    term_frac <- (cum_dev_h - (E * cum_dev_g)) / S0
    dev_x <- x_sorted - x_bar_obs
    H2_i <- (dev_x - term_frac) * delta_sorted

    H2_v_sums <- rowsum(H2_i, block_idx_sorted)
    H2_v <- H2_v_sums / n

    for (v in 1:V) {
      n_sv <- audit.size[v]
      n_av <- n_v[v]
      a_v <- alpha_vec[v]

      if (!is.na(n_sv) && n_sv > 1) {
        fpc <- (1/n_sv) - (1/n_av)
        var_alpha_term <- fpc * (n_sv / (n_sv - 1)) * ((1 - a_v) / (a_v^3))
        vec_h <- H2_v[v, ]
        V1 <- V1 + (tcrossprod(vec_h) * var_alpha_term)
      }
    }
  }

  Var_H <- V1 + V2
  J_inv <- tryCatch(solve(J), error = function(e) matrix(NA, p, p))

  if (any(is.na(J_inv))) {
    warning("Jacobian is singular. Variance matrix is NA.")
    Var_beta <- matrix(NA, p, p)
  } else {
    Var_beta <- J_inv %*% Var_H %*% t(J_inv)
  }

  var_names <- colnames(x)
  if (is.null(var_names)) var_names <- paste0("V", 1:p)
  names(coef_est) <- var_names
  rownames(Var_beta) <- colnames(Var_beta) <- var_names

  result <- list(coefficients = coef_est, var = Var_beta,
                 linear.predictors = as.vector(x %*% coef_est),
                 means = colMeans(x), n = n, nevent = sum(cens))

  class(result) <- "coxphELE"
  return(result)
}


#' @keywords internal
#' @export
fitcoxph.adjELE <- function(x, y, adjustment, control, ...) {

  # Data Retrieval and Validation
  full_data <- adjustment$data_ref$data
  if (is.null(full_data)) {
    stop("The 'adjustment' object does not contain linked data. ",
         "Please recreate the object with 'linked.data' provided.", call. = FALSE)
  }

  # Align Adjustment Data to Outcome Model (X, Y)
  # plcoxph() has already applied 'subset' and 'na.action' to x and y.
  # We use row names to synchronize the adjustment data parameters.

  subset_names <- rownames(x)

  # Handle edge case: User provided matrix without row names
  if (is.null(subset_names)) {
    if (nrow(x) != nrow(full_data)) {
      stop("Row mismatch: Model matrix 'x' has no row names and its length (", nrow(x),
           ") differs from the adjustment data (", nrow(full_data), "). ",
           "Ensure 'linked.data' matches the data passed to 'plcoxph' and no subsetting ",
           "occurred without row names.", call. = FALSE)
    }
    # Assumption: 1:1 implicit mapping if sizes match and no names exist
    idx_map <- seq_len(nrow(full_data))
  } else {
    # Match by name. Strict matching ensures order preservation.
    idx_map <- match(subset_names, rownames(full_data))

    if (anyNA(idx_map)) {
      stop("Row mismatch: Some observations in the model matrix could not be matched ",
           "to the adjustment data. This usually happens if 'data' in plcoxph() ",
           "is different from the data used to create the adjustment object.", call. = FALSE)
    }
  }

  # Extract and Subset Adjustment Parameters
  # We ensure that blocks, m.rates, and audit sizes correspond exactly
  # to the subset of data currently being fitted.

  # Extract original blocks and subset
  full_blocks <- adjustment$blocks
  current_blocks <- full_blocks[idx_map]

  # m.rate in adjustment object can be:
  #   1. Global (length 1)
  #   2. Per-record (length N_full)
  #   3. Per-block (length N_unique_blocks_full)
  m_rate_in <- adjustment$m.rate
  n_full <- nrow(full_data)
  n_unique_full <- length(unique(full_blocks[!is.na(full_blocks)]))

  # Expand m.rate to record-level (full size) to facilitate safe subsetting
  full_m_rate_vec <- NULL

  if (length(m_rate_in) == 1) {
    full_m_rate_vec <- rep(m_rate_in, n_full)
  } else if (length(m_rate_in) == n_full) {
    full_m_rate_vec <- m_rate_in
  } else if (length(m_rate_in) == n_unique_full) {
    # Map per-block rates to records based on unique block order
    # Note unique() preserves appearance order
    uni_b <- unique(full_blocks[!is.na(full_blocks)])
    # Create a lookup
    rate_map <- setNames(m_rate_in, as.character(uni_b))
    full_m_rate_vec <- unname(rate_map[as.character(full_blocks)])
  } else {
    # Should be caught by constructor, but just in case
    stop("Dimensions of 'm.rate' in adjustment object are inconsistent with data dimensions.", call. = FALSE)
  }

  # Subset to current model frame
  current_m_rate_vec <- full_m_rate_vec[idx_map]

  # Audit Size (audit.size)
  audit_in <- adjustment$audit.size
  current_audit_vec <- NULL

  if (!is.null(audit_in)) {
    full_audit_vec <- NULL

    if (length(audit_in) == 1) {
      full_audit_vec <- rep(audit_in, n_full)
    } else if (length(audit_in) == n_full) {
      full_audit_vec <- audit_in
    } else if (length(audit_in) == n_unique_full) {
      uni_b <- unique(full_blocks[!is.na(full_blocks)])
      audit_map <- setNames(audit_in, as.character(uni_b))
      full_audit_vec <- unname(audit_map[as.character(full_blocks)])
    }

    current_audit_vec <- full_audit_vec[idx_map]
  }

  # Prepare Arguments for Computational Engine (coxphELE)
  # Identify unique blocks in the current subset
  unique_current_blocks <- unique(current_blocks)

  # Collapse record-level rates back to block-level vectors for the engine
  # We take the first value for each unique block (assuming consistency within blocks)
  match_idx <- match(unique_current_blocks, current_blocks)

  engine_m_rate <- current_m_rate_vec[match_idx]

  engine_audit <- NULL
  if (!is.null(current_audit_vec)) {
    engine_audit <- current_audit_vec[match_idx]
  }

  # Prepare Response Data
  if (!inherits(y, "Surv")) {
    stop("Response must be a 'Surv' object.", call. = FALSE)
  }

  type <- attr(y, "type")
  if (is.null(type)) type <- "right" # Default assumption if attribute missing

  if (type != "right" && type != "mright") {
    warning("The current implementation of 'coxphELE' assumes right-censored data. ",
            "Results for '", type, "' censoring may be incorrect.", call. = FALSE)
  }

  # Matrix columns usually: time, status (for right censoring)
  time_vec <- y[, ncol(y) - 1]
  status_vec <- y[, ncol(y)]

  # Convert status to censoring indicator (1 = censored)
  cens_vec <- 1 - status_vec

  # Dispatch to Computational Engine
  fit <- coxphELE(
    x = x,
    y = time_vec,
    cens = cens_vec,
    m.rate = engine_m_rate,
    blocks = current_blocks,
    audit.size = engine_audit,
    control = control,
    ...
  )

  return(fit)
}
