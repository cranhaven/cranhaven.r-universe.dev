#' CoxPH with Mixture-Based Linkage Error Adjustment
#'
#' @description
#' Fits a Cox proportional hazards regression adjusting for mismatched data using
#' a mixture modeling framework in the secondary analysis setting. The method
#' relies on a two-component mixture model where true matches follow the Cox
#' model and mismatches follow the marginal distribution of the survival outcome.
#' Variance estimates are obtained via Louis' method.
#'
#' @param x A matrix or data.frame of covariates (design matrix).
#' @param y A numeric vector of observed time-to-event outcomes.
#' @param cens A numeric vector indicating censoring status (1 = censored, 0 = event).
#'   Note: This is the reverse of the standard `Surv` object convention where 1 usually
#'   indicates an event.
#' @param z A matrix or data.frame of mismatch covariates (e.g., match scores,
#'   blocking variables). Used to model the probability of a mismatch.
#' @param m.rate An optional numeric value between 0 and 1 specifying the assumed
#'   overall mismatch rate upper bound. If provided, the mismatch indicator model
#'   is constrained such that the average estimated mismatch rate does not exceed
#'   this bound.
#' @param safe.matches A logical vector indicating records known to be correct matches
#'   (TRUE). These records are fixed as matches (probability 1) during estimation.
#'   Defaults to all FALSE.
#' @param control An optional list of control parameters. Parameters can also be
#'   passed directly via `...`.
#'   \itemize{
#'     \item \code{louis.k}: Number of Monte Carlo iterations for variance
#'     estimation (default: 1000).
#'     \item \code{max.iter}: Maximum EM iterations (default: 1000).
#'     \item \code{cmax.iter}: Maximum iterations for the constrained optimization
#'     subroutine (default: 1000).
#'     \item \code{tol}: Convergence tolerance (default: 1e-4).
#'     \item \code{init.beta}: Initial estimates for outcome model coefficients.
#'     \item \code{init.gamma}: Initial estimates for mismatch model coefficients.
#'     \item \code{fy}: Pre-calculated marginal density of the response. If NULL,
#'     estimated non-parametrically.
#'   }
#' @param ... Additional arguments passed to `control`.
#'
#' @return An list of results:
#' \item{coefficients}{Estimated coefficients for the outcome model (beta).}
#' \item{m.coefficients}{Estimated coefficients for the mismatch model (gamma).}
#' \item{var}{Variance-covariance matrix of the estimates.}
#' \item{linear.predictors}{Linear predictors for the outcome model.}
#' \item{means}{Column means of the covariate matrix `x`.}
#' \item{n}{Number of observations.}
#' \item{nevent}{Number of events.}
#' \item{match.prob}{Posterior probabilities that each observation is a correct match.}
#' \item{objective}{Value of the negative log pseudo-likelihood at each iteration.}
#' \item{converged}{Logical indicating if the algorithm converged.}
#' \item{Lambdahat0}{Estimated baseline cumulative hazard.}
#' \item{gLambdahat0}{the baseline cumulative hazard for the marginal density
#' of the response variable (using Nelson-Aalen estimator)}
#'
#' @references
#' Bukke, P., Ben-David, E., Diao, G., Slawski, M., & West, B. T. (2025).
#' Cox Proportional Hazards Regression Using Linked Data: An Approach Based on
#' Mixture Modelling.
#'
#' @importFrom survival coxph.fit coxph.control Surv survfit
#' @importFrom stats glm coef quasibinomial plogis constrOptim vcov
#' @importFrom utils modifyList
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#'
#' @examples
#' library(survival)
#' set.seed(123)
#' n <- 200
#' # Generate covariates
#' x_cov <- seq(-3, 3, length = n)
#' d_cov <- rep(0:1, each = n/2)
#' X <- cbind(d_cov, x_cov, x_cov * d_cov)
#'
#' # True parameters
#' b <- c(-1.5, 1, 0.5)
#' sigma <- 0.25
#' mu <- X %*% b
#' y <- exp(drop(mu)) * rweibull(n, shape = 1/sigma)
#'
#' # Censoring
#' cens <- (y >= 1.5)
#' y[cens] <- 1.5
#'
#' # Generate mismatch errors
#' ps <- rbeta(n, 4.5, 0.5)
#' logit_ps <- log(ps / (1 - ps))
#' mp <- cbind(1, logit_ps)
#' gamma_true <- c(-0.5, 1)
#' m <- 1 - rbinom(n, prob = plogis(mp %*% gamma_true), size = 1)
#' yperm <- y
#' shuffled_ix <- sample(which(m == 1))
#' yperm[shuffled_ix] <- yperm[sample(shuffled_ix)]
#'
#' # Fit model
#' fit <- coxphMixture(x = X, y = yperm, cens = as.numeric(cens),
#'                     z = matrix(logit_ps, ncol = 1),
#'                     control = list(max.iter = 50))
#'
#' print(fit)
#' summary(fit)
#'
coxphMixture <- function(x, y, cens,
                         z, m.rate = NULL, safe.matches = NULL,
                         control = list(), ...) {

  # Handle call and arguments
  cl <- match.call()

  # Input validation
  x <- as.matrix(x)
  z <- as.matrix(z)
  n <- nrow(x)

  # Merge ... into control
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    control[names(extra_args)] <- extra_args
  }

  if (!is.null(m.rate)) {
    logitbound <- -log((1 - m.rate) / m.rate)
  } else {
    logitbound <- NULL
  }

  if (is.null(safe.matches)) safe.matches <- rep(FALSE, n)

  default_control <- list(init.beta = NULL, init.gamma = NULL, fy = NULL,
                          max.iter = 1000, tol = 1E-4, cmax.iter = 1000,
                          louis.k = 1000)

  control <- modifyList(default_control, control)

  # Estimate Marginal Density (if not provided)
  if (is.null(control$fy)) {
    event_status <- 1 - cens
    surv_marginal <- survfit(Surv(y, event_status) ~ 1)
    m_times <- surv_marginal$time
    m_cumhaz <- surv_marginal$cumhaz

    m_diff_times <- diff(m_times)
    m_diff_haz <- diff(m_cumhaz)
    m_diff_times[m_diff_times == 0] <- 1e-10

    D <- c(m_cumhaz[1], m_diff_haz / m_diff_times)
    idx_y <- findInterval(y, m_times)
    idx_y[idx_y == 0] <- 1
    g_lambdahat_0 <- D[idx_y]
    g_Lambdahat_0 <- m_cumhaz[idx_y]

    # fy is the density/mass for the likelihood term
    fy <- g_lambdahat_0^event_status * exp(-g_Lambdahat_0)
  } else {
    fy <- control$fy
    g_Lambdahat_0 <- numeric(n)
  }

  # Initial Estimates
  if (is.null(control$init.beta)) {
    # Naive fit
    init_fit <- coxph.fit(x, Surv(y, 1 - cens), strata = NULL, offset = NULL,
                          init = NULL, control = coxph.control(),
                          weights = NULL, method = "breslow", rownames = NULL)
    beta_cur <- init_fit$coefficients
  } else {
    beta_cur <- control$init.beta
  }

  if (is.null(control$init.gamma)) {
    if (!is.null(logitbound)) {
      if (ncol(z) == 1) {
        gamma_cur <- rep(-logitbound, ncol(z))
      } else {
        # Heuristic: set intercept (if any) or first coef to meet bound
        gamma_cur <- c(max(-logitbound, 0), rep(0, ncol(z) - 1))
      }
    } else {
      gamma_cur <- rep(0, ncol(z))
    }
  } else {
   gamma_cur <- -1 * control$init.gamma
  }

  # EM Algorithm
  eta <- as.vector(x %*% beta_cur)

  # Initial Baseline Hazard
  base_est <- calc_breslow(y, 1 - cens, rep(1, n), eta)
  idx_y <- findInterval(y, base_est$times)
  idx_y[idx_y == 0] <- 1
  lambdahat_0 <- base_est$dens[idx_y]
  Lambdahat_0 <- base_est$cumhaz[idx_y]

  iter <- 1
  p_cur <- numeric(n)
  objs <- numeric(control$max.iter)

  # Link function for match prob: h(eta) = P(match|z)
  # P(m=0|z) = h(z).
  h_gamma <- function(eta_g) {
    pi <- plogis(eta_g)
    list(fun = pi, dfun = pi * (1 - pi))
  }

  while (iter <= control$max.iter) {

    # E-Step
    eta_gamma <- as.vector(z %*% gamma_cur)
    hs <- h_gamma(eta_gamma)$fun
    hs[safe.matches] <- 1

    eta_beta <- as.vector(x %*% beta_cur)
    risk <- exp(eta_beta)

    # Likelihood component for Match (Cox)
    f_cox <- (risk * lambdahat_0)^(1 - cens) * exp(-risk * Lambdahat_0)

    # Posterior Match Probability
    num <- hs[!safe.matches] * f_cox[!safe.matches]
    denom <- num + (1 - hs[!safe.matches]) * fy[!safe.matches]

    p_cur[!safe.matches] <- num / denom
    p_cur[safe.matches] <- 1

    if (anyNA(p_cur)) {
      warning("NA weights in EM, potentially due to numerical instability.")
      p_cur[is.na(p_cur)] <- 0
    }

    p_cur[!safe.matches] <- pmax(pmin(p_cur[!safe.matches], 1 - 1e-6), 1e-6)

    # Compute Objective (Negative Log Pseudo-Likelihood)
    # L = p_match * f_cox + (1-p_match) * fy
    log_lik_terms <- log(hs * f_cox + (1 - hs) * fy)
    log_lik_terms[safe.matches] <- log(f_cox[safe.matches])

    # Avoid log(0)
    log_lik_terms[!is.finite(log_lik_terms)] <- -1e10
    current_obj <- -sum(log_lik_terms)
    objs[iter] <- current_obj

    if (iter > 1 && abs(objs[iter] - objs[iter - 1]) < control$tol) break

    # M-Step

    # Update Gamma (Mismatch Model)
    # We regress the posterior mismatch probability (1 - p_cur) on z
    if (sum(!safe.matches) > 0) {
      mismatch_prob <- 1 - p_cur[!safe.matches]
      z_sub <- z[!safe.matches, , drop = FALSE]

      if (!is.null(logitbound)) {
        # Constrained Optimization: mean(z %*% gamma_mismatch) <= bound
        # Note: gamma_cur tracks Match prob. Gamma_mismatch tracks Mismatch prob.
        # Sign opposites in logit scale.

        if (ncol(z) == 1 && all(z == 1)) {
          # Intercept only case
          # Bound: -gamma_match <= bound => gamma_match >= -bound
          glm_h <- glm(mismatch_prob ~ z_sub - 1, family = quasibinomial)
          gamma_mismatch <- coef(glm_h)
          # Apply constraint
          gamma_mismatch <- min(gamma_mismatch, logitbound)
          gamma_cur <- -gamma_mismatch

        } else {
          # General case
          glm_res <- constrained_logistic_regression(
            z_sub, mismatch_prob, logitbound, control$cmax.iter
          )
          # constrained_logistic_regression returns coefs for P(m=1)
          gamma_cur <- -glm_res$beta
        }
      } else {
        # Unconstrained
        glm_h <- glm(mismatch_prob ~ z_sub - 1, family = quasibinomial)
        # Convert P(m=1) coefs to P(m=0) coefs => negate
        gamma_cur <- -coef(glm_h)
      }
      names(gamma_cur) <- colnames(z)
    }

    # Update Beta (Outcome Model)
    # Weighted Cox Regression
    w_safe <- pmax(p_cur, 1e-6) # Avoid zero weights

    cfit <- coxph.fit(x, Surv(y, 1 - cens), strata = NULL, offset = NULL,
                      init = beta_cur, control = coxph.control(iter.max = 20),
                      weights = w_safe, method = "breslow", rownames = NULL)
    beta_cur <- cfit$coefficients

    # Update Baseline Hazard
    eta_new <- as.vector(x %*% beta_cur)
    base_est <- calc_breslow(y, 1 - cens, w_safe, eta_new)
    idx_y <- findInterval(y, base_est$times)
    idx_y[idx_y == 0] <- 1
    lambdahat_0 <- base_est$dens[idx_y]
    Lambdahat_0 <- base_est$cumhaz[idx_y]

    iter <- iter + 1
  }

  # Variance Estimation (Louis' Method)
  k_samples <- control$louis.k
  pt <- length(beta_cur) + length(gamma_cur)
  sum_hess <- matrix(0, pt, pt)
  sum_grad <- numeric(pt)
  sum_grad_sq <- matrix(0, pt, pt)

  eta_g_final <- as.vector(z %*% gamma_cur)
  prob_match_all <- plogis(eta_g_final)
  w_vec_const <- prob_match_all * (1 - prob_match_all)
  z_weighted_const <- z * sqrt(w_vec_const)
  hess_gamma_const <- crossprod(z_weighted_const)

  prob_mismatch_posterior <- 1 - p_cur
  beta_safe <- beta_cur; beta_safe[is.na(beta_safe)] <- 0

  show_progress <- interactive()
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = k_samples, style = 3)
  }

  for (s in 1:k_samples) {
    # Sample latent mismatch indicators
    m_sim <- rbinom(n, 1, prob_mismatch_posterior)
    idx_match <- which(m_sim == 0) # 0 = match

    grad_beta <- numeric(ncol(x))
    hess_beta <- matrix(0, ncol(x), ncol(x))

    if (length(idx_match) > ncol(x)) {
      x_sub <- x[idx_match, , drop = FALSE]
      y_sub <- y[idx_match]
      cens_sub <- cens[idx_match]

      # Hessian for Beta
      cfit_sim <- coxph.fit(x_sub, Surv(y_sub, 1 - cens_sub),
                            strata = NULL, offset = NULL, init = beta_safe,
                            control = coxph.control(iter.max = 0),
                            weights = NULL, method = "breslow", rownames = NULL)

      if (!is.null(cfit_sim$var) && all(dim(cfit_sim$var) == c(ncol(x), ncol(x))))
        hess_beta <- tryCatch(solve(cfit_sim$var), error = function(e) matrix(0, ncol(x), ncol(x)))

      # Gradient for Beta
      eta_sub <- as.vector(x_sub %*% beta_safe)
      risk_sub <- exp(eta_sub)

      ord <- order(y_sub)
      x_ord <- x_sub[ord, , drop = FALSE]
      events_ord <- (1 - cens_sub)[ord]
      risk_ord <- risk_sub[ord]

      denom_risk <- rev(cumsum(rev(risk_ord)))
      denom_risk[denom_risk == 0] <- 1e-10

      numer_risk <- apply(x_ord * risk_ord, 2, function(col) rev(cumsum(rev(col))))
      if (!is.matrix(numer_risk)) numer_risk <- matrix(numer_risk, ncol = ncol(x))

      E_x <- numer_risk / denom_risk

      is_event <- events_ord == 1
      if (sum(is_event) > 0) {
        sum_E <- colSums(E_x[is_event, , drop = FALSE])
        sum_O <- colSums(x_ord[is_event, , drop = FALSE])
        grad_beta <- sum_O - sum_E # Standard Cox Score: Observed - Expected
      }
    }

    # Gradient for Gamma (Mismatch Model)
    # Gamma parameterizes P(Match). m_sim=0 is Match.
    # Score for logistic: X^T * (Y - p). Here Y is "IsMatch" (1-m_sim).
    resid <- (1 - m_sim) - prob_match_all
    grad_gamma <- crossprod(z, resid)

    grad_total <- c(as.vector(grad_beta), as.vector(grad_gamma))
    hess_total <- matrix(0, pt, pt)
    hess_total[1:ncol(x), 1:ncol(x)] <- hess_beta
    hess_total[(ncol(x) + 1):pt, (ncol(x) + 1):pt] <- hess_gamma_const

    sum_hess <- sum_hess + hess_total
    sum_grad <- sum_grad + grad_total
    sum_grad_sq <- sum_grad_sq + tcrossprod(grad_total)

    if (show_progress) {
     utils::setTxtProgressBar(pb, s)
    }
  }

  if (show_progress) {
    close(pb)
  }

  mean_hess <- sum_hess / k_samples
  mean_grad <- sum_grad / k_samples
  mean_grad_sq <- sum_grad_sq / k_samples

  # Louis Formula: I_obs = E[Hess] - Var[Grad]
  # Var[Grad] = E[Grad^2] - (E[Grad])^2
  cov_grad <- mean_grad_sq - tcrossprod(mean_grad)
  I_obs <- mean_hess - cov_grad

  covhat <- tryCatch({ solve(I_obs) }, error = function(e) {
    warning("Observed Information Matrix is singular. Returning NA covariance.")
    matrix(NA, pt, pt)
  })

  idx_beta <- 1:ncol(x)
  idx_gamma <- (ncol(x) + 1):pt
  if (!anyNA(covhat)) {
   covhat[idx_beta, idx_gamma] <- -1 * covhat[idx_beta, idx_gamma]
   covhat[idx_gamma, idx_beta] <- -1 * covhat[idx_gamma, idx_beta]
  }

  gamma_cur <- as.numeric(-1 * gamma_cur)

  beta_names <- colnames(x); if (is.null(beta_names)) beta_names <- paste0("beta", 1:ncol(x))
  gamma_names <- colnames(z); if (is.null(gamma_names)) gamma_names <- paste0("gamma", 1:ncol(z))

  colnames(covhat) <- rownames(covhat) <- c(beta_names, paste0("m.", gamma_names))
  names(beta_cur) <- beta_names

  out <- list(coefficients = beta_cur,
              m.coefficients = gamma_cur,
              var = covhat,
              linear.predictors = as.vector(x %*% beta_cur),
              means = colMeans(x),
              n = n,
              nevent = sum(1 - cens),
              match.prob = as.numeric(hs),
              objective = objs[1:(iter - 1)],
              converged = iter < control$max.iter,
              Lambdahat0 = Lambdahat_0,
              gLambdahat0 = g_Lambdahat_0)

  # Assign class
  class(out) <- "coxphMixture"

  return(out)
}

#' @keywords internal
#' @export
fitcoxph.adjMixture <- function(x, y, adjustment, control, ...) {

  # -------------------------------------------------------------------------
  # 1. Data Retrieval and Validation
  # -------------------------------------------------------------------------
  full_data <- adjustment$data_ref$data
  if (is.null(full_data)) {
    stop("The 'adjustment' object does not contain linked data. ",
         "Please recreate the object with 'linked.data' provided.", call. = FALSE)
  }

  # -------------------------------------------------------------------------
  # 2. Stage 1: Align Adjustment Data to Outcome Model (X, Y)
  # -------------------------------------------------------------------------
  # plcoxph() has already applied 'subset' and 'na.action' to x and y.
  # We use row names to synchronize the adjustment data.

  subset_names <- rownames(x)

  # Handle edge case: User provided matrix without row names
  if (is.null(subset_names)) {
    if (nrow(x) != nrow(full_data)) {
      stop("Row mismatch: Model matrix 'x' has no row names and its length (", nrow(x),
           ") differs from the adjustment data (", nrow(full_data), "). ",
           "Ensure 'linked.data' matches the data passed to 'plcoxph'.", call. = FALSE)
    }
    # Assumption: 1:1 implicit mapping
    data_subset <- full_data
    # Create implicit index for tracking
    idx_map <- seq_len(nrow(full_data))
  } else {
    # Match by name. Strict matching ensures order preservation.
    idx_map <- match(subset_names, rownames(full_data))

    if (anyNA(idx_map)) {
      stop("Row mismatch: Some observations in the model matrix could not be matched ",
           "to the adjustment data. This usually happens if 'data' in plcoxph() ",
           "is different from the data used to create the adjustment object.", call. = FALSE)
    }
    data_subset <- full_data[idx_map, , drop = FALSE]
  }

  # -------------------------------------------------------------------------
  # 3. Construct Mismatch Covariates (Z)
  # -------------------------------------------------------------------------
  m_formula <- adjustment$m.formula

  # Use 'na.pass' to detect NAs manually in the next step
  Z_frame <- tryCatch({
    stats::model.frame(m_formula, data = data_subset, na.action = stats::na.pass)
  }, error = function(e) {
    stop("Failed to resolve variables in 'm.formula': ", e$message, call. = FALSE)
  })

  Z <- stats::model.matrix(m_formula, Z_frame)

  # -------------------------------------------------------------------------
  # 4. Stage 2: Secondary Intersection (Handle Missingness in Z)
  # -------------------------------------------------------------------------
  # X and Y are already clean (no NAs). But Z might have NAs.
  # If Z has NAs, we must drop those rows from X, Y, and Z to stay aligned.

  keep_idx <- stats::complete.cases(Z)

  if (!all(keep_idx)) {
    n_dropped <- sum(!keep_idx)
    warning(sprintf("Dropped %d observation(s) due to missing values in the
                    mismatch covariates (Z) that were not missing in the
                    outcome variables.", n_dropped), call. = FALSE)

    # Apply the subset to inputs
    x <- x[keep_idx, , drop = FALSE]
    y <- y[keep_idx, , drop = FALSE] # Surv object is a matrix
    Z <- Z[keep_idx, , drop = FALSE]

    # Update row map for safe matches alignment
    if (!is.null(subset_names)) {
      subset_names <- subset_names[keep_idx]
      idx_map <- idx_map[keep_idx]
    } else {
      idx_map <- idx_map[keep_idx]
    }
  }

  # -------------------------------------------------------------------------
  # 5. Process Safe Matches
  # -------------------------------------------------------------------------
  safe_matches_all <- adjustment$safe.matches
  safe_matches_sub <- NULL

  if (!is.null(safe_matches_all)) {
    # Align safe matches vector to the final subset
    safe_matches_sub <- safe_matches_all[idx_map]
  } else {
    safe_matches_sub <- rep(FALSE, nrow(x))
  }

  # -------------------------------------------------------------------------
  # 6. Final Function Input Preparation
  # -------------------------------------------------------------------------

  # Extract Censoring Status (Standard Surv: status 1=event, 0=censored)
  # Expect cens (1=censored, 0=event)
  # Surv object columns are usually "time" and "status"
  if (!inherits(y, "Surv") && !is.matrix(y)) {
    stop("Response y must be a Surv object with time and status.", call. = FALSE)
  }
  if (ncol(y) < 2) {
    stop("Response y must be a Surv object with time and status.", call. = FALSE)
  }
  status_vec <- as.numeric(y[, "status"])
  cens_vec <- 1 - status_vec
  time_vec <- as.numeric(y[, "time"])

  # Function Inputs
  n_obs <- nrow(x)
  m_rate <- adjustment$m.rate

  # -------------------------------------------------------------------------
  # 7. Function Input Validation
  # -------------------------------------------------------------------------

  if (length(time_vec) != n_obs) stop("Length of time vector must match nrow(x)")
  if (length(cens_vec) != n_obs) stop("Length of censoring vector must match nrow(x)")
  if (nrow(Z) != n_obs) stop("nrow(z) must match nrow(x)")

  if (!is.null(m_rate)) {
    if (m_rate <= 0 || m_rate >= 1) stop("m.rate must be strictly between 0 and 1")
  }

  # -------------------------------------------------------------------------
  # 8. Dispatch to Computational Function
  # -------------------------------------------------------------------------
  fit <- coxphMixture(
    x = x,
    y = time_vec,
    cens = cens_vec,
    z = Z,
    m.rate = m_rate,
    safe.matches = safe_matches_sub,
    control = control,
    ...
  )

  # -------------------------------------------------------------------------
  # 9. Post-Processing
  # -------------------------------------------------------------------------

  # Store provenance
  fit$adjustment <- adjustment
  fit$m.formula <- m_formula

  # Restore row names to observation-level outputs for consistency with R ecosystem
  if (!is.null(subset_names)) {
    if (!is.null(fit$match.prob)) names(fit$match.prob) <- subset_names
    if (!is.null(fit$linear.predictors)) names(fit$linear.predictors) <- subset_names
  }

  return(fit)
}
