#' Contingency Table Analysis with Mixture-Based Adjustment
#'
#' @description
#' Estimates the cell probabilities of a two-way contingency table in the presence
#' of linkage errors. The function implements the methodology described in
#' Slawski et al. (2025), modeling the observed table as a mixture of correctly
#' matched records (following a saturated model) and mismatched records
#' (assumed to follow an independence model).
#'
#' @details
#' In the absence of linkage errors, the standard estimator for cell probabilities
#' is the matrix of observed relative frequencies. When linkage errors are present
#' (specifically mismatches), this estimator is biased.
#'
#' This function corrects for this bias using an Expectation-Maximization (EM)
#' algorithm. The observed data is modeled as a mixture:
#' \deqn{P_{obs} = (1 - \alpha) P_{sat} + \alpha P_{ind}}
#' where:
#' \itemize{
#'   \item \eqn{\alpha} (\code{m.rate}) is the mismatch rate (fixed/known).
#'   \item \eqn{P_{sat}} is the distribution of correct matches (saturated model).
#'   \item \eqn{P_{ind}} is the distribution of mismatches (independence model).
#' }
#'
#' The algorithm iteratively updates the posterior probability of a record being
#' a correct match (E-step) and the estimates for the saturated and independence
#' distributions (M-step).
#'
#' @param tab A numeric matrix or table of counts representing the observed
#'   two-way contingency table.
#' @param m.rate A numeric value between 0 and 1 indicating the assumed rate
#'   of mismatched records in the data.
#' @param control A list of control parameters. See \code{...} for details.
#' @param ... Additional control arguments. If not provided in \code{control},
#'   these will be used. Supported arguments:
#'   \itemize{
#'     \item \code{max.iter}: Integer. Maximum number of EM iterations (default: 1000).
#'     \item \code{tol}: Numeric. Convergence tolerance for the negative log-likelihood (default: 1e-6).
#'   }
#'
#' @return A list of results:
#' \itemize{
#'   \item \code{phat}: Matrix of estimated cell probabilities for the correctly matched population (the target parameter).
#'   \item \code{phat0}: Matrix of estimated cell probabilities for the mismatched population (independence model).
#'   \item \code{var}: Estimated variance-covariance matrix of the estimators in \code{phat}.
#'   \item \code{ftable}: The estimated contingency table of counts for the correctly matched population (adjusted for bias).
#'   \item \code{objective}: The final value of the negative log-likelihood.
#'   \item \code{converged}: Logical indicating if the algorithm converged within \code{max.iter}.
#' }
#'
#' @references
#' Slawski, M., West, B. T., Bukke, P., Wang, Z., Diao, G., & Ben-David, E. (2025).
#' A general framework for regression with mismatched data based on mixture modelling.
#' \emph{Journal of the Royal Statistical Society Series A}.
#'
#' @examples
#' # Generate Synthetic Data
#' set.seed(1234)
#' K <- 3; L <- 4
#' n <- 1000
#' # Define true probabilities for a KxL table
#' cellprobs <- c(0.18, 0.05, 0.03, 0.04, 0.02, 0.14,
#'                0.02, 0.02, 0.10, 0.21, 0.15, 0.04)
#' matrix_probs <- matrix(cellprobs, nrow = K, ncol = L)
#'
#' # Generate multinomial counts
#' dat <- stats::rmultinom(n = n, size = 1, prob = cellprobs)
#' obs_idx <- apply(dat, 2, function(x) which(x == 1))
#' X <- ceiling(obs_idx / L) # Row indices
#' Y <- (obs_idx %% L); Y[Y == 0] <- L # Col indices
#'
#' # Introduce Linkage Error (Mismatches)
#' alpha <- 0.20 # 20% mismatch rate
#' n_mismatch <- round(n * alpha)
#' Y_perm <- Y
#' # Shuffle the first n_mismatch Y values to break dependence
#' Y_perm[1:n_mismatch] <- sample(Y[1:n_mismatch])
#'
#' # Create Observed Table (with error)
#' tab_obs <- table(X, Y_perm)
#'
#' # Apply Adjustment Method
#' fit <- ctableMixture(tab = tab_obs, m.rate = alpha)
#'
#' # Inspect Results
#' print(fit$converged)
#' # Compare estimated Correct Counts vs True Counts (approx)
#' print(round(fit$ftable))
#' print(round(table(X, Y))) # True table without errors
#'
#' @importFrom utils modifyList
#' @export
ctableMixture <- function(tab, m.rate, control = list(), ...) {

  # Argument Parsing & Validation
  if (!(is.matrix(tab) || is.table(tab)) || !is.numeric(tab)) {
   stop("Input 'tab' must be a numeric matrix or table.")
  }
  if (any(tab < 0, na.rm = TRUE)) {
   stop("Input 'tab' must contain non-negative counts.")
  }
  if (!is.numeric(m.rate) || length(m.rate) != 1L) {
   stop("'m.rate' must be a single numeric value.")
  }
  if (m.rate < 0 || m.rate >= 1) {
   stop("'m.rate' must be strictly between 0 and 1.")
  }

  # Default control parameters
  defaults <- list(max.iter = 1000, tol = 1e-6)

  # Capture ellipsis arguments
  dots <- list(...)

  # Merge defaults with explicit 'control' list, then override with any '...' arguments
  control <- modifyList(defaults, control)
  control <- modifyList(control, dots)

  # Dimensions and Constants
  k_rows  <- nrow(tab)
  l_cols  <- ncol(tab)
  n_total <- sum(tab)
  n_cells <- k_rows * l_cols
  alpha   <- m.rate

  # Initialization

  # Initialize p_sat (Target) as the observed proportions
  # In the presence of mismatches, this is a biased starting point, but sufficient for EM.
  p_sat <- tab / n_total

  # Initialize p_ind (Independence component) based on marginals of the start point
  row_marg <- rowSums(p_sat)
  col_marg <- colSums(p_sat)
  p_ind <- outer(row_marg, col_marg)

  # Negative Log-Likelihood Function
  calc_nloglik <- function(p_s, p_i) {
    mixture_prob <- p_s * (1 - alpha) + p_i * alpha
    # Add epsilon to prevent log(0)
    -sum(tab * log(mixture_prob + .Machine$double.eps))
  }

  iter <- 1
  converged <- FALSE
  objs <- numeric(control$max.iter)
  objs[1] <- calc_nloglik(p_sat, p_ind)

  # EM Algorithm

  while (iter < control$max.iter && !converged) {

    # E-Step
    # Calculate posterior probability that an observation is a correct match
    # w_ij = P(Correct | Data_ij) = [(1-alpha)*P_sat] / P_mixture
    numer <- p_sat * (1 - alpha)
    denom <- numer + (p_ind * alpha)
    weights <- numer / (denom + .Machine$double.eps)

    # M-Step

    # Update P_sat (Saturated Model for Correct Matches)
    # The MLE is the relative frequency of the data weighted by w_ij
    counts_sat_weighted <- tab * weights
    sum_sat <- sum(counts_sat_weighted)
    if (sum_sat > 0) {
      p_sat <- counts_sat_weighted / sum_sat
    }

    # Update P_ind (Independence Model for Mismatches)
    # The MLE for independence is the outer product of marginals of data weighted by (1 - w_ij)
    counts_ind_weighted <- tab * (1 - weights)
    sum_ind <- sum(counts_ind_weighted)

    if (sum_ind > 1e-12) {
      r_marg_ind <- rowSums(counts_ind_weighted) / sum_ind
      c_marg_ind <- colSums(counts_ind_weighted) / sum_ind
      p_ind <- outer(r_marg_ind, c_marg_ind)
    }
    # Note: If sum_ind is effectively 0, p_ind remains unchanged

    # Convergence Check
    iter <- iter + 1
    objs[iter] <- calc_nloglik(p_sat, p_ind)

    if (abs(objs[iter] - objs[iter - 1]) < control$tol) {
      converged <- TRUE
    }
  }

  # Variance Estimation
  # Calculating the Hessian of the observed log-likelihood
  # Using vectorization for efficiency

  counts_vec <- c(t(tab))  # Flatten by row
  p_sat_vec  <- c(t(p_sat))
  p_ind_vec  <- c(t(p_ind))

  # The denominator of the score function squared
  denom_sq <- ((1 - alpha) * p_sat_vec + alpha * p_ind_vec)^2

  # Approximation of the diagonal of the Fisher Information Matrix (Hessian component)
  # Derived from the second derivative of the mixture log-likelihood
  hess0 <- counts_vec * (1 - alpha)^2 / (denom_sq + .Machine$double.eps)

  # Construct full Hessian matrix approximating the observed information
  ones <- rep(1, n_cells)
  term_outer <- outer(ones / n_cells, hess0) + outer(hess0, ones / n_cells)

  # H = diag(hess0) - term_outer + mean(hess0)
  hess <- -term_outer + mean(hess0) / n_cells
  diag(hess) <- diag(hess) + hess0

  # Invert Hessian using SVD
  # SVD is preferred here because the Hessian for multinomial probabilities
  # is often singular or near-singular due to the sum-to-one constraint.
  svd_hess <- svd(hess)

  # We use rank = n_cells - 1 because of the linear constraint (sum(p) = 1)
  rank_idx <- seq_len(max(1, n_cells - 1))

  # Compute Pseudo-Inverse: V * D^-1 * U^T
  inv_d <- 1 / sqrt(svd_hess$d[rank_idx])
  # Compute scaled U: U * D^-0.5
  root_inv <- scale(svd_hess$u[, rank_idx, drop = FALSE],
                    center = FALSE, scale = 1/inv_d)
  vcov_phat <- tcrossprod(root_inv)

  # Formatting Results

  # Handle Dimension Names
  dn <- dimnames(tab)
  if (is.null(dn)) {
    dn <- list(Row = paste0("R", 1:k_rows), Col = paste0("C", 1:l_cols))
  }
  dimnames(p_sat) <- dn
  dimnames(p_ind) <- dn

  # Format Variance Matrix Labels
  grid_names <- expand.grid(dn[[2]], dn[[1]]) # Note: expand.grid order
  # Match the vectorization order (by row) used in Hessian calc
  vcov_labels <- paste0("(", grid_names$Var2, ", ", grid_names$Var1, ")")
  rownames(vcov_phat) <- colnames(vcov_phat) <- vcov_labels

  # Create adjusted count table (Effective counts for correct matches)
  res_ftable <- as.table(p_sat * n_total)
  dimnames(res_ftable) <- dn

  # Construct Result Object
  res <- list(
    phat      = p_sat,
    phat0     = p_ind,
    var       = vcov_phat,
    ftable    = res_ftable,
    objective = objs[iter],
    converged = converged
  )

  class(res) <- "ctableMixture"

  return(res)
}

#' @keywords internal
#' @export
fitctable.adjMixture <- function(ftable, adjustment, control = list(), ...) {

  # -------------------------------------------------------------------------
  # 1. Input Validation
  # -------------------------------------------------------------------------

  # Validate the Contingency Table (ftable)
  # Ensure it is a matrix-like object capable of being treated as counts
  counts_mat <- tryCatch(as.matrix(ftable), error = function(e) NULL)

  if (is.null(counts_mat) || !is.numeric(counts_mat)) {
    stop("The generated table is not a valid numeric matrix.", call. = FALSE)
  }

  if (any(counts_mat < 0, na.rm = TRUE)) {
    stop("The contingency table contains negative counts.", call. = FALSE)
  }

  if (any(is.na(counts_mat))) {
    warning("The contingency table contains NA values. These will be treated as zeros.", call. = FALSE)
    counts_mat[is.na(counts_mat)] <- 0
  }

  # -------------------------------------------------------------------------
  # 2. Extract and Validate Adjustment Parameters
  # -------------------------------------------------------------------------

  # Extract Mismatch Rate
  m_rate <- adjustment$m.rate

  # Engine Requirement Check:
  # The ctableMixture engine  requires a fixed, scalar, global m.rate.
  # It does not support estimating m.rate from data, nor covariate-dependent rates.

  if (is.null(m_rate)) {
    stop("The 'adjMixture' object has a NULL 'm.rate'. \n",
         "  The contingency table method requires a fixed, known mismatch rate.\n",
         "  Please specify 'm.rate' in the adjMixture() constructor.", call. = FALSE)
  }

  if (!is.numeric(m_rate) || length(m_rate) != 1L || m_rate < 0 || m_rate >= 1) {
    stop("'m.rate' must be a single numeric value strictly between 0 and 1.", call. = FALSE)
  }

  # Warn about ignored parameters
  # If the user specified a complex formula or safe matches, we must inform them
  # that this specific method ignores them.

  has_formula <- !is.null(adjustment$m.formula) &&
    length(all.vars(adjustment$m.formula)) > 0

  has_safe_matches <- !is.null(adjustment$safe.matches) &&
    any(adjustment$safe.matches)

  if (has_formula || has_safe_matches) {
    warning("Covariates (m.formula) and safe matches (safe.matches) in the adjustment object ",
            "are ignored by plctable().\n",
            "  Reason: The contingency table correction method assumes a global, ",
            "homogeneous mismatch rate across all cells.", call. = FALSE)
  }

  # -------------------------------------------------------------------------
  # 3. Dispatch
  # -------------------------------------------------------------------------

  # Invoke the internal function
  fit <- ctableMixture(
    tab = counts_mat,
    m.rate = m_rate,
    control = control,
    ...
  )

  # -------------------------------------------------------------------------
  # 4. Post-Processing
  # -------------------------------------------------------------------------

  fit$adjustment <- adjustment

  return(fit)
}
