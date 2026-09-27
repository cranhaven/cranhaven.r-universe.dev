#' Bayesian Two-Component Mixture Survival Regression Model
#'
#' Fits a Bayesian two-component parametric survival regression model
#' using Stan. Each observation is assumed to arise from one of two latent
#' components with component-specific survival regression parameters.
#'
#' The function supports \code{"gamma"} and \code{"weibull"} component
#' distributions, with both components sharing the same family. Right-censored
#' survival outcomes are supported.
#'
#' Posterior draws are returned for the component-specific regression
#' parameters and mixing weight. To improve interpretability of posterior
#' summaries, the function applies a post-processing step that aligns
#' component labels across posterior draws.
#'
#' @param X A numeric design matrix (\eqn{N \times K}), typically created by
#'   \code{model.matrix()}. Each row corresponds to one observation and each
#'   column to one covariate in the survival model. Missing values are not allowed.
#'
#' @param y A survival response. This can be either a two-column numeric matrix
#'   with columns \code{time} and \code{event}, where \code{event = 1} indicates
#'   an observed event and \code{event = 0} indicates right censoring, or a list
#'   with elements \code{time} and \code{event}. Missing values are not allowed.
#'
#' @param dist Character string specifying the parametric survival distribution
#'   used for both mixture components. Supported values are \code{"gamma"} and
#'   \code{"weibull"}.
#'
#' @param priors A named \code{list} of prior specifications, or \code{NULL}.
#'   Since the Stan models are pre-compiled, prior specifications are converted
#'   into the corresponding numeric hyperparameters and passed to the model as
#'   data. Any missing entries are automatically filled in using symmetric
#'   default values via
#'   \code{fill_defaults(priors, p_family = dist, model_type = "survival")}.
#'
#'   Intercept and slope priors are decoupled. Use \code{intercept1}/\code{intercept2}
#'   for the intercept of each component and \code{beta1}/\code{beta2} for the slope
#'   coefficients. By default, intercept priors are \code{normal(0, 10)} (weakly
#'   informative), while slope priors follow the distribution-specific defaults.
#'
#' @param control A named \code{list} of control parameters for posterior
#'   sampling. Defaults are:
#'   \itemize{
#'     \item \code{iterations} (default \code{1e4}): total number of iterations per chain;
#'     \item \code{burnin.iterations} (default \code{1e3}): number of warm-up iterations;
#'     \item \code{seed} (default: a random integer): random seed for reproducibility;
#'     \item \code{cores} (default \code{getOption("mc.cores", 1L)}): number of CPU cores used.
#'   }
#'   Values supplied through \code{...} override the corresponding entries in
#'   \code{control}.
#'
#' @param ... Optional overrides for elements of \code{control}, such as
#'   \code{iterations = 4000}, \code{burnin.iterations = 1000},
#'   \code{seed = 123}, or \code{cores = 2}.
#'
#' @return An object of class \code{"survMixBayes"} containing (at least):
#' \describe{
#'   \item{\code{m_samples}}{Posterior draws of aligned latent component
#'   labels (matrix of size draws × N), where component 1 corresponds to the
#'   correct-match component and component 2 to the incorrect-match component.}
#'
#'   \item{\code{estimates$coefficients}}{Posterior draws of regression
#'   coefficients for the correct-match component (component 1; draws × K).}
#'
#'   \item{\code{estimates$m.coefficients}}{Posterior draws of regression
#'   coefficients for the incorrect-match component (component 2; draws × K).}
#'
#'   \item{\code{estimates$theta}}{Posterior draws of the mixing weight for the
#'   correct-match component (component 1; vector of length draws).}
#'
#'   \item{\code{estimates$shape}}{Posterior draws of the shape parameter for
#'   the correct-match component (component 1; family-specific).}
#'
#'   \item{\code{estimates$m.shape}}{Posterior draws of the shape parameter for
#'   the incorrect-match component (component 2; family-specific).}
#'
#'   \item{\code{estimates$scale}}{Posterior draws of the scale parameter for
#'   the correct-match component (component 1; Weibull only).}
#'
#'   \item{\code{estimates$m.scale}}{Posterior draws of the scale parameter for
#'   the incorrect-match component (component 2; Weibull only).}
#'
#'   \item{\code{family}}{The survival distribution used in the model.}
#'
#'   \item{\code{call}}{The matched function call.}
#' }
#'
#' @section Label switching:
#'
#' Mixture models are invariant to permutations of component labels,
#' which can lead to label switching in MCMC output. To ensure
#' interpretable posterior summaries, this function applies a
#' post-processing step that aligns component labels across
#' posterior draws.
#'
#' First, an optional global swap of labels (1 and 2) is performed
#' if component 2 is more frequent overall. Then, labels are
#' aligned across draws using the \code{ECR-ITERATIVE-1}
#' relabeling algorithm.
#'
#' @references
#' Gutman, R., Sammartino, C., Green, T., & Montague, B. (2016).
#' Error adjustments for file linking methods using encrypted unique
#' client identifier (eUCI) with application to recently released prisoners
#' who are HIV+. \emph{Statistics in Medicine}, 35(1), 115--129.
#' \doi{10.1002/sim.6586}
#'
#' Stephens, M. (2000).
#' Dealing with label switching in mixture models.
#' \emph{Journal of the Royal Statistical Society: Series B
#' (Statistical Methodology)}, 62(4), 795--809.
#' \doi{10.1111/1467-9868.00265}
#'
#' Papastamoulis, P. (2016).
#' \emph{label.switching}: An R package for dealing with the label switching
#' problem in MCMC outputs.
#' \emph{Journal of Statistical Software}, 69(1), 1--24.
#' \doi{10.18637/jss.v069.c01}
#'
#' @examples
#' \donttest{
#' # Example: Bayesian mixture survival model fit to linked survival data
#' # with induced linkage mismatch errors
#'
#' # 1. Simulate a linked survival dataset
#' set.seed(301)
#' n <- 150
#' X <- matrix(rnorm(n * 2), ncol = 2)
#' colnames(X) <- c("x1", "x2")
#'
#' # Generate survival times from a Weibull AFT model
#' true_time <- rweibull(
#'   n,
#'   shape = 1.5,
#'   scale = exp(0.5 * X[, 1] - 0.5 * X[, 2])
#' )
#'
#' # Apply right-censoring
#' cens_time <- rexp(n, rate = 0.1)
#' event <- as.integer(true_time <= cens_time)
#' obs_time <- pmin(true_time, cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' event[mismatch_idx] <- event[shuffled]
#'
#' y <- cbind(time = obs_time, event = event)
#'
#' # 2. Fit the Bayesian two-component mixture survival model
#' # Note: Iterations are set artificially low for run time
#' fit <- survregMixBayes(
#'   X = X,
#'   y = y,
#'   dist = "weibull",
#'   control = list(iterations = 200, burnin.iterations = 100, seed = 123)
#' )
#'
#' # 3. Inspect posterior summaries
#' # (Label switching is handled automatically)
#' cat("Component 1 (Correct Links):\n")
#' print(colMeans(fit$estimates$coefficients))
#'
#' cat("Component 2 (Incorrect Links):\n")
#' print(colMeans(fit$estimates$m.coefficients))
#'
#' cat("Estimated probability of correct linkage:\n")
#' print(mean(fit$estimates$theta))
#' }
#'
#' @export
#' @importFrom rstan sampling
#' @import label.switching
survregMixBayes <- function(X, y, dist = "weibull", priors = NULL,
                            control = list(iterations = 1e4,
                                           burnin.iterations = 1e3,
                                           seed = sample.int(.Machine$integer.max, 1),
                                           cores = getOption("mc.cores", 1L)),
                            ...) {

 dcontrols <- list(...)
 iterations <- if ("iterations" %in% names(dcontrols)) {
  dcontrols$iterations
 } else if ("iterations" %in% names(control)) {
  control$iterations
 } else {
  1e4
 }

 burnin.iterations <- if ("burnin.iterations" %in% names(dcontrols)) {
  dcontrols$burnin.iterations
 } else if ("burnin.iterations" %in% names(control)) {
  control$burnin.iterations
 } else {
  1e3
 }

 seed <- if ("seed" %in% names(dcontrols)) {
  dcontrols$seed
 } else if ("seed" %in% names(control)) {
  control$seed
 } else {
  sample.int(.Machine$integer.max, 1)
 }

 cores <- if ("cores" %in% names(dcontrols)) {
  dcontrols$cores
 } else if ("cores" %in% names(control)) {
  control$cores
 } else {
  getOption("mc.cores", 1L)
 }

 dist <- .validate_survreg_dist(dist)
 if (!(dist %in% c("gamma", "weibull"))) {
  stop("Error: `dist` must be 'gamma' or 'weibull'.", call. = FALSE)
 }

 # Minimal input checks (formula/data checks handled upstream)
 if (!is.matrix(X) || !is.numeric(X)) stop("`X` must be a numeric matrix.", call. = FALSE)
 if (anyNA(X)) stop("NA values found in X.", call. = FALSE)

 yn <- .normalize_surv_y(y)
 time <- yn$time
 event <- yn$event
 if (length(time) != nrow(X)) stop("`y` must have length nrow(X).", call. = FALSE)
 if (anyNA(time) || anyNA(event)) stop("NA values found in y.", call. = FALSE)

 # Pre-compiled Stan Pipeline
 # Parse prior strings into a named list of numeric values using our helper
 prior_data <- prepare_stan_priors(priors, dist, model_type = "survival")

 # Combine core data with parsed prior data
 stan_data <- c(
  list(N = nrow(X), K = ncol(X), X = X, time = as.vector(time), event = as.integer(event)),
  prior_data
 )

 # Identify pre-compiled model from rstantools' internal stanmodels object
 model_name <- paste0("survMixBayes_", dist)

 # Sample instantly from the pre-compiled C++ DLL
 fit <- rstan::sampling(
  stanmodels[[model_name]],
  data   = stan_data,
  iter   = iterations,
  warmup = burnin.iterations,
  chains = 1,
  seed   = seed,
  cores  = cores
 )

 posterior <- rstan::extract(fit)
 z_samples <- posterior$z

 ##### Label switching adjustment #####

 # --- 0) Optional global pre-alignment by majority label -----------------------
 count_label2 <- sum(z_samples == 2L, na.rm = TRUE)
 total_labels <- length(z_samples)  # S * N
 if (is.finite(count_label2) && count_label2 > total_labels / 2) {
  message("Global label swap performed: label 2 dominates label 1.")

  # Flip z: 1 -> 2, 2 -> 1
  z_samples <- structure(3L - z_samples, dim = dim(z_samples))

  # Swap component-specific parameters inside `posterior` if they exist
  swap_if_present <- function(lst, a, b) {
   if (all(c(a, b) %in% names(lst))) {
    tmp <- lst[[a]]
    lst[[a]] <- lst[[b]]
    lst[[b]] <- tmp
   }
   lst
  }
  posterior <- swap_if_present(posterior, "beta1", "beta2")
  posterior <- swap_if_present(posterior, "phi1",  "phi2")     # gamma
  posterior <- swap_if_present(posterior, "shape1","shape2")   # weibull
  posterior <- swap_if_present(posterior, "scale1","scale2")   # weibull

  if ("theta" %in% names(posterior)) posterior$theta <- 1 - posterior$theta
 }

 # --- 1) Iterative ECR alignment of labels across MCMC draws -------------------
 ls_out <- label.switching::label.switching(
  method = "ECR-ITERATIVE-1",
  z      = z_samples,
  K      = 2
 )
 perm <- ls_out$permutations[["ECR-ITERATIVE-1"]]

 # Map z by the inverse permutation for each draw
 map_z <- function(z, perm) {
  if (!is.matrix(z)) stop("`z` must be an S x N matrix.", call. = FALSE)
  S <- nrow(z); K <- ncol(perm)
  if (K != 2L || nrow(perm) != S) stop("`perm` must be an S x 2 matrix of permutations.", call. = FALSE)
  for (i in seq_len(S)) {
   inv <- integer(K)
   inv[perm[i, ]] <- seq_len(K)
   z[i, ] <- inv[z[i, ]]
  }
  z
 }
 z_samples <- map_z(z_samples, perm)

 # Helper to permute paired component-specific parameters
 perm_pair <- function(a1, a2, perm) {
  if (!is.numeric(a1) || !is.numeric(a2)) stop("Inputs must be numeric.", call. = FALSE)

  d1 <- dim(a1); d2 <- dim(a2)

  # Convert a1/a2 into S x p matrices (p = product of remaining dims)
  if (is.null(d1)) {
   S <- length(a1); p <- 1L
   A1 <- matrix(a1, ncol = 1L)
  } else {
   S <- d1[1]
   p <- as.integer(length(a1) / S)
   if (!is.finite(S) || S < 1 || !is.finite(p) || p < 1) {
    stop("Invalid draws/shape for component parameter.", call. = FALSE)
   }
   A1 <- matrix(a1, nrow = S)
  }

  if (is.null(d2)) {
   if (length(a2) != S) stop("Shapes differ between component parameters.", call. = FALSE)
   A2 <- matrix(a2, ncol = 1L)
  } else {
   if (d2[1] != S) stop("Shapes differ between component parameters.", call. = FALSE)
   A2 <- matrix(a2, nrow = S)
  }

  if (!identical(dim(A1), dim(A2))) stop("Shapes differ between component parameters.", call. = FALSE)
  if (!is.matrix(perm) || nrow(perm) != S || ncol(perm) != 2L) {
   stop("`perm` must be an S x 2 matrix (one permutation per draw).", call. = FALSE)
  }

  arr <- array(NA_real_, dim = c(S, 2L, ncol(A1)))
  arr[, 1L, ] <- A1
  arr[, 2L, ] <- A2
  arrp <- label.switching::permute.mcmc(arr, permutations = perm)[[1]]

  out1 <- arrp[, 1L, , drop = TRUE]
  out2 <- arrp[, 2L, , drop = TRUE]
  if (ncol(A1) == 1L) {
   out1 <- as.numeric(out1); out2 <- as.numeric(out2)
  } else {
   out1 <- matrix(out1, nrow = S); out2 <- matrix(out2, nrow = S)
  }
  list(`1` = out1, `2` = out2)
 }

 if (is.null(posterior$z) || is.null(posterior$beta1) || is.null(posterior$beta2) || is.null(posterior$theta)) {
  stop("Missing expected parameters in posterior", call. = FALSE)
 }

 beta1.p <- posterior$beta1
 beta2.p <- posterior$beta2
 tmp <- perm_pair(beta1.p, beta2.p, perm)
 beta1.p <- tmp[[1]]
 beta2.p <- tmp[[2]]

 # theta: if draw perm swaps, theta -> 1-theta
 theta.p <- posterior$theta
 if (!is.numeric(theta.p)) stop("Expected theta draws.", call. = FALSE)
 if (length(theta.p) != nrow(z_samples)) stop("Theta draws length mismatch.", call. = FALSE)
 swap_draw <- perm[,1L] == 2L
 theta.p[swap_draw] <- 1 - theta.p[swap_draw]

 est <- list(
  coefficients   = beta1.p,
  m.coefficients = beta2.p,
  theta          = theta.p
 )

 if (dist == "gamma") {
  tmp <- perm_pair(posterior$phi1, posterior$phi2, perm)
  est$shape   <- tmp[[1]]
  est$m.shape <- tmp[[2]]
 }

 if (dist == "weibull") {
  tmp <- perm_pair(posterior$shape1, posterior$shape2, perm)
  est$shape   <- tmp[[1]]
  est$m.shape <- tmp[[2]]
  tmp <- perm_pair(posterior$scale1, posterior$scale2, perm)
  est$scale   <- tmp[[1]]
  est$m.scale <- tmp[[2]]
 }

 # Set coefficient names from X if available
 cn <- colnames(X)
 if (!is.null(cn) && is.matrix(beta1.p) && ncol(beta1.p) == length(cn)) {
  colnames(beta1.p) <- cn
  colnames(beta2.p) <- cn
 }

 out <- list(
  m_samples = z_samples,
  estimates = est,
  family = dist,
  dist = dist,
  call = match.call()
 )

 class(out) <- "survMixBayes"
 out
}

# S3 dispatch method for the postlink internal generic fitsurvreg()
#' @keywords internal
#' @export
fitsurvreg.adjMixBayes <- function(x, y, dist, adjustment, control, priors = NULL, ...) {

 full_data <- adjustment$data_ref$data
 if (is.null(full_data)) {
  stop("The 'adjustment' object does not contain linked data. ",
       "Please recreate the object with 'linked.data' provided.", call. = FALSE)
 }

 subset_names <- rownames(x)

 if (is.null(subset_names)) {
  if (nrow(x) != nrow(full_data)) {
   stop("Row mismatch: Model matrix 'x' has no row names and its length (", nrow(x),
        ") differs from the adjustment data (", nrow(full_data), "). ",
        "Ensure 'linked.data' matches the data passed to the upstream wrapper.", call. = FALSE)
  }
 } else {
  idx_map <- match(subset_names, rownames(full_data))
  if (anyNA(idx_map)) {
   stop("Row mismatch: Some observations in the model matrix could not be matched ",
        "to the adjustment data. This usually happens if the upstream 'data' ",
        "differs from the data used to create the adjustment object.", call. = FALSE)
  }
 }

 if (anyNA(x) || anyNA(y)) {
  stop("NA values found in x or y. Upstream wrapper should remove missingness.", call. = FALSE)
 }

 # Extract priors using hierarchy (function arg > dots > control > adjustment)
 dots <- list(...)
 final_priors <- priors

 if (is.null(final_priors) && "priors" %in% names(dots)) {
  final_priors <- dots$priors
  dots$priors <- NULL
 }
 if (is.null(final_priors) && !is.null(control) && is.list(control) && "priors" %in% names(control)) {
  final_priors <- control$priors
 }
 if (is.null(final_priors)) {
  final_priors <- adjustment$priors
 }

 fit <- do.call(
  survregMixBayes,
  c(
   list(X = x, y = y, dist = dist, priors = final_priors, control = control),
   dots
  )
 )

 fit$adjustment <- adjustment
 fit$call <- match.call()
 if (!is.null(subset_names)) fit$obs_names <- subset_names

 fit
}
