#' Bayesian Two-Component Mixture Generalized Linear Model
#'
#' Fits a Bayesian two-component mixture generalized linear model (GLM)
#' using Stan. Each observation is assumed to arise from one of two latent
#' components with component-specific regression coefficients.
#'
#' The function supports Gaussian, Poisson, Binomial, and Gamma outcome
#' families and returns posterior samples of the component-specific
#' regression parameters and mixture weight.
#'
#' @param X A numeric design matrix (N x K), typically from \code{model.matrix()}
#'   upstream. Missing values are not allowed.
#' @param y A response vector of length N. For \code{"gaussian"} and \code{"gamma"},
#'   \code{y} should be numeric; for \code{"poisson"} and \code{"binomial"}, \code{y}
#'   should be integer-valued (or coercible without loss).
#' @param family One of \code{"gaussian"}, \code{"poisson"}, \code{"binomial"}, or \code{"gamma"}.
#'   Controls the component-specific likelihood.
#' @param priors A named \code{list} (or \code{NULL}) of prior specifications.
#'   Because the Stan models are pre-compiled, these strings are parsed into numeric
#'   hyperparameters and passed to the model's data block. Any missing entries are
#'   automatically filled with symmetric defaults via
#'   \code{fill_defaults(priors, p_family = family, model_type = "glm")}.
#'
#'   Intercept and slope priors are decoupled. Use \code{intercept1}/\code{intercept2}
#'   for the intercept of each component and \code{beta1}/\code{beta2} for the slope
#'   coefficients. By default, intercept priors are \code{normal(0, 10)} (weakly
#'   informative), while slope priors follow the family-specific defaults.
#' @param control A named \code{list} of MCMC tuning parameters.
#'   Supported elements include:
#'   \itemize{
#'     \item \code{iterations}: total number of MCMC iterations per chain
#'       (default \code{1e4});
#'     \item \code{burnin.iterations}: number of warm-up iterations
#'       (default \code{1e3});
#'     \item \code{seed}: random seed used for reproducibility;
#'     \item \code{cores}: number of CPU cores used for sampling
#'       (default \code{getOption("mc.cores", 1L)}).
#'   }
#'   Values supplied through \code{...} override entries in \code{control}.
#' @param ... Optional arguments that override elements of
#'   \code{control}. For example,
#'   \code{iterations = 4000}, \code{burnin.iterations = 1000},
#'   \code{seed = 123}, or \code{cores = 2}.
#'
#' @return An object of class \code{"glmMixBayes"} containing (at least):
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
#'   \item{\code{estimates$dispersion}}{Posterior draws of the dispersion
#'   parameter for the correct-match component (component 1; family-specific).}
#'
#'   \item{\code{estimates$m.dispersion}}{Posterior draws of the dispersion
#'   parameter for the incorrect-match component (component 2; family-specific).}
#'
#'   \item{\code{family}}{The GLM family used in the model.}
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
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' fit <- glmMixBayes(
#'   X = x,
#'   y = y,
#'   family = "gaussian",
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#' }
#'
#' @export
#' @importFrom rstan sampling
#' @import label.switching
glmMixBayes <- function(X, y, family = "gaussian", priors = NULL,
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

 # normalize family input
 if (inherits(family, "family")) family <- family$family
 family <- tolower(trimws(as.character(family)[1]))

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

 if (!(family %in% c("gaussian", "poisson", "binomial", "gamma"))) {
  stop("Error: the family should be gaussian, poisson, binomial, or gamma", call. = FALSE)
 }

 # Minimal input checks (formula/data checks handled upstream)
 if (!is.matrix(X) || !is.numeric(X)) stop("`X` must be a numeric matrix.", call. = FALSE)
 if (!is.numeric(y) || length(y) != nrow(X)) stop("`y` must be numeric with length nrow(X).", call. = FALSE)
 if (anyNA(X) || anyNA(y)) stop("NA values found in X or y", call. = FALSE)

 # stan data (coerce for count/binary families if needed)
 stan_y <- y
 if (family %in% c("poisson", "binomial")) {
  stan_y <- as.integer(y)
  if (anyNA(stan_y)) stop("`y` could not be safely coerced to integer for family = '", family, "'.", call. = FALSE)
 }

 # Pre-compiled Stan Pipeline
 # Parse prior strings into a named list of numeric values using our helper
 prior_data <- prepare_stan_priors(priors, family, model_type = "glm")

 # Combine core data with parsed prior data
 stan_data <- c(
  list(N = nrow(X), K = ncol(X), X = X, y = stan_y),
  prior_data
 )

 # Identify pre-compiled model from rstantools' internal stanmodels object
 model_name <- paste0("glmMixBayes_", family)

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
  posterior <- swap_if_present(posterior, "sigma1", "sigma2")  # gaussian
  posterior <- swap_if_present(posterior, "phi1",   "phi2")    # gamma
 }


 if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
  old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
 } else {
  on.exit(rm(list = ".Random.seed", envir = .GlobalEnv), add = TRUE)
 }

 set.seed(seed)

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

 if (is.null(posterior$z) || is.null(posterior$beta1) || is.null(posterior$beta2)) {
  stop("Missing expected parameters in posterior", call. = FALSE)
 }

 beta1.p <- posterior$beta1
 beta2.p <- posterior$beta2
 tmp <- perm_pair(beta1.p, beta2.p, perm)
 beta1.p <- tmp[[1]]
 beta2.p <- tmp[[2]]

 if (family == "gamma") {
  tmp <- perm_pair(posterior$phi1, posterior$phi2, perm)
  phi1.p <- tmp[[1]]
  phi2.p <- tmp[[2]]
 }

 if (family == "gaussian") {
  tmp <- perm_pair(posterior$sigma1, posterior$sigma2, perm)
  sigma1.p <- tmp[[1]]
  sigma2.p <- tmp[[2]]
 }

 # Set coefficient names from X if available
 cn <- colnames(X)
 if (!is.null(cn) && is.matrix(beta1.p) && ncol(beta1.p) == length(cn)) {
  colnames(beta1.p) <- cn
  colnames(beta2.p) <- cn
 }

 out <- list(
  m_samples = z_samples,
  estimates = list(
   coefficients   = beta1.p,
   m.coefficients = beta2.p
  ),
  family = family,
  call = match.call()
 )

 if (family == "gamma") {
  out$estimates$dispersion   <- 1 / phi1.p
  out$estimates$m.dispersion <- 1 / phi2.p
 }
 if (family == "gaussian") {
  out$estimates$dispersion   <- sigma1.p^2
  out$estimates$m.dispersion <- sigma2.p^2
 }

 class(out) <- "glmMixBayes"
 out
}

# S3 dispatch method for the postlink internal generic fitglm()
#' @keywords internal
#' @export
fitglm.adjMixBayes <- function(x, y, family, adjustment, control, priors = NULL, ...) {

 # 1. Validation and Data Retrieval
 full_data <- adjustment$data_ref$data
 if (is.null(full_data)) {
  stop("The 'adjustment' object does not contain linked data. ",
       "Please recreate the object with 'linked.data' provided.", call. = FALSE)
 }

 # 2. Align Adjustment Data to Outcome Model (X, Y) via row names
 subset_names <- rownames(x)

 if (is.null(subset_names)) {
  if (nrow(x) != nrow(full_data)) {
   stop("Row mismatch: Model matrix 'x' has no row names and its length (", nrow(x),
        ") differs from the adjustment data (", nrow(full_data), "). ",
        "Ensure 'linked.data' matches the data passed to the upstream wrapper.", call. = FALSE)
  }
  idx_map <- seq_len(nrow(full_data))
 } else {
  idx_map <- match(subset_names, rownames(full_data))
  if (anyNA(idx_map)) {
   stop("Row mismatch: Some observations in the model matrix could not be matched ",
        "to the adjustment data. This usually happens if the upstream 'data' ",
        "differs from the data used to create the adjustment object.", call. = FALSE)
  }
 }

 # 3. Ensure no missingness in x/y
 if (anyNA(x) || anyNA(y)) {
  stop("NA values found in x or y. Upstream wrapper should remove missingness.", call. = FALSE)
 }

 # 4. Extract priors using hierarchy (function arg > dots > control > adjustment)
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

 # 5. Dispatch to computational engine
 fit <- do.call(
  glmMixBayes,
  c(
   list(X = x, y = y, family = family, priors = final_priors, control = control),
   dots
  )
 )

 # 6. Post-processing
 fit$adjustment <- adjustment
 fit$call <- match.call()

 if (!is.null(subset_names)) {
  fit$obs_names <- subset_names
 }

 fit
}
