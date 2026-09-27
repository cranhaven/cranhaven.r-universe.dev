#' GLM with Mixture-Based Linkage Error Adjustment
#'
#' Fits a generalized linear model (GLM) accounting for mismatch errors using a
#' mixture model framework in the secondary analysis setting. The variance-covariance
#' matrix is estimated using the sandwich formula.
#'
#' @param x Design matrix for the primary outcome model (numeric matrix or data frame).
#' @param y Response vector for the primary outcome model.
#' @param family A family object (e.g., \code{gaussian}, \code{binomial}) specifying
#'   the error distribution and link function. Can be a character string or a function.
#' @param z Design matrix for the mismatch indicator model (mismatch covariates).
#'   If NULL, an intercept-only model is assumed.
#' @param m.rate The assumed overall mismatch rate (a proportion between 0 and 1).
#'   If provided, it imposes a constraint on the mismatch model intercept.
#' @param safe.matches Logical vector; \code{TRUE} indicates a "safe match" (treated
#'   as definitely correct), \code{FALSE} indicates a potential mismatch.
#' @param control An optional list of control parameters. Arguments passed via \code{...}
#'   will override values in this list.
#'   \itemize{
#'     \item \code{max.iter}: Maximum EM iterations (default: 1000).
#'     \item \code{cmax.iter}: Maximum iterations for the subroutine in the constrained logistic regression function (default: 1000).
#'     \item \code{tol}: Convergence tolerance (default: 1e-4).
#'     \item \code{init.beta}: Initial parameter estimates for the outcome model.
#'     \item \code{init.gamma}: Initial parameter estimates for the mismatch indicator model.
#'     \item \code{fy}: Estimated marginal density of the response. If NULL, estimated using Kernel Density Estimation or parametric assumption.
#'   }
#' @param ... Additional arguments passed to \code{control}.
#'
#' @returns A list of results:
#' \item{coefficients}{A named vector of coefficients for the outcome model.}
#' \item{m.coefficients}{A named vector of coefficients for the mismatch indicator model (gamma).}
#' \item{match.prob}{The posterior correct match probabilities (weights) for each observation.}
#' \item{residuals}{The working residuals, defined as \code{y - fitted.values}.}
#' \item{fitted.values}{The fitted mean values of the outcome model, obtained by transforming the linear predictors by the inverse of the link function.}
#' \item{linear.predictors}{The linear fit on the link scale.}
#' \item{deviance}{The deviance of the weighted outcome model at convergence.}
#' \item{null.deviance}{The deviance of the weighted null outcome model.}
#' \item{var}{The estimated variance-covariance matrix of the parameters (sandwich estimator).}
#' \item{dispersion}{The estimated dispersion parameter (e.g., variance for Gaussian, 1/shape for Gamma).}
#' \item{objective}{A vector tracking the negative log pseudo-likelihood at each iteration of the EM algorithm.}
#' \item{converged}{Logical indicating if the EM algorithm converged within \code{max.iter}.}
#' \item{rank}{The numeric rank of the fitted linear model.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{df.null}{The residual degrees of freedom for the null model.}
#' \item{family}{The \code{family} object used.}
#' \item{\code{call}}{The matched call.}
#'
#' @references
#' Slawski, M.*, West, B. T., Bukke, P., Wang, Z., Diao, G., &
#' Ben-David, E. (2025). A general framework for regression with mismatched
#' data based on mixture modelling. \emph{Journal of the Royal Statistical Society
#' Series A: Statistics in Society}, 188(3), 896-919. \doi{10.1093/jrsssa/qnae083}
#'
#' Slawski, M.*, Diao, G., Ben-David, E. (2021). A pseudo-likelihood approach to
#' linear regression with partially shuffled data. \emph{Journal of Computational
#' and Graphical Statistics}. 30(4), 991-1003. \doi{10.1080/10618600.2020.1870482}
#'
#' @importFrom stats glm.fit dnorm dgamma dpois dbinom plogis qlogis density approxfun coef sd family
#' @importFrom stats gaussian binomial quasibinomial optim model.matrix model.frame model.response
#'
#' @examples
#' data(lifem)
#'
#' x <- cbind(1, poly(lifem$unit_yob, 3, raw = TRUE))
#' y <- lifem$age_at_death
#' z <- cbind(1, lifem$commf, lifem$comml)
#'
#' fit <- glmMixture(x, y, family = "gaussian",
#'                   z, m.rate = 0.05, safe.matches = lifem$hndlnk)
#'
#' @export
glmMixture <- function(x, y, family,
                       z = cbind(rep(1,nrow(x))), m.rate = NULL, safe.matches = NULL,
                       control = list(), ...) {

 # 1. Set-up and Arguments
 dots <- list(...)
 con <- list(init.beta = NULL, init.gamma = NULL, fy = NULL,
             max.iter = 1000, tol = 1E-4, cmax.iter = 1000)
 con[names(control)] <- control
 con[names(dots)] <- dots

 x <- as.matrix(x)
 y <- as.numeric(y)
 n <- nrow(x)
 p <- ncol(x)
 d <- p
 z <- as.matrix(z)

 if (is.null(safe.matches)) safe.matches <- rep(FALSE, n)

 if (!is.null(m.rate)) {
  logitbound <- -log((1 - m.rate) / m.rate)
 } else {
  logitbound <- NULL
 }

 family_arg <- if (!is.null(family)) family else stats::gaussian()
 if (is.character(family_arg)) {
  if (tolower(family_arg) == "gamma") family <- stats::Gamma(link = "log")
  else {
   family_fun <- try(get(family_arg, mode = "function", envir = parent.frame()), silent = TRUE)
   if (inherits(family_fun, "try-error")) stop(paste("Invalid family argument:", family_arg))
   family <- family_fun()
  }
 } else if (is.function(family_arg)) {
  family <- family_arg()
 } else {
  family <- family_arg
 }
 family_name <- tolower(family$family)

 if (family_name == "gaussian" && family$link != "identity") {
  stop("The 'gaussian' family is currently restricted to the 'identity' link.")
 }

 # Marginal Density (fy) Estimation
 fy <- con$fy
 if (is.null(fy)) {
  if (family_name == "gaussian") {
   fy <- stats::dnorm(y, mean = mean(y), sd = stats::sd(y))
  } else if (family_name == "binomial") {
   fy <- (mean(y)^y) * (1 - mean(y))^(1 - y)
  } else {
   kde_obj <- stats::density(y)
   g <- stats::approxfun(x = kde_obj$x, y = kde_obj$y, method = "linear")
   fy <- g(y)
  }
 }
 # prevent 0s in custom marginals or KDE boundaries
 if (!is.null(fy)) {
  fy[is.na(fy) | fy <= 0] <- 1e-10
 }

 # Initialization: Beta & Shape
 beta_cur <- con$init.beta

 # Define shape_update globally so it is always available for the EM loop
 shape_update <- function(mu_val, mme, pcur) {
  f <- function(shape) {
   -sum(pcur * stats::dgamma(y, shape = shape, rate = shape / mu_val, log = TRUE))
  }
  lower_bound <- max(1e-4, 0.1 * mme)
  upper_bound <- max(10, 10 * mme)
  stats::optimize(f = f, lower = lower_bound, upper = upper_bound)$minimum
 }

 if (is.null(beta_cur)) {
  if (family_name != "gamma") {
   init_fit <- stats::glm.fit(x, y, family = family)
   beta_cur <- stats::coef(init_fit)
  } else {
   init_fit <- stats::glm.fit(x, y, family = stats::Gamma(link = "log"))
   beta_cur <- stats::coef(init_fit)
  }
 }

 # Always initialize dispersion parameters, regardless of whether beta was provided
 shape_cur <- NA
 stdcur <- NA

 if (family_name == "gaussian") {
  stdcur <- sqrt(sum((y - x %*% beta_cur)^2) / max(1, n - p))
 } else if (family_name == "gamma") {
  mu_init <- pmax(family$linkinv(x %*% beta_cur), 1e-10)
  disp_init <- sum(((y - mu_init)/mu_init)^2) / max(1, n - p)
  if (is.na(disp_init) || disp_init <= 0) disp_init <- 1
  shape_cur <- shape_update(mu_init, mme = 1/disp_init, rep(1, n))
 }

 # Initialization: Gamma
 gamma_cur <- con$init.gamma
 if (!is.null(gamma_cur)) {
  gamma_cur <- -1 * gamma_cur
 }
 if (is.null(gamma_cur)) {
  if (!is.null(logitbound)) {
   if (ncol(z) == 1 && all(z == 1)) {
    gamma_cur <- rep(-logitbound, ncol(z))
   } else {
    gamma_cur <- c(max(-logitbound, 0), rep(0, ncol(z) - 1))
   }
  } else {
   gamma_cur <- rep(0, ncol(z))
  }
 }

 # Helpers
 hgamma <- function(eta) {
  pi <- stats::plogis(eta)
  list(fun = pi, dfun = pi * (1 - pi), d2fun = pi * (1 - pi) * (1 - 2 * pi))
 }

 fymu_eval <- function(mu, sigma, sub, shape) {
  mu_safe <- mu[sub]
  if (family_name %in% c("poisson", "gamma")) {
   mu_safe <- pmax(mu_safe, 1e-10)
  } else if (family_name == "binomial") {
   mu_safe <- pmax(pmin(mu_safe, 1 - 1e-10), 1e-10)
  }

  if (family_name == "gaussian") return(stats::dnorm((y[sub] - mu_safe), sd = sigma))
  if (family_name == "poisson") return(stats::dpois(y[sub], mu_safe))
  if (family_name == "binomial") return(stats::dbinom(y[sub], 1, mu_safe))
  if (family_name == "gamma") return(stats::dgamma(y[sub], shape, shape / mu_safe))
 }

 nloglik <- function(mu, sigma, hs, shape) {
  term1 <- hs[!safe.matches] * fymu_eval(mu, sigma, !safe.matches, shape) + (1 - hs[!safe.matches]) * fy[!safe.matches]
  term2 <- fymu_eval(mu, sigma, safe.matches, shape)

  sum(-log(pmax(term1, 1e-300))) - sum(log(pmax(term2, 1e-300)))
 }

 # 2. EM-Algorithm
 eta_cur <- x %*% beta_cur
 mu_cur <- family$linkinv(eta_cur)
 hs <- hgamma(z %*% gamma_cur)$fun
 hs[safe.matches] <- 1
 p_cur <- rep(0, n)

 iter <- 1
 objs <- numeric(con$max.iter)
 objs[iter] <- nloglik(mu_cur, stdcur, hs, shape_cur)

 converged_flag <- FALSE
 w_glm_fit <- NULL

 while (iter < con$max.iter) {
  # E-Step
  num <- hs[!safe.matches] * fymu_eval(mu_cur, stdcur, !safe.matches, shape_cur)
  denom <- num + (1 - hs[!safe.matches]) * fy[!safe.matches]
  denom[denom <= 0] <- 1e-10

  p_cur[!safe.matches] <- pmin(pmax(num / denom, 1e-10), 1 - 1e-10)
  p_cur[safe.matches] <- 1

  # M-Step: Gamma
  if (!is.null(logitbound)) {
   if (ncol(z) == 1 && all(z == 1)) {
    temp_glm_h <- tryCatch(stats::glm.fit(z[!safe.matches, , drop = FALSE], p_cur[!safe.matches], family = stats::quasibinomial()), error = function(e) NULL)
    if (!is.null(temp_glm_h) && !anyNA(stats::coef(temp_glm_h))) {
     gamma_cur <- max(stats::coef(temp_glm_h), -logitbound)
    } else {
     warning("Mismatch model failed to find valid coefficients. Terminating EM early.")
     break
    }
   } else {
    temp_glm_h <- constrained_logistic_regression(z[!safe.matches, , drop = FALSE], 1 - p_cur[!safe.matches], logitbound, con$cmax.iter)
    if (!is.null(temp_glm_h) && !anyNA(temp_glm_h$beta)) {
     gamma_cur <- -temp_glm_h$beta
    } else {
     warning("Mismatch model failed to find valid coefficients. Terminating EM early.")
     break
    }
   }
  } else {
   temp_glm_h <- tryCatch(stats::glm.fit(z[!safe.matches, , drop = FALSE], p_cur[!safe.matches], family = stats::quasibinomial()), error = function(e) NULL)
   if (!is.null(temp_glm_h) && !anyNA(stats::coef(temp_glm_h))) {
    gamma_cur <- stats::coef(temp_glm_h)
   } else {
    warning("Mismatch model failed to find valid coefficients. Terminating EM early.")
    break
   }
  }
  hs[!safe.matches] <- hgamma(z[!safe.matches, , drop = FALSE] %*% gamma_cur)$fun

  # M-Step: Beta & Dispersion
  if (family_name != "gamma") {
   temp_glm_fit <- tryCatch(stats::glm.fit(x, y, weights = p_cur, family = family, start = beta_cur), error = function(e) NULL)
   if (is.null(temp_glm_fit) || anyNA(stats::coef(temp_glm_fit))) {
    temp_glm_fit <- tryCatch(stats::glm.fit(x, y, weights = p_cur, family = family), error = function(e) NULL)
   }

   if (!is.null(temp_glm_fit) && !anyNA(stats::coef(temp_glm_fit))) {
    w_glm_fit <- temp_glm_fit
    beta_cur <- stats::coef(w_glm_fit)
   } else {
    warning("Outcome model failed to converge in M-step. Terminating EM algorithm early.")
    break
   }
  } else {
   temp_glm_fit <- tryCatch(stats::glm.fit(x, y, weights = p_cur, family = family, start = beta_cur), error = function(e) NULL)
   if (is.null(temp_glm_fit) || anyNA(stats::coef(temp_glm_fit))) {
    temp_glm_fit <- tryCatch(stats::glm.fit(x, y, weights = p_cur, family = family), error = function(e) NULL)
   }

   if (!is.null(temp_glm_fit) && !anyNA(stats::coef(temp_glm_fit))) {
    w_glm_fit <- temp_glm_fit
    beta_cur <- stats::coef(w_glm_fit)
    mu_w <- pmax(w_glm_fit$fitted.values, 1e-10)
    disp_w <- sum(p_cur * ((y - mu_w)/mu_w)^2) / max(sum(p_cur), 1)
    if (is.na(disp_w) || disp_w <= 0) disp_w <- 1

    try_shape <- tryCatch(shape_update(mu_w, mme = 1/disp_w, p_cur), error = function(e) NA)
    if (!is.na(try_shape)) shape_cur <- try_shape
   } else {
    warning("Outcome model failed to converge in M-step. Terminating EM algorithm early.")
    break
   }
  }

  eta_cur <- x %*% beta_cur
  mu_cur <- family$linkinv(eta_cur)

  if (family_name == "gaussian") {
   stdcur <- sqrt(stats::weighted.mean((y - mu_cur)^2, w = p_cur))
  }

  iter <- iter + 1
  objs[iter] <- nloglik(mu_cur, stdcur, hs, shape_cur)

  if (is.na(objs[iter])) {
   warning("EM algorithm did not converge. NA objective value occurred.")
   break
  }
  if (abs(objs[iter] - objs[iter - 1]) < con$tol) {
   converged_flag <- TRUE
   break
  }
 }

 # 3. Standard Errors
 hgamma_eval <- hgamma(z[!safe.matches, , drop = FALSE] %*% as.matrix(gamma_cur))

 if (family_name == "gaussian") {
  fymu_all_eval <- (function(mu, std, sub) {
   fun <- stats::dnorm((y - mu)[sub], sd = std)
   d_fun_beta <- fun * (y - mu)[sub] / (std^2)
   d_fun_sigma <- fun * ((y - mu)[sub]^2 / std^3 - 1 / std)
   d2_fun_beta <- d_fun_beta * (y - mu)[sub] / (std^2) - fun / (std^2)
   d2_fun_sigma <- d_fun_sigma * ((y - mu)[sub]^2 / std^3 - 1 / std) + fun * (1 / std^2 - 3 * (y - mu)[sub]^2 / std^4)
   d2_fun_beta_sigma <- d_fun_sigma * (y - mu)[sub] / (std^2) - 2 * fun * (y - mu)[sub] / (std^3)
   list(fun = fun, dfun_beta = d_fun_beta, dfun_sigma = d_fun_sigma,
        d2fun_beta = d2_fun_beta, d2fun_sigma = d2_fun_sigma, d2fun_beta_sigma = d2_fun_beta_sigma)
  })(mu_cur, stdcur, !safe.matches)

  mixprob <- fy[!safe.matches] * (1 - hgamma_eval$fun) + hgamma_eval$fun * fymu_all_eval$fun
  w_beta_score <- (-1) * fymu_all_eval$dfun_beta * hgamma_eval$fun / mixprob
  w_sigma_score <- (-1) * fymu_all_eval$dfun_sigma * hgamma_eval$fun / mixprob
  w_gamma_score <- (-1) * (fymu_all_eval$fun - fy[!safe.matches]) * hgamma_eval$dfun / mixprob

  Xw1 <- sweep(x[!safe.matches, , drop = FALSE], 1, w_beta_score, "*")
  Xw2 <- sweep(matrix(1, sum(!safe.matches), 1), 1, w_sigma_score, "*")
  Deltaw3 <- sweep(z[!safe.matches, , drop = FALSE], 1, w_gamma_score, "*")
  meat <- crossprod(cbind(Xw1, Xw2, Deltaw3))

  w_beta2_hess <- (-(hgamma_eval$fun * fymu_all_eval$d2fun_beta) / mixprob) + w_beta_score^2
  w_sigma2_hess <- (-(hgamma_eval$fun * fymu_all_eval$d2fun_sigma) / mixprob) + w_sigma_score^2
  w_gamma2_hess <- ((-(fymu_all_eval$fun - fy[!safe.matches]) * hgamma_eval$d2fun) / mixprob) + w_gamma_score^2
  w_beta_gamma_hess <- (-(fymu_all_eval$dfun_beta * hgamma_eval$dfun) / mixprob) +
   ((fymu_all_eval$fun - fy[!safe.matches]) * hgamma_eval$fun * hgamma_eval$dfun * fymu_all_eval$dfun_beta) / (mixprob^2)
  w_sigma_gamma_hess <- (-(fymu_all_eval$dfun_sigma * hgamma_eval$dfun) / mixprob) +
   ((fymu_all_eval$fun - fy[!safe.matches]) * hgamma_eval$fun * hgamma_eval$dfun * fymu_all_eval$dfun_sigma) / (mixprob^2)

  w_beta_sigma_hess <- (-(fymu_all_eval$d2fun_beta_sigma * hgamma_eval$dfun) / mixprob) + w_beta_score * w_sigma_score

  Xw4 <- sweep(x[!safe.matches, , drop = FALSE], 1, w_beta2_hess, "*")
  Deltaw6 <- sweep(z[!safe.matches, , drop = FALSE], 1, w_gamma2_hess, "*")
  Xw5 <- sweep(x[!safe.matches, , drop = FALSE], 1, w_beta_gamma_hess, "*")

  Hess <- matrix(0, d + 1 + ncol(z), d + 1 + ncol(z))
  one_v <- matrix(1, sum(!safe.matches), 1)

  Hess[1:d, 1:d] <- crossprod(x[!safe.matches, , drop = FALSE], Xw4)
  Hess[d+1, d+1] <- crossprod(one_v, sweep(one_v, 1, w_sigma2_hess, "*"))
  Hess[(d+2):ncol(Hess), (d+2):ncol(Hess)] <- crossprod(z[!safe.matches, , drop = FALSE], Deltaw6)

  Hess[1:(d+1), (d+2):ncol(Hess)] <- rbind(crossprod(Xw5, z[!safe.matches, , drop = FALSE]),
                                           crossprod(sweep(one_v, 1, w_sigma_gamma_hess, "*"), z[!safe.matches, , drop = FALSE]))
  Hess[(d+2):ncol(Hess), 1:(d+1)] <- t(Hess[1:(d+1), (d+2):ncol(Hess)])

  Hess[1:d, d+1] <- crossprod(x[!safe.matches, , drop = FALSE], sweep(one_v, 1, w_beta_sigma_hess, "*"))
  Hess[d+1, 1:d] <- t(Hess[1:d, d+1])

 } else {
  fymu_all_eval <- (function(eta, sub, family_obj, shape) {
   y_sub <- y[sub]
   eta_sub <- eta[sub]
   mu_sub <- family_obj$linkinv(eta_sub)

   if (family_obj$family %in% c("poisson", "Gamma")) mu_sub <- pmax(mu_sub, 1e-10)
   if (family_obj$family == "binomial") mu_sub <- pmax(pmin(mu_sub, 1 - 1e-10), 1e-10)

   if (family_obj$family == "poisson") {
    fun <- stats::dpois(y_sub, mu_sub)
   } else if (family_obj$family == "binomial") {
    fun <- stats::dbinom(y_sub, 1, mu_sub)
   } else if (family_obj$family == "Gamma") {
    fun <- stats::dgamma(y_sub, shape, shape / mu_sub)
   } else {
    fun <- rep(1, length(y_sub))
   }

   mu_prime <- family_obj$mu.eta(eta_sub)
   var_mu <- family_obj$variance(mu_sub)

   if (family_obj$family == "Gamma") {
    score_eta <- (y_sub - mu_sub) / var_mu * mu_prime * shape
    expected_hess <- - (mu_prime^2) / var_mu * shape
   } else {
    score_eta <- (y_sub - mu_sub) / var_mu * mu_prime
    expected_hess <- - (mu_prime^2) / var_mu
   }

   dfun <- fun * score_eta
   d2fun <- fun * (score_eta^2 + expected_hess)

   list(fun = fun, dfun = dfun, d2fun = d2fun)
  })(eta_cur, !safe.matches, family, shape_cur)

  mixprob <- fy[!safe.matches] * (1 - hgamma_eval$fun) + hgamma_eval$fun * fymu_all_eval$fun
  w_beta_score <- (-1) * fymu_all_eval$dfun * hgamma_eval$fun / mixprob
  w_gamma_score <- (-1) * (fymu_all_eval$fun - fy[!safe.matches]) * hgamma_eval$dfun / mixprob

  Xw1 <- sweep(x[!safe.matches, , drop = FALSE], 1, w_beta_score, "*")
  Deltaw3 <- sweep(z[!safe.matches, , drop = FALSE], 1, w_gamma_score, "*")
  meat <- crossprod(cbind(Xw1, Deltaw3))

  w_beta2_hess <- (-(hgamma_eval$fun * fymu_all_eval$d2fun) / mixprob) + w_beta_score^2
  w_gamma2_hess <- ((-(fymu_all_eval$fun - fy[!safe.matches]) * hgamma_eval$d2fun) / mixprob) + w_gamma_score^2
  w_beta_gamma_hess <- (-(fymu_all_eval$dfun * hgamma_eval$dfun) / mixprob) +
   ((fymu_all_eval$fun - fy[!safe.matches]) * hgamma_eval$fun * hgamma_eval$dfun * fymu_all_eval$dfun) / (mixprob^2)

  Xw4 <- sweep(x[!safe.matches, , drop = FALSE], 1, w_beta2_hess, "*")
  Deltaw6 <- sweep(z[!safe.matches, , drop = FALSE], 1, w_gamma2_hess, "*")
  Xw5 <- sweep(x[!safe.matches, , drop = FALSE], 1, w_beta_gamma_hess, "*")

  Hess <- matrix(0, d + ncol(z), d + ncol(z))
  Hess[1:d, 1:d] <- crossprod(x[!safe.matches, , drop = FALSE], Xw4)
  Hess[(d+1):ncol(Hess), (d+1):ncol(Hess)] <- crossprod(z[!safe.matches, , drop = FALSE], Deltaw6)
  Hess[1:d, (d+1):ncol(Hess)] <- crossprod(Xw5, z[!safe.matches, , drop = FALSE])
  Hess[(d+1):ncol(Hess), 1:d] <- t(Hess[1:d, (d+1):ncol(Hess)])
 }

 gamma_cur <- as.numeric(-1 * gamma_cur)

 cov_1_hat <- tryCatch(solve(Hess, meat), error = function(e) matrix(NA, nrow(Hess), ncol(Hess)))
 covhat <- if (anyNA(cov_1_hat)) matrix(NA, nrow(Hess), ncol(Hess)) else t(solve(Hess, t(cov_1_hat)))

 beta_n <- colnames(x)
 if(!is.null(colnames(z))){
 gamma_n <- colnames(z)
 } else{
 gamma_n <- c(1:ncol(z))
 }
 names(gamma_cur) <- gamma_n

 if (family_name == "gaussian") {
  if (!anyNA(covhat)) {
   covhat[d+1, ] <- covhat[d+1, ] * (2 * stdcur)
   covhat[, d+1] <- covhat[, d+1] * (2 * stdcur)
  }
  rn <- c(paste("coef", beta_n), "dispersion", paste("m.coef", gamma_n))
 } else {
  rn <- c(paste("coef", beta_n), paste("m.coef", gamma_n))
 }
 rownames(covhat) <- colnames(covhat) <- rn

 p_tot <- ncol(covhat)
 p_gamma <- ncol(z)
 idx_other <- 1:(p_tot - p_gamma)
 idx_gamma <- (p_tot - p_gamma + 1):p_tot

 if (!anyNA(covhat)) {
  covhat[idx_other, idx_gamma] <- -1 * covhat[idx_other, idx_gamma]
  covhat[idx_gamma, idx_other] <- -1 * covhat[idx_gamma, idx_other]
 }

 out <- list(coefficients = beta_cur,
             m.coefficients = gamma_cur,
             residuals = y - mu_cur,
             linear.predictors = eta_cur,
             fitted.values = mu_cur,
             deviance = if (!is.null(w_glm_fit)) w_glm_fit$deviance else NA,
             null.deviance = if (!is.null(w_glm_fit)) w_glm_fit$null.deviance else NA,
             df.residual = n - p,
             df.null = n - 1,
             rank = p,
             family = family,
             converged = converged_flag,
             match.prob = as.numeric(hs),
             var = covhat,
             objective = objs[1:iter],
             call = match.call())

 if (family_name == "gaussian") out$dispersion <- stdcur^2
 else if (family_name == "gamma") out$dispersion <- 1 / shape_cur
 else out$dispersion <- 1

 class(out) <- c("glmMixture")
 return(out)
}

#' @keywords internal
#' @export
fitglm.adjMixture <- function(x, y, family, adjustment, control, ...) {
 # 1. Validation and Data Retrieval
 full_data <- adjustment$data_ref$data
 if (is.null(full_data)) {
  stop("The 'adjustment' object does not contain linked data. ",
       "Please recreate the object with 'linked.data' provided.", call. = FALSE)
 }

 # 2. Stage 1: Align Adjustment Data to Outcome Model (X, Y)
 # plglm() has already applied 'subset' and 'na.action' to x and y.
 # We use row names to synchronize the adjustment data.
 subset_names <- rownames(x)

 # If x has no row names (rare), assume 1:1 mapping if sizes match.
 # This handles cases where users strip names or use matrices without names.
 if (is.null(subset_names)) {
  if (nrow(x) != nrow(full_data)) {
   stop("Row mismatch: Model matrix 'x' has no row names and its length (", nrow(x),
        ") differs from the adjustment data (", nrow(full_data), "). ",
        "Ensure 'linked.data' matches the data passed to 'plglm'.", call. = FALSE)
  }
  # Assume the user provided the exact same dataset in the same order
  data_subset <- full_data
 } else {
  # Match by name. Using strict matching ensures order is preserved.
  # Note: data.frames are guaranteed to have unique row names in R.
  idx_map <- match(subset_names, rownames(full_data))

  if (anyNA(idx_map)) {
   stop("Row mismatch: Some observations in the model matrix could not be matched ",
        "to the adjustment data. This usually happens if 'data' in plglm() ",
        "is different from the data used to create the adjustment object.", call. = FALSE)
  }
  data_subset <- full_data[idx_map, , drop = FALSE]
 }

 # 3. Construct Mismatch Covariates (Z)
 # We use model.frame/model.matrix to handle factors and transformations in m.formula
 m_formula <- adjustment$m.formula

 # We use 'na.pass' here because we want to detect NAs manually in the next step
 # to ensure we drop the corresponding rows in X and Y simultaneously.
 Z_frame <- tryCatch({
  stats::model.frame(m_formula, data = data_subset, na.action = stats::na.pass)
 }, error = function(e) {
  stop("Failed to resolve variables in 'm.formula': ", e$message, call. = FALSE)
 })

 Z <- stats::model.matrix(m_formula, Z_frame)

 # 4. Stage 2: Secondary Intersection (Handle Missingness in Z)
 # X and Y are already clean (no NAs). But Z might have NAs.
 # If Z has NAs, we must drop those rows from X, Y, and Z to stay aligned.

 # Identify complete cases in Z
 keep_idx <- stats::complete.cases(Z)

 if (!all(keep_idx)) {
  n_dropped <- sum(!keep_idx)
  warning(sprintf("Dropped %d observation(s) due to missing values in the
                    mismatch covariates (Z) that were not missing in the
                    outcome variables.", n_dropped), call. = FALSE)

  # Apply the subset
  x <- x[keep_idx, , drop = FALSE]
  y <- y[keep_idx]
  Z <- Z[keep_idx, , drop = FALSE]

  # Also update the row map for the safe matches step below
  if (!is.null(subset_names)) {
   subset_names <- subset_names[keep_idx]
   idx_map <- idx_map[keep_idx]
  } else {
   # If we were using implicit ordering, we must subset the implicit map
   # This logic handles the "no row names" edge case
   full_indices <- seq_len(nrow(full_data))
   idx_map <- full_indices[keep_idx]
  }
 }

 # 5. Process Safe Matches
 safe_matches_all <- adjustment$safe.matches
 safe_matches_sub <- NULL

 if (!is.null(safe_matches_all)) {
  # Ensure the safe.matches vector aligns with the final subset of data
  if (!is.null(subset_names)) {
   safe_matches_sub <- safe_matches_all[idx_map]
  } else {
   safe_matches_sub <- safe_matches_all[1:nrow(x)]
  }
 }

 # 6. Dispatch to Computational Engine
 fit <- glmMixture(
  x = x,
  y = y,
  family = family,
  z = Z,
  m.rate = adjustment$m.rate,
  safe.matches = safe_matches_sub,
  control = control,
  ...
 )

 # 7. Post-Processing
 fit$adjustment <- adjustment
 fit$m.formula  <- m_formula
 fit$call <- match.call()

 # Restore row names to observation-level outputs
 if (!is.null(subset_names)) {
  names(fit$residuals)         <- subset_names
  names(fit$fitted.values)     <- subset_names
  names(fit$linear.predictors) <- subset_names
  # match.prob is a vector in glmMixture, verify before naming
  if (!is.null(fit$match.prob)) names(fit$match.prob) <- subset_names
 }

 return(fit)
}
