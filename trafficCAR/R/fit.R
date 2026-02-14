#' Fit Gaussian CAR / ICAR regression via Gibbs sampling
#'
#' Fits a Gaussian regression with a CAR/ICAR latent effect:
#' \eqn{y = X\beta + x + \epsilon} with \eqn{\epsilon \sim N(0,\sigma^2 I)} and
#' \eqn{x \sim N(0, Q^{-1})}, where \eqn{Q = \tau (D - \rho A)} and
#' \eqn{D = diag(A 1)}. For ICAR, \eqn{\rho = 1}.
#'
#' The sampler updates \eqn{x}, \eqn{\beta} (if \code{X} is provided), and \eqn{\sigma^2}
#' using Gibbs steps.
#'
#' @param y Numeric response vector of length \code{n}.
#' @param A Square \code{n x n} adjacency/weight matrix (base matrix or \code{Matrix}).
#'   Diagonal entries are ignored.
#' @param X Optional \code{n x p} design matrix. If \code{NULL}, no regression is fit.
#' @param type Either \code{"icar"} or \code{"proper"}.
#' @param rho Spatial dependence parameter for proper CAR. Ignored for ICAR.
#' @param tau Positive scalar precision multiplier.
#' @param n_iter Total MCMC iterations.
#' @param burn_in Number of initial iterations to discard.
#' @param thin Keep every \code{thin}-th draw after burn-in.
#' @param beta_init Optional initial \eqn{\beta} (length \code{p}).
#' @param x_init Optional initial latent field \eqn{x} (length \code{n}).
#' @param sigma2_init Optional initial \eqn{\sigma^2} (positive scalar).
#' @param b0 Prior mean for \eqn{\beta} (length \code{p}). Default is zero vector.
#' @param B0 Prior covariance for \eqn{\beta} (\code{p x p}). Default is large diagonal.
#' @param a0 Shape parameter for inverse-gamma prior on \eqn{\sigma^2}.
#' @param b0_sigma Scale parameter for inverse-gamma prior on \eqn{\sigma^2}.
#' @param center_icar Logical; if \code{TRUE} and \code{type="icar"}, center \eqn{x}
#'   to sum-to-zero within each connected component.
#' @param verbose Logical; print coarse progress updates.
#'@importFrom methods as
#' @return A list of class \code{"trafficCAR_fit"} with elements:
#' \describe{
#'   \item{\code{draws}}{List with MCMC draws \code{x}, \code{beta}, \code{sigma2}.}
#'   \item{\code{keep}}{Iteration indices that were saved.}
#'   \item{\code{type}, \code{rho}, \code{tau}}{Model hyperparameters used.}
#' }
#' y = X beta + x + epsilon
#'
#' @export
fit_car <- function(
    y,
    A,
    X = NULL,
    type = c("icar", "proper"),
    rho = 0.99,
    tau = 1,
    n_iter = 2000,
    burn_in = floor(n_iter / 2),
    thin = 1,
    beta_init = NULL,
    x_init = NULL,
    sigma2_init = NULL,
    b0 = NULL,
    B0 = NULL,
    a0 = 2,
    b0_sigma = 1,
    center_icar = TRUE,
    verbose = FALSE
) {

  ## ---- Normalize adjacency matrix ----------------------------------------
  if (inherits(A, "Matrix")) {
    if (!inherits(A, "dgCMatrix")) {
      A <- as(A, "generalMatrix")
      A <- as(A, "dgCMatrix")
    }
  } else if (is.matrix(A)) {
    A <- Matrix::Matrix(A, sparse = TRUE)
  } else {
    stop("`A` must be a base matrix or a Matrix sparse type.")
  }

  type <- match.arg(type)

  ## ---- Validate y / dimensions -------------------------------------------
  if (!is.numeric(y) || any(!is.finite(y)))
    stop("`y` must be a finite numeric vector.")
  y <- as.double(y)
  n <- length(y)

  ## x_init validation
  if (!is.null(x_init)) {
    if (!is.numeric(x_init) ||
        length(x_init) != n ||
        any(!is.finite(x_init))) {
      stop("`x_init` must be a finite numeric vector of length n.")
    }
  }

  if (nrow(A) != ncol(A)) stop("`A` must be square.")
  if (nrow(A) != n) stop("`A` must have nrow(A) == length(y).")

  ## hyperparameters
  if (!is.numeric(tau) || tau <= 0)
    stop("`tau` must be positive.")
  if (!is.numeric(rho) || !is.finite(rho))
    stop("`rho` must be finite.")

  if (!is.numeric(n_iter) || n_iter < 1)
    stop("`n_iter` must be >= 1.")
  if (!is.numeric(burn_in) || burn_in < 0 || burn_in >= n_iter)
    stop("`burn_in` must be in {0, ..., n_iter-1}.")
  if (!is.numeric(thin) || thin < 1)
    stop("`thin` must be >= 1.")

  if (!is.numeric(a0) || a0 <= 0)
    stop("`a0` must be > 0.")
  if (!is.numeric(b0_sigma) || b0_sigma <= 0)
    stop("`b0_sigma` must be > 0.")

  ## X handling
  if (is.null(X)) {
    X <- matrix(0, nrow = n, ncol = 0)
  } else {
    if (!is.matrix(X)) X <- as.matrix(X)
    if (nrow(X) != n)
      stop("`X` must have nrow(X) == length(y).")
    if (!is.numeric(X) || any(!is.finite(X)))
      stop("`X` must be finite numeric.")
    X <- apply(X, 2, as.double)
    X <- matrix(X, nrow = n)
  }
  p <- ncol(X)

  ## beta_init validation
  if (!is.null(beta_init)) {
    if (!is.numeric(beta_init) ||
        length(beta_init) != p ||
        any(!is.finite(beta_init))) {
      stop("`beta_init` must be a finite numeric vector of length ncol(X).")
    }
  }

  ## beta prior
  if (is.null(b0)) b0 <- rep(0, p)
  if (length(b0) != p)
    stop("`b0` must have length ncol(X).")

  if (p == 0) {
    B0 <- matrix(0, 0, 0)
  } else {
    if (is.null(B0)) {
      B0 <- diag(1e6, p)
    } else {
      if (!is.matrix(B0)) B0 <- as.matrix(B0)
      if (nrow(B0) != p || ncol(B0) != p)
        stop("`B0` must be p x p.")
    }
  }

  ## sigma2_init validation
  if (!is.null(sigma2_init)) {
    if (!is.numeric(sigma2_init) ||
        length(sigma2_init) != 1L ||
        !is.finite(sigma2_init) ||
        sigma2_init <= 0) {
      stop("`sigma2_init` must be a positive scalar.")
    }
  }

  ## ---- Helpers -----------------------------------------------------------
  .components_from_A <- function(A) {

    ## Force general sparse form
    if (!inherits(A, "dgCMatrix")) {
      A <- as(A, "generalMatrix")
      A <- as(A, "dgCMatrix")
    }

    n <- nrow(A)

    ## Extract nonzero structure safely
    nz <- Matrix::summary(Matrix::drop0(A))
    adj <- vector("list", n)

    if (nrow(nz) > 0) {
      for (k in seq_len(nrow(nz))) {
        i <- nz$i[k]
        j <- nz$j[k]
        if (i != j) {
          adj[[i]] <- c(adj[[i]], j)
          adj[[j]] <- c(adj[[j]], i)
        }
      }
    }

    ## Connected components via DFS
    comp <- integer(n)
    cid <- 0L

    for (s in seq_len(n)) {
      if (comp[s] != 0L) next
      cid <- cid + 1L
      stack <- s
      comp[s] <- cid

      while (length(stack) > 0) {
        v <- stack[[length(stack)]]
        stack <- stack[-length(stack)]
        for (u in adj[[v]]) {
          if (comp[u] == 0L) {
            comp[u] <- cid
            stack <- c(stack, u)
          }
        }
      }
    }

    comp
  }



  .center_by_component <- function(x, comp) {
    x2 <- x
    for (k in unique(comp)) {
      idx <- which(comp == k)
      if (length(idx) > 0) {
        x2[idx] <- x2[idx] - mean(x2[idx])
      }
    }
    x2
  }

  ## ---- Initialization ----------------------------------------------------
  x <- if (is.null(x_init)) rep(0, n) else as.double(x_init)
  beta <- if (p == 0) numeric(0) else if (is.null(beta_init)) as.double(b0) else as.double(beta_init)

  sigma2 <- if (is.null(sigma2_init)) {
    s2 <- stats::var(y)
    if (!is.finite(s2) || s2 <= 0) 1 else s2
  } else {
    as.double(sigma2_init)
  }

  comp <- NULL
  if (type == "icar" && isTRUE(center_icar)) {
    comp <- .components_from_A(A)
  }

  ## ---- Storage -----------------------------------------------------------
  keep_idx <- seq.int(from = burn_in + 1L, to = n_iter, by = thin)
  n_keep <- length(keep_idx)

  x_draws <- matrix(NA_real_, n_keep, n)
  beta_draws <- matrix(NA_real_, n_keep, p)
  sigma2_draws <- numeric(n_keep)

  ## ---- Precision ---------------------------------------------------------
  Q_prior <- car_precision(A = A, type = type, rho = rho, tau = tau)
  if (!inherits(Q_prior, "Matrix")) {
    Q_prior <- Matrix::Matrix(Q_prior, sparse = TRUE)
  }
  I_n <- Matrix::Diagonal(n)

  ## ---- Gibbs sampler -----------------------------------------------------
  keep_pos <- 0L
  for (iter in seq_len(n_iter)) {

    r <- y
    if (p > 0) r <- r - drop(X %*% beta)

    Q_post <- Q_prior + (1 / sigma2) * I_n
    b_vec  <- (1 / sigma2) * r

    x <- rmvnorm_prec(Q_post, b = b_vec)

    if (type == "icar" && isTRUE(center_icar)) {
      x <- .center_by_component(x, comp)
    }

    if (p > 0) {
      beta <- update_beta_gaussian(y, X, x, sigma2, b0, B0)
    }

    sigma2 <- update_sigma2_ig(y, X, beta, x, a0, b0_sigma)

    if (iter %in% keep_idx) {
      keep_pos <- keep_pos + 1L
      x_draws[keep_pos, ] <- x
      if (p > 0) beta_draws[keep_pos, ] <- beta
      sigma2_draws[keep_pos] <- sigma2
    }
  }

  out <- list(
    type = type,
    rho = rho,
    tau = tau,
    n = n,
    p = p,
    keep = keep_idx,
    draws = list(
      x = x_draws,
      beta = beta_draws,
      sigma2 = sigma2_draws
    )
  )
  class(out) <- "trafficCAR_fit"
  out
}


#' @export
print.trafficCAR_fit <- function(x, ...) {
  cat("trafficCAR fit\n")
  cat("  type  :", x$type, "\n")
  cat("  n, p  :", x$n, ",", x$p, "\n")
  cat("  iter  :", x$n_iter, " (burn-in", x$burn_in, ", thin", x$thin, ")\n")
  cat("  kept  :", length(x$keep), "\n")
  invisible(x)
}
