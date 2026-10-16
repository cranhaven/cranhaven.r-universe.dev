#' Draw from a multivariate normal with sparse precision
#'
#' Sample \deqn{x \sim N(Q^{-1} b, Q^{-1})} where \eqn{Q} is symmetric positive definite.
#'
#' @param Q Sparse symmetric positive definite precision matrix (dgCMatrix).
#' @param b Optional numeric vector. If NULL, mean is 0.
#'
#' @return Numeric vector x.
#' @keywords internal

rmvnorm_prec <- function(Q, b = NULL) {
  if (!inherits(Q, "Matrix")) stop("`Q` must be a Matrix object.")
  if (nrow(Q) != ncol(Q)) stop("`Q` must be square.")
  n <- nrow(Q)

  if (!is.null(b)) {
    if (!is.numeric(b) || length(b) != n) stop("`b` must be numeric length nrow(Q).")
  }

  # sparse Cholesky of SPD precision
  ch <- tryCatch(
    Matrix::Cholesky(Q, LDL = FALSE, perm = TRUE),
    error = function(e) stop("Cholesky failed (Q must be SPD): ", conditionMessage(e))
  )

  # mean: mu = Q^{-1} b
  mu <- if (is.null(b)) rep.int(0, n) else as.numeric(Matrix::solve(ch, b))

  # sample z ~ N(0, Q^{-1}).
  # if Q = P' L L' P, then Q^{-1} = P' (L')^{-1} L^{-1} P
  # draw u ~ N(0, I), set w = (L')^{-1} u, then z = P' w
  ex <- Matrix::expand(ch)
  L <- ex$L
  P <- ex$P

  u <- stats::rnorm(n)
  w <- Matrix::solve(Matrix::t(L), u)
  z <- as.numeric(Matrix::t(P) %*% w)

  mu + z
}


#' @keywords internal
gibbs_step_proper_car <- function(y, Q_base, x, tau, kappa,
                                  a_tau, b_tau, a_kappa, b_kappa) {
  n <- length(y)

  # x | y, tau, kappa ~ N( (tau I + kappa Q)^(-1) (tau y), (tau I + kappa Q)^(-1) )
  Q_post <- tau * Matrix::Diagonal(n) + kappa * Q_base
  b_post <- tau * y
  x_new <- rmvnorm_prec(Q_post, b_post)

  # tau | x, y ~ Gamma(a_tau + n/2, b_tau + 0.5 * ||y-x||^2)
  resid <- y - x_new
  tau_new <- stats::rgamma(
    1L,
    shape = a_tau + 0.5 * n,
    rate  = b_tau + 0.5 * sum(resid * resid)
  )

  # kappa | x ~ Gamma(a_kappa + n/2, b_kappa + 0.5 * x' Q x)
  Qx <- as.numeric(Q_base %*% x_new)
  quad <- as.numeric(crossprod(x_new, Qx))
  kappa_new <- stats::rgamma(
    1L,
    shape = a_kappa + 0.5 * n,
    rate  = b_kappa + 0.5 * quad
  )

  list(x = x_new, tau = tau_new, kappa = kappa_new)
}



#' Gibbs sampler for a proper CAR latent Gaussian model
#'
#' Model:
#' \deqn{y \mid x, \tau \sim N(x, \tau^{-1} I)}
#' \deqn{x \mid \kappa \sim N(0, (\kappa Q)^{-1}), \quad Q = D - \rho A \text{ (proper CAR)}}
#' \deqn{\tau \sim \mathrm{Gamma}(a_{\tau}, b_{\tau}) \quad \text{(shape-rate)}}
#' \deqn{\kappa \sim \mathrm{Gamma}(a_{\kappa}, b_{\kappa}) \quad \text{(shape-rate)}}
#'
#' @param y Numeric vector of observations (length n).
#' @param A Adjacency matrix (dense or sparse). Diagonal ignored.
#' @param rho Proper CAR dependence parameter (must satisfy car_precision checks).
#' @param n_iter Integer number of iterations.
#' @param burn Integer burn-in iterations to drop (default 0).
#' @param thin Integer thinning interval (default 1).
#' @param a_tau,b_tau Gamma(shape, rate) prior for tau.
#' @param a_kappa,b_kappa Gamma(shape, rate) prior for kappa.
#' @param init Optional list with elements x, tau, kappa.
#' @param symmetrize Passed to car_precision().
#' @param check Passed to car_precision().
#'
#' @return List with x (matrix), tau, kappa, and settings.
#' @export
sample_proper_car <- function(y, A, rho = 0.99,
                              n_iter,
                              burn = 0L, thin = 1L,
                              a_tau = 1, b_tau = 1,
                              a_kappa = 1, b_kappa = 1,
                              init = NULL,
                              symmetrize = FALSE,
                              check = TRUE) {
  if (!is.numeric(y)) stop("`y` must be numeric.")
  n <- length(y)

  if (!is.numeric(n_iter) || length(n_iter) != 1L || !is.finite(n_iter) || n_iter < 1) {
    stop("`n_iter` must be a single integer >= 1.")
  }
  n_iter <- as.integer(n_iter)

  if (!is.numeric(burn) || length(burn) != 1L || burn < 0 || burn >= n_iter) {
    stop("`burn` must be a single integer with 0 <= burn < n_iter.")
  }
  burn <- as.integer(burn)

  if (!is.numeric(thin) || length(thin) != 1L || thin < 1) {
    stop("`thin` must be a single integer >= 1.")
  }
  thin <- as.integer(thin)

  # Build base precision Q = D - rho A (tau multiplier handled in Gibbs via kappa)
  Q_base <- car_precision(A, type = "proper", rho = rho, tau = 1,
                          symmetrize = symmetrize, check = check)

  if (nrow(Q_base) != n) stop("Length of `y` must equal nrow(A)/nrow(Q).")

  # Fail fast on SPD requirement for sampling steps (proper CAR should be SPD if rho valid)
  tryCatch(
    Matrix::Cholesky(Q_base, LDL = FALSE, perm = TRUE),
    error = function(e) stop("Base proper-CAR precision is not SPD: ", conditionMessage(e))
  )

  # init
  x <- if (!is.null(init$x)) {
    if (!is.numeric(init$x) || length(init$x) != n) stop("init$x must be numeric length n.")
    as.numeric(init$x)
  } else {
    rep.int(0, n)
  }

  tau <- if (!is.null(init$tau)) as.numeric(init$tau) else 1
  kappa <- if (!is.null(init$kappa)) as.numeric(init$kappa) else 1
  if (!is.finite(tau) || tau <= 0) stop("init$tau must be > 0.")
  if (!is.finite(kappa) || kappa <= 0) stop("init$kappa must be > 0.")

  keep_idx <- seq.int(from = burn + 1L, to = n_iter, by = thin)
  n_save <- length(keep_idx)

  x_save <- matrix(NA_real_, nrow = n_save, ncol = n)
  tau_save <- numeric(n_save)
  kappa_save <- numeric(n_save)

  s <- 0L
  for (it in seq_len(n_iter)) {
    upd <- gibbs_step_proper_car(
      y = y, Q_base = Q_base, x = x, tau = tau, kappa = kappa,
      a_tau = a_tau, b_tau = b_tau, a_kappa = a_kappa, b_kappa = b_kappa
    )
    x <- upd$x
    tau <- upd$tau
    kappa <- upd$kappa

    if (it %in% keep_idx) {
      s <- s + 1L
      x_save[s, ] <- x
      tau_save[s] <- tau
      kappa_save[s] <- kappa
    }
  }

  list(
    x = x_save,
    tau = tau_save,
    kappa = kappa_save,
    settings = list(
      n_iter = n_iter, burn = burn, thin = thin,
      rho = rho,
      a_tau = a_tau, b_tau = b_tau,
      a_kappa = a_kappa, b_kappa = b_kappa,
      symmetrize = symmetrize, check = check
    )
  )
}

