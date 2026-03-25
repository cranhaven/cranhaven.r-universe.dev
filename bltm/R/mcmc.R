to_matrix <- function(parm, type) {
  if (type == "phi") parm <- diag(parm)
  x <- matrix(parm, nrow = 1)
  if (type %in% c("alpha", "d", "sig_eta", "mu", "sig", "phi")) {
    names(x) <- paste0(rep(type, length(parm)), "[", seq_along(parm), "]")
  } else if (type == "beta") {
    names(x) <- sprintf("beta[%d,%d]",
                        rep(seq_len(nrow(parm)), ncol(parm)),
                        rep(seq_len(ncol(parm)), each = nrow(parm)))
  }
  x
}

#' Create the prior parameters.
#'
#' Define the priors parameters to be used with [ltm_mcmc()].
#'
#' Considering the following priors:
#'
#' * alpha ~ N(mu0, s0)
#' * sig2 ~ IG(n0/2, S0/2)
#' * sig_eta ~ IG(v0/2, V0/2)
#' * mu ~ N(m0, s0^2)
#' * (phi+1)/2 ~ Beta(a0, b0)
#'
#' @param a_mu0 mean of alpha normal distribution.
#' @param a_s0 standard deviation of alpha's normal distribution.
#' @param n0 sig2 inverse gamma shape parameter.
#' @param S0 sig2 inverse gamma location parameter.
#' @param v0 sig_eta inverse gamma shape parameter.
#' @param V0 sig_eta inverse gamma location parameter.
#' @param m0 mu normal's mean parameter.
#' @param s0 mu normals standard deviation.
#' @param a0 a0 beta's shape parameter.
#' @param b0 a0 beta's location parameter.
#'
#' @return List containing the hyperparameters used to fit the model.
#'   The default parameters are the same of the simulation example of
#'   the paper.
#'
#' @references
#' Nakajima, Jouchi, and Mike West. "Bayesian analysis of latent threshold
#' dynamic models." Journal of Business & Economic
#' Statistics 31.2 (2013): 151-164.
#'
#' @export
create_prior_parameters <- function(a_mu0 = 0, a_s0 = .1,
                                    # sig2 ~ IG(n0/2, S0/2)
                                    n0 = 6, S0 = 0.06,
                                    # sig_eta ~ IG(v0/2, V0/2)
                                    v0 = 6, V0 = 0.06,
                                    # mu ~ N(m0, s0^2)
                                    m0 = 0, s0 = 1,
                                    # (phi+1)/2 ~ Beta(a0, b0)
                                    a0 = 20, b0 = 1.5) {
  list(
    a_mu0 = a_mu0,
    a_s0 = a_s0,
    n0 = n0,
    S0 = S0,
    v0 = v0,
    V0 = V0,
    m0 = 0,
    s0 = 1,
    a0 = 20,
    b0 = 1.5
  )
}

#' MCMC LTM
#'
#' Given `x` and `y` performs the MCMC optimization.
#'
#' @param x data points
#' @param y response variable
#' @param burnin number of burnin iterations
#' @param iter number of iterations after burnin
#' @param K parameter K
#' @param prior_par List of parameters for prior distrributions.
#'    See [create_prior_parameters()].
#'
#' @references
#' Nakajima, Jouchi, and Mike West. "Bayesian analysis of latent threshold
#' dynamic models." Journal of Business & Economic
#' Statistics 31.2 (2013): 151-164.
#'
#' @return matrix containing the posterior samples. Each line is one
#'   sample after the burnin period and each column is one of the
#'   parameters of the model. Columns are named to find the parameters
#'   with ease.
#'
#' @examples
#' # Generates 10 series, each one with 500 observations and 2 regressors.
#'
#' d_sim <- ltm_sim(
#'   ns = 500, nk = 2, ni = 10,
#'   vmu = matrix(c(.5,.5), nrow = 2),
#'   mPhi = diag(2) * c(.99, .99),
#'   mSigs = c(.1,.1),
#'   dsig = .15,
#'   vd = matrix(c(.4,.4), nrow = 2),
#'   alpha = 0
#' )
#'
#' # Fit model
#'
#' fit_model <- ltm_mcmc(d_sim$mx, d_sim$vy, burnin = 0, iter = 2)
#'
#' @export
ltm_mcmc <- function(x, y, burnin = 2000, iter = 8000, K = 3, prior_par = create_prior_parameters()) {

  # variables -----
  if (length(dim(x)) == 2) x <- array(x, c(1, dim(x)[1], dim(x)[2]))
  if (is.null(dim(y))) y <- matrix(y, nrow = 1)

  ni <- dim(x)[1] # number of series
  ns <- dim(x)[2] # number of sample
  nk <- dim(x)[3] # number of variables

  # initial values
  dsig <- 0.1
  mSigs <- rep(0.01, nk)
  vmu <- matrix(0, nrow = nk)
  mPhi <- 0.9 * diag(nk)
  vd <- matrix(0, nk)
  betas <- matrix(.1, ncol = nk, nrow = ns)
  mu <- matrix(0, nrow = nk)
  alpha <- matrix(0, nrow = ni)

  saida <- mapply(
    to_matrix,
    list(alpha, dsig, mPhi, mu, vd, mSigs, betas),
    c("alpha", "sig", "phi", "mu", "d", "sig_eta", "beta")
  )
  nm <- unlist(lapply(saida, names))
  post <- do.call(cbind, saida)

  # priors -----

  # alpha ~ N(mu0, s0)
  a_mu0 <- prior_par$a_mu0; a_s0 <- prior_par$a_s0
  # sig2 ~ IG(n0/2, S0/2)
  n0 <- prior_par$n0; S0 <- prior_par$S0
  # sig_eta ~ IG(v0/2, V0/2)
  v0 <- prior_par$v0; V0 <- prior_par$V0
  # mu ~ N(m0, s0^2)
  m0 <- prior_par$m0; s0 <- prior_par$s0
  # (phi+1)/2 ~ Beta(a0, b0)
  a0 <- prior_par$a0; b0 <- prior_par$b0
  # d < |mu| + K * v

  # mcmc loop ------

  total <- burnin + iter
  iters <- unique(c(1, floor(total / 10 * 1:10)) - burnin)

  for (j in seq(-burnin+1, iter)) {

    if (j %in% iters) {
      p <- as.integer(floor((j + burnin) / total * 100))
      type <- ifelse(j >= 0, "Sampling", "Warmup")
      cat(sprintf("Iteration: % 5d / % 5d [% 3d%%]  (%s)\n",
                  as.integer(j+burnin), as.integer(total), p, type),
          sep = "")
    }

    # betas
    for (t in 1:ns) {
      betas[t,] <- sample_beta_t(
        t, betas[t,], vd, dsig, matrix(x[,t,], nrow = ni), y[,t],
        betas[t-(t!=1),], betas[t+(t!=ns),],
        mu, ns, mPhi, mSigs
      )
    }

    # sigma_eta, phi, mu
    for (i in 1:nk) {
      mSigs[i] <- sample_sig_eta(v0, V0, ns, betas[,i], mu[i,], mPhi[i,i], vd[i,], K)
      mPhi[i,i] <- sample_phi(betas[,i], mu[i,], ns, mSigs[i], vd[i,], a0, b0, mPhi[i,i], K)
      mu[i,] <- sample_mu(mPhi[i,i], mSigs[i], betas[,i], vd[i,], s0, ns, m0, K)
    }

    # sig, threshold
    dsig <- sample_sig(n0, S0, ns, betas[-(ns+1),], y, x)
    alpha <- sample_alpha(a_mu0, a_s0, betas[-(ns+1),], dsig, nk, ni, y, x)
    # print(alpha)

    vd <- sample_d(mu, K, mSigs, mPhi, vd, x, y, betas, dsig)

    # store values
    if (j > 0) {
      saida <- mapply(
        to_matrix,
        list(alpha, dsig, mPhi, mu, vd, mSigs, betas),
        c("alpha", "sig", "phi", "mu", "d", "sig_eta", "beta")
      )
      m_new <- do.call(cbind, saida)
      post <- rbind(post, m_new)
    }

    # if (j %% 10 == 0) {
    #   m <- "mu=%.2f, phi=%.2f, d=%.2f, sig_eta=%.2f, sig=%.2f"
    #   message(sprintf(m, mu[1,1], mPhi[1,1], vd[1,1], mSigs[1], dsig))
    # }
  }
  post <- post[-1,]
  colnames(post) <- nm
  post
}



