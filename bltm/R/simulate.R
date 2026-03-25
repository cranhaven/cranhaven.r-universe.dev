#' Simulate LTM model
#'
#' Simulate LTM model using many
#'
#' @param ns number of times
#' @param nk number of covariates
#' @param ni number of series
#' @param vmu vector mu
#' @param mPhi phi diagonal matrix with the parameters
#' @param mSigs sigma eta vector
#' @param dsig general sigma
#' @param vd threshold parameter
#' @param alpha intercept
#'
#' @return List containing the generated y, x, beta and thresholded beta.
#'
#' @references
#' Nakajima, Jouchi, and Mike West. "Bayesian analysis of latent threshold
#' dynamic models." Journal of Business & Economic
#' Statistics 31.2 (2013): 151-164.
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
#' str(d_sim)
#'
#' @export
ltm_sim <- function(ns, nk, ni, vmu, mPhi, mSigs, dsig, vd, alpha) {
  # variavel preditora
  mx <- array(stats::runif(ns*nk*ni)-.5, dim = c(ni, ns, nk))
  # betas variando no tempo
  mb <- matrix(0, nrow = ns, ncol = nk)
  # valor do primeiro beta
  mb[1,] <- t(vmu + (chol(solve(diag(nk) - mPhi^2)) * mSigs) %*% stats::rnorm(nk))
  # equação do beta
  for (i in seq_len(ns-1)) {
    mb[i+1,] <- t(vmu + mPhi %*% (t(mb[i,,drop=F]) - vmu) + mSigs * stats::rnorm(nk))
  }
  # beta zerado
  mb_zerado <- mb * t(apply(abs(mb), 1, function(x) x > vd))
  # resposta
  xbs <- t(apply(mx, 1, function(z) Rfast::rowsums(z * mb_zerado)))
  vy <- alpha + xbs + dsig * stats::rnorm(ns*ni)

  # resultados
  list(vy = vy, mx = mx, mb = mb, mb_zerado = mb_zerado)
}


ltm_pred <- function(post, newdata) {
  n_iter <- nrow(post)
  nm <- colnames(post)
  ni <- sum(grepl("alpha", nm))
  nk <- sum(grepl("phi", nm))
  nt <- dim(newdata)[2]
  soma <- array(0, dim = c(ni, nt))
  for(iter in seq_len(n_iter)) {
    # recuperando parametros
    post_i <- post[iter, ]
    mb <- matrix(post_i[grepl("beta", nm)], ncol = nk)
    mPhi <- post_i[grepl("phi", nm)] * diag(nk)
    mSigs <- post_i[grepl("sig_eta", nm)]
    vd <- post_i[grepl("d\\[", nm)]
    dsig <- post_i[grepl("sig\\[", nm)]
    alpha <- post_i[grepl("alpha\\[", nm)]
    vmu <- post_i[grepl("mu\\[", nm)]

    # betas variando no tempo
    mb_new <- matrix(0, nrow = nt, ncol = nk)
    # valor do primeiro beta
    mb_new[1,] <- t(vmu + (chol(solve(diag(nk) - mPhi^2)) * mSigs) %*% stats::rnorm(nk))
    # equacao do beta
    for (i in seq_len(nt-1)) {
      mb_new[1,] <- t(vmu + (chol(solve(diag(nk) - mPhi^2)) * mSigs) %*% stats::rnorm(nk))
    }
    # beta zerado
    mb_zerado <- mb_new * t(apply(abs(mb_new), 1, function(x) x > vd))
    # resposta
    xbs <- t(apply(newdata, 1, function(z) Rfast::rowsums(z * mb_zerado)))
    vy <- alpha + xbs + dsig * stats::rnorm(nt*ni)
    soma <- soma + vy
  }
  soma / n_iter
}

ltm_pred_iter <- function(iter, post, nm, ni, nk, newdata) {

  # recuperando parametros
  post_i <- post[iter, ]
  mb <- matrix(post_i[grepl("beta", nm)], ncol = nk)
  mPhi <- post_i[grepl("phi", nm)] * diag(nk)
  mSigs <- post_i[grepl("sig_eta", nm)]
  vd <- post_i[grepl("d\\[", nm)]
  dsig <- post_i[grepl("sig\\[", nm)]
  alpha <- post_i[grepl("alpha\\[", nm)]
  vmu <- post_i[grepl("mu\\[", nm)]

  ns_future <- dim(newdata)[2]
  # betas variando no tempo
  mb_new <- matrix(0, nrow = ns_future, ncol = nk)
  # valor do primeiro beta
  mb_new[1,] <- t(vmu + (chol(solve(diag(nk) - mPhi^2)) * mSigs) %*% stats::rnorm(nk))
  # equação do beta
  for (i in seq_len(ns_future-1)) {
    mb_new[i+1,] <- t(vmu + mPhi %*% (t(mb_new[i,,drop=F]) - vmu) + mSigs * stats::rnorm(nk))
  }
  # beta zerado
  mb_zerado <- mb_new * t(apply(abs(mb_new), 1, function(x) x > vd))
  # resposta
  xbs <- t(apply(newdata, 1, function(z) Rfast::rowsums(z * mb_zerado)))
  vy <- alpha + xbs + dsig * stats::rnorm(ns_future*ni)
  vy
}






