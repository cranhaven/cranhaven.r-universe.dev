#' GMM Estimates of Factors' Risk Prices under the Linear SDF Framework
#'
#' @description This function provides the GMM estimates of factors' risk prices under the linear SDF framework (including the common intercept).
#'
#' @param R A matrix of test assets with dimension \eqn{t \times N}, where \eqn{t} is the number of periods
#'        and \eqn{N} is the number of test assets;
#' @param f A matrix of factors with dimension \eqn{t \times k}, where \eqn{k} is the number of factors
#'        and \eqn{t} is the number of periods;
#' @param W Weighting matrix in GMM estimation (see \bold{Details}).
#'
#' @details
#'
#' We follow the notations in Section I of \insertCite{bryzgalova2023bayesian;textual}{BayesianFactorZoo}.
#' Suppose that there are \eqn{K} factors, \eqn{f_t = (f_{1t},...,f_{Kt})^\top, t=1,...,T}.
#' The returns of \eqn{N} test assets are denoted by \eqn{R_t = (R_{1t},...,R_{Nt})^\top}.
#'
#' Consider linear SDFs (\eqn{M}), that is, models of the form \eqn{M_t = 1- (f_t -E[f_t])^\top \lambda_f}.
#'
#' The model is estimated via GMM with moment conditions
#'
#' \deqn{E[g_t (\lambda_c, \lambda_f, \mu_f)] =E\left(\begin{array}{c} R_t - \lambda_c 1_N - R_t (f_t - \mu_f)^\top \lambda_f \\ f_t - \mu_f \end{array} \right) =\left(\begin{array}{c}  0_N \\ 0_K \end{array} \right)}
#' and the corresponding sample analog function \eqn{ g_T (\lambda_c, \lambda_f, \mu_f) = \frac{1}{T} \Sigma_{t=1}^T  g_t (\lambda_c, \lambda_f, \mu_f)}. Different weighting matrices deliver different point estimates. Two popular choices are
#' \deqn{	W_{ols}=\left(\begin{array}{cc}I_N & 0_{N \times K}  \\ 0_{K \times N} & \kappa I_K\end{array}\right), \ \ W_{gls}=\left(\begin{array}{cc} \Sigma_R^{-1} & 0_{N \times K}  \\ 0_{K \times N} & \kappa I_K\end{array}\right), }
#' where \eqn{\Sigma_R} is the covariance matrix of returns and \eqn{\kappa >0} is a large constant so that \eqn{\hat{\mu}_f = \frac{1}{T} \Sigma_{t=1}^{T} f_t }.
#'
#' The asymptotic covariance matrix of risk premia estimates, \code{Avar_hat}, is based on the assumption that
#' \eqn{g_t (\lambda_c, \lambda_f, \mu_f)} is independent over time.
#'
#' @references
#' \insertRef{bryzgalova2023bayesian}{BayesianFactorZoo}
#'
#'
#' @return
#' The return of \code{SDF_gmm} is a list of the following elements:
#' \itemize{
#'    \item \code{lambda_gmm}: Risk price estimates;
#'    \item \code{mu_f}: Sample means of factors;
#'    \item \code{Avar_hat}: Asymptotic covariance matrix of GMM estimates (see \bold{Details});
#'    \item \code{R2_adj}: Adjusted cross-sectional \eqn{R^2};
#'    \item \code{S_hat}: Spectral matrix.
#' }
#'
#' @export
#'


SDF_gmm <- function(R, f, W) {

  # f: matrix of factors with dimension t times k, where k is the number of factors and t is
  #    the number of periods;
  # R: matrix of test assets with dimension t times N, where N is the number of test assets;
  # W: weighting matrix in GMM estimation.

  T1 <- dim(R)[1]   # the sample size
  N <- dim(R)[2]    # the number of test assets
  K <- dim(f)[2]    # the number of factors
  C_f <- cov(R, f)  # covariance between test assets and factors
  one_N <- matrix(1, ncol=1, nrow=N)
  one_K <- matrix(1, ncol=1, nrow=K)
  one_T <- matrix(1, ncol=1, nrow=T1)
  C <- cbind(one_N, C_f)   # include a common intercept into regression
  mu_R <- matrix(colMeans(R), ncol=1)   # sample mean of test assets
  mu_f <- matrix(colMeans(f), ncol=1)   # sample mean of factors


  ## GMM estimates
  W1 <- W[1:N, 1:N]
  lambda_gmm <- solve(t(C)%*%W1%*%C) %*% t(C)%*%W1%*%mu_R
  lambda_c <- lambda_gmm[1]
  lambda_f <- lambda_gmm[2:(1+K),,drop=FALSE]   # price of risk, lambda_f


  ## Estimate the spectral matrix
  f_demean <- f - one_T %*% t(mu_f)
  moments <- matrix(0, ncol=N+K, nrow=T1)
  moments[1:T1, (1+N):(K+N)] <- f_demean
  # moments[1:T1, 1:N] <- (R - lambda_c*matrix(1,ncol=N,nrow=T1)
  #                        - diag(as.vector(f_demean%*%lambda_f)) %*% R)
  for (t in 1:T1) {
    R_t <- matrix(R[t,], ncol=1)
    f_t <- matrix(f[t,], ncol=1)
    moments[t, 1:N] <- t(R_t - lambda_c*one_N - R_t%*%t(f_t-mu_f)%*%lambda_f)
  }
  S_hat <- cov(moments)


  ## Estimate the asymptotic variance of GMM estimates
  G_hat <- matrix(0, ncol=2*K+1, nrow=N+K)
  G_hat[1:N, 1] <- -1
  G_hat[1:N, 2:(1+K)] <- -C_f
  G_hat[1:N, (K+2):(1+2*K)] <- mu_R %*% t(lambda_f)
  G_hat[(N+1):(N+K), (K+2):(1+2*K)] <- -diag(K)

  Avar_hat <- (1/T1)*(solve(t(G_hat)%*%W%*%G_hat) %*% t(G_hat)%*%W%*%S_hat%*%W%*%G_hat
                      %*% solve(t(G_hat)%*%W%*%G_hat))

  ## Cross-sectional R-Squared
  R2 <- (1 - t(mu_R - C%*%lambda_gmm) %*% W1 %*% (mu_R - C%*%lambda_gmm) /
           (t(mu_R - mean(mu_R))%*%W1%*%(mu_R - mean(mu_R))))
  R2_adj <- 1 - (1-R2) * (N-1) / (N-1-K)

  return(list(lambda_gmm = lambda_gmm,
              mu_f = mu_f,
              Avar_hat = Avar_hat,
              R2_adj = R2_adj,
              S_hat = S_hat))
}


