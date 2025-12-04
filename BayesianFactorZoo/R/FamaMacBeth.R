######################################################
########## Fama-MacBeth Two-Pass Regression ##########
######################################################


#' Fama MacBeth Two-Pass Regression
#'
#' @description This function provides the frequentist Fama-MacBeth Two-Pass Regression.
#'
#' @param f A matrix of factors with dimension \eqn{t \times k}, where \eqn{k} is the number of factors
#'        and \eqn{t} is the number of periods;
#' @param R A matrix of test assets with dimension \eqn{t \times N}, where \eqn{t} is the number of periods
#'        and \eqn{N} is the number of test assets;
#'
#' @details
#'
#' See Chapter 12.2 in \insertCite{cochrane2009asset;textual}{BayesianFactorZoo}. \code{t_stat} and \code{t_stat_gls}
#' are t-statistics of OLS and GLS risk premia estimates based on the asymptotic standard errors in equation (12.19) in
#' \insertCite{cochrane2009asset;textual}{BayesianFactorZoo}.
#'
#' @references
#' \insertRef{cochrane2009asset}{BayesianFactorZoo}
#'
#'
#' @return
#' The return of \code{Two_Pass_Regression} is a list of the following elements:
#' \itemize{
#'     \item lambda: Risk premia estimates in the OLS two-pass regression;
#'     \item lambda_gls: Risk premia estimates in the GLS two-pass regression;
#'     \item t_stat: The t-statistics of risk premia estimates in the OLS two-pass regression;
#'     \item t_stat_gls: The t-statistics of risk premia estimates in the GLS two-pass regression;
#'     \item R2_adj: Adjusted \eqn{R2} in the OLS two-pass regression;
#'     \item R2_adj_GLS: Adjusted \eqn{R2} in the GLS two-pass regression.
#' }
#'
#' @export
#'


Two_Pass_Regression <- function(f, R) {
  # f: a matrix of factors with dimension t times k, where k is the number of factors and t is the number of periods;
  # R: a matrix of test assets with dimension t times N, where N is the number of test assets;

  t <- dim(R)[1]
  k <- dim(f)[2]
  N <- dim(R)[2]
  ET_f <- as.matrix(colMeans(f), ncol = 1, nrow = k)
  f <-  f - matrix(1, nrow = t, ncol = 1) %*% t(ET_f)    # demean the factors

  ### Step 1. Time-Series Regression
  X <- cbind(matrix(1, nrow=t, ncol=1), f)
  B <- t(R) %*% X %*% solve(t(X) %*% X)
  beta <- B[ ,2:(k+1), drop=FALSE]
  epsilon <- t(R) - B %*% t(X)   # store the error terms epsilon
  cov_epsilon <- epsilon %*% t(epsilon) / t   # estimate the variance-covariance matrix of epsilon under i.i.d assumption
  XX.inv <- solve((t(X)%*%X))
  cov_beta <- kronecker(cov_epsilon, XX.inv)


  ### Step 2. Cross-Sectional Regression (OLS)
  C <- cbind(matrix(1, nrow=N, ncol=1), beta)
  mu <- t(R) %*% matrix(1, ncol = 1, nrow = t) / t
  lambda <- solve(t(C)%*%C) %*% t(C) %*% mu
  alpha <- mu - C %*% lambda   # calculate the pricing errors

  # From Cochrane (2010): we don't have the term Sigma_f in equation (12.19) since
  # all factors are demeaned.
  Omega_F <- matrix(0, nrow = (1+k), ncol = (1+k))
  Omega_F_inv <- matrix(0, nrow = (1+k), ncol = (1+k))
  Omega_F[2:(1+k), 2:(1+k)] <- cov(f)
  Omega_F_inv[2:(1+k), 2:(1+k)] <- solve(cov(f))
  cov_lambda <- (1/t) * (((solve(t(C)%*%C) %*% t(C) %*% cov_epsilon %*% C %*% solve(t(C)%*%C))
                          * as.vector(1 + t(lambda)%*%Omega_F_inv%*%lambda)) + Omega_F)
  tstat <- as.vector(lambda) / sqrt(diag(cov_lambda))
  y <- diag(N) - C %*% solve(t(C)%*%C) %*% t(C)
  cov_alpha <- (1/t) * y %*% cov_epsilon %*% y * as.vector(1 + t(lambda)%*%Omega_F_inv%*%lambda)
  t_alpha <- as.vector(alpha) / sqrt(diag(cov_alpha))


  ### Step 3. Cross-Sectional Regression (GLS)
  lambda_gls <- solve(t(C)%*% solve(cov_epsilon) %*% C) %*% t(C) %*% solve(cov_epsilon) %*% mu
  cov_lambda_gls <- (1/t) * ((solve(t(C)%*% solve(cov_epsilon) %*% C) *
                                as.vector(1 + t(lambda)%*%Omega_F_inv%*%lambda)) + Omega_F)
  tstat_gls <- as.vector(lambda_gls) / sqrt(diag(cov_lambda_gls))


  ### Reported Statistics

  ## (1) Adjusted R-Squared
  #R2 <- 1 - t(alpha) %*% alpha / ((N-1)*var(mu))
  R2 <- 1 - t(alpha) %*% alpha / (t(mu - mean(mu))%*%(mu - mean(mu)))
  R2_adj <- 1 - (1-R2) * (N-1) / (N-1-k)

  ## (2) GLS Adjusted R-Squared
  alpha_GLS <- mu - C %*% lambda_gls   # calculate the pricing errors
  #R2_GLS <- (1 - t(alpha_GLS-mean(alpha_GLS))%*%solve(cov_epsilon)%*%(alpha_GLS-mean(alpha_GLS)) /
  #             (t(mu - mean(mu))%*%solve(cov_epsilon)%*%(mu - mean(mu))))
  R2_GLS <- (1 - t(alpha_GLS)%*%solve(cov_epsilon)%*%alpha_GLS /
               (t(mu - mean(mu))%*%solve(cov_epsilon)%*%(mu - mean(mu))))
  # R2_GLS <- 1 - t(alpha_GLS)%*%solve(V)%*%alpha_GLS / (t(mu)%*%solve(V)%*%mu)
  R2_adj_GLS <- 1 - (1-R2_GLS) * (N-1) / (N-1-k)

  R2_GLS2 <- (1 - t(alpha)%*%solve(cov_epsilon)%*%alpha /
                (t(mu - mean(mu))%*%solve(cov_epsilon)%*%(mu - mean(mu))))
  R2_adj_GLS2 <- 1 - (1-R2_GLS2) * (N-1) / (N-1-k)


  ## (3) Cross-Section T-Squared Statistics of Shanken (1985):
  T_square <- t(alpha) %*% ginv(cov_alpha) %*% alpha

  ## (4) Quadratic q:
  q <- t(alpha) %*% ginv(y %*% cov_epsilon %*% y) %*% alpha

  reported_statistics <- list(lambda = lambda, t_stat = tstat, R2 = R2, R2_adj = R2_adj, R2_GLS = R2_GLS,
                              R2_adj_GLS = R2_adj_GLS, lambda_gls = lambda_gls, t_stat_gls = tstat_gls,
                              T_square = T_square, q = q, alpha = alpha, t_alpha = t_alpha,
                              beta=beta, cov_epsilon = cov_epsilon, cov_lambda = cov_lambda,
                              cov_lambda_gls = cov_lambda_gls, R2_GLS2 = R2_GLS2,
                              R2_adj_GLS2 = R2_adj_GLS2, cov_beta = cov_beta)
  return(reported_statistics)
}
