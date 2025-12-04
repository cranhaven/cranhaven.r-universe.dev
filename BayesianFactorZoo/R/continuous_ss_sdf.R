#' SDF model selection with continuous spike-and-slab prior
#'
#' @description This function provides the SDF model selection procedure using the continuous spike-and-slab prior.
#'              See Propositions 3 and 4 in \insertCite{bryzgalova2023bayesian;textual}{BayesianFactorZoo}.
#'
#' @param f A matrix of factors with dimension \eqn{t \times k}, where \eqn{k} is the number of factors
#'        and \eqn{t} is the number of periods;
#' @param R A matrix of test assets with dimension \eqn{t \times N}, where \eqn{t} is the number of periods
#'        and \eqn{N} is the number of test assets;
#' @param sim_length The length of monte-carlo simulations;
#' @param psi0 The hyper-parameter in the prior distribution of risk prices (see \bold{Details});
#' @param r The hyper-parameter related to the prior of risk prices (see \bold{Details});
#' @param aw The hyper-parameter related to the prior of \eqn{\gamma} (see \bold{Details});
#' @param bw The hyper-parameter related to the prior of \eqn{\gamma} (see \bold{Details});
#' @param type If \code{type = 'OLS'} (\code{type = 'GLS'}), the function returns Bayesian OLS (GLS) estimates of risk prices. The default is 'OLS'.
#' @param intercept If \code{intercept = TRUE} (\code{intercept = FALSE}), we include (exclude) the common intercept in the cross-sectional regression. The default is \code{intercept = TRUE}.
#'
#' @details
#'
#' To model the variable selection procedure, we introduce a vector of binary latent variables \eqn{\gamma^\top = (\gamma_0,\gamma_1,...,\gamma_K)},
#' where \eqn{\gamma_j \in \{0,1\} }. When \eqn{\gamma_j = 1}, factor \eqn{j} (with associated loadings \eqn{C_j}) should be included
#' in the model and vice verse.
#'
#' The continuous spike-and-slab prior of risk prices \eqn{\lambda} is
#' \deqn{ \lambda_j | \gamma_j, \sigma^2 \sim N (0, r(\gamma_j) \psi_j \sigma^2  ) .}
#' When the factor \eqn{j} is included, we have \eqn{ r(\gamma_j = 1)=1 }.
#' When the factor is excluded from the model, \eqn{ r(\gamma_j = 0) =r \ll 1 }.
#' Hence, the Dirac "spike" is replaced by a Gaussian spike, which is extremely concentrated at zero
#' (the default value for \eqn{r} is 0.001).
#' If \code{intercept = TRUE}, we choose \eqn{ \psi_j =  \psi \tilde{\rho}_j^\top \tilde{\rho}_j  },
#' where \eqn{ \tilde{\rho}_j = \rho_j - (\frac{1}{N} \Sigma_{i=1}^{N} \rho_{j,i}  ) \times 1_N }
#' is the cross-sectionally demeaned vector of factor \eqn{j}'s correlations with asset returns.
#' Instead, if \code{intercept = FALSE}, we choose \eqn{ \psi_j =  \psi \rho_j^\top \rho_j  }.
#' In the codes, \eqn{\psi} is equal to the value of \code{psi0}.
#'
#' The prior \eqn{\pi (\omega)} encoded the belief about the sparsity of the true model using the prior distribution
#' \eqn{\pi (\gamma_j = 1 | \omega_j) = \omega_j }. Following the literature on the variable selection, we set
#' \deqn{ \pi (\gamma_j = 1 | \omega_j) = \omega_j,  \ \ \omega_j \sim Beta(a_\omega, b_\omega) . }
#' Different hyperparameters \eqn{a_\omega} and \eqn{b_\omega} determine whether one a priori favors more parsimonious models or not.
#' We choose \eqn{a_\omega = 1} (\code{aw}) and \eqn{b_\omega=1} (\code{bw}) as the default values.
#'
#' For each posterior draw of factors' risk prices \eqn{\lambda^{(j)}_f}, we can define the SDF as
#' \eqn{m^{(j)}_t = 1 - (f_t - \mu_f)^\top \lambda^{(j)}_f}.The Bayesian model averaging of the SDF (BMA-SDF)
#' over \eqn{J} draws is
#' \deqn{m^{bma}_t = \frac{1}{J}  \sum^J_{j=1} m^{(j)}_t.}
#'
#' @references
#' \insertRef{bryzgalova2023bayesian}{BayesianFactorZoo}
#'
#'
#' @return
#' The return of \code{continuous_ss_sdf} is a list of the following elements:
#' \itemize{
#'   \item \code{gamma_path}: A \code{sim_length}\eqn{\times k} matrix of the posterior draws of \eqn{\gamma}. Each row represents
#'   a draw. If \eqn{\gamma_j = 1} in one draw, factor \eqn{j} is included in the model in this draw and vice verse.
#'   \item \code{lambda_path}: A \code{sim_length}\eqn{\times (k+1)} matrix of the risk prices \eqn{\lambda} if \code{intercept = TRUE}. Each row represents
#'   a draw. Note that the first column is \eqn{\lambda_c} corresponding to the constant term. The next \eqn{k} columns (i.e., the 2-th -- \eqn{(k+1)}-th columns)
#'   are the risk prices of the \eqn{k} factors. If \code{intercept = FALSE}, \code{lambda_path} is a \code{sim_length}\eqn{\times k} matrix of the risk prices,
#'   without the estimates of \eqn{\lambda_c}.
#'   \item \code{sdf_path}: A \code{sim_length}\eqn{\times t} matrix of posterior draws of SDFs. Each row represents a draw.
#'   \item \code{bma_sdf}: BMA-SDF.
#' }
#'
#' @importFrom MCMCpack rinvgamma
#'
#' @export
#'
#' @examples
#'
#' ## Load the example data
#' data("BFactor_zoo_example")
#' HML <- BFactor_zoo_example$HML
#' lambda_ols <- BFactor_zoo_example$lambda_ols
#' R2.ols.true <- BFactor_zoo_example$R2.ols.true
#' sim_f <- BFactor_zoo_example$sim_f
#' sim_R <- BFactor_zoo_example$sim_R
#' uf <- BFactor_zoo_example$uf
#'
#' ## sim_f: simulated strong factor
#' ## uf: simulated useless factor
#'
#' psi_hat <- psi_to_priorSR(sim_R, cbind(sim_f,uf), priorSR=0.1)
#' shrinkage <- continuous_ss_sdf(cbind(sim_f,uf), sim_R, 5000, psi0=psi_hat, r=0.001, aw=1, bw=1)
#' cat("Null hypothesis: lambda =", 0, "for each factor", "\n")
#' cat("Posterior probabilities of rejecting the above null hypotheses are:",
#'     colMeans(shrinkage$gamma_path), "\n")
#'
#' ## We also have the posterior draws of SDF: m(t) = 1 - lambda_g %*% (f(t) - mu_f)
#' sdf_path <- shrinkage$sdf_path
#'
#' ## We also provide the Bayesian model averaging of the SDF (BMA-SDF)
#' bma_sdf <- shrinkage$bma_sdf
#'


# data("BFactor_zoo_example")
# HML <- BFactor_zoo_example$HML
# lambda_ols <- BFactor_zoo_example$lambda_ols
# R2.ols.true <- BFactor_zoo_example$R2.ols.true
# sim_f <- BFactor_zoo_example$sim_f
# sim_R <- BFactor_zoo_example$sim_R
# uf <- BFactor_zoo_example$uf
#
# psi0 = 1
# r = 0.001
# aw = 1
# bw = 1
# sim_length = 2000
# type = "OLS"
# f <- sim_f
# R <- sim_R
# intercept <- FALSE
#
# bayes.est <- continuous_ss_sdf(sim_f, sim_R, sim_length=1000, psi0 = 1, r = 0.001,
#                                aw = 1, bw = 1, type = "GLS", intercept = FALSE)
# print(colMeans(bayes.est$gamma_path))
# print(colMeans(bayes.est$lambda_path))


continuous_ss_sdf <- function(f, R, sim_length, psi0 = 1, r = 0.001, aw = 1, bw = 1, type = "OLS", intercept = TRUE) {

  # f: matrix of factors with dimension t times k, where k is the number of factors and t is
  #    the number of periods.
  # R: matrix of test assets with dimension t times N, where N is the number of test assets;
  # sim_length: the length of MCMC;
  # psi0, r, aw, bw: hyper-parameters;

  k <- dim(f)[2]   # the number of factors
  t <- dim(f)[1]   # the number of time periods
  N <- dim(R)[2]   # the number of test assets
  p <- k + N       # the number of variables in Y(t)
  Y <- cbind(f, R)   # factors + tradable portfolios
  Sigma_ols <- cov(Y) # sample covariance matrix of Y(t)
  Corr_ols <- cor(Y)  # sample correlation matrix of Y(t)
  sd_ols <- colSds(Y) # sample standard deviations of Y(t)
  mu_ols <- matrix(colMeans(Y), ncol = 1)  # sample mean of Y(t);

  # Check the prequisite condition
  check_input2(f,R);

  ## Matrices as outputs
  if (intercept == FALSE) {
    lambda_path <- matrix(0, ncol = k, nrow = sim_length)
  } else {
    lambda_path <- matrix(0, ncol = (1+k), nrow = sim_length)
  }
  gamma_path <- matrix(0, ncol = k, nrow = sim_length)
  sdf_path <- matrix(0, ncol=t, nrow = sim_length)

  # Initialize some parameters:
  if (intercept == FALSE) {
    beta_ols <- Corr_ols[(k+1):p, 1:k, drop=FALSE]
  } else {
    beta_ols <- cbind(matrix(1, nrow = N, ncol = 1), Corr_ols[(k+1):p, 1:k])
  }
  a_ols <- mu_ols[(1+k):p,,drop=FALSE] / sd_ols[(k+1):p]
  Lambda_ols <- chol2inv(chol(t(beta_ols)%*%beta_ols)) %*% t(beta_ols) %*% a_ols
  omega <- rep(0.5, k)
  gamma <- rbinom(prob=omega, n = k, size = 1)
  sigma2 <- as.vector((1/N) * t(a_ols - beta_ols %*% Lambda_ols) %*% (a_ols - beta_ols %*% Lambda_ols))
  r_gamma <- ifelse(gamma==1, 1, r)


  ### Set the prior distribution for lambda_f
  rho <- cor(Y)[(k+1):p, 1:k, drop = FALSE]
  if (intercept == FALSE) {
    rho.demean <- rho
  } else {
    rho.demean <- rho - matrix(1, ncol = 1, nrow = N) %*% matrix(colMeans(rho), nrow = 1)
  }
  if (k == 1) {
    psi <- psi0 * c(t(rho.demean)%*%rho.demean)
  } else {
    psi <- psi0 * diag(t(rho.demean)%*%rho.demean)
  }


  ### Start the MCMC
  for (i in 1:sim_length) {

    #if (i %% 1000 == 0) {print(i)}
    set.seed(i)

    ## (1) First-Stage: time-series regression
    Sigma <- riwish(v=t-1, S=t*Sigma_ols)   # draw the covariance matrix of Y(t)
    Var_mu_half <- chol(Sigma/t)
    mu <- mu_ols + t(Var_mu_half) %*% matrix(rnorm(p), ncol = 1)  # draw the mean of Y(t)
    sd_Y <- matrix(sqrt(diag(Sigma)), ncol=1)   # standard deviation of Y(t)
    corr_Y <- Sigma / (sd_Y%*%t(sd_Y))
    C_f <- corr_Y[(k+1):p, 1:k]   # corr[R(t), f(t)]
    a <- mu[(1+k):p,1,drop=FALSE] / sd_Y[(1+k):p]  # Sharpe ratio of test assets;

    #### II. Second-Stage: cross-section regression (Gibbs Sampling)
    if (intercept == FALSE) {
      beta <- matrix(C_f, nrow = N)
    } else {
      beta <- cbind(matrix(1,nrow = N, ncol = 1), C_f)
    }
    corrR <- corr_Y[(k+1):p, (k+1):p]

    ## Step II.1. Draw lambda conditional on (data, sigma2, gamma, omega): equation (28)
    if (intercept == FALSE) {
      if (k == 1) {
        D <- matrix(1/(r_gamma*psi))
      } else {
        D <- diag(1/(r_gamma*psi))
      }
    } else {
      D <- diag(c(1/100000, 1/(r_gamma*psi)))
    }
    if (type=='OLS') {
      beta_D_inv <- chol2inv(chol(t(beta)%*%beta + D))
      cov_Lambda <- sigma2 * beta_D_inv
      Lambda_hat <- beta_D_inv %*% t(beta)%*%a
    }
    if (type=='GLS') {
      beta_D_inv <- chol2inv(chol(t(beta)%*%solve(corrR)%*%beta + D))
      cov_Lambda <- sigma2 * beta_D_inv
      Lambda_hat <- beta_D_inv %*% t(beta)%*%solve(corrR)%*%a
    }
    if (intercept == FALSE) {
      Lambda <- Lambda_hat + t(chol(cov_Lambda)) %*% matrix(rnorm(k), ncol = 1)
    } else {
      Lambda <- Lambda_hat + t(chol(cov_Lambda)) %*% matrix(rnorm(k+1), ncol = 1)
    }

    ## Step II.2. Draw gamma_j conditional on (data, Lambda, psi, sigma2, gamma_{-j}, omega):
    ##            See equation (29)
    if (intercept == FALSE) {
      log.odds <- log((omega/(1-omega))) + 0.5*log(r) + 0.5*c(Lambda)^2*(1/r-1)/(sigma2*psi)
    } else {
      log.odds <- log((omega/(1-omega))) + 0.5*log(r) + 0.5*Lambda[2:(k+1)]^2*(1/r-1)/(sigma2*psi)
    }
    odds <- exp(log.odds)
    odds <- ifelse(odds > 1000, 1000, odds)
    prob = odds / (1 + odds)
    gamma <- rbinom(prob=prob, n = k, size = 1)
    r_gamma <- ifelse(gamma==1, 1, r)
    gamma_path[i, ] <- gamma

    ## Step II.3. Draw omega: equation (30)
    omega <- rbeta(k, aw+gamma, bw+1-gamma)

    ## Step II.4. Draw sigma-squared: equation (31)
    if (type=='OLS') {
      if (intercept == FALSE) {
        sigma2 <- rinvgamma(1,shape=(N+k)/2,
                            scale=(t(a-beta%*%Lambda)%*%(a-beta%*%Lambda)+t(Lambda)%*%D%*%Lambda)/2)
      } else {
        sigma2 <- rinvgamma(1,shape=(N+k+1)/2,
                            scale=(t(a-beta%*%Lambda)%*%(a-beta%*%Lambda)+t(Lambda)%*%D%*%Lambda)/2)
      }
    }
    if (type=='GLS') {
      if (intercept == FALSE) {
        sigma2 <- rinvgamma(1,shape=(N+k)/2,
                            scale=(t(a-beta%*%Lambda)%*%solve(corrR)%*%(a-beta%*%Lambda)+t(Lambda)%*%D%*%Lambda)/2)
      } else {
        sigma2 <- rinvgamma(1,shape=(N+k+1)/2,
                            scale=(t(a-beta%*%Lambda)%*%solve(corrR)%*%(a-beta%*%Lambda)+t(Lambda)%*%D%*%Lambda)/2)
      }
    }
    lambda_path[i, ] <- as.vector(Lambda)
    if (intercept == FALSE) {
      Lambda_f <- Lambda/colSds(f)
    } else {
      Lambda_f <- Lambda[2:length(Lambda)]/colSds(f)
    }
    sdf_path[i,] <- as.vector(1 - f %*% Lambda_f)
    sdf_path[i,] <- 1 + sdf_path[i,] - mean(sdf_path[i,])   # normalize the SDF st it has a mean of one
  }

  return(list(gamma_path = gamma_path,
              lambda_path = lambda_path,
              sdf_path = sdf_path,
              bma_sdf = colMeans(sdf_path)))
}
