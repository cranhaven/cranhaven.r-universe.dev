#' Hypothesis testing for risk prices (Bayesian p-values) with Dirac spike-and-slab prior
#'
#' @description This function tests the null hypothesis, \eqn{H_0: \lambda = \lambda_0}, when \eqn{\gamma=0}.
#' When \eqn{\lambda_0 = 0}, we compare factor models using the algorithm in Proposition 1 of \insertCite{bryzgalova2023bayesian;textual}{BayesianFactorZoo}.
#' When \eqn{\lambda_0 \neq 0}, this function corresponds to Corollary 2 in Section II.A.2 of \insertCite{bryzgalova2023bayesian;textual}{BayesianFactorZoo}.
#' The function can also be used to compute the posterior probabilities of all possible models with up to a
#' given maximum number of factors (see examples).
#'
#' @param f A matrix of factors with dimension \eqn{t \times k}, where \eqn{k} is the number of factors
#'        and \eqn{t} is the number of periods;
#' @param R A matrix of test assets with dimension \eqn{t \times N}, where \eqn{t} is the number of periods
#'        and \eqn{N} is the number of test assets;
#' @param sim_length The length of Monte-Carlo simulations;
#' @param max_k The maximal number of factors in models (\code{max_k} is a positive integer or \code{NULL} if the user does not impose any restriction on the model dimension).
#' @param psi0 The hyper-parameter in the prior distribution of risk price \eqn{\lambda} (see \bold{Details});
#' @param lambda0 A \eqn{k \times 1} vector of risk prices under the null hypothesis (\eqn{\gamma=0});
#'
#' @details
#'
#' Let \eqn{D} denote a diagonal matrix with elements \eqn{c, \psi_1^{-1},..., \psi_K^{-1}}, and \eqn{D_\gamma} the submatrix of \eqn{D}
#' corresponding to model \eqn{\gamma}, where \eqn{c} is a small positive number corresponding to the common cross-sectional intercept
#' (\eqn{\lambda_c}). The prior for the prices of risk (\eqn{\lambda_\gamma}) of model \eqn{\gamma} is then
#' \deqn{ \lambda_\gamma | \sigma^2, \gamma \sim N (0, \sigma^2, D_{\gamma}^{-1}). }
#'
#' We choose
#' \eqn{ \psi_j =  \psi \tilde{\rho}_j^\top \tilde{\rho}_j  }, where \eqn{ \tilde{\rho}_j = \rho_j - (\frac{1}{N} \Sigma_{i=1}^{N} \rho_{j,i}  ) \times 1_N } is the cross-sectionally
#' demeaned vector of factor \eqn{j}'s correlations with asset returns. In the codes, \eqn{\psi} is equal to the value of \code{psi0}.
#'
#' @references
#' \insertRef{bryzgalova2023bayesian}{BayesianFactorZoo}
#'
#'
#' @return
#' The return of \code{dirac_ss_sdf_pvalue} is a list of the following elements:
#' \itemize{
#'   \item \code{gamma_path}: A \code{sim_length}\eqn{\times k} matrix of the posterior draws of \eqn{\gamma}. Each row represents
#'   a draw. If \eqn{\gamma_j = 1} in one draw, factor \eqn{j} is included in the model in this draw and vice verse.
#'   \item \code{lambda_path}: A \code{sim_length}\eqn{\times (k+1)} matrix of the risk prices \eqn{\lambda}. Each row represents
#'   a draw. Note that the first column is \eqn{\lambda_c} corresponding to the constant term. The next \eqn{k} columns (i.e., the 2-th -- \eqn{(k+1)}-th columns) are the risk prices of the \eqn{k} factors;
#'   \item \code{model_probs}: A \eqn{2^k \times (k+1)} matrix of posterior model probabilities, where the first k columns are the model indices and the final column is a vector of model probabilities.
#' }
#'
#'
#'
#' @export
#'
#' @examples
#'
#' ## <-------------------------------------------------------------------------------->
#' ## Example: Bayesian p-value (with the dirac spike-and-slab prior)
#' ## <-------------------------------------------------------------------------------->
#'
#' # Load the example data
#' data("BFactor_zoo_example")
#' HML <- BFactor_zoo_example$HML
#' lambda_ols <- BFactor_zoo_example$lambda_ols
#' R2.ols.true <- BFactor_zoo_example$R2.ols.true
#' sim_f <- BFactor_zoo_example$sim_f
#' sim_R <- BFactor_zoo_example$sim_R
#' uf <- BFactor_zoo_example$uf
#'
#' ### Now we estimate the Bayesian p-values defined in Corollary 2.
#'
#' #
#' ### Prior Sharpe ratio of factor model for different values of psi: see equation (27):
#' #
#' cat("--------------- Choose psi based on prior Sharpe ratio ----------------\n")
#' cat("if psi = 1, prior Sharpe ratio is", psi_to_priorSR(sim_R, sim_f, psi0=1), "\n")
#' cat("if psi = 2, prior Sharpe ratio is", psi_to_priorSR(sim_R, sim_f, psi0=2), "\n")
#' cat("if psi = 5, prior Sharpe ratio is", psi_to_priorSR(sim_R, sim_f, psi0=5), "\n")
#'
#' ## Test whether factors' risk prices equal 'matrix(lambda_ols[2]*sd(HML),ncol=1)'
#' ## Bayesian p-value is given by mean(shrinkage$gamma_path)
#' shrinkage <- dirac_ss_sdf_pvalue(sim_f, sim_R, 1000, matrix(lambda_ols[2]*sd(HML),ncol=1))
#' cat("Null hypothesis: lambda =", matrix(lambda_ols[2]*sd(HML)), "\n")
#' cat("Posterior probability of rejecting the above null hypothesis is:",
#'     mean(shrinkage$gamma_path), "\n")
#'
#' ## Test whether the risk price of factor 'sim_f' is equal to 0
#' shrinkage <- dirac_ss_sdf_pvalue(sim_f, sim_R, 1000, 0, psi0=1)
#' cat("Null hypothesis: lambda =", 0, "\n")
#' cat("Posterior probability of rejecting the above null hypothesis is:",
#'     mean(shrinkage$gamma_path), "\n")
#'
#'
#' ## One can also put more than one factor into the test
#' two_f = cbind(sim_f,uf) # sim_f is the strong factor while uf is the useless factor
#' # Test1: lambda of sim_f = 0, Test2: lambda of uf = 0
#' lambda0_null_vec = t(cbind(0,0)) # 2x1 vector
#' shrinkage <- dirac_ss_sdf_pvalue(two_f, sim_R, 1000, lambda0_null_vec, psi0=1)
#' cat("Null hypothesis: lambda =", 0, "for each factor", "\n")
#' cat("Posterior probabilities of rejecting the above null hypothesis are:",
#'     colMeans(shrinkage$gamma_path), "\n")
#'
#' ## We can also print the posterior model probabilities:
#' cat('Posterior model probabilities are:\n')
#' print(shrinkage$model_probs)
#'
#'
#' ## One can compute the posterior probabilities of all possible models with up to
#' ## a given maximum number of factors. For example, we consider two factors, but
#' ## the number of factors is restricted to be less than two.
#' lambda0_null_vec = t(cbind(0,0)) # 2x1 vector
#' shrinkage <- dirac_ss_sdf_pvalue(two_f, sim_R, 1000, lambda0_null_vec, psi0=1, max_k=1)
#' cat('Posterior model probabilities are:\n')
#' print(shrinkage$model_probs)
#' ## Comment: You may notice that the model with index (1, 1) has a posterior probability
#' ##          of exactly zero since the maximal number of factors is one.
#'



dirac_ss_sdf_pvalue <- function(f, R, sim_length, lambda0, psi0 = 1, max_k=NULL) {

  ## f: a matrix of factors with dimension t times k, where k is the number of factors and t is the number of periods;
  ## R: a matrix of test assets with dimension t times N, where N is the number of test assets;
  ## sim_length: the length of MCMC;
  ## psi0: is a tune parameter that controls the shrinkage in our method;
  ## lambda0: is the k times 1 vector of risk prices under the null hypothesis gamma = 0.
  ## max_k: the maximal number of factors in models.
  ## Remark: We are not testing the null of lambda_c (the intercept) since we always include the
  ##         intercept. Also, an improper (flat) prior is used for the intercept.

  compute_models_probs <- function(model_draws, max_k) {

    ndraws <- dim(model_draws)[1]
    k <- dim(model_draws)[2]

    modelsets <- matrix(0, ncol = k, nrow = 1)  # the null model;
    for (l in 1:max_k) {
      modelsets <- rbind(modelsets, cbind(t(combn(k,l)), matrix(0, ncol = k-l, nrow = dim(t(combn(k,l)))[1])))
    }
    out_table <- matrix(0, nrow=dim(modelsets)[1], ncol=dim(modelsets)[2])
    for (jj in 1:dim(modelsets)[1]) {
      index <- modelsets[jj,]
      if (sum(index) > 0) {
        index <- index[index!=0]
        out_table[jj, index] <- 1
      }
    }
    models_probs <- rep(NA, dim(out_table)[1])
    for (jj in 1:dim(out_table)[1]) {
      index <- out_table[jj,]
      models_probs[jj] <- mean(rowSums(model_draws == matrix(1,ncol=1,nrow=ndraws) %*% index) == k)
    }
    return(cbind(out_table, models_probs))
  }


  k <- dim(f)[2]   # the number of factors
  t <- dim(f)[1]   # the number of time periods
  N <- dim(R)[2]   # the number of test assets
  p <- k + N       # the number of variables in Y(t)
  Y <- cbind(f, R)    # factors + tradable portfolios
  Sigma_ols <- cov(Y) # sample covariance matrix of Y(t)
  Corr_ols <- cor(Y)  # sample correlation matrix of Y(t)
  sd_ols <- colSds(Y) # sample standard deviations of Y(t)
  mu_ols <- matrix(colMeans(Y), ncol = 1)  # sample mean of Y(t);

  if (is.null(max_k) == TRUE) {
    max_k = k
  }

  # Check the prequisite condition
  check_input2(f,R);

  ## Matrices as outputs
  lambda_path <- matrix(0, ncol = (1+k), nrow = sim_length)
  gamma_path <- matrix(0, ncol = k, nrow = sim_length)

  ### Set the prior distribution for lambda_f
  rho <- cor(Y)[(k+1):p, 1:k, drop = FALSE]
  rho.demean <- rho - matrix(1, ncol = 1, nrow = N) %*% matrix(colMeans(rho), nrow = 1)
  #psi <- psi0 * diag(t(rho.demean)%*%rho.demean)
  if (k == 1) {
    psi <- psi0 * c(t(rho.demean)%*%rho.demean)
  } else {
    psi <- psi0 * diag(t(rho.demean)%*%rho.demean)
  }
  D <- diag(c(1/100000, 1/psi))

  ### Generate a set of candidate models
  modelsets <- matrix(0, ncol = k, nrow = 1)   # the null model with only the common intercept
  for (l in 1:max_k) {
    modelsets <- rbind(modelsets, cbind(t(combn(k,l)), matrix(0, ncol = k-l, nrow = dim(t(combn(k,l)))[1])))
  }
  # if (is.null(max_k) == FALSE) {
  #   subset_index <- (rowSums(modelsets > 0) <= max_k)
  #   modelsets <- modelsets[subset_index,]
  # }
  # subset_index <- (rowSums(modelsets > 0) <= max_k)
  # modelsets <- modelsets[subset_index,]
  model.num <- dim(modelsets)[1]   # the number of candidate models


  ### Start the MCMC:
  for (j in 1:sim_length) {
    #print(j)
    set.seed(j)

    ## (1) First-Stage: time-series regression
    Sigma <- riwish(v=t-1, S=t*Sigma_ols)   # draw the covariance matrix of Y(t)
    Var_mu_half <- chol(Sigma/t)
    mu <- mu_ols + t(Var_mu_half) %*% matrix(rnorm(p), ncol = 1)  # draw the mean of Y(t)
    sd_Y <- matrix(sqrt(diag(Sigma)), ncol=1)   # standard deviation of Y(t)
    corr_Y <- Sigma / (sd_Y%*%t(sd_Y))
    C_f <- corr_Y[(k+1):p, 1:k, drop=FALSE]   # corr[R(t), f(t)]
    a <- mu[(1+k):p,1,drop=FALSE] / sd_Y[(1+k):p]  # Sharpe ratio of test assets;


    ## (2) Second-Stage: cross-sectional regression
    beta_f <- C_f
    beta <- cbind(matrix(1,nrow = N, ncol = 1), C_f)
    log_prob <- rep(0, model.num)

    ## Calculate the (log) probability of each model: see Proposition 2 and Corollary 2
    for (i in 1:model.num) {
      if (i == 1) { # null model with only the common intercept
        H_i <- matrix(1, nrow = N, ncol = 1)
        D_i <- matrix(1/100000)
        a_gamma <- a - beta_f %*% lambda0
      } else {
        index <- modelsets[i,]
        index <- index[index!=0]
        H_i <- cbind(matrix(1,nrow = N, ncol = 1), beta_f[,index,drop=FALSE])
        D_i <- D[c(1,1+index), c(1,1+index)]
        if (length(index)==k) { # full model with all factors included
          a_gamma <- a
        } else {
          a_gamma <- a - beta_f[,-index,drop=FALSE] %*% lambda0[-index,,drop=FALSE]
        }
      }
      HH_D.inv <- chol2inv(chol(t(H_i)%*%H_i + D_i))
      lambda_i <- HH_D.inv %*% t(H_i)%*%a_gamma
      SSR_i <- t(a_gamma) %*% a_gamma - t(a_gamma)%*%H_i %*% HH_D.inv %*% t(H_i)%*%a_gamma
      SSR_i <- as.vector(SSR_i)
      log_prob_i <- 0.5 * log(det(D_i)/det(t(H_i)%*%H_i + D_i)) - 0.5*N*log(0.5*SSR_i)
      #print(SSR_i)
      #print(log_prob_i)
      log_prob[i] <- log_prob_i
      #print(a_gamma)
    }
    probs <- exp(log_prob)
    probs <- probs/sum(probs)
    #print(lambda_i)
    #print(probs)

    ## draw the model according to its posterior probability probs
    i <- sample(1:model.num, size = 1, prob = probs)
    if (i == 1) { # null model with only the common intercept
      index <- modelsets[i,]
      index <- index[index!=0]
      H_i <- matrix(1,nrow = N, ncol = 1)
      D_i <- matrix(1/100000)
      a_gamma <- a - beta_f %*% lambda0
    } else {
      index <- modelsets[i,]
      index <- index[index!=0]
      H_i <- cbind(matrix(1,nrow = N, ncol = 1), beta_f[,index,drop=FALSE])
      D_i <- D[c(1,1+index), c(1,1+index)]
      gamma_path[j,index] <- 1
      if (length(index)==k) { # full model with all factors included
        a_gamma <- a
      } else {
        a_gamma <- a - beta_f[,-index,drop=FALSE] %*% lambda0[-index,,drop=FALSE]
      }
    }

    ## draw sigma2 and lambda conditional on model index gamma: see equation (16) and (17)
    HH_D.inv <- chol2inv(chol(t(H_i)%*%H_i + D_i))
    Lambda_hat <- HH_D.inv %*% t(H_i)%*%a_gamma
    sigma2 <- rinvgamma(1, shape=(N/2), scale=t(a_gamma-H_i%*%Lambda_hat)%*%(a_gamma-H_i%*%Lambda_hat)/2)
    cov_Lambda <- sigma2 * HH_D.inv
    Lambda <- Lambda_hat + t(chol(cov_Lambda)) %*% matrix(rnorm(length(Lambda_hat)), ncol = 1)
    lambda_path[j, c(1,1+index)] <- as.vector(Lambda)
    unselected <- setdiff(1:k, index)
    lambda_path[j, 1+unselected] <- lambda0[unselected]
    #print(Lambda)
    #print(index)
  }

  return(list(lambda_path=lambda_path,
              gamma_path=gamma_path,
              model_probs=compute_models_probs(gamma_path, max_k)))
}

