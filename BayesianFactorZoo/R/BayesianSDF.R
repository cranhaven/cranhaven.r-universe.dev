#' Bayesian estimation of Linear SDF (B-SDF)
#'
#' @description This function provides the Bayesian estimates of factors' risk prices.
#' The estimates with the flat prior are given by Definitions 1 and 2 in
#' \insertCite{bryzgalova2023bayesian;textual}{BayesianFactorZoo}.
#' The estimates with the normal prior are used in Table I (see the footnote of Table I).
#'
#' @param f A \eqn{t \times k} matrix of factors, where \eqn{k} is the number of factors
#'        and \eqn{t} is the number of periods
#' @param R A \eqn{t \times N} matrix of test assets, where \eqn{t} is the number of periods
#'        and \eqn{N} is the number of test assets
#' @param sim_length The length of MCMCs
#' @param intercept If \code{intercept = TRUE} (\code{intercept = FALSE}), the model includes (does not include) the intercept.
#'        The default is \code{intercept = TRUE}
#' @param type If \code{type = 'OLS'} (\code{type = 'GLS'}), the function returns Bayesian OLS (GLS) estimates of risk prices \eqn{\lambda}. The default is 'OLS'
#' @param prior If \code{type = 'Flat'} (\code{type = 'Normal'}), the function executes the Bayesian estimation with the flat prior (normal prior).
#' The default is 'Flat'
#' @param psi0 The hyper-parameter of the prior distribution of risk prices \eqn{\lambda} used in the normal prior (see \bold{Details}).
#' This parameter is needed only when the user chooses  the normal prior. The default value is 5
#' @param d The hyper-parameter of the prior distribution of risk prices \eqn{\lambda} used in the normal prior (see \bold{Details}). The default value is 0.5
#'
#' @details
#'
#' \strong{Intercept}
#'
#' Consider the cross-sectional step. If one includes the intercept, the model is
#' \deqn{\mu_R = \lambda_c 1_N + C_f \lambda_f = C \lambda,}
#' where \eqn{C = (1_N, C_f)} and \eqn{\lambda^\top = (\lambda_c^\top, \lambda_f^\top)^\top }.
#' If one doesn't include the intercept, the model is
#' \deqn{\mu_R = C_f \lambda_f = C \lambda,}
#' where \eqn{C = C_f} and \eqn{\lambda = \lambda_f}.
#'
#' \strong{Bayesian Estimation}
#'
#' Let \eqn{Y_t = f_t \cup R_t}. Conditional on the data \eqn{Y = \{Y_t\}_{t=1}^T}, we can draw \eqn{\mu_{Y}} and \eqn{\Sigma_{Y}} from the Normal-inverse-Wishart system
#' \deqn{\mu_Y | \Sigma_Y, Y \sim N (\hat{\mu}_Y , \Sigma_Y / T) , }
#' \deqn{\Sigma_Y | Y \sim W^{-1} (T-1, \Sigma_{t=1}^{T} (Y_t - \hat{\mu}_Y ) ( Y_t - \hat{\mu}_Y )^\top   ) , }
#' where \eqn{W^{-1}} is the inverse-Wishart distribution.
#' We do not standardize \eqn{Y_t} in the time-series regression.
#' In the empirical implementation, after obtaining posterior draws for \eqn{\mu_{Y}} and \eqn{\Sigma_{Y}},
#' we calculate \eqn{\mu_R} and \eqn{C_f} as the standardized expected returns of test assets and correlation
#' between test assets and factors. It follows that \eqn{C} is a matrix containing a vector of ones and \eqn{C_f}.
#'
#' The prior distribution of risk prices is either the flat prior or the normal prior.
#'
#'
#' With \code{prior = 'Flat'} and \code{type = 'OLS'}, for each draw, the risk price estimate is
#' \deqn{\hat{\lambda} = (C^{\top} C)^{-1}C^{T} \mu_{R}  .}
#'
#' With \code{prior = 'Flat'} and \code{type = 'GLS'}, for each draw, the risk price estimate is
#' \deqn{\hat{\lambda} = (C^{\top} \Sigma^{-1}_{R} C)^{-1} C^{\top} \Sigma^{-1}_{R} \mu_{R} }
#'
#' If one chooses \code{prior = 'Normal'}, the prior of factor \eqn{j}'s risk price is
#' \deqn{ \lambda_j | \sigma^2 \sim N(0, \sigma^2 \psi \tilde{\rho}_j^\top \tilde{\rho}_j  T^d  ) ,}
#' where \eqn{ \tilde{\rho}_j = \rho_j - (\frac{1}{N} \Sigma_{i=1}^{N} \rho_{j,i}  ) \times 1_N } is the cross-sectionally
#' demeaned vector of factor \eqn{j}'s correlations with asset returns. Equivalently,
#' \deqn{ \lambda | \sigma^2 \sim N(0, \sigma^2 D^{-1}) ,}
#' \deqn{D = diag \{ (\psi \tilde{\rho}_1^\top \tilde{\rho}_1  T^d)^{-1}, ..., (\psi \tilde{\rho}_k^\top \tilde{\rho}_k  T^d)^{-1} \} \ \ without \ intercept;}
#' \deqn{D = diag \{ c, (\psi \tilde{\rho}_1^\top \tilde{\rho}_1  T^d)^{-1}, ..., (\psi \tilde{\rho}_k^\top \tilde{\rho}_k  T^d)^{-1} \} \ \ with \ intercept;}
#' where \eqn{c} is a small positive number corresponding to the common cross-sectional intercept (\eqn{\lambda_c}).
#' Default values for \eqn{\psi} (\code{psi0}) and \eqn{d} (\code{d}) are 5 and 0.5, respectively.
#'
#' With \code{prior = 'Normal'} and \code{type = 'OLS'}, for each draw, the risk price estimate is
#' \deqn{ \hat{\lambda} = ( C^{\top} C +D )^{-1} C^{\top} \mu_R .}
#'
#' With \code{prior = 'Normal'} and \code{type = 'GLS'}, for each draw, the risk price estimate is
#' \deqn{ \hat{\lambda} = ( C^{\top} \Sigma_R^{-1} C +D )^{-1} C^{\top} \Sigma_R^{-1} \mu_R .}
#'
#'
#' @references
#' \insertRef{bryzgalova2023bayesian}{BayesianFactorZoo}
#'
#'
#' @return
#' The return of \code{BayesianSDF} is a list that contains the following elements:
#' \itemize{
#'    \item \code{lambda_path}: A \code{sim_length}\eqn{\times (k+1)} matrix if the intercept is included.
#'    NOTE: the first column \eqn{\lambda_c} corresponds to the intercept. The next \eqn{k} columns (i.e., the 2th -- \eqn{(k+1)}-th columns)
#'    are the risk prices of \eqn{k} factors. If the intercept is excluded, the dimension of \code{lambda_path} is \code{sim_length}\eqn{\times k}.
#'    \item \code{R2_path}: A \code{sim_length}\eqn{\times 1} matrix, which contains the posterior draws of the OLS or GLS \eqn{R^2}.
#' }
#'
#'
#'
#' @export
#'
#' @examples
#' ## <-------------------------------------------------------------------------------->
#' ## Example: Bayesian estimates of risk prices and R2
#' ## This example is from the paper (see Section III. Simulation)
#' ## <-------------------------------------------------------------------------------->
#'
#' library(reshape2)
#' library(ggplot2)
#'
#' # Load the example data
#' data("BFactor_zoo_example")
#' HML <- BFactor_zoo_example$HML
#' lambda_ols <- BFactor_zoo_example$lambda_ols
#' R2.ols.true <- BFactor_zoo_example$R2.ols.true
#' sim_f <- BFactor_zoo_example$sim_f
#' sim_R <- BFactor_zoo_example$sim_R
#' uf <- BFactor_zoo_example$uf
#' W_ols <- BFactor_zoo_example$W_ols
#'
#' cat("Load the simulated example \n")
#'
#' cat("Cross-section: Fama-French 25 size and value portfolios \n")
#' cat("True pricing factor in simulations: HML \n")
#' cat("Pseudo-true cross-sectional R-squared:", R2.ols.true, "\n")
#' cat("Pseudo-true (monthly) risk price:", lambda_ols[2], "\n")
#'
#' cat("----------------------------- Bayesian SDF ----------------------------\n")
#' cat("------------------------ See definitions 1 and 2 ----------------------\n")
#'
#' cat("--------------------- Bayesian SDF: Strong factor ---------------------\n")
#'
#' sim_result <- SDF_gmm(sim_R, sim_f, W_ols)   # GMM estimation
#' # sim_result$lambda_gmm
#' # sqrt(sim_result$Avar_hat[2,2])
#' # sim_result$R2_adj
#'
#' ## Now estimate the model using Bayesian method
#' two_step <- BayesianSDF(sim_f, sim_R, sim_length =  2000, psi0 = 5, d = 0.5)
#' # apply(X = two_step$lambda_path, FUN = quantile, MARGIN = 2, probs = c(0.05, 0.95))
#' # quantile(two_step$R2_path, probs = c(0.05, 0.5, 0.95))
#'
#' # Note that the first element correspond to lambda of the constant term
#' # So we choose k=2 to get lambda of the strong factor
#' k <- 2
#' m1 <- sim_result$lambda_gmm[k]
#' sd1 <- sqrt(sim_result$Avar_hat[k,k])
#'
#' bfm<-two_step$lambda_path[1001:2000, k]
#' fm<-rnorm(5000,mean = m1, sd=sd1)
#' data<-data.frame(cbind(fm, bfm))
#' colnames(data)<-c("GMM-OLS", "BSDF-OLS")
#' data.long<-melt(data)
#'
#' #
#' ### Figure 1(c)
#' #
#' p <- ggplot(aes(x=value, colour=variable, linetype=variable), data=data.long)
#' p+
#'  stat_density(aes(x=value, colour=variable),
#'               geom="line",position="identity", size = 2, adjust=1) +
#'  geom_vline(xintercept = lambda_ols[2], linetype="dotted", color = "#8c8c8c", size=1.5)+
#'  guides(colour = guide_legend(override.aes=list(size=2), title.position = "top",
#'  title.hjust = 0.5, nrow=1,byrow=TRUE))+
#'  theme_bw()+
#'  labs(color=element_blank()) +
#'  labs(linetype=element_blank()) +
#'  theme(legend.key.width=unit(4,"line")) +
#'  theme(legend.position="bottom")+
#'  theme(text = element_text(size = 26))+
#'  xlab(bquote("Risk price ("~lambda[strong]~")")) +
#'  ylab("Density" )
#'
#'
#' cat("--------------------- Bayesian SDF: Useless factor --------------------\n")
#'
#' sim_result <- SDF_gmm(sim_R, uf, W_ols)
#' # sim_result$lambda_gmm
#' # sqrt(sim_result$Avar_hat[2,2])
#' # sim_result$R2_adj
#'
#' two_step <- BayesianSDF(uf, sim_R, sim_length =  2000, psi0 = 5, d = 0.5)
#' #apply(X = two_step$lambda_path, FUN = quantile, MARGIN = 2, probs = c(0.05, 0.95))
#'
#'
#' ## Posterior (Asymptotic) Distribution of lambda
#' k <- 2
#' m1 <- sim_result$lambda[k]
#' sd1 <- sqrt(sim_result$Avar_hat[k,k])
#'
#' bfm<-two_step$lambda_path[1001:2000, k]
#' fm<-rnorm(5000,mean = m1, sd=sd1)
#' data<-data.frame(cbind(fm, bfm))
#' colnames(data)<-c("GMM-OLS", "BSDF-OLS")
#' data.long<-melt(data)
#'
#' #
#' ### Figure 1(a)
#' #
#' p <- ggplot(aes(x=value, colour=variable, linetype=variable), data=data.long)
#' p+
#'  stat_density(aes(x=value, colour=variable),
#'               geom="line",position="identity", size = 2, adjust=2) +
#'  geom_vline(xintercept = 0, linetype="dotted", color = "#8c8c8c", size=1.5)+
#'  guides(colour = guide_legend(override.aes=list(size=2),
#'  title.position = "top", title.hjust = 0.5, nrow=1,byrow=TRUE))+
#'  theme_bw()+
#'  labs(color=element_blank()) +
#'  labs(linetype=element_blank()) +
#'  theme(legend.key.width=unit(4,"line")) +
#'  theme(legend.position="bottom")+
#'  theme(text = element_text(size = 26))+
#'  xlab(bquote("Risk price ("~lambda[spurious]~")")) +
#'  ylab("Density" )
#'
#'
#'


BayesianSDF <- function(f, R, sim_length = 10000, intercept = TRUE, type = 'OLS',
                                prior = 'Flat', psi0 = 5, d = 0.5) {

  f <- as.matrix(f)  # factors: txk dimension
  R <- as.matrix(R)  # test assets: txN dimension
  k <- dim(f)[2]   # the number of factors
  t <- dim(f)[1]   # the number of time periods in factors
  N <- dim(R)[2]   # the number of test assets

  # Check whether the prerequisite conditions satisfy
  check_input(f, R , intercept, type, prior);

  p <- N+k
  Y <- cbind(f, R)
  Sigma_ols <- cov(Y)
  ones.N <- matrix(1,nrow = N, ncol = 1)
  mu_ols <- matrix(colMeans(Y), ncol = 1)

  ## Store the ordinary estimates of lambda_ols or lambda_gls:
  if (intercept == TRUE) {
    # the first element is constant
    lambda_path <- matrix(0, ncol = (1+k), nrow = sim_length)
  } else {
    lambda_path <- matrix(0, ncol = k, nrow = sim_length)
  }

  ## Store the estimates of cross-sectional R-squared: R2_ols or R_gls
  R2_path <- rep(0, sim_length)

  ## Determine the degree of shrinkage based on correlation between asset returns and factors:
  rho <- cor(Y)[(k+1):(N+k), 1:k, drop = FALSE]
  rho.demean <- rho - matrix(1, ncol = 1, nrow = N) %*% matrix(colMeans(rho), nrow = 1)
  if (prior=='Normal') {
    psi <- psi0 * diag(t(rho.demean)%*%rho.demean)
    if (intercept==TRUE) {
      D <- diag(c(1/100000, 1/psi)) * (1/t)^d
    } else {
      if (k == 1) {
        D <- matrix((1/psi)*(1/t)^d, ncol=1, nrow=1)
      } else {
        D <- diag(1/psi) * (1/t)^d
      }
    }
  }


  for (i in 1:sim_length) {

    ## (1) First-Stage: time-series regression
    Sigma <- riwish(v=t-1, S=t*Sigma_ols)
    Sigma_R <- Sigma[(k+1):(N+k), (k+1):(N+k)]
    Var_mu_half <- chol(Sigma/t)
    mu <- mu_ols + t(Var_mu_half) %*% matrix(rnorm(N+k), ncol = 1)
    sd_Y <- matrix(sqrt(diag(Sigma)), ncol=1)   # standard deviation of Y(t)
    corr_Y <- Sigma / (sd_Y%*%t(sd_Y))
    C_f <- corr_Y[(k+1):p, 1:k, drop=FALSE]   # corr[R(t), f(t)]
    a <- mu[(1+k):p,1,drop=FALSE] / sd_Y[(1+k):p]  # Sharpe ratio of test assets;
    ER <- mu[(1+k):p,1,drop=FALSE]
    sd_R <- sd_Y[(1+k):p]

    ## (2) Second-Stage: cross-sectional regression
    if (intercept == TRUE) { # if the intercept is included
      H <- cbind(ones.N, C_f)
    } else {
      H <- C_f
    }

    Sigma.inv <- chol2inv(chol(corr_Y[(k+1):(N+k), (k+1):(N+k)]))

    ## The Lambda estimates and R2
    if (prior=='Flat') {
      if (type=='OLS') { # Case I.1: Flat prior and OLS
        Lambda <- chol2inv(chol(t(H)%*%H)) %*% t(H) %*% a
        R2 <- 1 - ((t(a - H %*% Lambda) %*% (a - H %*% Lambda)) / as.vector((N-1)*var(a)))
      } else if (type=='GLS') { # Case I.2: Flat prior and GLS
        Lambda <- chol2inv(chol(t(H)%*%Sigma.inv%*%H)) %*% t(H)%*%Sigma.inv%*%a
        R2 <- (1 - t(a - H %*% Lambda)%*%Sigma.inv%*%(a - H %*% Lambda) /
                 (t(a - mean(a))%*%Sigma.inv%*%(a - mean(a))))
      }
    } else if (prior=='Normal') {
      if (type=='OLS') { # Case II.1 Normal prior and OLS
        Lambda <- chol2inv(chol(t(H)%*%H + D)) %*% t(H) %*% a
        R2 <- 1 - ((t(a - H%*%Lambda) %*% (a - H%*%Lambda)) / as.vector((N-1)*var(a)))
      } else if (type=='GLS') { # Case II.2 Normal prior and GLS
        Lambda <- chol2inv(chol(t(H)%*%Sigma.inv%*%H + D)) %*% t(H)%*%Sigma.inv%*%a
        R2 <- (1 - t(a - H%*%Lambda)%*%Sigma.inv%*%(a-H %*%Lambda) /
                      (t(a - mean(a))%*%Sigma.inv%*%(a - mean(a))))
      }
    }


    ## Record the estimates and R2
    lambda_path[i, ] <- Lambda
    R2_path[i] <- 1 - (1-R2) * (N-1) / (N-1-k)

  }

  return(list(lambda_path=lambda_path, R2_path=R2_path))

}


