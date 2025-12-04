###########################################
########## Bayesian Fama-MacBeth ##########
###########################################



#' Bayesian Fama-MacBeth
#'
#' @description This function provides the Bayesian Fama-MacBeth regression.
#'
#' @param f A matrix of factors with dimension \eqn{t \times k}, where \eqn{k} is the number of factors
#'        and \eqn{t} is the number of periods;
#' @param R A matrix of test assets with dimension \eqn{t \times N}, where \eqn{t} is the number of periods
#'        and \eqn{N} is the number of test assets;
#' @param sim_length The length of MCMCs;
#'
#' @details
#'
#' \code{BayesianFM} is similar to another twin function in this package, \code{BayesianSDF},
#' except that we estimate factors' risk premia rather than risk prices in this function.
#' Unlike \code{BayesianSDF}, we use factor loadings, \eqn{\beta_f}, instead of covariance exposures, \eqn{C_f},
#' in the Fama-MacBeth regression. In particular, after we obtain the posterior draws of \eqn{\mu_{Y}} and \eqn{\Sigma_{Y}}
#' (details can be found in the section introducing \code{BayesianSDF} function),
#' we calculate \eqn{\beta_f} as follows: \eqn{\beta_f = C_f \Sigma_f^{-1}}, and \eqn{\beta = (1_N, \beta_f)}.
#'
#' \strong{Bayesian Fama-MacBeth (BFM)}
#'
#' The posterior distribution of \eqn{\lambda} conditional on \eqn{\mu_{Y}}, \eqn{\Sigma_{Y}}, and the data, is a Dirac distribution at
#' \eqn{(\beta^\top \beta)^{-1} \beta^\top \mu_R}.
#'
#' \strong{Bayesian Fama-MacBeth GLS (BFM-GLS)}
#'
#' The posterior distribution of \eqn{\lambda} conditional on \eqn{\mu_{Y}}, \eqn{\Sigma_{Y}}, and the data, is a Dirac distribution at
#' \eqn{ (\beta^\top \Sigma_R^{-1} \beta)^{-1} \beta^\top \Sigma_R^{-1} \mu_R  }.
#'
#'
#'
#'
#' @return
#' The return of \code{BayesianFM} is a list of the following elements:
#' \itemize{
#'   \item \code{lambda_ols_path}: A \code{sim_length}\eqn{\times (k+1)} matrix of OLS risk premia estimates (Each row represents a draw.
#'                                 Note that the first column is \eqn{\lambda_c} corresponding to the constant term.
#'                                 The next \eqn{k} columns are the risk premia estimates of the \eqn{k} factors);
#'   \item \code{lambda_gls_path}: A \code{sim_length}\eqn{\times (k+1)} matrix of the risk premia estimates \eqn{\lambda} (GLS);
#'   \item \code{R2_ols_path}: A \code{sim_length}\eqn{\times 1} matrix of the \eqn{R^2_{OLS}};
#'   \item \code{R2_gls_path}: A \code{sim_length}\eqn{\times 1} matrix of the \eqn{R^2_{GLS}}.
#' }
#'
#'
#' @export
#'
#' @examples
#'
#' ## <-------------------------------------------------------------------------------->
#' ##   Example: Bayesian Fama-MacBeth
#' ## <-------------------------------------------------------------------------------->
#'
#' library(reshape2)
#' library(ggplot2)
#'
#' # Load Data
#' data("BFactor_zoo_example")
#' HML <- BFactor_zoo_example$HML
#' lambda_ols <- BFactor_zoo_example$lambda_ols
#' R2.ols.true <- BFactor_zoo_example$R2.ols.true
#' sim_f <- BFactor_zoo_example$sim_f
#' sim_R <- BFactor_zoo_example$sim_R
#' uf <- BFactor_zoo_example$uf
#'
#' ## <-------------------Case 1: strong factor---------------------------------------->
#'
#' # the Frequentist Fama-MacBeth
#' # sim_f: simulated factor, sim_R: simulated return
#' # sim_f is the useful (i.e., strong) factor
#' results.fm <- Two_Pass_Regression(sim_f, sim_R)
#'
#' # the Bayesian Fama-MacBeth with 10000 simulations
#' results.bfm <- BayesianFM(sim_f, sim_R, 2000)
#'
#' # Note that the first element correspond to lambda of the constant term
#' # So we choose k=2 to get lambda of the strong factor
#' k <- 2
#' m1 <- results.fm$lambda[k]
#' sd1 <- sqrt(results.fm$cov_lambda[k,k])
#'
#' bfm<-results.bfm$lambda_ols_path[1001:2000,k]
#' fm<-rnorm(20000,mean = m1, sd=sd1)
#' data<-data.frame(cbind(fm, bfm))
#' colnames(data)<-c("Frequentist FM", "Bayesian FM")
#' data.long<-melt(data)
#'
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
#'  xlab(bquote("Risk premium ("~lambda[strong]~")")) +
#'  ylab("Density" )
#'
#'
#' ## <-------------------Case 2: useless factor--------------------------------------->
#'
#' # uf is the useless factor
#' # the Frequentist Fama-MacBeth
#' results.fm <- Two_Pass_Regression(uf, sim_R)
#'
#' # the Bayesian Fama-MacBeth with 10000 simulations
#' results.bfm <- BayesianFM(uf, sim_R, 2000)
#'
#' # Note that the first element correspond to lambda of the constant term
#' # So we choose k=2 to get lambda of the useless factor
#' k <- 2
#' m1 <- results.fm$lambda[k]
#' sd1 <- sqrt(results.fm$cov_lambda[k,k])
#'
#'
#' bfm<-results.bfm$lambda_ols_path[1001:2000,k]
#' fm<-rnorm(20000,mean = m1, sd=sd1)
#' data<-data.frame(cbind(fm, bfm))
#' colnames(data)<-c("Frequentist FM", "Bayesian FM")
#' data.long<-melt(data)
#'
#' p <- ggplot(aes(x=value, colour=variable, linetype=variable), data=data.long)
#' p+
#'  stat_density(aes(x=value, colour=variable),
#'               geom="line",position="identity", size = 2, adjust=1) +
#'  geom_vline(xintercept = lambda_ols[2], linetype="dotted", color = "#8c8c8c", size=1.5)+
#'  guides(colour = guide_legend(override.aes=list(size=2),
#'  title.position = "top", title.hjust = 0.5, nrow=1,byrow=TRUE))+
#'  theme_bw()+
#'  labs(color=element_blank()) +
#'  labs(linetype=element_blank()) +
#'  theme(legend.key.width=unit(4,"line")) +
#'  theme(legend.position="bottom")+
#'  theme(text = element_text(size = 26))+
#'  xlab(bquote("Risk premium ("~lambda[strong]~")")) +
#'  ylab("Density" )
#'
#'
#'

BayesianFM <- function(f, R, sim_length) {
  # f: a matrix of factors with dimension t times k, where k is the number of factors
  #    and t is the number of periods;
  # R: a matrix of test assets with dimension t times N, where N is the number of test assets;
  # sim_length: the length of monte carlo simulation;

  k <- dim(f)[2]   # the number of factors
  t <- dim(f)[1]   # the number of time periods
  N <- dim(R)[2]   # the number of test assets
  Y <- cbind(f, R)

  # Check the prequisite condition
  check_input2(f,R);

  ET_f <- as.matrix(colMeans(f), ncol = 1)    # mean of the factors;
  ET_R <- as.matrix(colMeans(R), ncol = 1)    # the sample mean of test assets;

  lambda_ols_path <- matrix(0, ncol = (1+k), nrow = sim_length) # store the estimates of lambda_ols;
  lambda_gls_path <- matrix(0, ncol = (1+k), nrow = sim_length) # store the estimates of lambda_gls;
  R2_ols_path <- rep(0, sim_length)
  R2_gls_path <- rep(0, sim_length)
  ER_path <- matrix(0, nrow = sim_length, ncol = N)

  Sigma_ols <- cov(Y)
  ones.N <- matrix(1,nrow = N, ncol = 1)
  ones.t <- matrix(1,nrow = t, ncol = 1)
  ones.k <- matrix(1,nrow = k, ncol = 1)
  mu_ols <- matrix(colMeans(Y), ncol = 1)

  for (i in 1:sim_length) {
    #print(i)

    ## (1) First-Stage: time-series regression
    Sigma <- riwish(v=t-1, S=t*Sigma_ols)
    Sigma_R <- Sigma[(k+1):(N+k), (k+1):(N+k)]
    Sigma_f <- Sigma[1:k, 1:k]
    Sigma_Rf <- Sigma[(k+1):(N+k), 1:k]

    # expected returns - beta representation
    C <- Sigma[(k+1):(N+k), 1:k] %*% solve(Sigma_f)
    Var_mu_half <- chol(Sigma/t)
    mu <- mu_ols + t(Var_mu_half) %*% matrix(rnorm(N+k), ncol = 1)
    a <- mu[(k+1):(N+k),1,drop=FALSE]
    mu_f <- mu[1:k,1,drop=FALSE]
    Sigma.eps <- Sigma_R - Sigma_Rf %*% solve(Sigma_f) %*% t(Sigma_Rf)


    ## (2) Second-Stage: cross-sectional regression
    H <- cbind(ones.N, C)
    HH.inv <- chol2inv(chol(t(H)%*%H))
    Sigma.inv <- solve(Sigma.eps)
    Lambda_ols <- HH.inv %*% t(H) %*% a
    Lambda_gls <- chol2inv(chol(t(H)%*%Sigma.inv%*%H)) %*% t(H)%*%Sigma.inv%*%a
    sigma2_ols <- as.vector((1/N) * t(a - H %*% Lambda_ols) %*% (a - H %*% Lambda_ols))
    sigma2 <- rinvgamma(n = 1, shape = 0.5*(N-k-1), 0.5*N*sigma2_ols)

    ## R-Squared and pricing errors alpha
    alpha <- a - H %*% Lambda_ols
    R2_ols <- 1 - ((t(a - H %*% Lambda_ols) %*% (a - H %*% Lambda_ols)) / as.vector((N-1)*var(a)))
    R2_gls <- (1 - t(a - H %*% Lambda_gls)%*%Sigma.inv%*%(a - H %*% Lambda_gls) /
                 (t(a - mean(a))%*%Sigma.inv%*%(a - mean(a))))

    ## Record the estimates
    lambda_ols_path[i, ] <- Lambda_ols
    lambda_gls_path[i, ] <- Lambda_gls
    R2_ols_path[i] <- 1 - (1-R2_ols) * (N-1) / (N-1-k)
    R2_gls_path[i] <- 1 - (1-R2_gls) * (N-1) / (N-1-k)

  }

  return(list(lambda_ols_path=lambda_ols_path,
              lambda_gls_path=lambda_gls_path,
              R2_ols_path=R2_ols_path,
              R2_gls_path=R2_gls_path))
}

