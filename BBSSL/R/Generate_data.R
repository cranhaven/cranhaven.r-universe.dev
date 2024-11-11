#' @export
#' @name Generate_data
#' @title Generate_data
#'
#' @description This function generates data (X,y) with specified correlation and noise standard deviation.
#'
#' @param truth.beta A vector of active beta's (s * 1, with s the number of active coordinates).
#' @param p Number of covariates.
#' @param n Number of observations.
#' @param truth.sigma Noise standard deviation.
#' @param rho Correlation Coefficient.
#' @param correlation Correlation structure. Correlation = "block" means predictors are grouped into equi-size blocks where each block contains one active predictor, and the within-block correlation coefficient is rho; predictors in different blocks are mutually independent. Correlation = "all" means all predictors are equi-correlated with coefficient rho.
#' @param NumOfBlock Number of blocks, used only when correlation = 'block'.
#'
#' @return A list, including vector 'y' (n1), matrix 'X' (np), vector 'beta' (p1).
Generate_data = function(truth.beta, p, n, truth.sigma, rho, correlation=c("block","all"), NumOfBlock){

  s = length(truth.beta)

  if (correlation == "block"){
    block = rho + diag(1-rho, p/NumOfBlock, p/NumOfBlock)
    Sigma = kronecker(diag(NumOfBlock), block) # block diagonal matrix
    trueId = 1 + p/NumOfBlock * (1:s-1)
    beta = rep(0, p)
    beta[trueId] = truth.beta
  }else if (correlation == 'all'){
    Sigma = rho + diag(1-rho, p, p)
    trueId = 1:s
    beta = c(truth.beta, rep(0, p-s))
  }else {
    print("unknown correlation structure!")
  }
  X = mvnfast::rmvn(n = n, mu = rnorm(p, 0, 1), sigma = Sigma)
  s = length(truth.beta)

  # standardize X so that each column has l2-norm sqrt(n)
  X <- scale(X, center = TRUE, scale = FALSE)
  Xnorm = apply(X^2, 2, sum)
  X = X * matrix(sqrt(n)/sqrt(Xnorm), byrow = T, nrow = n, ncol = p)
  y = X[,trueId] %*% truth.beta + rnorm(n, 0, truth.sigma)

  return(list(y=y, X=X, beta = beta))
}


