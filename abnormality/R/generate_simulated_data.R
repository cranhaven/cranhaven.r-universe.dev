#' Generate a matrix of correlated variables
#'
#' @param n number of observations
#' @param p number of features/variables
#' @param corr the correlation coefficient (-1 < r < 1)
#' @param constant_cov_matrix should the value of corr be constant in the covariance matrix, or should corr be the average value in the covariance matrix.
#' @param mean the mean value of the generated variables.
#'
#' @return an n x p matrix
#' @export
#'
#' @examples
#' Subject <- generate_correlated_matrix(1, 100, corr = .75,constant_cov_matrix = TRUE)
#' Reference_Population <- generate_correlated_matrix(100, 100, corr = .75,constant_cov_matrix = TRUE)
generate_correlated_matrix <- function(n, p, corr,constant_cov_matrix = T,mean=0){
  mu <- rep(mean,p)
  if(constant_cov_matrix==TRUE){
    Sigma <- matrix(corr, nrow=p, ncol=p) + diag(p)*(1-corr)
  }else{
    variance <- (corr - .001) * (.999 - corr)*.15 #var must be less than (mean - min) * (max - mean)
    Matrix_Values <- rgbeta(p*p, mean = corr, var = variance, min = .001, max = .999) #create matrix with mean = corr, but min=0 and max=1
    S <- matrix(Matrix_Values, p, p)
    S <- Matrix::forceSymmetric(S)
    for(i in 1:p){S[i,i] <- 1} #ones along the diagonal
    Sigma <- S
  }
  rawvars <- MASS::mvrnorm(n, mu=mu, Sigma=Sigma,tol=1)
  rawvars
}




