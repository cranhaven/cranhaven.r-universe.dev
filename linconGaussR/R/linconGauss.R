isPD <- function(matr){
  if(!isSymmetric(matr)){
    return(FALSE)
  }
  eigensystem <- eigen(matr,T,T)
  return(all(eigensystem$values>0))
}

#' Sample Gaussian distribution with linear constraints
#' Taking truncated sample of Gaussian distribution over a linear constraint domain.
#' @param n number of samples to take
#' @param A a matrix with M by D dimensions, the linear constraints, such that Ax+b>=0
#' @param b the offset of the linear constraints with dimension M such that Ax+b>=0
#' @param Sigma covariance matrix of the Gaussian
#' @param mu mean vector of the Gaussian
#' @param x_init the sample to start with, if NULL, a sample will be drawn using rejection method
#' @param intersection bool whether sample from the intersection or the union of the linear constraints, default true, sample from the intersection
#' @param n_retry_init how many times to try finding a initial value 
#' @param nskp how many sample to skip during the sampling routine
#' @return a matrix with truncated sample, row as samples
#' @examples 
#' my_sample <- linconGauss(100, diag(2),c(0,0),diag(2),c(0,0))
#' MASS_sample <- MASS::mvrnorm(1000,c(0,0),diag(2))
#' plot(MASS_sample)
#' points(my_sample,col = "red")
#' abline(h=0)
#' abline(v=0)
#' 

linconGauss <- function(n, A, b, Sigma, mu, x_init = NULL, intersection = TRUE,n_retry_init = 1000,nskp = 5){
  M <- nrow(A)
  D <- ncol(A)
  b <- as.matrix(b)
  mu <- as.matrix(mu)
  if(ncol(b)>1 | nrow(b)!=M){
    stop("b should be a vector and have number of rows same to A")
  }
  if(nrow(Sigma)!=D|ncol(Sigma)!=D){
    stop("Sigma should have the same dimension as nrow(A) and should be square")
  }
  if(!isPD(Sigma)){
    stop("Sigma should be positive definite")
  }
  if(ncol(mu)>1 | nrow(mu)!=D){
    stop("b should be a vector and have number of rows same to Sigma")
  }
  if(is.null(x_init)){
    for(i in 1:n_retry_init){
      x_init <- mvrnorm(1, mu, Sigma)
      flag <- all( A %*% x_init + b>=0 )
      if(flag){
        break
      }
    }
    if(!flag){
      stop("fail to initialize the sample, please provide an initial point")
    }
  }
  else{
    if(length(x_init)!=D){
      stop("initial point must have the same dimension as Sigma")
    }
  }
  linconGauss_cpp(n,  A , b , Sigma,  mu,x_init , intersection,nskp )
}
