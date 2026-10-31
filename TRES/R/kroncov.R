#' The covariance estimation of tensor normal distribution
#'
#' This function provides the MLE of the covariance matrix of tensor normal distribution, where the covariance has a separable Kronecker structure, i.e. \eqn{\Sigma=\Sigma_{m}\otimes \ldots \otimes\Sigma_{1}}. The algorithm is a generalization of the MLE algorithm in Manceur, A. M., & Dutilleul, P. (2013).
#'
#' The individual component covariance matrices \eqn{\Sigma_i, i=1,\ldots, m} are not identifiable. To overcome the identifiability issue, each matrix \eqn{\Sigma_i} is normalized at the end of the iteration such that \eqn{||\Sigma_i||_F = 1}. And an overall normalizing constant \eqn{\lambda} is extracted so that the overall covariance matrix \eqn{\Sigma} is defined as
#' \deqn{\Sigma = \lambda \Sigma_m \otimes \cdots \otimes \Sigma_1.}
#'
#' If \code{Tn} is a \eqn{p \times n} design matrix for a multivariate random variable, then \code{lambda = 1} and \code{S} is a length-one list containing the sample covariance matrix.
#'
#' @param Tn A \eqn{p_1\times\cdots p_m\times n} matrix, array or tensor, where \eqn{n} is the sample size.
#' @param tol The convergence tolerance with default value \code{1e-6}. The iteration terminates when \eqn{||\Sigma_i^{(t+1)} - \Sigma_i^{(t)}||_F <} \code{tol} for some covariance matrix \eqn{\Sigma_i}.
#' @param maxiter The maximal number of iterations. The default value is 10.
#'
#' @return
#' \item{lambda}{The normalizing constant.}
#' \item{S}{A matrix list, consisting of each normalized covariance matrix \eqn{\Sigma_1,\ldots,\Sigma_m}.}
#'
#' @references Manceur, A.M. and Dutilleul, P., 2013. Maximum likelihood estimation for the tensor normal distribution: Algorithm, minimum sample size, and empirical bias and dispersion. Journal of Computational and Applied Mathematics, 239, pp.37-49.
#'
#' @export
#' @import rTensor
#' @importFrom pracma sqrtm
kroncov <- function(Tn, tol = 1e-6, maxiter = 10){
  ss <- dim(Tn)
  if(is.null(ss) || length(ss) <= 1) stop("The dimension of Tn should be larger than one.")
  if(!inherits(Tn, "Tensor")){
    if(is.matrix(Tn) || inherits(Tn, "array")){
      Tn <- as.tensor(Tn)
    }
    else stop("y should be matrix, array or Tensor.")
  }
  n <- ss[length(ss)]
  r <- ss[1:(length(ss)-1)]
  m <- length(r)
  prodr <- prod(r)

  ## Centering
  mu <- apply(Tn@data, seq_along(r), mean)
  tmp <- lapply(1:n, function(x) mu)
  tmp <- array(unlist(tmp), c(r, n))
  Tn <- as.tensor(Tn@data-tmp)

  ## Initialization: identity matrix
  lambda <- 1
  S <- vector("list", m)
  for (i in 1:m) {
    S[[i]] <- diag(r[i])
  }
  Sinvhalf <- S

  if(length(tol) == 1){
    tol <- rep(tol, m)
    }else{
      if(length(tol) != m) stop("length(tol) must be 1 or length(dim(Tn))-1.")
    }

  if (m > 1) {
    flag <- 0
    for(iter in seq_len(maxiter)){
       for (i in 1:m){
         Si0 <- S[[i]]
         idx <- c(1:(m+1))[-i]
         len <- length(idx)
         Tsn <- ttl(Tn, Sinvhalf[c(idx[1:(len-1)])], ms=idx[1:(len-1)])
         idxprod <- (r[i]/n)/prodr
         TsnTsn <- ttt(Tsn, Tsn, ms = idx)@data*idxprod
         S[[i]] <- TsnTsn/sqrt(sum(TsnTsn^2))
         Sinvhalf[[i]] <- sqrtm(S[[i]])$Binv
         if(sqrt(sum((Si0 - S[[i]])^2)) < tol[i]){flag <- 1;break}
       }
      if(flag == 1) break
    }
    Tsn <- ttl(Tn, Sinvhalf, 1:m)
    lambda <- sum((Tsn@data)^2)/prod(c(r, n))
  } else {
    # If Tn is matrix
    lambda <- 1
    S[[m]] <- ttt(Tn, Tn, ms = 2)@data*(1/n)
  }

  list(lambda=lambda, S=S)
}
