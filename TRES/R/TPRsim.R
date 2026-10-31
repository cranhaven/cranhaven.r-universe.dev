#' Generate simulation data for tensor predictor regression (TPR)
#'
#' This function is used to generate simulation data used in tensor prediction regression.
#'
#' The tensor predictor regression model is of the form,
#' \deqn{Y = B_{(m+1)}vec(X) + \epsilon}
#' where response \eqn{Y \in R^{r}}, predictor \eqn{X \in R^{p_1\times \cdots\times p_m}}, \eqn{B \in \in R^{p_1 \times\cdots\times p_m \times r}} and the error term is multivariate normal distributed. The predictor is tensor normal distributed,
#' \deqn{X\sim TN(0;\Sigma_1,\dots,\Sigma_m)}
#' According to the tensor envelope structure, we have
#' \deqn{B = [\Theta; \Gamma_1,\ldots, \Gamma_m, I_p],}
#' \deqn{\Sigma_k = \Gamma_k \Omega_k \Gamma_k^{T}+ \Gamma_{0k} \Omega_{0k} \Gamma_{0k}^T,}
#' for some \eqn{\Theta \in R^{u_1 \times\cdots\times u_m \times p}}, \eqn{\Omega_k \in R^{u_k \times u_k}} and \eqn{\Omega_{0k} \in \in R^{(p_k - u_k) \times (p_k - u_k)}}, \eqn{k=1,\ldots,m}.
#'
#' @note The length of \code{p} must match that of \code{u}, and each element of \code{u} must be less than the corresponding element in \code{p}.
#'
#' @param p The dimension of predictor, a vector in the form of \eqn{(p_1,\cdots, p_m)}.
#' @param r The dimension of response, a scale.
#' @param u The structural dimension of envelopes at each mode, a vector with the same length as p.
#' @param n The sample size.
#' @return
#' \item{x}{The predictor of dimension \eqn{p_1\times \cdots\times p_m \times n}.}
#' \item{y}{The response of dimension \eqn{r\times n}.}
#' \item{Gamma}{A list of envelope subspace basis of dimension \eqn{p_k \times u_k, \ k=1,\ldots,m}.}
#' \item{coefficients}{The tensor coefficients of dimension \eqn{p_1\times \cdots\times p_m \times r}.}
#' \item{Sigma}{A lists of estimated covariance matrices at each mode for the tensor predictors, i.e., \eqn{\Sigma_1,\dots, \Sigma_m}.}
#' \item{p, r, u}{The input \code{p,r,u}.}
#'
#' @examples
#' p <- c(10, 10, 10)
#' u <- c(1, 1, 1)
#' r <- 5
#' n <- 200
#' dat <- TPRsim(p = p, r = r, u = u, n = n)
#' x <- dat$x
#' y <- dat$y
#' fit_std <- TPR.fit(x, y, method="standard")
#'
#' @seealso \code{\link{TPR.fit}, \link{TRRsim}}.
#' @references Zhang, X. and Li, L., 2017. Tensor envelope partial least-squares regression. Technometrics, 59(4), pp.426-436.

#' @export
#' @importFrom stats rnorm
TPRsim <- function(p, r, u, n){
  if(length(p) != length(u)){stop("The length of p must match the length of u.")}
  if(any(p < u)){stop("u must less than p element-wise.")}
  m <- length(p)
  Gamma <- Gamma0 <- Omega <- Omega0 <- Sig <- Sigsqrtm <- vector("list", m)
  for(i in 1:m) {
    tmp <- matrix(runif(p[i]*u[i]), p[i], u[i])
    Gamma[[i]] <- qr.Q(qr(tmp))
    Gamma0[[i]] <- qr.Q(qr(tmp), complete=TRUE)[, (u[i]+1):p[i]]
    Omega[[i]] <- diag(u[i])
    Omega0[[i]] <- 0.01*diag(p[i]-u[i])
    Sig[[i]] <- Gamma[[i]] %*% Omega[[i]] %*% t(Gamma[[i]])+
      Gamma0[[i]] %*% Omega0[[i]] %*% t(Gamma0[[i]])
    Sig[[i]] <- 2*Sig[[i]]/sqrt(sum(Sig[[i]]^2))
    Sigsqrtm[[i]] <- pracma::sqrtm(Sig[[i]])$B
  }
  A <- matrix(runif(r^2), r, r)
  SigY <- tcrossprod(A)
  SigY <- SigY/sqrt(sum(SigY^2))

  ##generate data
  Epsilon <- MASS::mvrnorm(n, mu=rep(0, r), Sigma=SigY)
  tmp2 <- array(rnorm(prod(p, n)), c(p, n))
  x <- rTensor::as.tensor(tmp2)
  x <- rTensor::ttl(x, Sigsqrtm, ms = c(1:m))
  vecx <- matrix(x@data, prod(p), n)
  eta <- array(runif(prod(u,r)), c(u,r))
  eta <- rTensor::as.tensor(eta)
  B <- rTensor::ttl(eta,Gamma, ms = c(1:m))
  vecB <- vecB <- matrix(B@data, prod(p), r)
  Y_tmp <- crossprod(vecB, vecx)
  y <- Y_tmp + t(Epsilon)

  output <- list(x = x, y = y, Gamma = Gamma, coefficients = B, Sigma = Sig, p = p, r = r, u = u)
  output
}
