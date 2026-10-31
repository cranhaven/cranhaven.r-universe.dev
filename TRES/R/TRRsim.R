#' Generate simulation data for tensor response regression (TRR)
#'
#' This function is used to generate simulation data used in tensor response regression.
#'
#' The tensor response regression model is of the form,
#' \deqn{Y = B \bar{\times}_{(m+1)} X + \epsilon}
#' where predictor \eqn{X \in R^{p}}, response \eqn{Y \in R^{r_1\times \cdots\times r_m}}, \eqn{B \in R^{r_1\times \cdots\times r_m \times p}} and the error term is tensor normal distributed as follows,
#' \deqn{\epsilon \sim TN(0;\Sigma_1,\dots,\Sigma_m).}
#' According to the tensor envelope structure, we have
#' \deqn{B = [\Theta;\Gamma_1,\ldots,\Gamma_m, I_p],}
#' \deqn{\Sigma_k = \Gamma_k \Omega_k \Gamma_k^{T} + \Gamma_{0k} \Omega_{0k} \Gamma_{0k}^T,}
#' for some \eqn{\Theta \in R^{u_1\times\cdots\times u_m \times p}}, \eqn{\Omega_k \in R^{u_k \times u_k}} and \eqn{\Omega_{0k} \in \in R^{(p_k - u_k) \times (p_k - u_k)}}, \eqn{k=1,\ldots,m}.
#'
#' @note The length of \code{r} must match that of \code{u}, and each element of \code{u} must be less than the corresponding element in \code{r}.
#'
#' @param r The dimension of response, a vector with length larger than 2.
#' @param p The dimension of predictor, a scale.
#' @param u The structural dimension of envelopes at each mode, a vector with the same length as \code{r}.
#' @param n The sample size.
#' @return
#' \item{x}{The predictor of dimension \eqn{p\times n}.}
#' \item{y}{The response of dimension \eqn{r_1\times \cdots\times r_m \times n}.}
#' \item{Gamma}{The envelope subspace basis of dimension \eqn{r_k \times u_k, \ k=1,\ldots,m}.}
#' \item{coefficients}{The tensor coefficients of dimension \eqn{r_1\times \cdots\times r_m \times p}.}
#' \item{Sigma}{A lists of estimated covariance matrices at each mode for the error term, i.e., \eqn{\Sigma_1,\dots,\Sigma_m}.}
#' \item{p, r, u}{The input \code{p,r,u}.}
#'
#' @examples
#' r <- c(10, 10, 10)
#' u <- c(2, 2, 2)
#' p <- 5
#' n <- 100
#' dat <- TRRsim(r = r, p = p, u = u, n = n)
#' x <- dat$x
#' y <- dat$y
#' fit_std <- TRR.fit(x, y, method="standard")
#'
#' @seealso \code{\link{TPR.fit}, \link{TPRsim}}.
#' @references Li, L. and Zhang, X., 2017. Parsimonious tensor response regression. Journal of the American Statistical Association, 112(519), pp.1131-1146.

#' @export
TRRsim <- function(r, p, u, n){
  if(length(r) != length(u)){stop("The length of r must match the length of u.")}
  if(any(r < u)){stop("u must be less than r element-wise.")}
  m <- length(r)

  # Gamma is the list of envelopes.
  Gamma <- Gamma0 <- Omega <- Omega0 <- Sig <- Sigsqrtm <- vector("list", m)
  for (i in 1:m){
    tmp <- matrix(runif(r[i]*u[i]), r[i], u[i])
    Gamma[[i]] <- qr.Q(qr(tmp))
    Gamma0[[i]] <- qr.Q(qr(Gamma[[i]]),complete=TRUE)[,(u[i]+1):r[i]]
    A <- matrix(runif(u[i]^2), u[i], u[i])
    Omega[[i]] <- tcrossprod(A)
    A <- matrix(runif((r[i]-u[i])^2), (r[i]-u[i]), (r[i]-u[i]))
    Omega0[[i]] <- tcrossprod(A)
    Sig[[i]] <- Gamma[[i]] %*% Omega[[i]] %*% t(Gamma[[i]])+
      Gamma0[[i]] %*% Omega0[[i]] %*% t(Gamma0[[i]])
    Sig[[i]] <- 10*Sig[[i]]/sqrt(sum(Sig[[i]]^2)) + 0.01*diag(r[i])
    Sigsqrtm[[i]] <- pracma::sqrtm(Sig[[i]])$B
  }
  eta <- array(runif(prod(u)*p), c(u, p))
  eta <- rTensor::as.tensor(eta)
  B <- rTensor::ttl(eta, Gamma, ms=1:m)
  x <- matrix(stats::rnorm(p*n), p, n)
  Epsilon <- array(stats::rnorm(prod(r)*n), c(r, n))
  Epsilon <- rTensor::as.tensor(Epsilon)
  Epsilon <- rTensor::ttl(Epsilon, Sigsqrtm, ms=1:m)
  y <- Epsilon + rTensor::ttm(B, t(x), m+1)

  output <- list(x = x, y = y, Gamma = Gamma, coefficients = B, Sigma = Sig, p = p, r = r, u = u)
  output
}
