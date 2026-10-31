#' The \eqn{p}-value and standard error of coefficient in tensor response regression (TRR) model.
#'
#' Obtain \eqn{p}-value of each element in the tensor regression coefficient estimator. Two-sided t-tests are implemented on the coefficient estimator, where asymptotic covariance of the OLS estimator is used.
#'
#' @param x The response tensor instance \eqn{ r_1\times r_2\times \cdots \times r_m}.
#' @param y A vector predictor of dimension \eqn{p}.
#' @param Bhat The estimator of tensor regression coefficient.
#'
#' The \eqn{p}-value and the standard error of estimated coefficient are not provided for tensor predictor regression since they depend on \eqn{\widehat{\mathrm{cov}}^{-1}\{\mathrm{vec}(\mathbf{X})\}} which is unavailable due to the ultra-high dimension of \eqn{\mathrm{vec}(\mathbf{X})}.
#'
#' @return
#' \item{p_ols}{The p-value tensor of OLS estimator.}
#' \item{p_val}{The p-value tensor of \code{Bhat}.}
#' \item{se}{The standard error tensor of \code{Bhat}.}
#'
#' @examples
#' ## Use dataset bat
#' data("bat")
#' x <- bat$x
#' y <- bat$y
#' fit_std <- TRR.fit(x, y, method="standard")
#' Tenv_Pval(x, y, fit_std$coefficients)
#'
#' @export
#' @importFrom rTensor as.tensor
#' @importFrom stats pt cov
Tenv_Pval <- function(x, y, Bhat){
  ss <- dim(y)
  len <- length(ss)
  n <- ss[len]
  r <- ss[1:(len-1)]
  m <- length(r)
  prodr <- prod(r)
  p <- dim(x)[1]
  mux <- as.matrix(apply(x, 1, mean))
  x <- x -mux[, rep(1, n)]
  muy <- apply(y@data, c(1:m), mean)

  tmp1 <- lapply(1:n, function(x) muy)
  tmp1 <- array(unlist(tmp1), c(r, n))
  tmp2 <- y@data-tmp1

  y <- as.tensor(tmp2)
  x_inv <- chol2inv(chol(tcrossprod(x))) %*% x
  Btil <- ttm(y, x_inv, m+1)
  En <- y - ttm(Btil, t(x), m+1)

  res <- kroncov(En)
  lambda <- res$lambda
  Sig <- res$S

  Sb <- chol2inv(chol(cov(t(x))))
  if(min(dim(Sb))>1){Sb <- matrix(diag(Sb))}

  for(i in 1:m) {
    Sb <- kronecker(Sb, diag(Sig[[i]]))
  }

  Sb <- array(sqrt(lambda*Sb), c(r, p))
  Ttil <- (sqrt(n)*Btil@data)/Sb
  That <- (sqrt(n)*Bhat@data)/Sb

  Ptil <- 2*(1-pt(abs(Ttil), n-1))
  Phat <- 2*(1-pt(abs(That), n-1))

  Sb <- as.tensor(Sb)
  Ptil <- as.tensor(Ptil)
  Phat <- as.tensor(Phat)

  return(list(p_ols=Ptil, p_val=Phat, se=Sb))
}
