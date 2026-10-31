#' Envelope dimension selection for tensor response regression (TRR)
#'
#' This function uses the 1D-BIC criterion proposed by Zhang, X., & Mai, Q. (2018) to select envelope dimensions in tensor response regression. Refer to \code{\link{oneD_bic}} for more details.
#'
#' @param x The predictor matrix of dimension \eqn{p \times n}. Vector of length \eqn{n} is acceptable.
#' @param y The response tensor instance with dimension \eqn{r_1\times r_2\times\cdots\times r_m \times n}, where \eqn{n} is the sample size. Array with the same dimensions and matrix with dimension \eqn{r\times n} are acceptable.
#' @param C The parameter passed to \code{\link{oneD_bic}}. Default is \code{nrow(x) = p}.
#' @param maxdim The maximum envelope dimension to be considered. Default is 10.
#' @param ... Additional arguments passed to \code{\link{oneD_bic}}.
#'
#' @details See \code{\link{oneD_bic}} for more details on the definition of 1D-BIC criterion and on the arguments \eqn{C} and the additional arguments.
#'
#' Let \eqn{B} denote the estimated envelope with the selected dimension \code{u}, then the prediction is \eqn{\hat{Y}_i = B \bar{\times}_{(m+1)} X_i} for each observation. And the mean squared error is defined as \eqn{1/n\sum_{i=1}^n||Y_i-\hat{Y}_i||_F^2}, where \eqn{||\cdot||_F} denotes the Frobenius norm.
#'
#' @return
#' \item{bicval}{The minimal BIC values for each mode.}
#' \item{u}{The optimal envelope subspace dimension \eqn{(u_1, u_2,\cdots,u_m).}}
#' \item{mse}{The prediction mean squared error using the selected envelope basis.}
#'
#' @seealso \code{\link{oneD_bic}}, \code{\link{TRRsim}}.
#'
#' @examples
#' # The dimension of response
#' r <- c(10, 10, 10)
#' # The envelope dimensions u.
#' u <- c(2, 2, 2)
#' # The dimension of predictor
#' p <- 5
#' # The sample size
#' n <- 100
#'
#' # Simulate the data with TRRsim.
#' dat <- TRRsim(r = r, p = p, u = u, n = n)
#' x <- dat$x
#' y <- dat$y
#'
#' TRRdim(x, y) # The estimated envelope dimensions are the same as u.
#'
#' ## Use dataset bat. (time-consuming)
#' \donttest{
#' data("bat")
#' x <- bat$x
#' y <- bat$y
#' # check the dimension of y
#' dim(y)
#' # use 32 as the maximal envelope dimension
#' TRRdim(x, y, maxdim=32)
#' }
#'
#' @references Li, L. and Zhang, X., 2017. Parsimonious tensor response regression. Journal of the American Statistical Association, 112(519), pp.1131-1146.
#'
#' Zhang, X. and Mai, Q., 2018. Model-free envelope dimension selection. Electronic Journal of Statistics, 12(2), pp.2193-2216.
#'
#'
#' @export
#' @importFrom rTensor ttm ttl
#' @importFrom pracma kron sqrtm

TRRdim <- function(x, y, C = NULL, maxdim = 10, ...){
  if(!is.matrix(x)){
    if(is.vector(x)){
      x <- t(as.matrix(x))
    }
    else stop("x should be vector or matrix.")
  }
  if(!inherits(y, "Tensor")){
    if(is.matrix(y) || is.array(y)){
      y <- as.tensor(y)
    }
    else stop("y should be matrix, array or Tensor.")
  }
  ss <- dim(y)
  len <- length(ss)
  n <- ss[len]
  r <- ss[1:(len-1)]
  m <- length(r)
  prodr <- prod(r)
  p <- dim(x)[1]
  if(is.null(C)) C <- p
  ##center the data
  mux <- as.matrix(apply(x, 1, mean))
  x <- x-mux[, rep(1, n)]
  muy <- apply(y@data, c(1:m), mean)
  tmp1 <- lapply(1:n, function(x) muy)
  tmp1 <- array(unlist(tmp1), c(r, n))
  tmp2 <- y@data-tmp1
  y <- rTensor::as.tensor(tmp2)
  x_inv <- chol2inv(chol(tcrossprod(x))) %*% x
  Btil <- rTensor::ttm(y, x_inv, m+1)
  En <- y - rTensor::ttm(Btil, t(x), m+1)

  res <- kroncov(En)
  lambda <- res$lambda
  Sig <- res$S

  Sinvhalf <- vector("list", m)
  for (i in 1:m) {
    Sinvhalf[[i]] <- sqrtm(Sig[[i]])$Binv
  }

  bicval <- rep(NA_real_, m)
  u <- rep(NA_real_, m)
  PGamma <- vector("list", m)
  for (i in 1:m) {
    M <- lambda*Sig[[i]]
    idx <-  c(1:(m+1))[-i]
    len <- length(idx)
    if (len > 1) {
      Ysn <- ttl(y, Sinvhalf[c(idx[1:(len-1)])], ms=idx[1:(len-1)])
    }else {
      Ysn <- ttl(y, Sinvhalf, ms=1)
    }
    idxprod <- (r[i]/n)/prodr
    YsnYsn <- ttt(Ysn, Ysn, ms=idx)@data*idxprod
    U <- YsnYsn - M
    result <- oneD_bic(M, U, n, C, maxdim, ...)
    PGamma[[i]] <- tcrossprod(result$Gamma)
    bicval[i] <- result$bicval[result$u]
    u[i] <- result$u
  }
  tp <- rTensor::ttl(y, PGamma, ms=1:m)
  B <- rTensor::ttm(tp, x_inv, m+1)
  mse <- PMSE(x, y, B)$mse

  return(list(bicval = bicval, u = u, mse = mse))
}
