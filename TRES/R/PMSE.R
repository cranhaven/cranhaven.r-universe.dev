#' Prediction and mean squared error.
#'
#' Evaluate the tensor response regression (TRR) or tensor predictor regression (TPR) model through the mean squared error.
#'
#' @param x A predictor tensor, array, matrix or vector.
#' @param y A response tensor, array, matrix or vector.
#' @param B An coefficient tensor tensor, array, matrix or vector.
#'
#' @return \item{mse}{The mean squared error.}
#' \item{pred}{The predictions.}
#'
#' @details There are three situations:
#' \itemize{
#'  \item TRR model: If \code{y} is an \eqn{m}-way tensor (array), \code{x} should be matrix or vector and \code{B} should be tensor or array.
#'  \item TPR model: If \code{x} is an \eqn{m}-way tensor (array), \code{y} should be matrix or vector and \code{B} should be tensor or array.
#'  \item Other: If \code{x} and \code{y} are both matrix or vector, \code{B} should be matrix. In this case, the prediction is calculated as \code{pred = B*X}.
#' }
#'
#' In any cases, users are asked to ensure the dimensions of \code{x}, \code{y} and \code{B} match. See \code{\link{TRRsim}} and \code{\link{TPRsim}} for more details of the TRR and TPR models.
#'
#' Let \eqn{\hat{Y}_i} denote each prediction, then the mean squared error is defined as \eqn{1/n\sum_{i=1}^n||Y_i-\hat{Y}_i||_F^2}, where \eqn{||\cdot||_F} denotes the Frobenius norm.
#'
#' @examples
#' ## Dataset in TRR model
#' r <- c(10, 10, 10)
#' u <- c(2, 2, 2)
#' p <- 5
#' n <- 100
#' dat <- TRRsim(r = r, p = p, u = u, n = n)
#' x <- dat$x
#' y <- dat$y
#'
#' # Fit data with TRR.fit
#' fit_std <- TRR.fit(x, y, method="standard")
#' result <- PMSE(x, y, fit_std$coefficients)

#' ## Dataset in TPR model
#' p <- c(10, 10, 10)
#' u <- c(1, 1, 1)
#' r <- 5
#' n <- 200
#' dat <- TPRsim(p = p, r = r, u = u, n = n)
#' x <- dat$x
#' y <- dat$y
#'
#' # Fit data with TPR.fit
#' fit_std <- TPR.fit(x, y, u, method="standard")
#' result <- PMSE(x, y, fit_std$coefficients)
#'
#' @seealso \code{\link{TRRsim}, \link{TPRsim}}.
#'
#' @export

PMSE <- function(x, y, B){
  if((inherits(x, "array") && !inherits(x, "matrix")) || inherits(x, "Tensor")){
  ## If x is tensor and y is matrix
    if(inherits(x, "Tensor")) x <- x@data
    if(!is.matrix(y)){
      if(is.vector(y)){
        y <- t(as.matrix(y))
      }else stop("x is array (or tensor), y should be vector or matrix.")
    }
    if(!inherits(B, "array") || inherits(B, "matrix")){
      if(inherits(B, "Tensor")){
        B <- B@data
      }else stop("x is array (or tensor), B should be array or Tensor.")
    }
    ss <- dim(x)
    len <- length(ss)
    n <- ss[len]
    p <- ss[1:(len-1)]
    r <- dim(y)[1]
    m <- length(p)
    if(n!=dim(y)[2]){stop("Unmatched dimensions between x and y.")}
    if(dim(B)[length(dim(B))] != r){stop("Unmatched dimensions between B and y.")}
    if(any(dim(B)[1:(length(dim(B))-1)] != p)){stop("Unmatched dimensions between B and x.")}
    tp1 <- matrix(B, c(prod(p), r))
    tp2 <- matrix(x, c(prod(p), n))
    pred <- crossprod(tp1, tp2)
    res <- y - pred
    mse <- sum(res^2)/n
  }else if((inherits(y, "array") && !inherits(y, "matrix")) || inherits(y, "Tensor")){
    ## If y is tensor and x is matrix
    if(inherits(y, "Tensor")) y <- y@data
    if(!is.matrix(x)){
      if(is.vector(x)){
        x <- t(as.matrix(x))
      }else stop("y is array (or tensor), x should be vector or matrix.")
    }
    if(!inherits(B, "Tensor")){
      if(inherits(B, "array") && !inherits(B, "matrix")){
        B <- as.tensor(B)
      }else stop("y is array (or tensor), B should be array or Tensor.")
    }
    ss <- dim(y)
    len <- length(ss)
    n <- ss[len]
    r <- ss[1:(len-1)]
    m <- length(r)
    p <- dim(x)[1]
    if(n != dim(x)[2]){stop("Unmatched dimension.")}
    if(dim(B)[length(dim(B))] != p){stop("Unmatched dimensions between B and x.")}
    if(any(dim(B)[1:(length(dim(B))-1)] != r)){stop("Unmatched dimensions between B and y.")}
    pred <- rTensor::ttm(B, t(x), m+1)
    res <- y - pred
    mse <- sum(res@data^2)/n
  }else if(is.matrix(x) || is.vector(x)){
    if(is.vector(x)) x <- t(as.matrix(x))
    if(!is.matrix(y)){
      if(is.vector(y)){
        y <- t(as.matrix(y))
      }else stop("x is matrix (or vector) and y is not high-order array, y should be vector or matrix.")
    }
    if(!is.matrix(B)) stop("x and y are matrices (or vectors), B should matrix.")
    p <- dim(x)[1]
    n <- dim(x)[2]
    r <- dim(y)[1]
    if(n != dim(y)[2]){stop("Unmatched dimension.")}
    if(r != dim(B)[1]){stop("Unmatched dimensions between B and y.")}
    if(p != dim(B)[2]){stop("Unmatched dimensions between B and x.")}
    pred <- B %*% x
    res <- y - pred
    mse <- sum(res^2)/n
  }else stop("The dimensions of x, y and B do not match.")

  return(list(mse=mse, pred=pred))
}
