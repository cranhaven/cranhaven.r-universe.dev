#' Envelope dimension by cross-validation for tensor predictor regression (TPR).
#'
#' Select the envelope dimension by cross-validation for tensor predictor regression.
#'
#' @param x The predictor tensor instance of dimension \eqn{p_1\times p_2\times\cdots\times p_m \times n}, where \eqn{n} is the sample size. Array with the same dimensions and matrix with dimension \eqn{p\times n} are acceptable.
#' @param y The response matrix of dimension \eqn{r \times n}, where \eqn{n} is the sample size. Vector of length \eqn{n} is acceptable.
#' @param maxdim The largest dimension to be considered for selection.
#' @param nfolds Number of folds for cross-validation.
#'
#' @return
#' \item{mincv}{The minimal cross-validation mean squared error.}
#' \item{u}{The envelope subspace dimension selected.}
#'
#' @details According to Zhang and Li (2017), the dimensions of envelopes at each mode are assumed to be equal, so the \code{u} returned is a single value representing the equal envelope dimension.
#'
#' For each dimension \code{u} in \code{1:maxdim}, we obtain the prediction
#' \deqn{\hat{Y}_i = \hat{B}_{(m+1)} vec(X_i)}
#'  for each predictor \eqn{X_i} in the \eqn{k}-th testing dataset, \eqn{k = 1,\ldots,}\code{nfolds}, where \eqn{\hat{B}} is the estimated coefficient based on the \eqn{k}-th training dataset. And the mean squared error for the \eqn{k}-th testing dataset is defined as
#'  \deqn{1/nk \sum_{i=1}^{nk}||Y_i-\hat{Y}_i||_F^2,}
#' where \eqn{nk} is the sample size of the \eqn{k}-th testing dataset and \eqn{||\cdot||_F} denotes the Frobenius norm. Then, the average of \code{nfolds} mean squared error is recorded as cross-validation mean squared error for the dimension \code{u}.
#'
#' @references Zhang, X. and Li, L., 2017. Tensor envelope partial least-squares regression. Technometrics, 59(4), pp.426-436.
#'
#' @examples
#' # The dimension of predictor
#' p <- c(10, 10, 10)
#' # The envelope dimensions u.
#' u <- c(1, 1, 1)
#' # The dimension of response
#' r <- 5
#' # The sample size
#' n <- 200
#' dat <- TPRsim(p = p, r = r, u = u, n = n)
#' x <- dat$x
#' y <- dat$y
#' TPRdim(x, y, maxdim = 5)
#'
#' ## Use dataset square. (time-consuming)
#' \donttest{
#' data("square")
#' x <- square$x
#' y <- square$y
#' # check the dimension of x
#' dim(x)
#' # use 32 as the maximal envelope dimension
#' TPRdim(x, y, maxdim=32)}
#'
#' @seealso \code{\link{TPRsim}}.
#'
#' @export
#' @importFrom rTensor as.tensor ttl
#' @importFrom pracma kron sqrtm
TPRdim <- function(x, y, maxdim=10, nfolds=5) {
  if(!is.matrix(y)){
    if(is.vector(y)){
      y <- t(as.matrix(y))
    }
    else stop("y should be vector or matrix.")
  }
  if(!inherits(x, "Tensor")){
    if(is.matrix(x) || inherits(x, "array")){
      x <- as.tensor(x)
    }
    else stop("x should be matrix, array or Tensor.")
  }
  ss <- dim(x)
  len <- length(ss)
  n <- ss[len]
  p <- ss[1:(len-1)]
  r <- dim(y)[1]
  m <- length(p)
  x_unfold <- matrix(x@data, prod(p), n)
  idx <- sample(1:n, n, replace = FALSE)
  Ntest <- floor(n/nfolds)
  Ntrain <- n - Ntest
  cv_mse <- rep(0, maxdim)

  for (i in 1:nfolds) {
    testid <- 1:Ntest + (i-1)*Ntest
    testid <- idx[testid]
    x_train <- x_unfold[, -testid]
    x_test <- x_unfold[, testid]
    y_train <- matrix(y[, -testid], ncol = Ntrain)
    y_test <- matrix(y[, testid], ncol = Ntest)
    # centering training dataset
    mu_xtrain <- as.matrix(apply(x_train, 1, mean))
    mu_y <- as.matrix(apply(y_train, 1, mean))
    y_train <- y_train - mu_y[, rep(1, Ntrain)]
    x_train <- x_train - mu_xtrain[, rep(1, Ntrain)]
    tp <- array(x_train, c(p, Ntrain))
    xtrain_tsr <- as.tensor(tp)
    #centering testing dataset
    y_test <- y_test - mu_y[, rep(1, Ntest)]
    x_test <- x_test - mu_xtrain[, rep(1, Ntest)]
    tp2 <- array(x_test, c(p, Ntest))
    xtest_tsr <- as.tensor(tp2)
    ## Fit with TPR.fit
    res <- TPR.fit(xtrain_tsr, y_train, u = rep(maxdim, m), method = "PLS")
    Gamma <- res$Gamma
    Sigx <- res$Sigma
    ###

    W <- vector("list", m)
    PGamma <- vector("list", m)
    for (k in 1:maxdim){
      for (j in 1:m){
        if (k==p[j]){
          W[[j]] <- diag(p[j])
        }else{
          W[[j]] <- Gamma[[j]][, 1:k, drop = FALSE]
        }
        PGamma[[j]] <- W[[j]] %*% chol2inv(chol(t(W[[j]]) %*% Sigx[[j]] %*% W[[j]])) %*% t(W[[j]])
      }
      Bhat <- ttl(xtrain_tsr, c(PGamma, list(y_train)), 1:(m+1))/Ntrain
      Bhat_unfold <- matrix(Bhat@data, nrow = c(prod(p)))
      error <- crossprod(Bhat_unfold, x_test) - y_test
      cv_mse[k] <- cv_mse[k] + sum(error^2)/Ntest
    }
  }
  cv_mse <- Re(cv_mse)/nfolds
  mincv <- min(cv_mse)
  u <- which.min(cv_mse)
  return(list(mincv = mincv, u = u))
}
