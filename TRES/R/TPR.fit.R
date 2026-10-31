#' Tensor predictor regression
#'
#' This function is used for estimation of tensor predictor regression. The available method including standard OLS type estimation, PLS type of estimation as well as envelope estimation with FG, 1D and ECD approaches.
#'
#' Please refer to \strong{Details} part of \code{\link{TPRsim}} for the description of the tensor predictor regression model.
#'
#' @aliases TPR
#' @usage TPR.fit(x, y, u, method=c('standard', 'FG', '1D', 'ECD', 'PLS'), Gamma_init = NULL)
#'
#' @param x The predictor tensor instance of dimension \eqn{p_1\times p_2\times\cdots\times p_m \times n}, where \eqn{n} is the sample size. Array with the same dimensions and matrix with dimension \eqn{p\times n} are acceptable. If \code{y} is missing, \code{x} should be a list or an environment consisting of predictor and response datasets.
#' @param y The response matrix of dimension \eqn{r \times n}, where \eqn{n} is the sample size. Vector of length \eqn{n} is acceptable.
#' @param u The dimension of envelope subspace. \eqn{u=(u_1,\cdots, u_m)}. Used for methods "FG", "1D", "ECD" and "PLS". User can use \code{\link{TPRdim}} to select dimension.
#' @param method The method used for estimation of tensor response regression. There are four possible choices.
#' \itemize{
#'   \item{\code{"standard"}}: The standard OLS type estimation.
#'   \item{\code{"FG"}}: Envelope estimation with full Grassmannian (FG) algorithm.
#'   \item{\code{"1D"}}: Envelope estimation with one dimensional optimization approaches by 1D algorithm.
#'   \item{\code{"ECD"}}: Envelope estimation with one dimensional optimization approaches by ECD algorithm.
#'   \item{\code{"PLS"}}: The SIMPLS-type estimation without manifold optimization.
#' }
#' @param Gamma_init A list specifying the initial envelope subspace basis for "FG" method. By default, the estimators given by "1D" algorithm is used.
#'
#' @return
#' \code{TPR.fit} returns an object of class "Tenv".
#'
#' The function \code{\link{summary}} (i.e., \code{\link{summary.Tenv}}) is used to print the summary of the results, including additional information, e.g., the p-value and the standard error for coefficients, and the prediction mean squared error.
#'
#' The functions \code{coefficients}, \code{fitted.values} and \code{residuals} can be used to extract different features returned from \code{TPR.fit}.
#'
#' The function \code{\link{plot}} (i.e., \code{\link{plot.Tenv}}) plots the two-dimensional coefficients and p-value for object of class "Tenv".
#'
#' The function \code{\link{predict}} (i.e., \code{\link{predict.Tenv}}) predicts response for the object returned from \code{\link{TPR.fit}} function.
#'   \item{x}{The original predictor dataset.}
#'   \item{y}{The original response dataset.}
#'   \item{call}{The matched call.}
#'   \item{method}{The implemented method.}
#'   \item{coefficients}{The estimation of regression coefficient tensor.}
#'   \item{Gamma}{The estimation of envelope subspace basis.}
#'   \item{Sigma}{A lists of estimated covariance matrices at each mode for the tensor predictors.}
#'   \item{fitted.values}{The fitted response matrix.}
#'   \item{residuals}{The residuals matrix.}
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
#'
#' # Simulate the data with TPRsim.
#' dat <- TPRsim(p = p, r = r, u = u, n = n)
#' x <- dat$x
#' y <- dat$y
#' B <- dat$coefficients
#'
#' fit_std <- TPR.fit(x, y, method="standard")
#' fit_FG <- TPR.fit(x, y, u, method="FG")
#' fit_pls <- TPR.fit(x, y, u, method="PLS")
#'
#' rTensor::fnorm(B-stats::coef(fit_std))
#' rTensor::fnorm(B-stats::coef(fit_FG))
#' rTensor::fnorm(B-stats::coef(fit_pls))
#'
#' ## ----------- Pass a list or an environment to x also works ------------- ##
#' # Pass a list to x
#' l <- dat[c("x", "y")]
#' fit_std_l <- TPR.fit(l, method="standard")
#'
#' # Pass an environment to x
#' e <- new.env()
#' e$x <- dat$x
#' e$y <- dat$y
#' fit_std_e <- TPR.fit(e, method="standard")
#'
#' ## ----------- Use dataset "square" included in the package ------------- ##
#' data("square")
#' x <- square$x
#' y <- square$y
#' fit_std <- TPR.fit(x, y, method="standard")
#'
#' @seealso \code{\link{summary.Tenv}} for summaries, calculating mean squared error from the prediction.
#'
#' \code{\link{plot.Tenv}}(via \code{graphics::image}) for drawing the two-dimensional coefficient plot.
#'
#' \code{\link{predict.Tenv}} for prediction.
#'
#' The generic functions \code{\link{coef}, \link{residuals}, \link{fitted}}.
#'
#' \code{\link{TPRdim}} for selecting the dimension of envelope by cross-validation.
#'
#' \code{\link{TPRsim}} for generating the simulated data used in tensor prediction regression.
#'
#' The simulated data \code{\link{square}} used in tensor predictor regression.
#'
#' @references Zhang, X. and Li, L., 2017. Tensor envelope partial least-squares regression. Technometrics, 59(4), pp.426-436.
#'
#' @export
#' @import rTensor
#' @importFrom pracma sqrtm kron
#' @importFrom stats cov

# This function gives all the estimation of tensor predictor regression
# The tensor predictor should be 2-dimensional or 3-dimensional

TPR.fit <- function(x, y, u, method=c('standard', 'FG', '1D', 'ECD', 'PLS'), Gamma_init = NULL){
  cl <- match.call()
  method <- match.arg(method)
  if(missing(y)){
    tmp <- x
    if(is.list(tmp)){
      if(!is.null(names(tmp))){
        x <- tmp$x
        y <- tmp$y
      }
      else{
        if(length(x) < 2) stop("x or y is missing.")
        x <- tmp[[1]]
        y <- tmp[[2]]
      }
    }
    else if(is.environment(x)){
      x <- tmp$x
      y <- tmp$y
    }
    else{
      stop("y is null, x should be a list or an environment.")
    }
    if(is.null(x) || is.null(y)) stop("x or y is missing. Check names(x).")
  }
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
  x_old <- x
  y_old <- y
  ss <- dim(x)
  len <- length(ss)
  n <- ss[len]
  if(n != dim(y)[2]){stop("Unmatched dimension.")}
  p <- ss[1:(len-1)]
  m <- length(p)
  r <- dim(y)[1]
  ##center the data
  muy <- as.matrix(apply(y, 1, mean))
  y <- y - muy[, rep(1, n)]
  mux <- apply(x@data, c(1:m), mean)
  ttmp <- lapply(1:n, function(x) mux)
  ttmp <- array(unlist(ttmp), c(p, n))
  ttmp2 <- x@data - ttmp
  ###

  x <- as.tensor(ttmp2)
  vecx <- matrix(x@data, prod(p), n)
  res <- kroncov(x)
  lambda <- res$lambda
  Sigx <- res$S
  Sigx[[1]] <- lambda*Sigx[[1]]

  if(method == "standard") {
    Sigxinv <- lapply(Sigx, function(x){chol2inv(chol(x))})
    Bhat <- ttl(x, c(Sigxinv, list(y)), 1:(m+1))/n
    Gamma <- NULL
  }else{
    if(missing(u)){stop("A user-defined u is required.")}
    Sinvhalf <- vector("list", m)
    for (i in 1:m) {
      Sinvhalf[[i]] <- sqrtm(Sigx[[i]])$Binv
    }
    Sigy <- (n-1)*cov(t(y))/n
    Sinvhalf[[m+1]] <- sqrtm(Sigy)$Binv
    C <- ttm(x, y, m+1)/n
    Gamma <- PGamma <- vector("list", m)
    for (i in 1:m){
      M <- Sigx[[i]]
      idx <- c(1:(m+1))[-i]
      Ck <- ttl(C, Sinvhalf[idx], ms = idx)
      U <- unfold(Ck, row_idx = i, col_idx = idx)@data
      idxprod <- (p[i]/r)/prod(p)
      Uk <- idxprod * tcrossprod(U)
      if(method == "PLS"){
        Gamma[[i]] <- simplsMU(M, Uk, u[i])
      }else if(method == "1D"){
        Gamma[[i]] <- OptM1D(Sigx[[i]], Uk, u[i])
      }else if(method == "ECD"){
        Gamma[[i]] <- ECD(Sigx[[i]], Uk, u[i])
      }else if(method == "FG"){
        Gamma[[i]] <- OptMFG(Sigx[[i]], Uk, u[i])
      }
      tmp <- t(Gamma[[i]]) %*% Sigx[[i]] %*% Gamma[[i]]
      PGamma[[i]] <- Gamma[[i]] %*% chol2inv(chol(tmp)) %*% t(Gamma[[i]]) %*% Sigx[[i]]
    }
    Bhat <- ttl(x, c(PGamma, list(y)), 1:(m+1))/n
  }
  tp1 <- matrix(Bhat@data, nrow = c(prod(p)))
  tp2 <- matrix(x_old@data, prod(p), n)
  fitted.values <- crossprod(tp1, tp2)
  residuals <- y_old - fitted.values
  output <- list(x = x_old, y = y_old, call = cl, method = method, coefficients=Bhat, Gamma=Gamma, Sigma=Sigx, fitted.values = fitted.values, residuals=residuals)
  class(output) <- "Tenv"
  output
}
