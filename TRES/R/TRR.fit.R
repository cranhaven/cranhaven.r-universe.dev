#' Tensor response regression
#'
#' This function is used for estimation of tensor response regression. The available method including standard OLS type estimation, PLS type of estimation as well as envelope estimation with FG, 1D and ECD approaches.
#'
#' Please refer to \strong{Details} part of \code{\link{TRRsim}} for the description of the tensor response regression model.
#'
#' When samples are insufficient, it is possible that the estimation of error covariance matrix \code{Sigma} is not available. However, if using ordinary least square method (\code{method = "standard"}), as long as sample covariance matrix of predictor \code{x} is nonsingular, \code{coefficients}, \code{fitted.values}, \code{residuals} are still returned.
#'
#' @aliases TRR
#' @usage TRR.fit(x, y, u, method=c('standard', 'FG', '1D', 'ECD', 'PLS'), Gamma_init = NULL)
#'
#' @param x The predictor matrix of dimension \eqn{p \times n}. Vector of length \eqn{n} is acceptable. If \code{y} is missing, \code{x} should be a list or an environment consisting of predictor and response datasets.
#' @param y The response tensor instance with dimension \eqn{r_1\times r_2\times\cdots\times r_m \times n}, where \eqn{n} is the sample size. Array with the same dimensions and matrix with dimension \eqn{r\times n} are acceptable.
#' @param u The dimension of envelope subspace. \eqn{u=(u_1,\cdots, u_m)}. Used for methods "FG", "1D", "ECD" and "PLS". User can use \code{\link{TRRdim}} to select dimension.
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
#' \code{TRR.fit} returns an object of class "Tenv".
#'
#' The function \code{\link{summary}} (i.e., \code{\link{summary.Tenv}}) is used to print the summary of the results, including additional information, e.g., the p-value and the standard error for coefficients, and the prediction mean squared error.
#'
#' The functions \code{coefficients}, \code{fitted.values} and \code{residuals} can be used to extract different features returned from \code{TRR.fit}.
#'
#' The function \code{\link{plot}} (i.e., \code{\link{plot.Tenv}}) plots the two-dimensional coefficients and p-value for object of class "Tenv".
#'
#' The function \code{\link{predict}} (i.e., \code{\link{predict.Tenv}}) predicts response for the object returned from \code{\link{TRR.fit}} function.
#'
#'  \item{x}{The original predictor dataset.}
#'  \item{y}{The original response dataset.}
#'  \item{call}{The matched call.}
#'  \item{method}{The implemented method.}
#'  \item{coefficients}{The estimation of regression coefficient tensor.}
#'  \item{Gamma}{The estimation of envelope subspace basis.}
#'  \item{Sigma}{A lists of estimated covariance matrices at each mode for the error term.}
#'  \item{fitted.values}{The fitted response tensor.}
#'  \item{residuals}{The residuals tensor.}
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
#' B <- dat$coefficients
#'
#' fit_std <- TRR.fit(x, y, method="standard")
#' fit_fg <- TRR.fit(x, y, u, method="FG")
#' fit_1D <- TRR.fit(x, y, u, method="1D")
#' fit_pls <- TRR.fit(x, y, u, method="PLS")
#' fit_ECD <- TRR.fit(x, y, u, method="ECD")
#'
#' rTensor::fnorm(B-stats::coef(fit_std))
#' rTensor::fnorm(B-stats::coef(fit_fg))
#' rTensor::fnorm(B-stats::coef(fit_1D))
#' rTensor::fnorm(B-stats::coef(fit_pls))
#' rTensor::fnorm(B-stats::coef(fit_ECD))
#'
#' # Extract the mean squared error, p-value and standard error from summary
#' summary(fit_std)$mse
#' summary(fit_std)$p_val
#' summary(fit_std)$se
#'
#' ## ----------- Pass a list or an environment to x also works ------------- ##
#' # Pass a list to x
#' l <- dat[c("x", "y")]
#' fit_std_l <- TRR.fit(l, method="standard")
#'
#' # Pass an environment to x
#' e <- new.env()
#' e$x <- dat$x
#' e$y <- dat$y
#' fit_std_e <- TRR.fit(e, method="standard")
#'
#' ## ----------- Use dataset "bat" included in the package ------------- ##
#' data("bat")
#' x <- bat$x
#' y <- bat$y
#' fit_std <- TRR.fit(x, y, method="standard")
#'
#' @seealso \code{\link{summary.Tenv}} for summaries, calculating mean squared error from the prediction.
#'
#' \code{\link{plot.Tenv}}(via \code{graphics::image}) for drawing the two-dimensional coefficient plot and \eqn{p}-value plot.
#'
#' \code{\link{predict.Tenv}} for prediction.
#'
#' The generic functions \code{\link{coef}, \link{residuals}, \link{fitted}}.
#'
#' \code{\link{TRRdim}} for selecting the dimension of envelope by information criteria.
#'
#' \code{\link{TRRsim}} for generating the simulated data used in tensor response regression.
#'
#' The simulated data \code{\link{bat}} used in tensor response regression.
#'
#' @references Li, L. and Zhang, X., 2017. Parsimonious tensor response regression. Journal of the American Statistical Association, 112(519), pp.1131-1146.
#'
#' @export

# This function gives all the estimation of tensor response regression
TRR.fit <- function(x, y, u, method=c('standard', 'FG', '1D', 'ECD', 'PLS'), Gamma_init = NULL) {
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
  if(!is.matrix(x)){
    if(is.vector(x)){
      x <- t(as.matrix(x))
    }
    else stop("x should be vector or matrix.")
  }
  if(!inherits(y, "Tensor")){
    if(is.matrix(y) || inherits(y, "array")){
      y <- as.tensor(y)
    }
    else stop("y should be matrix, array or Tensor.")
  }
  x_old <- x
  y_old <- y
  ss <- dim(y)
  len <- length(ss)
  n <- ss[len]
  if(n != dim(x)[2]){stop("Unmatched dimension.")}
  r <- ss[1:(len-1)]
  m <- length(r)
  prodr <- prod(r)
  p <- dim(x)[1]
  ##center the data
  mux <- as.matrix(apply(x, 1, mean))
  x <- x-mux[, rep(1, n)]
  muy <- apply(y@data, c(1:m), mean)
  tmp1 <- lapply(1:n, function(x) muy)
  tmp1 <- array(unlist(tmp1), c(r, n))
  tmp2 <- y@data-tmp1
  ###

  y <- rTensor::as.tensor(tmp2)
  x_inv <- chol2inv(chol(tcrossprod(x))) %*% x
  Btil <- rTensor::ttm(y, x_inv, m+1)
  En <- y - rTensor::ttm(Btil, t(x), m+1)
  res <- try(kroncov(En))
  if(inherits(res, "try-error")){
    if(method == "standard") {
      message("Warning: The estimation of error covariance is unavailable, which may due to insufficient samples. The coefficient, fitted values and residuals are returned.")
      Bhat <- Btil
      Gamma <- NULL
      Sig <- NULL
    }else{
      stop("Error: The estimation of error covariance is unavailable, which may due to insufficient samples.")
    }
  }else{
    lambda <- res$lambda
    Sig <- res$S
    if(method == "standard") {
      Bhat <- Btil
      Gamma <- NULL
    }else{
      if(missing(u)){stop("A user-defined u is required.")}
      Sinvhalf <- vector("list", m)
      for (i in 1:m) {
        Sinvhalf[[i]] <- pracma::sqrtm(Sig[[i]])$Binv
      }
      Gamma <- PGamma <- vector("list", m)
      for (i in 1:m) {
        #one-step estimator
        M <- lambda*Sig[[i]]
        idx <-  c(1:(m+1))[-i]
        len <- length(idx)
        if (len > 1) {
          Ysn <- rTensor::ttl(y, Sinvhalf[c(idx[1:(len-1)])], ms=idx[1:(len-1)])
        }else {
          Ysn <- rTensor::ttl(y, Sinvhalf, ms=1)
        }
        idxprod <- (r[i]/n)/prodr
        YsnYsn <- ttt(Ysn, Ysn, ms=idx)@data*idxprod
        U <- YsnYsn - M
        if (method == "1D"){
          Gamma[[i]] <- OptM1D(M, U, u[i])
        }else if(method == "ECD"){
          Gamma[[i]] <- ECD(M, U, u[i])
        }else if(method == "PLS"){
          Gamma[[i]] <- simplsMU(M, U, u[i])
        }else if(method == "FG"){
          Gamma[[i]] <- OptMFG(M, U, u[i])
        }
        PGamma[[i]] <- tcrossprod(Gamma[[i]])
      }
      tp <- rTensor::ttl(y, PGamma, ms=1:m)
      Bhat <- rTensor::ttm(tp, x_inv, m+1)
    }
  }
  fitted.values <- rTensor::ttm(Bhat, t(x_old), m+1)
  residuals <- y_old - fitted.values
  output <- list(x = x_old, y = y_old, call = cl, method = method, coefficients=Bhat, Gamma=Gamma, Sigma=Sig, fitted.values = fitted.values, residuals = residuals)
  class(output) <- "Tenv"
  output
}
