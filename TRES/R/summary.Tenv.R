#' Summarize method for Tenv object.
#'
#' Summary method for object returned from \code{TRR.fit} and \code{TPR.fit} functions.
#'
#' Extract \code{call}, \code{method}, \code{coefficients}, \code{residuals}, \code{Gamma} from \code{object}. And append \code{mse}, \eqn{p}-value and the standard error of estimated coefficient.
#'
#' The mean squared error \code{mse} is defined as \eqn{1/n\sum_{i=1}^n||Y_i-\hat{Y}_i||_F^2}, where \eqn{\hat{Y}_i} is the prediction and \eqn{||\cdot||_F} is the Frobenius norm of tensor.
#'
#' Since the \eqn{p}-value and standard error depend on the estimation of cov\eqn{^{-1}}(vec\eqn{(X)}) which is unavailable for the ultra-high dimensional \eqn{vec(X)} in tensor predictor regression (TPR), the two statistics are only provided for the object returned from \code{TRR.fit}.
#'
#' \code{print.summary.Tenv} provides a more readable form of the statistics contained in \code{summary.Tenv}. If \code{object} is returned from \code{\link{TRR.fit}}, then \code{p-val} and \code{se} are also returned.
#'
#' @param object An object of class \code{"Tenv"}, as the ones returned from \code{\link{TPR.fit}} or \code{\link{TRR.fit}}.
#' @param ... Additional arguments. No available arguments exist in this version.
#' @param x An object of class \code{"summary.Tenv"}, usually, a result of a call to \code{summary.Tenv}.
#' @name summary.Tenv
#' @return Return \code{object} with additional components
#'  \item{call}{The matched call.}
#'  \item{method}{The implemented method.}
#'  \item{n}{The sample size.}
#'  \item{xdim}{The dimension of predictor.}
#'  \item{ydim}{The dimension of response.}
#'  \item{coefficients}{The tensor coefficients estimated from \code{TPR.fit} or \code{TRR.fit}.}
#'  \item{residuals}{The residuals, which equals to the response minus the fitted values.}
#'  \item{Gamma}{A list of envelope subspace basis.}
#'  \item{mse}{The mean squared error. The mean squared Frobenius norm of the difference between each response \eqn{Y_i} and fitted value \eqn{\hat{Y}_i}.}
#'  \item{p_val}{The p-value for coefficients. Only for the object returned from \code{TRR.fit}.}
#'  \item{se}{The standard error for coefficients. Only for the object returned from \code{TRR.fit}.}
#'
#' @seealso Fitting functions \code{\link{TRR.fit}}, \code{\link{TPR.fit}}.
#'
#' @examples
#' data("bat")
#' x <- bat$x
#' y <- bat$y
#' fit <- TRR.fit(x, y, method="standard")
#' ##print summary
#' summary(fit)
#'
#' ##Extract the p-value and standard error from summary
#' summary(fit)$p_val
#' summary(fit)$se
#'
#' @rdname summary.Tenv
#' @export
#' @importFrom stats coef residuals

summary.Tenv <- function(object, ...){
  ans <- object[c("call", "method", "coefficients", "residuals", "Gamma")]
  if (!inherits(object, "Tenv"))
    warning("calling summary.Tenv(<fake-Tenv-object>) ...")
  n <- dim(object$x)[length(dim(object$x))]
  if(object$call[1] == "TPR.fit()"){
    ans$mse <- sum(residuals(object)^2)/n
  }else if(object$call[1] == "TRR.fit()"){
    ans$mse <- sum(residuals(object)@data^2)/n
    tmp <- Tenv_Pval(object$x, object$y, Bhat = coef(object))
    ans$p_val <- tmp$p_val
    ans$se <- tmp$se
  }
  ans$n <- n
  ans$xdim <- dim(object$x)
  ans$ydim <- dim(object$y)
  class(ans) <- "summary.Tenv"
  ans
}

#' @rdname summary.Tenv
#' @method print summary.Tenv
#' @export
#' @importFrom stats coef
print.summary.Tenv <- function(x, ...){
  if(x$call[1] == "TPR.fit()"){
    cat("\n               Tensor predictor regression analysis               \n\n")
  }else if(x$call[1] == "TRR.fit()"){
    cat("\n               Tensor response regression analysis               \n\n")
  }
  cat("Call: ", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n", sep = "")
  cat("Dimensions: ", "x:", "(", paste(x$xdim, collapse = ","), "), ", "y:", "(", paste(x$ydim, collapse = ","), ")", "\n", sep = "")
  cat("Method: ", "\"", x$method, "\"", ", ", "sample size: ", x$n, ", ", "mean squared error: ", x$mse, "\n\n", sep = "")
  cat("Coefficients:\n")
  print(coef(x))
  cat("\n")
  if(!is.null(x$p_val)){
    cat("p-value:\n")
    print(x$p_val)
    cat("\n")
    cat("standard error:\n")
    print(x$se)
  }
  cat("\n")
  invisible(x)
}
