#' Solve the Penalized SVM with Nonconvex Penalties
#'
#' This function fits the penalized SVM using nonconvex penalties 
#' such as SCAD or MCP. It allows for flexible control over the regularization parameters and 
#' offers advanced options for initializing and optimizing the fit.
#'
#' @param x Matrix of predictors, with dimensions (nobs * nvars); each row represents an observation.
#' @param y Response variable, with length \eqn{n}.
#' @param lambda Optional user-supplied sequence of \code{lambda} values. If unspecified, the program 
#'   calculates its own sequence based on \code{nlambda} and \code{lambda.factor}. Supplying a decreasing 
#'   sequence of \code{lambda} values is advisable to leverage the warm-start optimization.
#' @param pen Specifies the type of nonconvex penalty: "SCAD" or "MCP".
#' @param aval The parameter value for the SCAD or MCP penalty. Default is 3.7 for SCAD and 2 for MCP.
#' @param lam2 Regularization parameter \code{lambda2} for the quadratic penalty on the coefficients. 
#'   Only one value of \code{lambda2} is used per fit.
#' @param ini_beta Optional initial coefficients to start the fitting process.
#' @param lla_step Number of Local Linear Approximation (LLA) steps. Default is 3.
#' @param ... Additional arguments passed to \code{\link{hdsvm}}.
#'
#' @return 
#' An object with S3 class \code{nc.hdsvm} consisting of
#'   \item{call}{the call that produced this object}
#'   \item{b0}{intercept sequence of length \code{length(lambda)}}
#'   \item{beta}{a \code{p*length(lambda)} matrix of coefficients,
#'               stored as a sparse matrix (\code{dgCMatrix} class,
#'               the standard class for sparse numeric matrices in
#'               the \code{Matrix} package.). To convert it into
#'               normal type matrix, use \code{as.matrix()}.}
#'   \item{lambda}{the actual sequence of \code{lambda} values used}
#'   \item{df}{the number of nonzero coefficients for each value
#'             of \code{lambda}.}
#'   \item{npasses}{the number of iterations for every lambda value}
#'   \item{jerr}{error flag, for warnings and errors, 0 if no error.}#'
#' @keywords svm classification
#' @useDynLib hdsvm, .registration=TRUE
#' @export
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x1 <- matrix(rnorm(n / 2 * p, -0.25, 0.1), n / 2)
#' x2 <- matrix(rnorm(n / 2 * p, 0.25, 0.1), n / 2)
#' x <- rbind(x1, x2)
#' beta <- 0.1 * rnorm(p)
#' prob <- plogis(c(x %*% beta))
#' y <- 2 * rbinom(n, 1, prob) - 1
#' lam2 <- 0.01
#' lambda <- 10^(seq(1,-4, length.out = 30)) 
#' nc.fit <- nc.hdsvm(x = x, y = y, lambda = lambda, lam2 = lam2, pen = "scad")

nc.hdsvm <- function(x, y, lambda, pen="scad", aval=NULL, lam2=1, 
    ini_beta=NULL, lla_step=3, ...) {

  if (!match(pen, c("scad", "mcp"), FALSE)) {
    warning("Only 'scad' and 'mcp' available for pen; 'scad' used.")
  }
  if (pen == "scad") {
    if (is.null(aval)) aval = 3.7
    pen_deriv <- deriv_scad
  }
  if (pen == "mcp") {
    if (is.null(aval)) aval = 2
    pen_deriv <- deriv_mcp
  }
  
  if (length(ini_beta) != NCOL(x) & !is.null(ini_beta)) {
    warning("wrong length of ini_beta")
    ini_beta <- NULL
  }
  if (is.null(ini_beta)) {
    fit <- cv.hdsvm(x, y, lambda=lambda, lam2=lam2, ...)
    ini_beta <- coef(fit, s="lambda.1se")[-1]
  } else {
    if(is.null(lambda)) stop("lambda must be provided with ini_beta.")
  }
  nlam <- length(lambda)
  pfmat <- matrix(NA, NCOL(x), ncol=nlam)

  beta <- matrix(ini_beta, length(ini_beta), nlam)
  for (j in seq(lla_step)) {
    for (l in seq(nlam)) {
      pfmat[ ,l] <- as.vector(pen_deriv(abs(beta[, l, drop=FALSE]), lambda[l]))
    }
    fit <- hdsvm(x, y, lambda=rep(1, nlam), nlambda = nlam, lam2=lam2, pf=pfmat, ...)
    beta <- coef(fit)[-1, , drop=FALSE]
  }
  fit$nc.lambda <- lambda
  class(fit) <- c("nc.hdsvm")
  fit
}

