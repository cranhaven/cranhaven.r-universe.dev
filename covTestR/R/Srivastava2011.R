#' @export
#'
#' @references Srivastava, M. S., Kollo, T., and Rosen, D. von. (2011). 
#' Some Tests for the Covariance Matrix with Fewer Observations then 
#' the Dimension Under Non-normality. Journal of Multivariate Analysis, 
#' 102(6), 1090-1103. \href{http://doi.org/10.1016/j.jmva.2011.03.003}{10.1016/j.jmva.2011.03.003}
#' @rdname structureStatistics
Srivastava2011 <- function(x, Sigma = "identity", ...){
  UseMethod("Srivastava2011")
}

#' @export
#' @keywords internal
#' @importFrom stats pchisq
#'
Srivastava2011.covariance <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- attributes(x)$df + 1
  S <- x

  if(Sigma[[1]] == "identity"){
    S_ <- x
  }else{
    svCov <- svd(x)
    sv <- svd(Sigma)
    x_ <- svCov$u %*% diag(sqrt(svCov$d)) %*%
      solve(sv$u %*% diag(sqrt(sv$d)))
    S_ <- t(x_) %*% x_
  }

  statistic <- Srivastava2011_(n - 1, p, S_)
  names(statistic) <- "Standard Normal"

  parameter <- c(0, 1)
  names(parameter) <- c("Mean", "Variance")

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"
  
  p.value <- 1 - pnorm(abs(statistic))

  estimate <- S

  estimate <- if(nrow(estimate) > 5){
    NULL
  }else{
    estimate
  }

  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = estimate,
              null.value = null.value,
              alternative = "two.sided",
              method = "Srivastava et al. 2011 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @export
#' @keywords internal
#' @importFrom stats cov
#' @importFrom stats pchisq
#'
Srivastava2011.matrix <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- nrow(x)
  S <- cov(x)

  if(Sigma[[1]] == "identity"){
    S_ <- S
  }else{
    sv <- svd(Sigma)
    svDf <- svd(S)
    x_ <- svDf$u %*% diag(sqrt(sv$d)) %*% solve(sv$u %*% diag(sqrt(sv$d)))
    S_ <- t(x_) %*% x_
  }


  statistic <- Srivastava2011_(n - 1, p, S_)
  names(statistic) <- "Standard Normal"

  parameter <- c(0, 1)
  names(parameter) <- c("Mean", "Variance")

  null.value <- 0
  names(null.value) <- "difference between the Sample Covariance Matrix and the Null Covariance Matrix Structure"
  
  p.value <- 1 - pnorm(abs(statistic))

  estimate <- S

  estimate <- if(nrow(estimate) > 5){
    NULL
  }else{
    estimate
  }

  obj <- list(statistic = statistic,
              parameter = parameter,
              p.value = p.value,
              estimate = estimate,
              null.value = null.value,
              alternative = "two.sided",
              method = "Srivastava et al. 2011 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @keywords internal
Srivastava2011_ <- function(n, p, S_){
  n * (((n ^ 2) / ((n - 1) * (n + 2))) * (tr(S_ %*% S_) - tr(S_) ^ 2 / n) / p -
    2 * (tr(S_) / p) + 1) / 2
}
