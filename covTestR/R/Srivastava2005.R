#' @export
#'
#' @references Srivastava, M. S. (2005). Some Tests Concerning the 
#' Covariance Matrix in High Dimensional Data. Journal of the Japan 
#' Statistical Society, 35(2), 251-272. \href{http://doi.org/10.14490/jjss.35.251}{10.14490/jjss.35.251}
#' @rdname structureStatistics
Srivastava2005 <- function(x, Sigma = "identity", ...){
  UseMethod("Srivastava2005")
}

#' @export
#' @keywords internal
#' @importFrom stats pchisq
#'
Srivastava2005.covariance <- function(x, Sigma = "identity", ...){
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

  statistic <- Srivastava2005Stat(S_)
  names(statistic) <- "Standard Normal"

  parameter <- c(0, 1)
  names(parameter) <- c("Mean", "Variance")

  null.value <- 0
  names(null.value) <- "difference between the Sample \nCovariance Matrix and the Null Covariance Matrix Structure\n"

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
              method = "Srivastava 2005 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @export
#' @keywords internal
#' @importFrom stats cov
#' @importFrom stats pchisq
#'
Srivastava2005.matrix <- function(x, Sigma = "identity", ...){
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

  statistic <- Srivastava2005Stat(S_)
  names(statistic) <- "Standard Normal"

  parameter <- c(0, 1)
  names(parameter) <- c("Mean", "Variance")

  null.value <- 0
  names(null.value) <- "difference between the Sample \nCovariance Matrix and the Null Covariance Matrix Structure\n"
  
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
              method = "Srivastava 2005 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}
