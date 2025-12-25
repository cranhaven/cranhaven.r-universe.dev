#' @export
#'
#' @references Chen, S., et al. (2010). Tests for High-Dimensional 
#' Covariance Matrices. Journal of the American Statistical Association, 
#' 105(490):810-819. \href{http://doi.org/10.1198/jasa.2010.tm09560}{10.1198/jasa.2010.tm09560}
#' @rdname structureStatistics
Chen2010 <- function(x, Sigma = "identity", ...){
  UseMethod("Chen2010")
}

#' @export
#' @keywords internal
#' @importFrom stats pnorm
#'
Chen2010.covariance <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- attributes(x)$df + 1
  S <- x

  if(Sigma[[1]] == "identity"){
    svCov <- svd(x)
    x_ <- svCov$u %*% diag(sqrt(svCov$d))
  }else{
    svCov <- svd(x)
    sv <- svd(Sigma)
    x_ <- svCov$u %*% diag(sqrt(svCov$d)) %*%
      solve(sv$u %*% diag(sqrt(sv$d)))
  }


  statistic <- Chen2010Stat(x_)
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
              method = "Chen et al. 2010 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @export
#' @keywords internal
#' @importFrom stats cov
#' @importFrom stats pnorm
#'
Chen2010.matrix <- function(x, Sigma = "identity", ...){
  p <- ncol(x)
  n <- nrow(x)
  S <- cov(x)

  if(Sigma[[1]] == "identity"){
    x_ <- x
  }else{
    sv <- svd(Sigma)
    x_ <- x %*% solve(sv$u %*% diag(sqrt(sv$d)))
  }

  statistic <- Chen2010Stat(x_)
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
              method = "Chen et al. 2010 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}
