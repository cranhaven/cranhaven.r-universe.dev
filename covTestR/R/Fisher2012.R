#' @export
#'
#' @references Fisher, T. J. (2012). On Testing for an Identity 
#' Covariance Matrix when the Dimensionality Equals or Exceeds the 
#' Sample Size. Journal of Statistical Planning and Infernece, 142(1), 
#' 312-326. \href{http://doi.org/10.1016/j.jspi.2011.07.019}{10.1016/j.jspi.2011.07.019}
#' @rdname structureStatistics
Fisher2012 <- function(x, Sigma = "identity", ...){
  UseMethod("Fisher2012")
}

#' @export
#' @keywords internal
#' @importFrom stats pnorm
#'
Fisher2012.covariance <- function(x, Sigma = "identity", ...){
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

  statistic <- Fisher2012_(n - 1, p, S_)
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
              method = "Fisher 2012 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @export
#' @keywords internal
#' @importFrom stats pnorm cov
#'
Fisher2012.matrix <- function(x, Sigma = "identity", ...){
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

  statistic <- Fisher2012_(n - 1, p, S_)
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
              method = "Fisher 2012 Test of Covariance Matrix Structure")
  class(obj) <- "htest"
  obj
}

#' @keywords internal
Fisher2012_ <- function(n, p, S_){
  c <- p / n
  ahat2 <- ((n ^ 2) / ((n - 1) * (n + 2) * p)) *
    (sum(diag(S_ %*% S_)) - (sum(diag(S_)) ^ 2) / n)
  gamma <- ((n ^ 5) * (n ^ 2 + n + 2)) /
    ((n + 1) * (n + 2) * (n + 4) * (n + 6) * (n - 1) * (n - 2) * (n - 3))
  ahat4 <- (gamma / p) * (sum(diag(S_ %*% S_ %*% S_ %*% S_)) -
                            (4 / n) * sum(diag(S_ %*% S_ %*% S_)) * sum(diag(S_)) -
                            ((2 * (n ^ 2) + 3 * n - 6) / (n * (n ^ 2 + n + 2))) *
                            (sum(diag(S_ %*% S_)) ^ 2) +
                            ((2 * (5 * n + 6)) / (n * (n ^ 2 + n + 2))) *
                            sum(diag(S_ %*% S_)) * (sum(diag(S_)) ^ 2) -
                            ((5 * n + 6) / ((n ^ 2) * (n ^ 2 + n + 2))) *
                            (sum(diag(S_)) ^ 4))
  (n / sqrt(8 * (c ^ 2 + 12 * c + 8))) * (ahat4 - 2 * ahat2 + 1)
}
